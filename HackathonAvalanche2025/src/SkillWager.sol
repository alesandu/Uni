// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

contract SkillWager is ReentrancyGuard, Ownable {
    constructor() Ownable(msg.sender) {}
    // --- Enums & Structs ---

    enum MatchState {
        OPEN,
        LOCKED,
        DISPUTE_L1,
        DISPUTE_L2,
        RESOLVED,
        CANCELLED
    }

    enum Result {
        NONE,
        WIN,
        LOSS
    }

    struct Match {
        uint256 id;
        address playerA;
        address playerB;
        uint256 wagerAmount;
        uint256 bondAmount;
        MatchState state;
        Result resultA; // Result claimed by A
        Result resultB; // Result claimed by B
        uint256 lastActionTimestamp;
        
        // Dispute Resolution
        address[] jurors;
        mapping(address => Result) juryVotes;
        uint256 votesForA;
        uint256 votesForB;
        
        // Cancellation
        bool cancelVoteA;
        bool cancelVoteB;
    }

    // --- State Variables ---

    uint256 public matchIdCounter;
    mapping(uint256 => Match) public matches;
    
    // Config
    uint256 public constant TIMEOUT_WINDOW = 5 minutes;
    uint256 public constant JURY_SIZE = 3;
    
    // Mock Jurors for Hackathon Demo
    address[] public mockJurors;

    // --- Events ---

    event MatchCreated(uint256 indexed matchId, address indexed playerA, uint256 wager, uint256 bond);
    event MatchJoined(uint256 indexed matchId, address indexed playerB);
    event ResultSubmitted(uint256 indexed matchId, address indexed player, Result result);
    event MatchResolved(uint256 indexed matchId, address indexed winner);
    event MatchCancelled(uint256 indexed matchId);
    event DisputeStarted(uint256 indexed matchId);
    event JuryVoted(uint256 indexed matchId, address indexed juror, Result vote);

    // --- Modifiers ---

    modifier onlyPlayer(uint256 _matchId) {
        require(msg.sender == matches[_matchId].playerA || msg.sender == matches[_matchId].playerB, "Not a player");
        _;
    }

    modifier inState(uint256 _matchId, MatchState _state) {
        require(matches[_matchId].state == _state, "Invalid state");
        _;
    }

    // --- Core Logic ---

    function createMatch(uint256 _wagerAmount, uint256 _bondAmount) external payable nonReentrant {
        require(msg.value == _wagerAmount + _bondAmount, "Incorrect value sent");

        uint256 newMatchId = matchIdCounter++;
        Match storage newMatch = matches[newMatchId];
        
        newMatch.id = newMatchId;
        newMatch.playerA = msg.sender;
        newMatch.wagerAmount = _wagerAmount;
        newMatch.bondAmount = _bondAmount;
        newMatch.state = MatchState.OPEN;
        newMatch.lastActionTimestamp = block.timestamp;

        emit MatchCreated(newMatchId, msg.sender, _wagerAmount, _bondAmount);
    }

    function joinMatch(uint256 _matchId) external payable nonReentrant inState(_matchId, MatchState.OPEN) {
        Match storage m = matches[_matchId];
        require(msg.sender != m.playerA, "Cannot join own match");
        require(msg.value == m.wagerAmount + m.bondAmount, "Incorrect value sent");

        m.playerB = msg.sender;
        m.state = MatchState.LOCKED;
        m.lastActionTimestamp = block.timestamp;

        emit MatchJoined(_matchId, msg.sender);
    }

    function submitResult(uint256 _matchId, Result _result) external onlyPlayer(_matchId) inState(_matchId, MatchState.LOCKED) {
        Match storage m = matches[_matchId];
        require(_result == Result.WIN || _result == Result.LOSS, "Invalid result");

        if (msg.sender == m.playerA) {
            m.resultA = _result;
        } else {
            m.resultB = _result;
        }

        emit ResultSubmitted(_matchId, msg.sender, _result);
        m.lastActionTimestamp = block.timestamp;

        // Check if both submitted
        if (m.resultA != Result.NONE && m.resultB != Result.NONE) {
            _evaluateResults(_matchId);
        }
    }

    function _evaluateResults(uint256 _matchId) internal {
        Match storage m = matches[_matchId];

        // Case 1: Agreement (A Win / B Loss OR A Loss / B Win)
        if (m.resultA == Result.WIN && m.resultB == Result.LOSS) {
            _payoutWinner(_matchId, m.playerA);
        } else if (m.resultA == Result.LOSS && m.resultB == Result.WIN) {
            _payoutWinner(_matchId, m.playerB);
        } 
        // Case 2: Disagreement (Both Win or Both Loss)
        else {
            m.state = MatchState.DISPUTE_L1;
            _draftJurors(_matchId);
            emit DisputeStarted(_matchId);
        }
    }

    function _payoutWinner(uint256 _matchId, address _winner) internal {
        Match storage m = matches[_matchId];
        m.state = MatchState.RESOLVED;

        uint256 totalPot = (m.wagerAmount * 2) + (m.bondAmount * 2);
        // Winner gets Wager*2 + Bond. Loser gets Bond back.
        // Wait, standard logic: Winner takes Wager*2. Both get Bond back if honest?
        // Spec says: "Vincitore prende tutto + Bond, Perdente riprende Bond".
        // So Winner: 2*Wager + 1*Bond. Loser: 1*Bond.
        
        uint256 winnerPayout = (m.wagerAmount * 2) + m.bondAmount;
        uint256 loserPayout = m.bondAmount;

        address loser = (_winner == m.playerA) ? m.playerB : m.playerA;

        payable(_winner).transfer(winnerPayout);
        payable(loser).transfer(loserPayout);

        emit MatchResolved(_matchId, _winner);
    }

    // --- Anti-Ghosting & Safety ---

    function withdrawPreMatch(uint256 _matchId) external nonReentrant inState(_matchId, MatchState.OPEN) {
        Match storage m = matches[_matchId];
        require(msg.sender == m.playerA, "Only creator");

        m.state = MatchState.CANCELLED;
        payable(m.playerA).transfer(m.wagerAmount + m.bondAmount);
        
        emit MatchCancelled(_matchId);
    }

    function voteToCancel(uint256 _matchId) external onlyPlayer(_matchId) {
        Match storage m = matches[_matchId];
        require(m.state == MatchState.LOCKED || m.state == MatchState.DISPUTE_L1, "Cannot cancel now");

        if (msg.sender == m.playerA) m.cancelVoteA = true;
        else m.cancelVoteB = true;

        if (m.cancelVoteA && m.cancelVoteB) {
            m.state = MatchState.CANCELLED;
            payable(m.playerA).transfer(m.wagerAmount + m.bondAmount);
            payable(m.playerB).transfer(m.wagerAmount + m.bondAmount);
            emit MatchCancelled(_matchId);
        }
    }

    function claimTimeoutVictory(uint256 _matchId) external onlyPlayer(_matchId) inState(_matchId, MatchState.LOCKED) {
        Match storage m = matches[_matchId];
        require(block.timestamp > m.lastActionTimestamp + TIMEOUT_WINDOW, "Timeout not reached");

        // If I submitted WIN and opponent submitted nothing -> I win
        // If I submitted nothing and opponent submitted nothing -> Cancel? No, claim victory implies I want to win.
        
        // Logic: If I have submitted a result and opponent hasn't within timeout.
        bool isA = msg.sender == m.playerA;
        Result myResult = isA ? m.resultA : m.resultB;
        Result oppResult = isA ? m.resultB : m.resultA;

        require(myResult == Result.WIN, "Must claim win first");
        require(oppResult == Result.NONE, "Opponent already acted");

        // Change: Timeout now triggers a dispute instead of auto-win
        m.state = MatchState.DISPUTE_L1;
        _draftJurors(_matchId);
        emit DisputeStarted(_matchId);
    }

    // --- Jury System (Simplified for Hackathon) ---

    function addMockJuror(address _juror) external onlyOwner {
        mockJurors.push(_juror);
    }

    function _draftJurors(uint256 _matchId) internal {
        Match storage m = matches[_matchId];
        // For hackathon, just assign all mock jurors
        for (uint256 i = 0; i < mockJurors.length; i++) {
            m.jurors.push(mockJurors[i]);
        }
    }

    function juryVote(uint256 _matchId, Result _vote) external inState(_matchId, MatchState.DISPUTE_L1) {
        Match storage m = matches[_matchId];
        require(_isJuror(_matchId, msg.sender), "Not a juror");
        require(m.juryVotes[msg.sender] == Result.NONE, "Already voted");
        require(_vote == Result.WIN || _vote == Result.LOSS, "Invalid vote"); 
        // Vote WIN means Player A wins, LOSS means Player B wins (simplification for binary outcome relative to A)
        // Actually let's stick to: Vote for Player A or Player B.
        // Let's interpret Result.WIN as "Player A wins" and Result.LOSS as "Player B wins" for the juror? 
        // Or better: pass address of who they think won.
        
        // Let's change signature to be clearer
        // But to keep interface simple, let's say:
        // Input: 1 (Vote for A), 2 (Vote for B).
        
        if (_vote == Result.WIN) {
            m.votesForA++;
        } else if (_vote == Result.LOSS) {
            m.votesForB++;
        }
        
        m.juryVotes[msg.sender] = _vote;
        emit JuryVoted(_matchId, msg.sender, _vote);

        // Check majority
        uint256 majority = (m.jurors.length / 2) + 1;
        if (m.votesForA >= majority) {
            _resolveDispute(_matchId, m.playerA);
        } else if (m.votesForB >= majority) {
            _resolveDispute(_matchId, m.playerB);
        }
    }

    function _isJuror(uint256 _matchId, address _user) internal view returns (bool) {
        Match storage m = matches[_matchId];
        for (uint256 i = 0; i < m.jurors.length; i++) {
            if (m.jurors[i] == _user) return true;
        }
        return false;
    }

    function _resolveDispute(uint256 _matchId, address _winner) internal {
        Match storage m = matches[_matchId];
        m.state = MatchState.RESOLVED;
        
        // In dispute, loser loses Bond too (slashed to jurors? or burned? or to winner?)
        // Spec: "Minoranza slashata" refers to jurors.
        // "Baro perde Wager AND Bond".
        // So Winner takes EVERYTHING (2*Wager + 2*Bond).
        
        uint256 totalPot = (m.wagerAmount * 2) + (m.bondAmount * 2);
        payable(_winner).transfer(totalPot);
        
        emit MatchResolved(_matchId, _winner);
    }
    function adminResolveDispute(uint256 _matchId, address _winner) external onlyOwner {
        Match storage m = matches[_matchId];
        require(m.state == MatchState.DISPUTE_L1 || m.state == MatchState.DISPUTE_L2, "Not in dispute");
        require(_winner == m.playerA || _winner == m.playerB, "Invalid winner");

        m.state = MatchState.RESOLVED;

        // Payout Logic:
        // Winner gets: (Wager * 2) + Own Bond
        // Admin gets: Loser's Bond (Tax)
        // Loser gets: 0

        uint256 winnerPayout = (m.wagerAmount * 2) + m.bondAmount;
        uint256 adminFee = m.bondAmount;

        payable(_winner).transfer(winnerPayout);
        payable(owner()).transfer(adminFee);

        emit MatchResolved(_matchId, _winner);
    }
}
