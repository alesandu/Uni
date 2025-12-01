export const SKILLWAGER_ABI = [
    {
        "inputs": [],
        "stateMutability": "nonpayable",
        "type": "constructor"
    },
    {
        "inputs": [
            {
                "internalType": "address",
                "name": "owner",
                "type": "address"
            }
        ],
        "name": "OwnableInvalidOwner",
        "type": "error"
    },
    {
        "inputs": [
            {
                "internalType": "address",
                "name": "account",
                "type": "address"
            }
        ],
        "name": "OwnableUnauthorizedAccount",
        "type": "error"
    },
    {
        "inputs": [],
        "name": "ReentrancyGuardReentrantCall",
        "type": "error"
    },
    {
        "anonymous": false,
        "inputs": [
            {
                "indexed": true,
                "internalType": "uint256",
                "name": "matchId",
                "type": "uint256"
            }
        ],
        "name": "DisputeStarted",
        "type": "event"
    },
    {
        "anonymous": false,
        "inputs": [
            {
                "indexed": true,
                "internalType": "uint256",
                "name": "matchId",
                "type": "uint256"
            },
            {
                "indexed": true,
                "internalType": "address",
                "name": "juror",
                "type": "address"
            },
            {
                "indexed": false,
                "internalType": "enum SkillWager.Result",
                "name": "vote",
                "type": "uint8"
            }
        ],
        "name": "JuryVoted",
        "type": "event"
    },
    {
        "anonymous": false,
        "inputs": [
            {
                "indexed": true,
                "internalType": "uint256",
                "name": "matchId",
                "type": "uint256"
            }
        ],
        "name": "MatchCancelled",
        "type": "event"
    },
    {
        "anonymous": false,
        "inputs": [
            {
                "indexed": true,
                "internalType": "uint256",
                "name": "matchId",
                "type": "uint256"
            },
            {
                "indexed": true,
                "internalType": "address",
                "name": "playerA",
                "type": "address"
            },
            {
                "indexed": false,
                "internalType": "uint256",
                "name": "wager",
                "type": "uint256"
            },
            {
                "indexed": false,
                "internalType": "uint256",
                "name": "bond",
                "type": "uint256"
            }
        ],
        "name": "MatchCreated",
        "type": "event"
    },
    {
        "anonymous": false,
        "inputs": [
            {
                "indexed": true,
                "internalType": "uint256",
                "name": "matchId",
                "type": "uint256"
            },
            {
                "indexed": true,
                "internalType": "address",
                "name": "playerB",
                "type": "address"
            }
        ],
        "name": "MatchJoined",
        "type": "event"
    },
    {
        "anonymous": false,
        "inputs": [
            {
                "indexed": true,
                "internalType": "uint256",
                "name": "matchId",
                "type": "uint256"
            },
            {
                "indexed": true,
                "internalType": "address",
                "name": "winner",
                "type": "address"
            }
        ],
        "name": "MatchResolved",
        "type": "event"
    },
    {
        "anonymous": false,
        "inputs": [
            {
                "indexed": true,
                "internalType": "address",
                "name": "previousOwner",
                "type": "address"
            },
            {
                "indexed": true,
                "internalType": "address",
                "name": "newOwner",
                "type": "address"
            }
        ],
        "name": "OwnershipTransferred",
        "type": "event"
    },
    {
        "anonymous": false,
        "inputs": [
            {
                "indexed": true,
                "internalType": "uint256",
                "name": "matchId",
                "type": "uint256"
            },
            {
                "indexed": true,
                "internalType": "address",
                "name": "player",
                "type": "address"
            },
            {
                "indexed": false,
                "internalType": "enum SkillWager.Result",
                "name": "result",
                "type": "uint8"
            }
        ],
        "name": "ResultSubmitted",
        "type": "event"
    },
    {
        "inputs": [],
        "name": "JURY_SIZE",
        "outputs": [
            {
                "internalType": "uint256",
                "name": "",
                "type": "uint256"
            }
        ],
        "stateMutability": "view",
        "type": "function"
    },
    {
        "inputs": [],
        "name": "TIMEOUT_WINDOW",
        "outputs": [
            {
                "internalType": "uint256",
                "name": "",
                "type": "uint256"
            }
        ],
        "stateMutability": "view",
        "type": "function"
    },
    {
        "inputs": [
            {
                "internalType": "uint256",
                "name": "_matchId",
                "type": "uint256"
            },
            {
                "internalType": "address",
                "name": "_winner",
                "type": "address"
            }
        ],
        "name": "adminResolveDispute",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
    },
    {
        "inputs": [
            {
                "internalType": "address",
                "name": "_juror",
                "type": "address"
            }
        ],
        "name": "addMockJuror",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
    },
    {
        "inputs": [
            {
                "internalType": "uint256",
                "name": "_matchId",
                "type": "uint256"
            }
        ],
        "name": "claimTimeoutVictory",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
    },
    {
        "inputs": [
            {
                "internalType": "uint256",
                "name": "_wagerAmount",
                "type": "uint256"
            },
            {
                "internalType": "uint256",
                "name": "_bondAmount",
                "type": "uint256"
            }
        ],
        "name": "createMatch",
        "outputs": [],
        "stateMutability": "payable",
        "type": "function"
    },
    {
        "inputs": [
            {
                "internalType": "uint256",
                "name": "_matchId",
                "type": "uint256"
            }
        ],
        "name": "joinMatch",
        "outputs": [],
        "stateMutability": "payable",
        "type": "function"
    },
    {
        "inputs": [
            {
                "internalType": "uint256",
                "name": "_matchId",
                "type": "uint256"
            },
            {
                "internalType": "enum SkillWager.Result",
                "name": "_vote",
                "type": "uint8"
            }
        ],
        "name": "juryVote",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
    },
    {
        "inputs": [],
        "name": "matchIdCounter",
        "outputs": [
            {
                "internalType": "uint256",
                "name": "",
                "type": "uint256"
            }
        ],
        "stateMutability": "view",
        "type": "function"
    },
    {
        "inputs": [
            {
                "internalType": "uint256",
                "name": "",
                "type": "uint256"
            }
        ],
        "name": "matches",
        "outputs": [
            {
                "internalType": "uint256",
                "name": "id",
                "type": "uint256"
            },
            {
                "internalType": "address",
                "name": "playerA",
                "type": "address"
            },
            {
                "internalType": "address",
                "name": "playerB",
                "type": "address"
            },
            {
                "internalType": "uint256",
                "name": "wagerAmount",
                "type": "uint256"
            },
            {
                "internalType": "uint256",
                "name": "bondAmount",
                "type": "uint256"
            },
            {
                "internalType": "enum SkillWager.MatchState",
                "name": "state",
                "type": "uint8"
            },
            {
                "internalType": "enum SkillWager.Result",
                "name": "resultA",
                "type": "uint8"
            },
            {
                "internalType": "enum SkillWager.Result",
                "name": "resultB",
                "type": "uint8"
            },
            {
                "internalType": "uint256",
                "name": "lastActionTimestamp",
                "type": "uint256"
            },
            {
                "internalType": "uint256",
                "name": "votesForA",
                "type": "uint256"
            },
            {
                "internalType": "uint256",
                "name": "votesForB",
                "type": "uint256"
            },
            {
                "internalType": "bool",
                "name": "cancelVoteA",
                "type": "bool"
            },
            {
                "internalType": "bool",
                "name": "cancelVoteB",
                "type": "bool"
            }
        ],
        "stateMutability": "view",
        "type": "function"
    },
    {
        "inputs": [
            {
                "internalType": "uint256",
                "name": "",
                "type": "uint256"
            }
        ],
        "name": "mockJurors",
        "outputs": [
            {
                "internalType": "address",
                "name": "",
                "type": "address"
            }
        ],
        "stateMutability": "view",
        "type": "function"
    },
    {
        "inputs": [],
        "name": "owner",
        "outputs": [
            {
                "internalType": "address",
                "name": "",
                "type": "address"
            }
        ],
        "stateMutability": "view",
        "type": "function"
    },
    {
        "inputs": [],
        "name": "renounceOwnership",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
    },
    {
        "inputs": [
            {
                "internalType": "uint256",
                "name": "_matchId",
                "type": "uint256"
            },
            {
                "internalType": "enum SkillWager.Result",
                "name": "_result",
                "type": "uint8"
            }
        ],
        "name": "submitResult",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
    },
    {
        "inputs": [
            {
                "internalType": "address",
                "name": "newOwner",
                "type": "address"
            }
        ],
        "name": "transferOwnership",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
    },
    {
        "inputs": [
            {
                "internalType": "uint256",
                "name": "_matchId",
                "type": "uint256"
            }
        ],
        "name": "voteToCancel",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
    },
    {
        "inputs": [
            {
                "internalType": "uint256",
                "name": "_matchId",
                "type": "uint256"
            }
        ],
        "name": "withdrawPreMatch",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
    }
] as const;
