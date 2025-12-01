'use client';

import { useState, useEffect } from 'react';
import { useReadContract, useWriteContract, useWaitForTransactionReceipt, useAccount } from 'wagmi';
import { SKILLWAGER_ABI } from '@/utils/abi';
import { ConnectButton } from '@rainbow-me/rainbowkit';
import { CONTRACT_ADDRESS } from '@/utils/contract';
import { formatEther } from 'viem';
import Link from 'next/link';

export default function JuryDashboard() {
    const { address } = useAccount();
    const [disputedMatches, setDisputedMatches] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);

    // 1. Get Contract Owner
    const { data: ownerAddress } = useReadContract({
        address: CONTRACT_ADDRESS,
        abi: SKILLWAGER_ABI,
        functionName: 'owner',
    });

    // 2. Get Match Count
    const { data: matchCount } = useReadContract({
        address: CONTRACT_ADDRESS,
        abi: SKILLWAGER_ABI,
        functionName: 'matchIdCounter',
    });

    // 3. Fetch Matches (Simple loop for Hackathon)
    useEffect(() => {
        const fetchMatches = async () => {
            if (!matchCount) return;

            const disputes = [];
            // Check last 20 matches for disputes
            const total = Number(matchCount);
            const start = Math.max(0, total - 20);

            // Note: In a real app, use multicall or a subgraph. 
            // Here we fetch sequentially or use a helper if available.
            // For simplicity/speed in this environment, we'll rely on individual fetches 
            // or just assume we can fetch them. 
            // Actually, let's just use a simple fetch loop here since we can't easily use hooks in a loop.
            // We'll use the public client if we had access, but here we might need to rely on a different approach.
            // Let's use a "DisputeList" component that takes an ID and renders if disputed?
            // No, we want a list.

            // Alternative: Just render a list of "Potential Disputes" components that fetch their own data
            // and hide themselves if not disputed. This is inefficient but works for a demo.
        };

        // fetchMatches();
    }, [matchCount]);

    const { writeContract, data: hash, isPending } = useWriteContract();
    const { isSuccess } = useWaitForTransactionReceipt({ hash });

    // Auto-refresh on success
    useEffect(() => {
        if (isSuccess) {
            // In a real app we would re-fetch. Since we use a simple list here, 
            // we can just force a reload or rely on the component re-rendering if data changes.
            // For this demo, let's just reload the page to be sure we get fresh state.
            window.location.reload();
        }
    }, [isSuccess]);

    const resolveDispute = (matchId: bigint, winner: string) => {
        writeContract({
            address: CONTRACT_ADDRESS,
            abi: SKILLWAGER_ABI,
            functionName: 'adminResolveDispute',
            args: [matchId, winner as `0x${string}`],
        });
    };

    // Access Control
    if (ownerAddress && address && ownerAddress !== address) {
        return (
            <main className="flex min-h-screen flex-col items-center justify-center p-8 bg-black text-white">
                <h1 className="text-4xl font-bold text-red-500 mb-4">ACCESS DENIED</h1>
                <p className="text-gray-400">You are not the High Judge.</p>
                <Link href="/" className="mt-8 text-primary hover:underline">Return to Arena</Link>
            </main>
        );
    }

    return (
        <main className="flex min-h-screen flex-col items-center p-8 bg-[#0a0a0a] text-white font-sans">
            <nav className="w-full max-w-6xl flex justify-between items-center mb-12">
                <div className="flex items-center gap-3">
                    <span className="text-3xl">⚖️</span>
                    <h1 className="text-3xl font-black tracking-tighter text-transparent bg-clip-text bg-gradient-to-r from-red-500 to-orange-500">
                        HIGH TRIBUNAL
                    </h1>
                </div>
                <ConnectButton />
            </nav>

            <div className="w-full max-w-4xl">
                <div className="mb-12 text-center">
                    <h2 className="text-xl text-gray-400">Pending Disputes</h2>
                    <p className="text-sm text-gray-600">Only the owner can resolve these cases.</p>
                </div>

                <div className="space-y-6">
                    {matchCount && Number(matchCount) > 0 ? (
                        Array.from({ length: Number(matchCount) }, (_, i) => BigInt(i)).reverse().map((id) => (
                            <DisputeCard
                                key={id.toString()}
                                matchId={id}
                                onResolve={resolveDispute}
                                isPending={isPending}
                            />
                        ))
                    ) : (
                        <p className="text-center text-gray-500">No matches found.</p>
                    )}
                </div>
            </div>
        </main>
    );
}

// Sub-component to fetch and render individual match if it's a dispute
function DisputeCard({ matchId, onResolve, isPending }: { matchId: bigint, onResolve: (id: bigint, winner: string) => void, isPending: boolean }) {
    const { data: matchData } = useReadContract({
        address: CONTRACT_ADDRESS,
        abi: SKILLWAGER_ABI,
        functionName: 'matches',
        args: [matchId],
    });

    const [evidenceA, setEvidenceA] = useState<string[]>([]);
    const [evidenceB, setEvidenceB] = useState<string[]>([]);
    const [playerATag, setPlayerATag] = useState('');
    const [playerBTag, setPlayerBTag] = useState('');

    // Fetch GamerTags
    useEffect(() => {
        if (!matchData) return;
        const pA = (matchData as any)[1];
        const pB = (matchData as any)[2];
        const baseUrl = process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001';

        if (pA) fetch(`${baseUrl}/api/gamertag/${pA}`).then(res => res.json()).then(data => setPlayerATag(data.tag));
        if (pB) fetch(`${baseUrl}/api/gamertag/${pB}`).then(res => res.json()).then(data => setPlayerBTag(data.tag));
    }, [matchData]);

    useEffect(() => {
        if (!matchData) return;
        const pA = (matchData as any)[1];
        const pB = (matchData as any)[2];

        const fetchEvidence = () => {
            fetch(`${process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001'}/api/chat/${matchId}`)
                .then(res => res.json())
                .then(data => {
                    const evA: string[] = [];
                    const evB: string[] = [];

                    data.messages.forEach((m: any) => {
                        if (m.message.startsWith('[IMAGE]:')) {
                            const hash = m.message.replace('[IMAGE]:', '');
                            // Check if sender matches Player A's tag or address
                            if (m.sender === playerATag || m.sender === pA?.slice(0, 6)) {
                                evA.push(hash);
                            }
                            // Check if sender matches Player B's tag or address
                            else if (m.sender === playerBTag || m.sender === pB?.slice(0, 6)) {
                                evB.push(hash);
                            }
                            // Fallback: if sender is unknown, maybe just put it in a "General" list? 
                            // For now, let's assume tags match.
                        }
                    });
                    setEvidenceA(evA);
                    setEvidenceB(evB);
                })
                .catch(err => console.error("Failed to fetch evidence", err));
        };

        fetchEvidence();
        const interval = setInterval(fetchEvidence, 3000); // Poll every 3 seconds
        return () => clearInterval(interval);
    }, [matchId, matchData, playerATag, playerBTag]);

    if (!matchData) return null;

    const state = (matchData as any)[5]; // MatchState
    // 2 = DISPUTE_L1, 3 = DISPUTE_L2
    if (state !== 2 && state !== 3) return null;

    const playerA = (matchData as any)[1];
    const playerB = (matchData as any)[2];
    const wager = (matchData as any)[3];
    const bond = (matchData as any)[4];
    const totalPot = (BigInt(wager) * 2n) + (BigInt(bond) * 2n);

    // Judge Compensation is typically the bond amount (or a portion of it)
    // In this contract logic, the judge takes the loser's bond.
    const judgeCompensation = bond;

    const renderEvidence = (hashes: string[]) => (
        <div className="flex flex-wrap gap-2 mt-4">
            {hashes.length > 0 ? hashes.map((hash, idx) => (
                <a
                    key={idx}
                    href={hash.startsWith('QmMock')
                        ? `${process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001'}/api/ipfs/${hash}`
                        : `https://gateway.pinata.cloud/ipfs/${hash}`
                    }
                    target="_blank"
                    rel="noopener noreferrer"
                    className="block w-20 h-20 relative rounded-lg overflow-hidden border border-white/20 hover:border-primary transition-colors"
                >
                    <img
                        src={hash.startsWith('QmMock')
                            ? `${process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001'}/api/ipfs/${hash}`
                            : `https://gateway.pinata.cloud/ipfs/${hash}`
                        }
                        alt={`Evidence ${idx + 1}`}
                        className="w-full h-full object-cover"
                        onError={(e) => {
                            (e.target as HTMLImageElement).src = 'https://placehold.co/400x300?text=Mock+IPFS+Image';
                        }}
                    />
                </a>
            )) : <p className="text-xs text-gray-600 italic">No evidence.</p>}
        </div>
    );

    return (
        <div className="bg-white/5 border border-red-500/30 rounded-2xl p-6 relative overflow-hidden group hover:border-red-500/50 transition-all">
            <div className="absolute top-0 left-0 w-1 h-full bg-red-500"></div>

            <div className="flex justify-between items-start mb-6">
                <div>
                    <h3 className="text-2xl font-bold text-white mb-1">Match #{matchId.toString()}</h3>
                    <p className="text-red-400 text-sm font-mono uppercase tracking-wider">Dispute In Progress</p>
                </div>
                <div className="text-right">
                    <p className="text-gray-400 text-xs uppercase tracking-wider">Total Pot</p>
                    <p className="text-2xl font-black text-primary">{formatEther(totalPot)} AVAX</p>
                    <div className="mt-2 bg-yellow-500/10 px-3 py-1 rounded border border-yellow-500/30 inline-block">
                        <p className="text-yellow-500 text-xs font-bold uppercase">Judge Fee: {formatEther(judgeCompensation)} AVAX</p>
                    </div>
                </div>
            </div>

            <div className="grid grid-cols-2 gap-8 mb-8">
                <div className="bg-black/40 p-4 rounded-xl border border-white/5 flex flex-col h-full">
                    <div className="flex-1">
                        <p className="text-gray-500 text-xs uppercase mb-2">Player A (Challenger)</p>
                        <p className="font-mono text-sm text-white break-all mb-1">Player A</p>
                        <p className="font-mono text-xs text-gray-600 break-all mb-4">{playerA}</p>

                        <div className="border-t border-white/10 pt-2 mb-4">
                            <p className="text-xs text-gray-400 uppercase">Submitted Evidence</p>
                            {renderEvidence(evidenceA)}
                        </div>
                    </div>
                    <button
                        onClick={() => onResolve(matchId, playerA)}
                        disabled={isPending}
                        className="w-full py-3 bg-primary/10 border border-primary/50 text-primary font-bold rounded-lg hover:bg-primary hover:text-black transition-all mt-auto"
                    >
                        🏆 DECLARE WINNER
                    </button>
                </div>

                <div className="bg-black/40 p-4 rounded-xl border border-white/5 flex flex-col h-full">
                    <div className="flex-1">
                        <p className="text-gray-500 text-xs uppercase mb-2">Player B (Opponent)</p>
                        <p className="font-mono text-sm text-white break-all mb-1">Player B</p>
                        <p className="font-mono text-xs text-gray-600 break-all mb-4">{playerB}</p>

                        <div className="border-t border-white/10 pt-2 mb-4">
                            <p className="text-xs text-gray-400 uppercase">Submitted Evidence</p>
                            {renderEvidence(evidenceB)}
                        </div>
                    </div>
                    <button
                        onClick={() => onResolve(matchId, playerB)}
                        disabled={isPending}
                        className="w-full py-3 bg-secondary/10 border border-secondary/50 text-secondary font-bold rounded-lg hover:bg-secondary hover:text-black transition-all mt-auto"
                    >
                        🏆 DECLARE WINNER
                    </button>
                </div>
            </div>

            <div className="border-t border-white/10 pt-4">
                <div className="flex gap-4">
                    <a
                        href={`${process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001'}/api/chat/${matchId}`}
                        target="_blank"
                        className="text-xs text-gray-500 hover:text-white underline"
                    >
                        View Full Chat Logs (JSON)
                    </a>
                </div>
            </div>
        </div>
    );
}
