'use client';

import { useReadContract } from 'wagmi';
import { SKILLWAGER_ABI } from '@/utils/abi';
import { formatEther } from 'viem';
import Link from 'next/link';
import { useEffect, useState } from 'react';

import { CONTRACT_ADDRESS } from '@/utils/contract';

export default function MatchListItem({ matchId }: { matchId: bigint }) {
    const { data: matchData } = useReadContract({
        address: CONTRACT_ADDRESS,
        abi: SKILLWAGER_ABI,
        functionName: 'matches',
        args: [matchId],
    });

    if (!matchData) return null;

    const [id, playerA, playerB, wager, bond, state] = matchData as any;
    const matchState = Number(state); // 0: OPEN

    // Only show OPEN matches in the main list for now, or show all with status
    if (matchState !== 0 && matchState !== 1) return null; // Hide resolved/cancelled

    return (
        <div className="group relative bg-surface/80 backdrop-blur-sm p-6 rounded-xl border border-gray-800 hover:border-primary transition-all duration-300 hover:shadow-neon hover:-translate-y-1">
            <div className="absolute inset-0 bg-gradient-to-r from-primary/5 to-transparent opacity-0 group-hover:opacity-100 transition-opacity rounded-xl" />

            <div className="relative flex justify-between items-center z-10">
                <div className="space-y-2">
                    <div className="flex items-center gap-3">
                        <h3 className="text-xl font-bold text-white group-hover:text-primary transition-colors">
                            Match #{matchId.toString()}
                        </h3>
                        <span className={`px-2 py-0.5 rounded text-[10px] font-bold uppercase tracking-wider ${matchState === 0
                                ? 'bg-primary/20 text-primary border border-primary/50'
                                : 'bg-yellow-500/20 text-yellow-500 border border-yellow-500/50'
                            }`}>
                            {matchState === 0 ? "Open" : "Live"}
                        </span>
                    </div>

                    <div className="flex items-center gap-4 text-sm">
                        <div className="flex items-center gap-2 text-gray-300">
                            <span className="text-gray-500">Wager:</span>
                            <span className="font-mono text-primary">{formatEther(wager)} AVAX</span>
                        </div>
                        <div className="w-1 h-1 rounded-full bg-gray-700" />
                        <div className="text-gray-500 text-xs">
                            {matchState === 0 ? "Waiting for opponent..." : "Match in progress"}
                        </div>
                    </div>
                </div>

                <Link
                    href={`/match/${matchId}`}
                    className={`
                        relative overflow-hidden px-6 py-2.5 rounded font-bold text-sm tracking-wide transition-all duration-300
                        ${matchState === 0
                            ? 'bg-primary text-black hover:bg-white hover:shadow-[0_0_20px_rgba(255,255,255,0.5)]'
                            : 'bg-transparent border border-gray-600 text-gray-300 hover:border-white hover:text-white'
                        }
                    `}
                >
                    <span className="relative z-10">{matchState === 0 ? "JOIN MATCH" : "SPECTATE"}</span>
                </Link>
            </div>
        </div>
    );
}
