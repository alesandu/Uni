'use client';

import { ConnectButton } from '@rainbow-me/rainbowkit';
import Link from 'next/link';
import { useReadContract, useAccount } from 'wagmi';
import { SKILLWAGER_ABI } from '@/utils/abi';
import { formatEther } from 'viem';
import { useState, useEffect } from 'react';

import MatchListItem from '@/components/MatchListItem';
import { CONTRACT_ADDRESS } from '@/utils/contract';

export default function Home() {
  const { address } = useAccount();
  const { data: ownerAddress } = useReadContract({
    address: CONTRACT_ADDRESS,
    abi: SKILLWAGER_ABI,
    functionName: 'owner',
  });

  const { data: matchCount } = useReadContract({
    address: CONTRACT_ADDRESS,
    abi: SKILLWAGER_ABI,
    functionName: 'matchIdCounter',
    query: {
      refetchInterval: 2000,
    }
  });

  const matches = matchCount && Number(matchCount) > 0
    ? Array.from({ length: Number(matchCount) - 1 }, (_, i) => BigInt(i + 1)).reverse()
    : [];

  return (
    <main className="flex min-h-screen flex-col items-center p-8 bg-background text-foreground overflow-x-hidden">
      <nav className="w-full max-w-6xl flex justify-between items-center mb-20 pt-4">
        <div className="flex items-center gap-2">
          <div className="w-3 h-3 bg-primary animate-pulse-slow" />
          <h1 className="text-2xl font-black tracking-tighter italic bg-clip-text text-transparent bg-gradient-to-r from-primary via-white to-accent">
            SKILLWAGER
          </h1>
        </div>

        <div className="flex items-center gap-6">
          {ownerAddress && address && ownerAddress === address && (
            <Link href="/jury" className="text-red-500 font-bold hover:text-red-400 transition-colors flex items-center gap-2">
              <span>⚖️</span> TRIBUNALE
            </Link>
          )}
          <ConnectButton />
        </div>
      </nav>

      <div className="w-full max-w-4xl relative">
        {/* Background Glow Effects */}
        <div className="absolute -top-20 -left-20 w-64 h-64 bg-primary/10 rounded-full blur-[100px] pointer-events-none" />
        <div className="absolute top-40 -right-20 w-96 h-96 bg-accent/10 rounded-full blur-[120px] pointer-events-none" />

        {/* Hero Section */}
        <div className="text-center mb-24 relative z-10">
          <div className="inline-block mb-6 px-4 py-1.5 rounded-full border border-primary/30 bg-primary/5 backdrop-blur-sm">
            <span className="text-primary text-xs font-bold tracking-[0.2em] uppercase">Avalanche C-Chain</span>
          </div>

          <h2 className="text-7xl font-black mb-8 tracking-tighter leading-none">
            COMPETE. <br />
            <span className="text-transparent bg-clip-text bg-gradient-to-r from-primary to-accent animate-glow">WIN.</span> EARN.
          </h2>

          <p className="text-xl text-gray-400 max-w-2xl mx-auto leading-relaxed">
            The decentralized e-sports arena. Create a match, lock your wager in the smart contract, and prove your skills on-chain.
          </p>
        </div>

        <div className="flex flex-col md:flex-row justify-between items-end mb-10 gap-6">
          <div>
            <h2 className="text-3xl font-bold flex items-center gap-3 mb-2">
              <span className="w-1.5 h-8 bg-gradient-to-b from-primary to-transparent rounded-full inline-block"></span>
              Live Arena
            </h2>
            <p className="text-gray-500 text-sm">Join an existing match or create your own.</p>
          </div>

          <Link
            href="/create"
            className="group relative px-8 py-4 bg-primary text-black font-black text-lg rounded hover:bg-white transition-all duration-300 shadow-neon hover:shadow-[0_0_40px_rgba(0,255,157,0.6)] hover:-translate-y-1 overflow-hidden"
          >
            <span className="relative z-10 flex items-center gap-2">
              CREATE MATCH
              <svg className="w-5 h-5 transition-transform group-hover:translate-x-1" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M13 7l5 5m0 0l-5 5m5-5H6"></path></svg>
            </span>
            <div className="absolute inset-0 bg-white transform scale-x-0 group-hover:scale-x-100 transition-transform origin-left duration-300" />
          </Link>
        </div>

        <div className="grid gap-4 relative z-10">
          {matches.length > 0 ? (
            matches.map((id) => (
              <MatchListItem key={id.toString()} matchId={id} />
            ))
          ) : (
            <div className="text-center py-24 bg-surface/30 backdrop-blur-md rounded-2xl border border-gray-800 border-dashed">
              <div className="w-16 h-16 bg-gray-800/50 rounded-full flex items-center justify-center mx-auto mb-4">
                <svg className="w-8 h-8 text-gray-600" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M19 11H5m14 0a2 2 0 012 2v6a2 2 0 01-2 2H5a2 2 0 01-2-2v-6a2 2 0 012-2m14 0V9a2 2 0 00-2-2M5 11V9a2 2 0 012-2m0 0V5a2 2 0 012-2h6a2 2 0 012 2v2M7 7h10"></path></svg>
              </div>
              <p className="text-gray-400 text-lg font-medium">No active matches found</p>
              <p className="text-gray-600 text-sm mt-2">Be the first to create one and start earning!</p>
            </div>
          )}

          <div className="mt-16 pt-8 border-t border-gray-800/50 flex justify-center">
            <div className="px-6 py-3 rounded-full bg-surface-light/50 border border-gray-800 backdrop-blur text-sm text-gray-400 font-mono">
              {matchCount ? `Total Matches Created: ${matchCount.toString()}` : 'Loading stats...'}
            </div>
          </div>
        </div>
      </div>
    </main>
  );
}
