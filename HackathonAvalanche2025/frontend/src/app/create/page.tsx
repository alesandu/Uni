'use client';

import { useState, useEffect } from 'react';
import { useWriteContract, useWaitForTransactionReceipt, useAccount } from 'wagmi';
import { parseEther } from 'viem';
import { SKILLWAGER_ABI } from '@/utils/abi';
import { useRouter } from 'next/navigation';
import { ConnectButton } from '@rainbow-me/rainbowkit';

import { CONTRACT_ADDRESS } from '@/utils/contract';

export default function CreateMatch() {
    const router = useRouter();
    const { address } = useAccount();
    const [gamerTag, setGamerTag] = useState('');
    const [wager, setWager] = useState('0.1');
    const [bond, setBond] = useState('0.05');
    const [showErrorModal, setShowErrorModal] = useState(false);

    const { data: hash, writeContract, isPending, error: writeError } = useWriteContract();
    const { isLoading: isConfirming, isSuccess, data } = useWaitForTransactionReceipt({
        hash,
    });

    useEffect(() => {
        if (isSuccess && data) {
            // Find MatchCreated event
            const matchCreatedLog = data.logs.find(log => {
                try {
                    // MatchCreated(uint256,address,uint256,uint256)
                    // Hash: 0xab8c9f12c79b09660cc1e067055451fdf910f3c9bf0349316c5bd5dd14619acc
                    return log.topics[0] === '0xab8c9f12c79b09660cc1e067055451fdf910f3c9bf0349316c5bd5dd14619acc';
                } catch { return false; }
            });

            if (matchCreatedLog) {
                // The first indexed argument (topics[1]) is the matchId
                const matchIdHex = matchCreatedLog.topics[1];
                if (matchIdHex) {
                    const matchId = BigInt(matchIdHex).toString();
                    const timer = setTimeout(() => router.push(`/match/${matchId}`), 2000);
                    return () => clearTimeout(timer);
                }
            }

            // Fallback if event not found
            const timer = setTimeout(() => router.push('/'), 2000);
            return () => clearTimeout(timer);
        }
    }, [isSuccess, data, router]);

    const handleAmountChange = (value: string, setter: (val: string) => void) => {
        // Replace comma with dot
        const normalized = value.replace(',', '.');
        // Allow only numbers and one dot
        if (/^\d*\.?\d*$/.test(normalized)) {
            setter(normalized);
        }
    };

    const handleCreate = async () => {
        if (!address) {
            setShowErrorModal(true);
            return;
        }
        if (!gamerTag) return alert("Please enter your GamerTag");

        // 1. Register GamerTag with Backend
        try {
            await fetch(`${process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001'}/api/gamertag`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ address, tag: gamerTag }),
            });
        } catch (e) {
            console.error("Backend error", e);
            // Proceed anyway for demo
        }

        // 2. Create Match On-Chain
        writeContract({
            address: CONTRACT_ADDRESS,
            abi: SKILLWAGER_ABI,
            functionName: 'createMatch',
            args: [parseEther(wager), parseEther(bond)],
            value: parseEther(wager) + parseEther(bond),
        });
    };

    return (
        <main className="flex min-h-screen flex-col items-center p-8 bg-background text-foreground overflow-hidden relative">
            {/* Background Decorations */}
            <div className="absolute top-0 left-0 w-full h-full overflow-hidden pointer-events-none">
                <div className="absolute top-[-10%] left-[-10%] w-[40%] h-[40%] bg-primary/5 rounded-full blur-[120px]" />
                <div className="absolute bottom-[-10%] right-[-10%] w-[40%] h-[40%] bg-secondary/5 rounded-full blur-[120px]" />
            </div>

            <nav className="w-full max-w-6xl flex justify-between items-center mb-16 relative z-10">
                <div
                    className="flex items-center gap-2 cursor-pointer group"
                    onClick={() => router.push('/')}
                >
                    <div className="p-2 rounded-lg bg-surface border border-gray-800 group-hover:border-primary transition-colors">
                        <svg className="w-5 h-5 text-gray-400 group-hover:text-primary transition-colors" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M10 19l-7-7m0 0l7-7m-7 7h18"></path></svg>
                    </div>
                    <h1 className="text-xl font-bold text-gray-400 group-hover:text-white transition-colors">
                        Back to Arena
                    </h1>
                </div>
                <ConnectButton />
            </nav>

            <div className="w-full max-w-lg relative z-10">
                <div className="bg-surface/80 backdrop-blur-md p-8 rounded-2xl border border-gray-800 shadow-2xl relative overflow-hidden">
                    <div className="absolute top-0 left-0 w-full h-1 bg-gradient-to-r from-primary via-accent to-secondary" />

                    <h2 className="text-4xl font-black mb-2 text-center tracking-tight">CREATE MATCH</h2>
                    <p className="text-gray-500 text-center mb-8">Set your terms and wait for a challenger.</p>

                    <div className="space-y-6">
                        <div className="space-y-2">
                            <label className="text-xs font-bold text-primary uppercase tracking-wider ml-1">Your GamerTag</label>
                            <div className="relative group">
                                <input
                                    type="text"
                                    value={gamerTag}
                                    onChange={(e) => setGamerTag(e.target.value)}
                                    className="w-full bg-black/50 border border-gray-700 rounded-lg p-4 text-white focus:border-primary focus:shadow-[0_0_15px_rgba(0,255,157,0.1)] outline-none transition-all placeholder:text-gray-700"
                                    placeholder="e.g. xX_Slayer_Xx"
                                />
                                <div className="absolute inset-0 rounded-lg bg-gradient-to-r from-primary/20 to-accent/20 opacity-0 group-hover:opacity-100 pointer-events-none transition-opacity" />
                            </div>
                        </div>

                        <div className="grid grid-cols-2 gap-4">
                            <div className="space-y-2">
                                <label className="text-xs font-bold text-primary uppercase tracking-wider ml-1">Wager (AVAX)</label>
                                <div className="relative">
                                    <input
                                        type="text"
                                        value={wager}
                                        onChange={(e) => handleAmountChange(e.target.value, setWager)}
                                        className="w-full bg-black/50 border border-gray-700 rounded-lg p-4 text-white focus:border-primary focus:shadow-[0_0_15px_rgba(0,255,157,0.1)] outline-none transition-all font-mono text-lg"
                                        placeholder="0.1"
                                    />
                                    <span className="absolute right-4 top-1/2 -translate-y-1/2 text-gray-600 text-sm font-bold">AVAX</span>
                                </div>
                            </div>

                            <div className="space-y-2">
                                <label className="text-xs font-bold text-primary uppercase tracking-wider ml-1">Bond (AVAX)</label>
                                <div className="relative">
                                    <input
                                        type="text"
                                        value={bond}
                                        onChange={(e) => handleAmountChange(e.target.value, setBond)}
                                        className="w-full bg-black/50 border border-gray-700 rounded-lg p-4 text-white focus:border-primary focus:shadow-[0_0_15px_rgba(0,255,157,0.1)] outline-none transition-all font-mono text-lg"
                                        placeholder="0.05"
                                    />
                                    <span className="absolute right-4 top-1/2 -translate-y-1/2 text-gray-600 text-sm font-bold">AVAX</span>
                                </div>
                            </div>
                        </div>

                        <div className="bg-primary/5 border border-primary/10 rounded-lg p-4 flex gap-3 items-start">
                            <svg className="w-5 h-5 text-primary shrink-0 mt-0.5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"></path></svg>
                            <p className="text-xs text-gray-400 leading-relaxed">
                                The <strong>Bond</strong> is a security deposit returned to you after the match if you report the result honestly.
                            </p>
                        </div>

                        <button
                            onClick={handleCreate}
                            disabled={isPending || isConfirming || isSuccess}
                            className="w-full bg-primary text-black font-black text-lg py-4 rounded-lg hover:bg-white hover:shadow-[0_0_30px_rgba(0,255,157,0.4)] transition-all disabled:opacity-50 disabled:hover:bg-primary disabled:hover:shadow-none relative overflow-hidden group"
                        >
                            <span className="relative z-10">
                                {isPending ? 'CHECK WALLET...' : isConfirming ? 'CONFIRMING...' : isSuccess ? 'REDIRECTING...' : 'CREATE MATCH'}
                            </span>
                            <div className="absolute inset-0 bg-white/20 transform translate-y-full group-hover:translate-y-0 transition-transform duration-300" />
                        </button>

                        {writeError && (
                            <div className="p-3 bg-red-500/10 border border-red-500/20 rounded-lg">
                                <p className="text-red-500 text-xs text-center font-mono">
                                    Error: {writeError.message.split('\n')[0]}
                                </p>
                            </div>
                        )}
                    </div>
                </div>
            </div>
            {/* Error Modal */}
            {showErrorModal && (
                <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-black/80 backdrop-blur-sm">
                    <div className="bg-[#0a0a0a] border border-red-500/30 rounded-2xl p-8 max-w-md w-full shadow-2xl relative overflow-hidden">
                        <div className="absolute top-0 left-0 w-full h-1 bg-gradient-to-r from-transparent via-red-500 to-transparent"></div>

                        <div className="w-16 h-16 bg-red-500/10 rounded-full flex items-center justify-center mx-auto mb-6 border border-red-500/20">
                            <svg className="w-8 h-8 text-red-500" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"></path></svg>
                        </div>

                        <h3 className="text-2xl font-bold text-white mb-2 text-center">Wallet Not Connected</h3>
                        <p className="text-gray-400 text-center mb-8">
                            You must connect your wallet to create a match. Please connect your wallet using the button in the top right corner.
                        </p>

                        <button
                            onClick={() => setShowErrorModal(false)}
                            className="w-full py-3 rounded-xl bg-red-500 text-white font-bold hover:bg-red-600 transition-all shadow-[0_0_20px_rgba(239,68,68,0.3)]"
                        >
                            Close
                        </button>
                    </div>
                </div>
            )}
        </main>
    );
}
