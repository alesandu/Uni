'use client';

import { useEffect, useState } from 'react';
import { useParams } from 'next/navigation';
import { useAccount, useReadContract, useWriteContract, useWaitForTransactionReceipt } from 'wagmi';
import { SKILLWAGER_ABI } from '@/utils/abi';
import { formatEther, parseEther } from 'viem';
import { ConnectButton } from '@rainbow-me/rainbowkit';
import ChatBox from '@/components/ChatBox';
import { CONTRACT_ADDRESS } from '@/utils/contract';

export default function MatchRoom() {
    const params = useParams();
    const matchId = params.id as string;
    const { address } = useAccount();
    const [playerATag, setPlayerATag] = useState('');
    const [playerBTag, setPlayerBTag] = useState('');
    const [opponentTag, setOpponentTag] = useState('');
    const [showResultModal, setShowResultModal] = useState(false);
    const [showErrorModal, setShowErrorModal] = useState(false);
    const [pendingResult, setPendingResult] = useState<number | null>(null);
    const [uploading, setUploading] = useState(false);
    const [ipfsHash, setIpfsHash] = useState('');
    const [previewUrl, setPreviewUrl] = useState<string | null>(null);

    // Contract Read
    const { data: matchData, refetch, isError } = useReadContract({
        address: CONTRACT_ADDRESS,
        abi: SKILLWAGER_ABI,
        functionName: 'matches',
        args: [BigInt(matchId)],
        query: {
            retry: false,
            refetchInterval: 2000,
        }
    });

    const { data: ownerAddress } = useReadContract({
        address: CONTRACT_ADDRESS,
        abi: SKILLWAGER_ABI,
        functionName: 'owner',
    });

    // Contract Writes
    const { writeContract, data: hash, isPending, error: writeError } = useWriteContract();
    const { isSuccess } = useWaitForTransactionReceipt({ hash });

    useEffect(() => {
        if (isSuccess) refetch();
    }, [isSuccess, refetch]);

    // Fetch GamerTags
    useEffect(() => {
        if (matchData) {
            const pA = (matchData as any)[1];
            const pB = (matchData as any)[2];
            const baseUrl = process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001';

            if (pA && pA !== '0x0000000000000000000000000000000000000000') {
                fetch(`${baseUrl}/api/gamertag/${pA}`)
                    .then(res => res.json())
                    .then(data => setPlayerATag(data.tag || 'Unknown'));
            }

            if (pB && pB !== '0x0000000000000000000000000000000000000000') {
                fetch(`${baseUrl}/api/gamertag/${pB}`)
                    .then(res => res.json())
                    .then(data => setPlayerBTag(data.tag || 'Unknown'));
            }
        }
    }, [matchData]);

    if (isError) return <div className="p-8 text-center text-red-500">Error loading match. Please try again.</div>;
    if (!matchData) return <div className="p-8 text-center">Loading Match...</div>;

    const [id, playerA, playerB, wager, bond, state, resA, resB, lastAction, votesA, votesB] = matchData as any;
    const matchState = Number(state); // 0: OPEN, 1: LOCKED, 2: DISPUTE_L1, 3: DISPUTE_L2, 4: RESOLVED, 5: CANCELLED

    const isPlayer = address === playerA || address === playerB;

    // Actions
    const joinMatch = async () => {
        if (!address) {
            setShowErrorModal(true);
            return;
        }
        if (!opponentTag.trim()) {
            alert('Please enter your Gamer Tag');
            return;
        }

        // Save Gamer Tag
        const baseUrl = process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001';
        try {
            await fetch(`${baseUrl}/api/gamertag`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ address, tag: opponentTag })
            });
        } catch (e) {
            console.error('Failed to save gamer tag', e);
        }

        const totalValue = BigInt(wager) + BigInt(bond);
        console.log('Joining Match:', {
            matchId,
            wager: wager.toString(),
            bond: bond.toString(),
            totalValue: totalValue.toString(),
            playerA
        });

        writeContract({
            address: CONTRACT_ADDRESS,
            abi: SKILLWAGER_ABI,
            functionName: 'joinMatch',
            args: [BigInt(matchId)],
            value: totalValue,
            gas: BigInt(500000), // Force gas limit to avoid estimation errors
        });
    };

    const submitResult = (result: number) => { // 1: WIN, 2: LOSS
        setPendingResult(result);
        setShowResultModal(true);
    };

    const confirmResult = () => {
        if (pendingResult === null) return;
        writeContract({
            address: CONTRACT_ADDRESS,
            abi: SKILLWAGER_ABI,
            functionName: 'submitResult',
            args: [BigInt(matchId), pendingResult],
            gas: BigInt(500000), // Fix for race condition in gas estimation
        });
        setShowResultModal(false);
    };

    const voteCancel = () => {
        writeContract({
            address: CONTRACT_ADDRESS,
            abi: SKILLWAGER_ABI,
            functionName: 'voteToCancel',
            args: [BigInt(matchId)],
        });
    };

    const claimTimeout = () => {
        writeContract({
            address: CONTRACT_ADDRESS,
            abi: SKILLWAGER_ABI,
            functionName: 'claimTimeoutVictory',
            args: [BigInt(matchId)],
        });
    };

    const handleFileUpload = async (e: React.ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (!file) return;

        // Create local preview immediately
        const objectUrl = URL.createObjectURL(file);
        setPreviewUrl(objectUrl);

        setUploading(true);
        const formData = new FormData();
        formData.append('file', file);

        try {
            const baseUrl = process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001';
            const res = await fetch(`${baseUrl}/api/upload`, {
                method: 'POST',
                body: formData,
            });
            const data = await res.json();
            if (data.ipfsHash) {
                setIpfsHash(data.ipfsHash);

                // Also send to chat so Admin can see it
                const senderName = isPlayer ? (address === playerA ? playerATag : playerBTag) : 'Spectator';
                await fetch(`${baseUrl}/api/chat/message`, {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                        matchId,
                        sender: senderName || address?.slice(0, 6),
                        message: `[IMAGE]:${data.ipfsHash}`
                    })
                });
            }
        } catch (error) {
            console.error('Upload failed', error);
            alert('Upload failed');
        } finally {
            setUploading(false);
        }
    };

    return (
        <main className="flex min-h-screen flex-col p-8 bg-[#0a0a0a] text-white font-sans selection:bg-primary selection:text-black">
            {/* Background Gradients */}
            <div className="fixed inset-0 z-0 pointer-events-none">
                <div className="absolute top-[-10%] left-[-10%] w-[40%] h-[40%] bg-primary/5 rounded-full blur-[120px]"></div>
                <div className="absolute bottom-[-10%] right-[-10%] w-[40%] h-[40%] bg-blue-500/5 rounded-full blur-[120px]"></div>
            </div>

            <nav className="relative z-10 flex justify-between items-center mb-12">
                <div className="flex items-center gap-6">
                    <button
                        onClick={() => window.location.href = '/'}
                        className="group flex items-center gap-2 text-gray-400 hover:text-white transition-colors"
                    >
                        <span className="group-hover:-translate-x-1 transition-transform">←</span>
                        Back to Lobby
                    </button>
                    <div className="h-6 w-px bg-gray-800"></div>
                    <h1 className="text-xl font-bold tracking-tight">
                        MATCH <span className="text-primary">#{matchId}</span>
                    </h1>
                </div>
                <ConnectButton />
            </nav>

            <div className="relative z-10 grid grid-cols-1 lg:grid-cols-3 gap-8 max-w-7xl mx-auto w-full">
                {/* Left: Match Info & Actions */}
                <div className="lg:col-span-2 space-y-6">

                    {/* Status Banner */}
                    <div className={`p-6 rounded-2xl border backdrop-blur-md transition-all duration-300 ${matchState === 0 ? 'bg-blue-500/5 border-blue-500/20 shadow-[0_0_30px_rgba(59,130,246,0.1)]' :
                        matchState === 1 ? 'bg-yellow-500/5 border-yellow-500/20 shadow-[0_0_30px_rgba(234,179,8,0.1)]' :
                            matchState === 2 ? 'bg-red-500/5 border-red-500/20 shadow-[0_0_30px_rgba(239,68,68,0.1)]' :
                                matchState === 4 ? 'bg-green-500/5 border-green-500/20 shadow-[0_0_30px_rgba(34,197,94,0.1)]' :
                                    'bg-gray-500/5 border-gray-500/20'
                        }`}>
                        <div className="flex items-center gap-3 mb-2">
                            <div className={`w-2.5 h-2.5 rounded-full ${matchState === 0 ? 'bg-blue-500 animate-pulse shadow-[0_0_10px_rgba(59,130,246,0.8)]' :
                                matchState === 1 ? 'bg-yellow-500 animate-pulse shadow-[0_0_10px_rgba(234,179,8,0.8)]' :
                                    matchState === 2 ? 'bg-red-500 shadow-[0_0_10px_rgba(239,68,68,0.8)]' :
                                        matchState === 4 ? 'bg-green-500 shadow-[0_0_10px_rgba(34,197,94,0.8)]' :
                                            'bg-gray-500'
                                }`}></div>
                            <h2 className={`text-lg font-bold tracking-widest uppercase ${matchState === 0 ? 'text-blue-400' :
                                matchState === 1 ? 'text-yellow-400' :
                                    matchState === 2 ? 'text-red-400' :
                                        matchState === 4 ? 'text-green-400' :
                                            'text-gray-400'
                                }`}>
                                {['Open', 'Locked', 'Dispute', 'Appeal', 'Resolved', 'Cancelled'][matchState]}
                            </h2>
                        </div>
                        <p className="text-gray-400 text-sm leading-relaxed ml-6">
                            {matchState === 0 ? "Waiting for a worthy opponent to accept the challenge." :
                                matchState === 1 ? "Match is live! Good luck, have fun." :
                                    matchState === 2 ? "Under review by the SkillWager Jury." :
                                        matchState === 4 ? (
                                            <span>
                                                Match Resolved. Winner: <span className="text-green-400 font-bold">
                                                    Check contract events for official winner.
                                                </span>
                                            </span>
                                        ) :
                                            "This match has concluded."}
                        </p>
                    </div>

                    {/* Players */}
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                        {/* Challenger Card */}
                        <div className="bg-white/5 backdrop-blur-sm p-8 rounded-2xl border border-white/10 relative overflow-hidden group hover:border-primary/30 transition-all duration-300">
                            <div className="absolute top-0 right-0 p-6 opacity-5 group-hover:opacity-10 transition-opacity transform group-hover:scale-110 duration-500">
                                <svg className="w-32 h-32 text-primary" fill="currentColor" viewBox="0 0 24 24"><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 3c1.66 0 3 1.34 3 3s-1.34 3-3 3-3-1.34-3-3 1.34-3 3-3zm0 14.2c-2.5 0-4.71-1.28-6-3.22.03-1.99 4-3.08 6-3.08 1.99 0 5.97 1.09 6 3.08-1.29 1.94-3.5 3.22-6 3.22z" /></svg>
                            </div>
                            <div className="relative z-10">
                                <p className="text-[10px] font-bold text-gray-500 tracking-[0.2em] mb-3 uppercase">Challenger</p>
                                <p className="text-2xl font-bold text-white mb-2 truncate">
                                    {playerATag || 'Loading...'}
                                    {playerA === address && <span className="text-xs text-primary ml-2 bg-primary/10 px-2 py-0.5 rounded align-middle">YOU</span>}
                                </p>
                                <p className="text-xs font-mono text-gray-500 bg-black/30 inline-block px-3 py-1.5 rounded-lg border border-white/5">
                                    {playerA.slice(0, 6)}...{playerA.slice(-4)}
                                </p>
                            </div>
                        </div>

                        {/* Opponent Card */}
                        <div className="bg-white/5 backdrop-blur-sm p-8 rounded-2xl border border-white/10 relative overflow-hidden group hover:border-blue-500/30 transition-all duration-300">
                            <div className="absolute top-0 right-0 p-6 opacity-5 group-hover:opacity-10 transition-opacity transform group-hover:scale-110 duration-500">
                                <svg className="w-32 h-32 text-blue-500" fill="currentColor" viewBox="0 0 24 24"><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 3c1.66 0 3 1.34 3 3s-1.34 3-3 3-3-1.34-3-3 1.34-3 3-3zm0 14.2c-2.5 0-4.71-1.28-6-3.22.03-1.99 4-3.08 6-3.08 1.99 0 5.97 1.09 6 3.08-1.29 1.94-3.5 3.22-6 3.22z" /></svg>
                            </div>
                            <div className="relative z-10">
                                <p className="text-[10px] font-bold text-gray-500 tracking-[0.2em] mb-3 uppercase">Opponent</p>
                                {playerB === '0x0000000000000000000000000000000000000000' ? (
                                    <div className="animate-pulse">
                                        <p className="text-2xl font-bold text-gray-700 mb-2">Waiting...</p>
                                        <p className="text-xs text-gray-800">---</p>
                                    </div>
                                ) : (
                                    <>
                                        <p className="text-2xl font-bold text-white mb-2 truncate">
                                            {playerBTag || 'Loading...'}
                                            {playerB === address && <span className="text-xs text-blue-400 ml-2 bg-blue-500/10 px-2 py-0.5 rounded align-middle">YOU</span>}
                                        </p>
                                        <p className="text-xs font-mono text-gray-500 bg-black/30 inline-block px-3 py-1.5 rounded-lg border border-white/5">
                                            {playerB.slice(0, 6)}...{playerB.slice(-4)}
                                        </p>
                                    </>
                                )}
                            </div>
                        </div>
                    </div>

                    {/* Action Area */}
                    <div className="bg-gradient-to-b from-white/5 to-transparent p-8 rounded-2xl border border-white/10 shadow-2xl">
                        {matchState === 0 && address !== playerA && (
                            <div className="text-center py-4">
                                {ownerAddress === address ? (
                                    <div className="p-4 bg-red-500/10 border border-red-500/20 rounded-xl">
                                        <p className="text-red-400 font-bold">ADMIN RESTRICTED</p>
                                        <p className="text-sm text-gray-400">As the High Judge, you cannot participate in matches.</p>
                                    </div>
                                ) : (
                                    <>
                                        <div className="mb-8">
                                            <p className="text-gray-400 mb-2 text-sm uppercase tracking-wider">Entry Fee</p>
                                            <p className="text-4xl font-black text-white tracking-tight">
                                                {formatEther(wager + bond)} <span className="text-primary text-2xl">AVAX</span>
                                            </p>
                                            <p className="text-xs text-gray-500 mt-3 bg-white/5 inline-block px-4 py-2 rounded-full">
                                                {formatEther(wager)} Wager + {formatEther(bond)} Bond (Refundable)
                                            </p>
                                        </div>
                                        <div className="mb-4">
                                            <label className="block text-xs text-gray-500 uppercase tracking-wider mb-2">Your Gamer Tag</label>
                                            <input
                                                type="text"
                                                value={opponentTag}
                                                onChange={(e) => setOpponentTag(e.target.value)}
                                                placeholder="Enter your tag..."
                                                className="w-full bg-black/40 border border-white/10 rounded-xl px-4 py-3 text-white focus:border-primary focus:outline-none transition-colors text-center font-bold"
                                            />
                                        </div>
                                        <button
                                            onClick={joinMatch}
                                            disabled={isPending || !opponentTag.trim()}
                                            className="bg-primary text-black font-bold py-4 px-12 rounded-xl hover:bg-[#00ff9d] hover:scale-105 transition-all shadow-[0_0_20px_rgba(0,255,157,0.3)] hover:shadow-[0_0_40px_rgba(0,255,157,0.5)] w-full md:w-auto disabled:opacity-50 disabled:hover:scale-100"
                                        >
                                            {isPending ? 'Processing...' : 'JOIN MATCH NOW'}
                                        </button>
                                    </>
                                )}
                            </div>
                        )}

                        {matchState === 1 && isPlayer && (
                            <div className="space-y-8">
                                <div className="text-center">
                                    <h3 className="font-bold text-xl text-white mb-2">Report Result</h3>
                                    <p className="text-gray-400 text-sm">Honesty is enforced by the bond system.</p>
                                </div>
                                <div className="grid grid-cols-2 gap-6">
                                    <button
                                        onClick={() => submitResult(1)}
                                        disabled={isPending}
                                        className="group relative overflow-hidden bg-primary/10 border border-primary/50 text-primary hover:bg-primary hover:text-black font-bold py-8 rounded-2xl transition-all duration-300"
                                    >
                                        <span className="relative z-10 text-xl">I WON 🏆</span>
                                        <div className="absolute inset-0 bg-primary/20 transform scale-x-0 group-hover:scale-x-100 transition-transform origin-left duration-300"></div>
                                    </button>
                                    <button
                                        onClick={() => submitResult(2)}
                                        disabled={isPending}
                                        className="group relative overflow-hidden bg-red-500/10 border border-red-500/50 text-red-500 hover:bg-red-500 hover:text-white font-bold py-8 rounded-2xl transition-all duration-300"
                                    >
                                        <span className="relative z-10 text-xl">I LOST 💀</span>
                                        <div className="absolute inset-0 bg-red-500/20 transform scale-x-0 group-hover:scale-x-100 transition-transform origin-left duration-300"></div>
                                    </button>
                                </div>

                                {writeError && (
                                    <div className="p-3 bg-red-500/10 border border-red-500/20 rounded-lg text-center">
                                        <p className="text-red-500 text-xs font-mono">
                                            Error: {writeError.message.split('\n')[0]}
                                        </p>
                                    </div>
                                )}

                                <div className="pt-6 border-t border-white/10 flex justify-between items-center px-4">
                                    <button onClick={voteCancel} className="text-xs text-gray-500 hover:text-white underline transition-colors">
                                        Propose Mutual Cancellation
                                    </button>
                                    <button onClick={claimTimeout} className="text-xs text-red-900/50 hover:text-red-500 transition-colors">
                                        Claim Timeout
                                    </button>
                                </div>
                            </div>
                        )}

                        {matchState === 2 && (
                            <div className="text-center py-8">
                                <div className="w-20 h-20 bg-red-500/10 rounded-full flex items-center justify-center mx-auto mb-6 border border-red-500/20">
                                    <span className="text-3xl">⚖️</span>
                                </div>
                                <p className="text-red-400 font-bold text-xl mb-2 tracking-wide">DISPUTE IN PROGRESS</p>
                                <p className="text-sm text-gray-400 max-w-md mx-auto leading-relaxed">
                                    Jurors are currently reviewing the evidence. Please ensure you have uploaded any relevant screenshots or chat logs to IPFS.
                                </p>

                                <div className="max-w-xs mx-auto mt-6">
                                    <label className="block w-full cursor-pointer group">
                                        <input
                                            type="file"
                                            onChange={handleFileUpload}
                                            disabled={uploading}
                                            className="hidden"
                                            accept="image/*"
                                        />
                                        <div className="flex flex-col items-center justify-center w-full h-32 border-2 border-dashed border-gray-600 rounded-2xl group-hover:border-primary group-hover:bg-primary/5 transition-all">
                                            <div className="flex flex-col items-center justify-center pt-5 pb-6">
                                                <svg className="w-8 h-8 mb-3 text-gray-400 group-hover:text-primary transition-colors" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M15 13l-3-3m0 0l-3 3m3-3v12"></path></svg>
                                                <p className="mb-2 text-sm text-gray-400 group-hover:text-primary transition-colors"><span className="font-semibold">Click to upload</span> evidence</p>
                                                <p className="text-xs text-gray-500">PNG, JPG (MAX. 5MB)</p>
                                            </div>
                                        </div>
                                    </label>

                                    {uploading && (
                                        <div className="mt-4 text-center">
                                            <div className="inline-block w-6 h-6 border-2 border-primary border-t-transparent rounded-full animate-spin"></div>
                                            <p className="text-xs text-primary mt-2">Uploading to IPFS...</p>
                                        </div>
                                    )}

                                    {(ipfsHash || previewUrl) && (
                                        <div className="mt-6 p-4 bg-black/40 border border-white/10 rounded-xl overflow-hidden">
                                            <p className="text-green-500 text-xs font-bold mb-3 flex items-center gap-2">
                                                <span className="w-2 h-2 bg-green-500 rounded-full"></span>
                                                Evidence Uploaded
                                            </p>
                                            <div className="relative aspect-video w-full rounded-lg overflow-hidden border border-white/10 bg-black/50">
                                                <img
                                                    src={previewUrl || (ipfsHash.startsWith('QmMock')
                                                        ? `${process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001'}/api/ipfs/${ipfsHash}`
                                                        : `https://gateway.pinata.cloud/ipfs/${ipfsHash}`
                                                    )}
                                                    alt="Evidence"
                                                    className="object-contain w-full h-full"
                                                />
                                            </div>
                                            {ipfsHash && (
                                                <a
                                                    href={ipfsHash.startsWith('QmMock')
                                                        ? `${process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001'}/api/ipfs/${ipfsHash}`
                                                        : `https://gateway.pinata.cloud/ipfs/${ipfsHash}`
                                                    }
                                                    target="_blank"
                                                    rel="noopener noreferrer"
                                                    className="block mt-3 text-center text-xs text-gray-500 hover:text-white transition-colors"
                                                >
                                                    View on IPFS ↗
                                                </a>
                                            )}
                                        </div>
                                    )}
                                </div>
                            </div>
                        )}

                        {/* Spectator View */}
                        {!isPlayer && matchState !== 0 && (
                            <div className="text-center py-12">
                                <p className="text-gray-500 italic text-lg">You are spectating this match.</p>
                            </div>
                        )}
                    </div>
                </div>

                {/* Right: Chat & Stakes */}
                <div className="lg:col-span-1 space-y-6">
                    <ChatBox
                        matchId={matchId}
                        userAddress={address || ''}
                        gamerTag={isPlayer ? (address === playerA ? playerATag : playerBTag) : 'Spectator'}
                        isPlayer={isPlayer}
                    />

                    <div className="p-6 bg-white/5 backdrop-blur-sm rounded-2xl border border-white/10">
                        <h3 className="font-bold text-xs text-gray-500 mb-6 uppercase tracking-[0.2em]">Stakes</h3>
                        <div className="space-y-4">
                            <div className="flex justify-between items-center p-4 bg-black/40 rounded-xl border border-white/5">
                                <span className="text-gray-400 text-sm">Wager (Each)</span>
                                <span className="text-white font-mono">{formatEther(wager)} AVAX</span>
                            </div>
                            <div className="flex justify-between items-center p-4 bg-black/40 rounded-xl border border-white/5">
                                <span className="text-gray-400 text-sm">Bond (Refundable)</span>
                                <span className="text-white font-mono">{formatEther(bond)} AVAX</span>
                            </div>
                            <div className="h-px bg-gradient-to-r from-transparent via-white/10 to-transparent my-4"></div>
                            <div className="flex justify-between items-center">
                                <span className="text-primary font-bold tracking-wide">PRIZE POOL</span>
                                <span className="text-primary font-black font-mono text-2xl drop-shadow-[0_0_10px_rgba(0,255,157,0.5)]">
                                    {formatEther(wager * 2n)} AVAX
                                </span>
                            </div>
                            <div className="text-right">
                                <span className="text-[10px] text-gray-600 uppercase tracking-wider">Total Value Locked: {formatEther((wager * 2n) + (bond * 2n))} AVAX</span>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            {/* Confirmation Modal */}
            {
                showResultModal && (
                    <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-black/80 backdrop-blur-sm">
                        <div className="bg-[#0a0a0a] border border-white/10 rounded-2xl p-8 max-w-md w-full shadow-2xl relative overflow-hidden">
                            <div className="absolute top-0 left-0 w-full h-1 bg-gradient-to-r from-transparent via-primary to-transparent"></div>

                            <h3 className="text-2xl font-bold text-white mb-4">Confirm Result</h3>

                            <div className="bg-primary/5 border border-primary/20 rounded-xl p-4 mb-6">
                                <p className="text-primary text-sm font-mono mb-2">ACTION REQUIRED</p>
                                <p className="text-gray-300 text-sm">
                                    You are about to submit your result. This action requires a wallet signature to verify your identity and the outcome.
                                </p>
                            </div>

                            <p className="text-gray-400 text-sm mb-8">
                                Please check your wallet extension to sign the transaction after clicking Confirm.
                            </p>

                            <div className="flex gap-4">
                                <button
                                    onClick={() => setShowResultModal(false)}
                                    className="flex-1 px-6 py-3 rounded-xl border border-white/10 text-gray-400 hover:text-white hover:bg-white/5 transition-all font-bold"
                                >
                                    Cancel
                                </button>
                                <button
                                    onClick={confirmResult}
                                    className="flex-1 px-6 py-3 rounded-xl bg-primary text-black font-bold hover:bg-[#00ff9d] hover:shadow-[0_0_20px_rgba(0,255,157,0.3)] transition-all"
                                >
                                    Confirm & Sign
                                </button>
                            </div>
                        </div>
                    </div>
                )
            }

            {/* Error Modal */}
            {
                showErrorModal && (
                    <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-black/80 backdrop-blur-sm">
                        <div className="bg-[#0a0a0a] border border-red-500/30 rounded-2xl p-8 max-w-md w-full shadow-2xl relative overflow-hidden">
                            <div className="absolute top-0 left-0 w-full h-1 bg-gradient-to-r from-transparent via-red-500 to-transparent"></div>

                            <div className="w-16 h-16 bg-red-500/10 rounded-full flex items-center justify-center mx-auto mb-6 border border-red-500/20">
                                <svg className="w-8 h-8 text-red-500" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"></path></svg>
                            </div>

                            <h3 className="text-2xl font-bold text-white mb-2 text-center">Wallet Not Connected</h3>
                            <p className="text-gray-400 text-center mb-8">
                                You must connect your wallet to join a match. Please connect your wallet using the button in the top right corner.
                            </p>

                            <button
                                onClick={() => setShowErrorModal(false)}
                                className="w-full py-3 rounded-xl bg-red-500 text-white font-bold hover:bg-red-600 transition-all shadow-[0_0_20px_rgba(239,68,68,0.3)]"
                            >
                                Close
                            </button>
                        </div>
                    </div>
                )
            }
        </main >
    );
}
