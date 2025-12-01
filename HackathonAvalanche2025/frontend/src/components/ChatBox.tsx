'use client';

import { useEffect, useState, useRef } from 'react';
import { io, Socket } from 'socket.io-client';

interface Message {
    sender: string;
    message: string;
    timestamp: number;
}

interface ChatBoxProps {
    matchId: string;
    userAddress: string;
    gamerTag: string;
    isPlayer: boolean;
}

export default function ChatBox({ matchId, userAddress, gamerTag, isPlayer }: ChatBoxProps) {
    const [socket, setSocket] = useState<Socket | null>(null);
    const [messages, setMessages] = useState<Message[]>([]);
    const [input, setInput] = useState('');
    const fileInputRef = useRef<HTMLInputElement>(null);
    const scrollRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
        // Connect to backend
        const newSocket = io(process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001');
        setSocket(newSocket);

        newSocket.emit('join_match', matchId);

        // Load history
        fetch(`${process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001'}/api/chat/${matchId}`)
            .then(res => res.json())
            .then(data => setMessages(data.messages));

        newSocket.on('new_message', (msg: Message) => {
            setMessages(prev => [...prev, msg]);
            // Scroll to bottom
            setTimeout(() => scrollRef.current?.scrollIntoView({ behavior: 'smooth' }), 100);
        });

        return () => {
            newSocket.disconnect();
        };
    }, [matchId]);

    const sendMessage = () => {
        if (!input.trim() || !socket || !isPlayer) return;

        const senderName = gamerTag || userAddress.slice(0, 6);
        socket.emit('send_message', { matchId, sender: senderName, message: input });
        setInput('');
    };

    const handleFileUpload = async (e: React.ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (!file || !socket || !isPlayer) return;

        const formData = new FormData();
        formData.append('file', file);

        try {
            const res = await fetch(`${process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001'}/api/upload`, {
                method: 'POST',
                body: formData,
            });
            const data = await res.json();

            if (data.ipfsHash) {
                const senderName = gamerTag || userAddress.slice(0, 6);
                // Send as special formatted message
                const imgMessage = `[IMAGE]:${data.ipfsHash}`;
                socket.emit('send_message', { matchId, sender: senderName, message: imgMessage });
            }
        } catch (err) {
            console.error('Upload failed', err);
            alert('Failed to upload image');
        }
    };

    const isImageMessage = (msg: string) => msg.startsWith('[IMAGE]:');
    const getImageHash = (msg: string) => msg.replace('[IMAGE]:', '');

    return (
        <div className="flex flex-col h-[400px] bg-surface/80 backdrop-blur-md border border-gray-800 rounded-xl overflow-hidden shadow-2xl relative">
            <div className="absolute top-0 left-0 w-full h-0.5 bg-gradient-to-r from-transparent via-primary to-transparent opacity-50" />

            <div className="bg-black/40 p-3 border-b border-gray-800 flex justify-between items-center">
                <div className="flex items-center gap-2">
                    <div className="w-2 h-2 rounded-full bg-primary animate-pulse" />
                    <span className="font-bold text-xs tracking-widest text-gray-400 uppercase">Live Comms</span>
                </div>
                <div className="text-[10px] text-gray-600 font-mono">ENCRYPTED</div>
            </div>

            <div className="flex-1 overflow-y-auto p-4 space-y-4 custom-scrollbar">
                {messages.map((msg, i) => (
                    <div key={i} className={`flex flex-col ${msg.sender === (gamerTag || userAddress.slice(0, 6)) ? 'items-end' : 'items-start'}`}>
                        <span className="text-[10px] text-gray-500 mb-1 font-mono uppercase tracking-wider">{msg.sender}</span>
                        <div className={`
                            relative p-3 rounded-lg max-w-[85%] text-sm leading-relaxed
                            ${msg.sender === (gamerTag || userAddress.slice(0, 6))
                                ? 'bg-primary/10 text-primary border border-primary/20 rounded-tr-none'
                                : 'bg-surface-light text-gray-200 border border-gray-700 rounded-tl-none'
                            }
                        `}>
                            {isImageMessage(msg.message) ? (
                                <div className="mt-1">
                                    <img
                                        src={getImageHash(msg.message).startsWith('QmMock')
                                            ? `${process.env.NEXT_PUBLIC_BACKEND_URL || 'http://localhost:3001'}/api/ipfs/${getImageHash(msg.message)}`
                                            : `https://gateway.pinata.cloud/ipfs/${getImageHash(msg.message)}`
                                        }
                                        alt="Evidence"
                                        className="rounded-lg max-h-40 border border-white/10"
                                        onError={(e) => {
                                            // Fallback for local dev/mocking if needed, or just show error
                                            (e.target as HTMLImageElement).src = 'https://placehold.co/400x300?text=Mock+IPFS+Image';
                                        }}
                                    />
                                </div>
                            ) : (
                                msg.message
                            )}
                            {/* Decorative corner accent */}
                            <div className={`absolute top-0 w-2 h-2 border-t border-current opacity-50 ${msg.sender === (gamerTag || userAddress.slice(0, 6)) ? 'right-0 border-r' : 'left-0 border-l'}`} />
                        </div>
                    </div>
                ))}
                <div ref={scrollRef} />
            </div>

            <div className="p-3 bg-black/40 border-t border-gray-800 flex gap-2 backdrop-blur-sm">
                {isPlayer ? (
                    <>
                        <input
                            type="file"
                            ref={fileInputRef}
                            className="hidden"
                            accept="image/*"
                            onChange={handleFileUpload}
                        />
                        <button
                            onClick={() => fileInputRef.current?.click()}
                            className="p-2 text-gray-400 hover:text-white transition-colors"
                            title="Upload Evidence"
                        >
                            📎
                        </button>
                        <input
                            type="text"
                            value={input}
                            onChange={(e) => setInput(e.target.value)}
                            onKeyDown={(e) => e.key === 'Enter' && sendMessage()}
                            className="flex-1 bg-surface-light/50 border border-gray-700 rounded px-4 py-2 text-sm text-white focus:border-primary focus:shadow-[0_0_10px_rgba(0,255,157,0.1)] outline-none transition-all placeholder:text-gray-600"
                            placeholder="Type a message..."
                        />
                        <button
                            onClick={sendMessage}
                            className="bg-primary text-black font-bold px-6 rounded text-xs tracking-wider hover:bg-white hover:shadow-[0_0_15px_rgba(0,255,157,0.4)] transition-all uppercase"
                        >
                            Send
                        </button>
                    </>
                ) : (
                    <div className="w-full text-center text-gray-500 text-xs py-2 font-mono border border-dashed border-gray-800 rounded bg-black/20">
                        // SPECTATOR MODE: READ ONLY //
                    </div>
                )}
            </div>
        </div>
    );
}
