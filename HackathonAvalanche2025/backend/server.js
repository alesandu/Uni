import express from 'express';
import { createServer } from 'http';
import { Server } from 'socket.io';
import cors from 'cors';
import multer from 'multer';
import axios from 'axios';
import FormData from 'form-data';

const app = express();
const httpServer = createServer(app);
const io = new Server(httpServer, {
  cors: {
    origin: "*", // Allow all for hackathon
    methods: ["GET", "POST"]
  }
});

app.use(cors());
app.use(express.json());

// In-memory storage
const gamerTags = {}; // address -> gamerTag
const matchChats = {}; // matchId -> Array<{sender, message, timestamp}>

// API Routes
app.post('/api/gamertag', (req, res) => {
  const { address, tag } = req.body;
  if (!address || !tag) return res.status(400).send('Missing address or tag');
  gamerTags[address.toLowerCase()] = tag;
  console.log(`Registered GamerTag: ${tag} for ${address}`);
  res.json({ success: true });
});

app.get('/api/gamertag/:address', (req, res) => {
  const { address } = req.params;
  const tag = gamerTags[address.toLowerCase()];
  res.json({ tag: tag || null });
});

app.get('/api/chat/:matchId', (req, res) => {
  const { matchId } = req.params;
  res.json({ messages: matchChats[matchId] || [] });
});

app.post('/api/chat/message', (req, res) => {
  const { matchId, sender, message } = req.body;
  if (!matchId || !sender || !message) return res.status(400).send('Missing fields');

  const msgData = { sender, message, timestamp: Date.now() };
  if (!matchChats[matchId]) matchChats[matchId] = [];
  matchChats[matchId].push(msgData);

  io.to(matchId).emit('new_message', msgData);
  res.json({ success: true });
});

// File Upload (IPFS/Pinata)
const upload = multer({ storage: multer.memoryStorage() });
const mockIpfsStorage = {}; // hash -> { buffer, mimetype }

app.get('/api/ipfs/:hash', (req, res) => {
  const { hash } = req.params;
  const fileData = mockIpfsStorage[hash];
  if (fileData) {
    res.setHeader('Content-Type', fileData.mimetype);
    res.send(fileData.buffer);
  } else {
    res.status(404).send('Not found');
  }
});

app.post('/api/upload', upload.single('file'), async (req, res) => {
  if (!req.file) return res.status(400).send('No file uploaded');

  try {
    if (process.env.PINATA_JWT) {
      const formData = new FormData();
      formData.append('file', req.file.buffer, req.file.originalname);

      const response = await axios.post('https://api.pinata.cloud/pinning/pinFileToIPFS', formData, {
        headers: {
          'Authorization': `Bearer ${process.env.PINATA_JWT}`,
          ...formData.getHeaders()
        }
      });

      res.json({ ipfsHash: response.data.IpfsHash });
    } else {
      // Mock for hackathon
      console.log('Mocking IPFS upload for:', req.file.originalname);
      const mockHash = 'QmMockHashForHackathonDemo' + Date.now();
      mockIpfsStorage[mockHash] = {
        buffer: req.file.buffer,
        mimetype: req.file.mimetype
      };
      res.json({ ipfsHash: mockHash });
    }
  } catch (error) {
    console.error('Upload error:', error);
    res.status(500).send('Upload failed');
  }
});

// Socket.io
io.on('connection', (socket) => {
  console.log('User connected:', socket.id);

  socket.on('join_match', (matchId) => {
    socket.join(matchId);
    console.log(`User ${socket.id} joined match ${matchId}`);
  });

  socket.on('send_message', ({ matchId, sender, message }) => {
    const msgData = { sender, message, timestamp: Date.now() };

    if (!matchChats[matchId]) matchChats[matchId] = [];
    matchChats[matchId].push(msgData);

    io.to(matchId).emit('new_message', msgData);
  });

  socket.on('match_update', ({ matchId, status, action }) => {
    // Broadcast status updates (e.g. "Player A voted cancel")
    io.to(matchId).emit('status_update', { status, action });
  });

  socket.on('disconnect', () => {
    console.log('User disconnected:', socket.id);
  });
});

const PORT = process.env.PORT || 3001;
httpServer.listen(PORT, () => {
  console.log(`Backend server running on port ${PORT}`);
});
