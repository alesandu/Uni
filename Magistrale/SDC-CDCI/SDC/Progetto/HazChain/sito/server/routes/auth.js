// --- Route di autenticazione ---
const express = require("express");
const bcrypt = require("bcrypt");
const jwt = require("jsonwebtoken");
const db = require("../db");

const router = express.Router();
const JWT_SECRET = process.env.JWT_SECRET;

// Registrazione
router.post("/register", async (req, res) => {
    try {
        const { username, password, role } = req.body;

        if (!username || !password || !role) {
            return res.status(400).json({ error: "Username, password, and role are required" });
        }
        // Verifica se gia esiste lo stesso utente
        db.get("SELECT id FROM users WHERE username = ?", [username], async (err, row) => {
            if (err) {
                return res.status(500).json({ error: "Database error" });
            }
            if (row) {
                return res.status(400).json({ error: "Username already exists" });
            }
            // Hash della password
            const saltRounds = 10;
            const password_hash = await bcrypt.hash(password, saltRounds);
            // inserisci nuovo utente
            db.run("INSERT INTO users (username, password_hash, role) VALUES (?, ?, ?)",
                [username, password_hash, role],
                function (err) {
                    if (err) {
                        return res.status(500).json({ error: "Failed to create user" });
                    }
                    res.status(201).json({ message: "User registered successfully", userId: this.lastID });
                });
        });
    } catch (error) {
        console.error("Registration error:", error);
        res.status(500).json({ error: "Internal server error" });
    }
});

// Login
router.post("/login", (req, res) => {
    try {
        const { username, password } = req.body;
        if (!username || !password) {
            return res.status(400).json({ error: "Username and password are required" });
        }
        // prendi username e pass da database
        db.get("SELECT * FROM users WHERE username = ?", [username], async (err, user) => {
            if (err) {
                return res.status(500).json({ error: "Database error" });
            }
            if (!user) {
                return res.status(401).json({ error: "Invalid username or password" });
            }
            // verifica password
            const match = await bcrypt.compare(password, user.password_hash);
            if (!match) {
                return res.status(401).json({ error: "Invalid username or password" });
            }

            // generazione token per rimanere connessi
            const token = jwt.sign(
                { id: user.id, username: user.username, role: user.role },
                JWT_SECRET,
                { expiresIn: "24h" }
            );

            res.json({
                message: "Login successful",
                token,
                user: {
                    id: user.id,
                    username: user.username,
                    role: user.role
                }
            });
        });
    } catch (error) {
        console.error("Login error:", error);
        res.status(500).json({ error: "Internal server error" });
    }
});

module.exports = router;
