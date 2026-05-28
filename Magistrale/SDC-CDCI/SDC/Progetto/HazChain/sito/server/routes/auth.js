// --- Route di autenticazione (blockchain-based, no SQLite) ---
const express = require("express");
const bcrypt = require("bcrypt");
const jwt = require("jsonwebtoken");
const Joi = require("joi");
const bc = require("../services/blockchain");
const logger = require("../services/logger");
const { blacklistToken } = require("../middleware/auth");

const router = express.Router();
const JWT_SECRET = process.env.JWT_SECRET;

// --- Schemi di validazione Joi ---

// Ruoli ammessi per auto-registrazione (SUPERUSER ESCLUSO)
const VALID_ROLES = ["LAB", "TRANSPORT_LIGHT", "HUB", "TRANSPORT_HAZ", "LANDFILL", "OPERATOR"];

const registerSchema = Joi.object({
    username: Joi.string().alphanum().min(3).max(30).required()
        .messages({ "string.min": "Username must be at least 3 characters" }),
    password: Joi.string().min(8).max(72).required()
        .messages({ "string.min": "Password must be at least 8 characters" }),
    role: Joi.string().valid(...VALID_ROLES).required()
        .messages({ "any.only": `Role must be one of: ${VALID_ROLES.join(", ")}` })
});

const loginSchema = Joi.object({
    username: Joi.string().min(1).max(30).required(),
    password: Joi.string().min(1).max(72).required()
});

// Registrazione — scrive su LOGIN su blockchain
// SECURITY: SUPERUSER non è registrabile via API. Solo ruoli operativi.
router.post("/register", async (req, res) => {
    const ip = req.ip || req.connection?.remoteAddress || "unknown";
    let intentLogId;

    try {
        // Validazione input con Joi
        const { error, value } = registerSchema.validate(req.body, { abortEarly: false });
        if (error) {
            const details = error.details.map(d => d.message).join("; ");
            intentLogId = await logger.logIntent(
                { username: "SYSTEM", role: "SYSTEM" },
                "register",
                { username: req.body.username, role: req.body.role },
                ip
            );
            await logger.logValidation(intentLogId, "BLOCKED", `Validation failed: ${details}`);
            return res.status(400).json({ error: details });
        }

        const { username, password, role } = value;

        // LOG: intento registrazione
        intentLogId = await logger.logIntent(
            { username: "SYSTEM", role: "SYSTEM" },
            "register",
            { username, role },
            ip
        );

        // Verifica duplicato su blockchain
        const existing = await bc.getLogin(username);
        if (existing) {
            await logger.logValidation(intentLogId, "BLOCKED", "Username already exists");
            return res.status(400).json({ error: "Username already exists" });
        }

        // Validazione superata
        await logger.logValidation(intentLogId, "SUCCESS", "User does not exist, proceeding");

        // Hash password
        const saltRounds = 10;
        const passwordHash = await bcrypt.hash(password, saltRounds);

        // Scrivi LOGIN su blockchain
        const loginData = {
            username,
            passwordHash,
            role,
            createdAt: new Date().toISOString(),
            lastLoginAt: null,
            active: true
        };
        await bc.setLogin(username, loginData);

        await logger.logExecution(intentLogId, "LOGIN", username, "SUCCESS");

        // Conferma
        await logger.logConfirm(intentLogId, "SUCCESS", `User ${username} registered with role ${role}`);

        console.log(`[AUTH] User ${username} registered (role: ${role})`);
        res.status(201).json({ message: "User registered successfully", username });
    } catch (error) {
        console.error("Registration error:", error);
        if (intentLogId) {
            await logger.logConfirm(intentLogId, "ERROR", error.message).catch(() => {});
        }
        res.status(500).json({ error: "Internal server error" });
    }
});

// Login — verifica su blockchain, genera JWT
router.post("/login", async (req, res) => {
    const ip = req.ip || req.connection?.remoteAddress || "unknown";

    try {
        // Validazione input con Joi
        const { error, value } = loginSchema.validate(req.body);
        if (error) {
            return res.status(400).json({ error: "Username and password are required" });
        }

        const { username, password } = value;

        // LOG: intento login in background (non-blocking)
        const intentLogPromise = logger.logIntent(
            { username, role: "unknown" },
            "login",
            { username },
            ip
        ).catch(e => { console.error("[LOG] logIntent failed:", e.message); return null; });

        // PATH CRITICO: solo la lettura della blockchain e il confronto password
        // Recupera credenziali da blockchain
        const loginData = await bc.getLogin(username);
        if (!loginData) {
            // Risolve il log intent e scrive la violazione in background
            intentLogPromise.then(intentLogId => {
                logger.logSecurityViolation(
                    { username, role: "unknown" },
                    "login",
                    `Login attempt for non-existent user: ${username}`,
                    null,
                    intentLogId
                ).catch(() => {});
            });
            return res.status(401).json({ error: "Invalid username or password" });
        }

        // Verifica account attivo
        if (!loginData.active) {
            intentLogPromise.then(intentLogId => {
                logger.logSecurityViolation(
                    { username, role: loginData.role },
                    "login",
                    `Login attempt for deactivated account: ${username}`,
                    null,
                    intentLogId
                ).catch(() => {});
            });
            return res.status(401).json({ error: "Account deactivated" });
        }

        // Verifica password (CPU-bound, deve restare in path critico)
        const match = await bcrypt.compare(password, loginData.passwordHash);
        if (!match) {
            intentLogPromise.then(intentLogId => {
                logger.logSecurityViolation(
                    { username, role: loginData.role },
                    "login",
                    `Invalid password for user: ${username}`,
                    null,
                    intentLogId
                ).catch(() => {});
            });
            return res.status(401).json({ error: "Invalid username or password" });
        }

        // ─── LOGIN OK ──────────────────────────────────────────────────
        // Genera JWT — usa JWT_EXPIRES_IN dal .env
        const token = jwt.sign(
            { username: loginData.username, role: loginData.role },
            JWT_SECRET,
            { expiresIn: process.env.JWT_EXPIRES_IN || "12h" }
        );

        // Set token in httpOnly cookie
        res.cookie('token', token, {
            httpOnly: true,
            secure: process.env.NODE_ENV === 'production',
            sameSite: 'strict',
            maxAge: 12 * 60 * 60 * 1000 // 12 hours
        });

        // Risponde subito al client senza aspettare i log
        console.log(`[AUTH] User ${username} logged in (role: ${loginData.role})`);
        res.json({
            message: "Login successful",
            user: {
                username: loginData.username,
                role: loginData.role
            }
        });

        // Scritture blockchain in background (dopo la risposta)
        intentLogPromise.then(intentLogId => {
            if (!intentLogId) return;
            Promise.all([
                logger.logValidation(intentLogId, "SUCCESS", "Credentials verified").catch(() => {}),
                logger.logConfirm(intentLogId, "SUCCESS", `User ${username} logged in`).catch(() => {})
            ]);
        });

        // Aggiorna lastLoginAt in background
        loginData.lastLoginAt = new Date().toISOString();
        bc.setLogin(username, loginData).catch(e => {
            console.error("[AUTH] Failed to update lastLoginAt:", e.message);
        });

    } catch (error) {
        console.error("Login error:", error);
        res.status(500).json({ error: "Internal server error" });
    }
});

// Logout — revoke token
router.post("/logout", async (req, res) => {
    try {
        let token = req.cookies.token;
        if (!token && req.headers.authorization) {
            const auth = req.headers.authorization.split(" ");
            if (auth[0] === "Bearer" && auth[1]) token = auth[1];
        }

        if (token) {
            blacklistToken(token);
        }
        res.clearCookie('token');
        res.json({ message: "Logout successful" });
    } catch (error) {
        console.error("Logout error:", error);
        res.status(500).json({ error: "Internal server error" });
    }
});

// --- Admin: creazione utenti da parte del SUPERUSER ---
const { authRequired, requireRole } = require("../middleware/auth");

// POST /api/admin/create-user — Solo SUPERUSER
router.post("/admin/create-user", authRequired, requireRole("SUPERUSER"), async (req, res) => {
    const ip = req.ip || req.connection?.remoteAddress || "unknown";
    let intentLogId;

    try {
        const ALL_ROLES = ["LAB", "TRANSPORT_LIGHT", "HUB", "TRANSPORT_HAZ", "LANDFILL", "OPERATOR", "SUPERUSER"];

        const adminCreateSchema = Joi.object({
            username: Joi.string().alphanum().min(3).max(30).required(),
            password: Joi.string().min(8).max(72).required(),
            role: Joi.string().valid(...ALL_ROLES).required()
                .messages({ "any.only": `Role must be one of: ${ALL_ROLES.join(", ")}` })
        });

        const { error, value } = adminCreateSchema.validate(req.body, { abortEarly: false });
        if (error) {
            const details = error.details.map(d => d.message).join("; ");
            return res.status(400).json({ error: details });
        }

        const { username, password, role } = value;

        intentLogId = await logger.logIntent(
            req.user,
            "admin-create-user",
            { username, role },
            ip
        );

        // Verifica duplicato
        const existing = await bc.getLogin(username);
        if (existing) {
            await logger.logValidation(intentLogId, "BLOCKED", "Username already exists");
            return res.status(400).json({ error: "Username already exists" });
        }

        await logger.logValidation(intentLogId, "SUCCESS", "User does not exist, proceeding");

        const saltRounds = 10;
        const passwordHash = await bcrypt.hash(password, saltRounds);

        const loginData = {
            username,
            passwordHash,
            role,
            createdAt: new Date().toISOString(),
            createdBy: req.user.username,
            lastLoginAt: null,
            active: true
        };
        await bc.setLogin(username, loginData);

        await logger.logExecution(intentLogId, "LOGIN", username, "SUCCESS");
        await logger.logConfirm(intentLogId, "SUCCESS", `User ${username} created by admin ${req.user.username} with role ${role}`);

        console.log(`[ADMIN] User ${username} created by ${req.user.username} (role: ${role})`);
        res.status(201).json({ message: "User created successfully", username, role });
    } catch (error) {
        console.error("Admin create-user error:", error);
        if (intentLogId) {
            await logger.logConfirm(intentLogId, "ERROR", error.message).catch(() => {});
        }
        res.status(500).json({ error: "Internal server error" });
    }
});

// GET /api/admin/users — Lista utenti (Solo SUPERUSER)
router.get("/admin/users", authRequired, requireRole("SUPERUSER"), async (req, res) => {
    try {
        const usernames = await bc.getAllLogins();
        const users = [];
        for (const uname of usernames) {
            const data = await bc.getLogin(uname);
            if (data) {
                users.push({
                    username: data.username,
                    role: data.role,
                    createdAt: data.createdAt,
                    createdBy: data.createdBy || "self-registered",
                    lastLoginAt: data.lastLoginAt,
                    active: data.active
                });
            }
        }
        res.json(users);
    } catch (error) {
        console.error("Admin list users error:", error);
        res.status(500).json({ error: "Internal server error" });
    }
});

// PUT /api/admin/update-user — Modifica utente (Solo SUPERUSER)
router.put("/admin/update-user", authRequired, requireRole("SUPERUSER"), async (req, res) => {
    const ip = req.ip || req.connection?.remoteAddress || "unknown";
    let intentLogId;

    try {
        const ALL_ROLES = ["LAB", "TRANSPORT_LIGHT", "HUB", "TRANSPORT_HAZ", "LANDFILL", "OPERATOR", "SUPERUSER"];

        const adminUpdateSchema = Joi.object({
            username: Joi.string().alphanum().min(3).max(30).required(),
            role: Joi.string().valid(...ALL_ROLES).optional(),
            password: Joi.string().min(8).max(72).optional().allow(""),
            active: Joi.boolean().optional()
        });

        const { error, value } = adminUpdateSchema.validate(req.body, { abortEarly: false });
        if (error) {
            const details = error.details.map(d => d.message).join("; ");
            return res.status(400).json({ error: details });
        }

        const { username, role, password, active } = value;

        intentLogId = await logger.logIntent(
            req.user,
            "admin-update-user",
            { username, role, active },
            ip
        );

        // Recupera utente esistente
        const existing = await bc.getLogin(username);
        if (!existing) {
            await logger.logValidation(intentLogId, "BLOCKED", "User not found");
            return res.status(404).json({ error: "User not found" });
        }

        // Non permettere la modifica di se stesso
        if (existing.username === req.user.username) {
            await logger.logValidation(intentLogId, "BLOCKED", "Cannot modify own account");
            return res.status(403).json({ error: "Cannot modify your own account" });
        }

        await logger.logValidation(intentLogId, "SUCCESS", "User found, proceeding with update");

        // Applica le modifiche
        if (role !== undefined) existing.role = role;
        if (active !== undefined) existing.active = active;
        if (password && password.length >= 8) {
            const saltRounds = 10;
            existing.passwordHash = await bcrypt.hash(password, saltRounds);
        }
        existing.updatedAt = new Date().toISOString();
        existing.updatedBy = req.user.username;

        await bc.setLogin(username, existing);

        await logger.logExecution(intentLogId, "LOGIN", username, "SUCCESS");
        await logger.logConfirm(intentLogId, "SUCCESS", `User ${username} updated by admin ${req.user.username}`);

        console.log(`[ADMIN] User ${username} updated by ${req.user.username}`);
        res.json({ message: "User updated successfully", username, role: existing.role, active: existing.active });
    } catch (error) {
        console.error("Admin update-user error:", error);
        if (intentLogId) {
            await logger.logConfirm(intentLogId, "ERROR", error.message).catch(() => {});
        }
        res.status(500).json({ error: "Internal server error" });
    }
});

// DELETE /api/admin/delete-user/:username — Cancella utente (Solo SUPERUSER)
router.delete("/admin/delete-user/:username", authRequired, requireRole("SUPERUSER"), async (req, res) => {
    const ip = req.ip || req.connection?.remoteAddress || "unknown";
    let intentLogId;

    try {
        const username = req.params.username;

        if (!username || username.length < 3) {
            return res.status(400).json({ error: "Invalid username" });
        }

        intentLogId = await logger.logIntent(
            req.user,
            "admin-delete-user",
            { username },
            ip
        );

        // Recupera utente esistente
        const existing = await bc.getLogin(username);
        if (!existing) {
            await logger.logValidation(intentLogId, "BLOCKED", "User not found");
            return res.status(404).json({ error: "User not found" });
        }

        // Non permettere la cancellazione di se stesso
        if (existing.username === req.user.username) {
            await logger.logValidation(intentLogId, "BLOCKED", "Cannot delete own account");
            return res.status(403).json({ error: "Cannot delete your own account" });
        }

        await logger.logValidation(intentLogId, "SUCCESS", "User found, proceeding with deletion");

        // Cancella dalla blockchain
        await bc.delKV("LOGIN", ["LGN", username]);

        await logger.logExecution(intentLogId, "LOGIN", username, "SUCCESS");
        await logger.logConfirm(intentLogId, "SUCCESS", `User ${username} deleted by admin ${req.user.username}`);

        console.log(`[ADMIN] User ${username} deleted by ${req.user.username}`);
        res.json({ message: "User deleted successfully", username });
    } catch (error) {
        console.error("Admin delete-user error:", error);
        if (intentLogId) {
            await logger.logConfirm(intentLogId, "ERROR", error.message).catch(() => {});
        }
        res.status(500).json({ error: "Internal server error" });
    }
});

// GET /api/me — Verifica sessione corrente (JWT cookie)
router.get("/me", async (req, res) => {
    const jwt = require("jsonwebtoken");
    const { isTokenBlacklisted } = require("../middleware/auth");

    const token = req.cookies.token;
    if (!token || isTokenBlacklisted(token)) {
        return res.status(401).json({ error: "Not authenticated" });
    }
    try {
        const decoded = jwt.verify(token, JWT_SECRET);
        return res.json({ username: decoded.username, role: decoded.role });
    } catch (err) {
        return res.status(401).json({ error: "Invalid or expired token" });
    }
});

module.exports = router;
