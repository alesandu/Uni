// --- Middleware di autenticazione, autorizzazione e logging preventivo ---
const jwt = require("jsonwebtoken");
const bc = require("../services/blockchain");
const logger = require("../services/logger");

const JWT_SECRET = process.env.JWT_SECRET;

// --- Token Blacklist (in-memory, si resetta al riavvio) ---
const _tokenBlacklist = new Set();

function blacklistToken(token) {
    _tokenBlacklist.add(token);
    try {
        const decoded = jwt.decode(token);
        if (decoded && decoded.exp) {
            const ttl = (decoded.exp * 1000) - Date.now();
            if (ttl > 0) {
                setTimeout(() => _tokenBlacklist.delete(token), ttl);
            } else {
                _tokenBlacklist.delete(token);
            }
        }
    } catch (_) {
        setTimeout(() => _tokenBlacklist.delete(token), 24 * 60 * 60 * 1000);
    }
}

function isTokenBlacklisted(token) {
    return _tokenBlacklist.has(token);
}

/**
 * Middleware: verifica JWT + baseline check su blockchain.
 * 1. Verifica JWT valido
 * 2. Verifica che l'utente esista e sia attivo nella classe LOGIN
 * Aggiunge req.user = { username, role }
 */
async function authRequired(req, res, next) {
    let token = req.cookies.token;
    
    // Fallback on header for older clients or API tools
    if (!token && req.headers.authorization) {
        const auth = req.headers.authorization.split(" ");
        if (auth[0] === "Bearer" && auth[1]) {
            token = auth[1];
        }
    }

    if (!token) {
        return res.status(401).json({ error: "Missing authentication token" });
    }

    // Check blacklist
    if (_tokenBlacklist.has(token)) {
        return res.status(401).json({ error: "Token has been revoked" });
    }

    try {
        const decoded = jwt.verify(token, JWT_SECRET);

        // Baseline check: verifica su blockchain che l'utente sia attivo
        const loginData = await bc.getLogin(decoded.username);
        if (!loginData) {
            await logger.logSecurityViolation(
                decoded,
                req.path,
                `JWT valid but user ${decoded.username} not found on blockchain`
            );
            return res.status(401).json({ error: "User not found on ledger" });
        }
        if (!loginData.active) {
            await logger.logSecurityViolation(
                decoded,
                req.path,
                `JWT valid but user ${decoded.username} is deactivated`
            );
            return res.status(401).json({ error: "Account deactivated" });
        }

        req.user = { username: decoded.username, role: loginData.role };
        return next();
    } catch (err) {
        return res.status(401).json({ error: "Invalid or expired token" });
    }
}

/**
 * Middleware factory: verifica che l'utente abbia uno dei ruoli specificati.
 * SUPERUSER è sempre autorizzato.
 * @param  {...string} roles - Ruoli permessi (es. "LAB", "LANDFILL")
 */
function requireRole(...roles) {
    return (req, res, next) => {
        const userRole = req.user?.role;
        if (userRole === "SUPERUSER" || roles.includes(userRole)) {
            return next();
        }
        return res.status(403).json({
            error: `Forbidden: requires role ${roles.join(" or ")}, you are ${userRole}`
        });
    };
}

/**
 * Middleware: registra l'intento di ogni operazione nel LOG prima di processarla.
 * Attacca req.intentLogId per il tracciamento downstream.
 */
async function logPreRequest(req, res, next) {
    try {
        const ip = req.ip || req.connection?.remoteAddress || "unknown";
        const action = req.path.replace(/^\//, "") || "unknown";

        const intentLogId = await logger.logIntent(
            req.user,
            action,
            req.body,
            ip
        );
        req.intentLogId = intentLogId;
    } catch (err) {
        console.error("[LOG-PRE] Failed to log intent:", err.message);
        req.intentLogId = null;
    }
    return next();
}

/**
 * Middleware: gate per pagine protette e file JS sensibili.
 * Redirect a login per HTML, 401 per JS/API.
 */
function pageAuthGate(req, res, next) {
    let token = req.cookies.token;
    if (!token && req.headers.authorization) {
        const auth = req.headers.authorization.split(" ");
        if (auth[0] === "Bearer" && auth[1]) token = auth[1];
    }

    if (!token || _tokenBlacklist.has(token)) {
        if (req.path.endsWith('.html')) {
            return res.redirect('/login-only.html');
        }
        return res.status(401).json({ error: "Authentication required" });
    }

    try {
        jwt.verify(token, JWT_SECRET);
        return next();
    } catch (err) {
        if (req.path.endsWith('.html')) {
            return res.redirect('/login-only.html');
        }
        return res.status(401).json({ error: "Invalid or expired token" });
    }
}

/**
 * Pagine e JS riservati esclusivamente al SUPERUSER.
 * Operatori normali (incluso OPERATOR) vengono reindirizzati.
 */
const SUPERUSER_PAGES = ['/logs.html', '/alerts.html', '/admin.html'];
const SUPERUSER_JS = ['/js/logs.js', '/js/alerts.js', '/js/admin.js'];

function superuserPageGate(req, res, next) {
    if (!SUPERUSER_PAGES.includes(req.path) && !SUPERUSER_JS.includes(req.path)) {
        return next();
    }

    let token = req.cookies.token;
    if (!token && req.headers.authorization) {
        const auth = req.headers.authorization.split(" ");
        if (auth[0] === "Bearer" && auth[1]) token = auth[1];
    }

    if (!token || _tokenBlacklist.has(token)) {
        if (req.path.endsWith('.html')) {
            return res.redirect('/login-only.html');
        }
        return res.status(401).json({ error: "Authentication required" });
    }

    try {
        const decoded = jwt.verify(token, JWT_SECRET);
        if (decoded.role !== "SUPERUSER") {
            if (req.path.endsWith('.html')) {
                // Reindirizza alla pagina appropriata per il ruolo
                const rolePages = {
                    'LAB': '/lab.html',
                    'TRANSPORT_LIGHT': '/van.html',
                    'HUB': '/hub.html',
                    'TRANSPORT_HAZ': '/truck.html',
                    'LANDFILL': '/landfill.html',
                    'OPERATOR': '/history.html'
                };
                return res.redirect(rolePages[decoded.role] || '/login-only.html');
            }
            return res.status(403).json({ error: "Forbidden: SUPERUSER access only" });
        }
        return next();
    } catch (err) {
        if (req.path.endsWith('.html')) {
            return res.redirect('/login-only.html');
        }
        return res.status(401).json({ error: "Invalid or expired token" });
    }
}

module.exports = { authRequired, requireRole, logPreRequest, pageAuthGate, superuserPageGate, blacklistToken, isTokenBlacklisted };
