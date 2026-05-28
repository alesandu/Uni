require("dotenv").config();
const express = require("express"); // framework per creare il server
const path = require("path"); // per prendere il percorso delle pagine statiche
const cors = require("cors"); // per permettere le richieste cross-origin
const helmet = require("helmet"); // HTTP security headers
const rateLimit = require("express-rate-limit"); // rate limiting
const cookieParser = require("cookie-parser"); // per leggere JWT dai cookie

// Route modulari
const authRoutes = require("./routes/auth");
const operationRoutes = require("./routes/operations");
const queryRoutes = require("./routes/queries");

// Middleware
const { authRequired, logPreRequest, pageAuthGate, superuserPageGate } = require("./middleware/auth");

const app = express();

// --- SICUREZZA ---
// Helmet: HTTP security headers (CSP permissiva per fonts Google)
app.use(helmet({
    contentSecurityPolicy: {
        directives: {
            defaultSrc: ["'self'"],
            styleSrc: ["'self'", "'unsafe-inline'", "https://fonts.googleapis.com"],
            fontSrc: ["'self'", "https://fonts.gstatic.com"],
            scriptSrc: ["'self'", "'unsafe-inline'"],
            scriptSrcAttr: ["'unsafe-inline'"],
            imgSrc: ["'self'", "data:"],
            connectSrc: ["'self'"],
            upgradeInsecureRequests: null
        }
    }
}));

// CORS ristretto: solo stesso host
const ALLOWED_ORIGINS = [
    `http://localhost:${process.env.PORT || 3000}`,
    `http://127.0.0.1:${process.env.PORT || 3000}`,
    'http://10.172.22.31:3000'
];
app.use(cors({
    origin: function (origin, callback) {
        // Richieste same-origin (browser) non hanno header Origin
        if (!origin || ALLOWED_ORIGINS.includes(origin)) {
            return callback(null, true);
        }
        else {
            true
        }
        return callback(new Error('Not allowed by CORS'));
    },
    credentials: true
}));

// Rate limiting — Login: max 5 tentativi ogni 15 minuti
const loginLimiter = rateLimit({
    windowMs: 60 * 1000,
    max: 5,
    message: { error: "Too many login attempts, try again after 1 minute" },
    standardHeaders: true,
    legacyHeaders: false
});

// Rate limiting — API generica: max 100 req/minuto
const apiLimiter = rateLimit({
    windowMs: 60 * 1000,
    max: 100,
    message: { error: "Too many requests, slow down" },
    standardHeaders: true,
    legacyHeaders: false
});

app.use(express.json({ limit: "1mb" })); // Limita dimensione body
app.use(cookieParser()); // Parsing dei cookie

// --- AUTH GATE: protegge pagine e JS sensibili PRIMA di servire file statici ---
const PROTECTED_PAGES = [
    '/lab.html', '/van.html', '/hub.html', '/truck.html',
    '/landfill.html', '/alerts.html', '/logs.html', '/history.html',
    '/admin.html'
];
const PROTECTED_JS = [
    '/js/builder.js', '/js/inspector.js', '/js/alerts.js',
    '/js/logs.js', '/js/history.js', '/js/admin.js'
];

app.use((req, res, next) => {
    if (PROTECTED_PAGES.includes(req.path) || PROTECTED_JS.includes(req.path)) {
        return pageAuthGate(req, res, next);
    }
    next();
});

// --- SUPERUSER GATE: pagine logs, alerts, admin solo per SUPERUSER ---
app.use(superuserPageGate);

app.use(express.static(path.join(__dirname, "../public")));

// Root → redirect smart basato su sessione JWT
app.get("/", (req, res) => {
    const jwt = require("jsonwebtoken");
    const { isTokenBlacklisted } = require("./middleware/auth");

    const token = req.cookies.token;

    if (!token || isTokenBlacklisted(token)) {
        return res.sendFile(path.join(__dirname, "../public/login-only.html"));
    }

    try {
        const decoded = jwt.verify(token, process.env.JWT_SECRET);
        const rolePages = {
            'LAB': '/lab.html',
            'TRANSPORT_LIGHT': '/van.html',
            'HUB': '/hub.html',
            'TRANSPORT_HAZ': '/truck.html',
            'LANDFILL': '/landfill.html',
            'OPERATOR': '/history.html',
            'SUPERUSER': '/alerts.html'
        };
        return res.redirect(rolePages[decoded.role] || '/login-only.html');
    } catch (err) {
        // Token scaduto o invalido: pulisce il cookie e mostra login
        res.clearCookie("token");
        return res.sendFile(path.join(__dirname, "../public/login-only.html"));
    }
});

// --- ROUTES ---
// Auth — con rate limiting su login
app.use("/api/login", loginLimiter);
app.use("/api", authRoutes);               // /api/login, /api/register

// Operazioni — authRequired PRIMA di logPreRequest (fix: era invertito)
app.use("/api/operations", apiLimiter, authRequired, logPreRequest, operationRoutes);

// Query — protette da autenticazione
app.use("/api/query", apiLimiter, queryRoutes);          // /api/query/*

// --- SERVER ---
const PORT = process.env.PORT || 3000;
const HOST = '0.0.0.0';

// --- GLOBAL ERROR HANDLER — maschera errori verbose (stack trace, percorsi) ---
app.use((err, req, res, next) => {
    // Log completo solo lato server (mai esposto al client)
    console.error(`[ERROR] ${req.method} ${req.path} —`, err.message);

    // Body malformato (JSON syntax error)
    if (err.type === 'entity.parse.failed') {
        return res.status(400).json({ error: "Bad request: invalid JSON" });
    }

    // Payload troppo grande
    if (err.type === 'entity.too.large') {
        return res.status(413).json({ error: "Payload too large" });
    }

    // Errore CORS
    if (err.message && err.message.includes('CORS')) {
        return res.status(403).json({ error: "Forbidden: origin not allowed" });
    }

    // Qualsiasi altro errore → generico 400/500 senza dettagli
    const statusCode = err.status || err.statusCode || 500;
    return res.status(statusCode).json({ error: "Request error" });
});

// 404 catch-all — nessuna informazione sul framework
app.use((req, res) => {
    res.status(404).json({ error: "Not found" });
});

app.listen(PORT, HOST, (err) => {
    if (err) {
        console.error(err);
    } else {
        console.log(`Server in ascolto su http://${HOST}:${PORT}`);
    }
});