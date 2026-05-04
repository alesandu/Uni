// --- Middleware di autenticazione e autorizzazione ---
const jwt = require("jsonwebtoken");

const JWT_SECRET = process.env.JWT_SECRET;

/**
 * Middleware: verifica JWT nel header Authorization.
 * Aggiunge req.user = { id, username, role, iat, exp }
 */
function authRequired(req, res, next) {
    const auth = req.headers.authorization || "";
    const [type, token] = auth.split(" ");

    if (type !== "Bearer" || !token) {
        return res.status(401).json({ error: "Missing Authorization: Bearer <token>" });
    }

    try {
        req.user = jwt.verify(token, JWT_SECRET);
        return next();
    } catch {
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

module.exports = { authRequired, requireRole };
