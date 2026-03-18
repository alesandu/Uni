require("dotenv").config();
const express = require("express") // framework per creare il server
const path = require("path"); // per prendere il percorso delle pagine statiche
const cors = require("cors"); // per permettere le richieste cross-origin (senza non potrei comunicare con altre porte)
const axios = require("axios"); // per fare le richieste all'hyperledger fabric
const bcrypt = require("bcrypt"); // per criptare le password
const jwt = require("jsonwebtoken"); // per generare i token
const db = require("./db"); // per connettersi al database

const JWT_SECRET = process.env.JWT_SECRET;


const app = express()

app.use(express.json());
app.use(cors());
app.use(express.static(path.join(__dirname, "../public")));


// MIDDLEWARE: verifica JWT
function authRequired(req, res, next) {
    const auth = req.headers.authorization || "";
    const [type, token] = auth.split(" ");

    if (type !== "Bearer" || !token) {
        return res.status(401).json({ error: "Missing Authorization: Bearer <token>" });
    }

    try {
        req.user = jwt.verify(token, JWT_SECRET); // {id, username, role, iat, exp}
        return next();
    } catch {
        return res.status(401).json({ error: "Invalid or expired token" });
    }
}

// AUTORIZZAZIONE: matrice RBAC
function isAllowed(role, payload) {
    const cmd = payload?.cmd;
    const cls = payload?.class;

    const READ = new Set(["GetKV", "GetKeys", "GetKeyHistory", "GetClasses", "GetNumKeys"]);
    if (READ.has(cmd)) return true;

    if (cmd === "AddKV") {
        if (cls === "ALERT") return true;
        if (cls === "ACTOR") return role === "SUPERUSER";
        if (cls === "PACKAGE") return ["LAB", "LANDFILL", "SUPERUSER"].includes(role);
        if (cls === "TRANSFER") return ["LAB", "HUB", "TRANSPORT_LIGHT", "TRANSPORT_HAZ", "SUPERUSER"].includes(role);
    }

    if (cmd === "DelKV") return role === "SUPERUSER";
    return false;
}


// HELPER non fa altro che prendere il pacchetto (GETKV)
async function getPackage(packageId) {
    try {
        console.log(`[DEBUG] Fetching package state for ${packageId}...`);

        // Creazione chiamata API
        try {
            const payload = {
                cmd: "GetKV",
                class: "PACKAGE",
                key: ["PKG", packageId]
            };
            console.log("[DEBUG] Sending GetKV:", JSON.stringify(payload));

            const response = await axios.post("http://localhost:9999/api", payload);
            console.log(`[DEBUG] GetKV response status: ${response.status}`);

            const data = response.data;

            // successo
            if (data && data.answer && data.answer.value) {
                console.log("[DEBUG] Found data.answer.value");
                return data.answer.value;
            }

            // fallback
            if (data && data.value) {
                console.log("[DEBUG] Found data.value");
                return data.value;
            }

            console.log("[DEBUG] Could not extract Package attributes from response:", JSON.stringify(data));

        } catch (e) {
            console.log(`[DEBUG] GetKV failed: ${e.message}`);
            if (e.response) console.log(`[DEBUG] Server responded:`, e.response.data);
        }
        return null;
    } catch (error) {
        console.error(`[DEBUG] Failed to fetch package ${packageId}:`, error.message);
        return null;
    }
}

// INTERMEDIARIO PROXY ENDPOINT (AddKV)
app.post("/api/proxy", authRequired, async (req, res) => {
    try {
        const payload = req.body;
        const role = req.user.role;
        console.log(`[DEBUG] Proxying request: ${payload.cmd} ${payload.class} (role: ${role})`);

        // Controllo autorizzazione RBAC
        if (!isAllowed(role, payload)) {
            return res.status(403).json({ error: "Forbidden: action not allowed for this role" });
        }

        // Esecuzione chiamata API
        const response = await axios.post("http://localhost:9999/api", payload);

        // In caso era una chiamata TRANSFER aggiorna gli stati e nuovi custodi
        if (response.status === 200 && payload.cmd === "AddKV" && payload.class === "TRANSFER") {
            console.log("Intercepted TRANSFER. Attempting automatic Package update...");

            // Prendi IDs dalla chiave: ["TR", "TRANSFER-ID"]
            const keyParts = payload.key;
            console.log(`[DEBUG] Key Parts:`, keyParts);

            if (Array.isArray(keyParts) && keyParts.length >= 2) {
                const transferId = keyParts[1];
                const transferData = payload.value;
                const packageId = transferData.packageId;

                if (!packageId) {
                    console.log("[DEBUG] No packageId in transfer data. Skipping update.");
                    return res.status(response.status).json(response.data);
                }

                // Prendi il package corrente per avere dati attuali
                const pkgData = await getPackage(packageId);
                if (pkgData) {
                    console.log(`[DEBUG] Current Package State:`, pkgData);

                    // Nuovo stato e custodian del package
                    let newCustodian = pkgData.currentCustodian;
                    let newState = pkgData.state;

                    const status = transferData.status;

                    // se lo status sta in waiting significa che il nuovo custode è quello a cui sta andando
                    if (status === "WAITING") {
                        newCustodian = transferData.from;
                    }
                    // se lo status è in shipping significa che il nuovo custode è il trasportatore
                    else if (status === "SHIPPING") {
                        newCustodian = transferData.from;
                        if (transferData.to.includes("HUB")) {
                            newState = "F_TRANSPORT";
                        }
                        else if (transferData.to.includes("LANDFILL")) {
                            newState = "C_TRANSPORT";
                        }
                    }
                    // se lo status è completed o delivered significa che il nuovo custode è quello a cui sta andando 
                    else if (status === "COMPLETED" || status === "DELIVERED") {
                        newCustodian = transferData.to;
                        if (transferData.to.includes("HUB")) {
                            newState = "IN_HUB";
                        } else if (transferData.to.includes("LANDFILL")) {
                            newState = "IN_LANDFILL";
                        }
                    }

                    console.log(`[DEBUG] Updating Custodian: ${pkgData.currentCustodian} -> ${newCustodian}`);
                    console.log(`[DEBUG] Updating State: ${pkgData.state} -> ${newState}`);

                    // Costruzione nuovi valori
                    const newPkgValue = {
                        ...pkgData,
                        currentCustodian: newCustodian,
                        state: newState,
                        lastTransferId: transferId,
                        lastUpdateTs: new Date().toISOString()
                    };
                    if (transferData.weightKg) newPkgValue.weightKg = transferData.weightKg;
                    if (transferData.wasteType) newPkgValue.wasteType = transferData.wasteType;
                    if (transferData.riskCode) newPkgValue.riskCode = transferData.riskCode;

                    // Payload aggiornamento
                    const updatePayload = {
                        cmd: "AddKV",
                        class: "PACKAGE",
                        key: ["PKG", packageId],
                        value: newPkgValue
                    };

                    console.log("Auto-updating Package:", JSON.stringify(updatePayload, null, 2));

                    // Chiamta API per il trasferimento
                    try {
                        await axios.post("http://localhost:9999/api", updatePayload);
                        console.log("Package auto-update successful.");
                    } catch (updateErr) {
                        console.error("Package auto-update failed:", updateErr.message);
                    }
                } else {
                    console.log("Could not retrieve current package data. Skipping update.");
                }
            } else {
                console.log("[DEBUG] Key doesn't match expected format ['TR', 'PKG-ID', 'T-ID']");
            }
        }

        res.status(response.status).json(response.data);
    } catch (error) {
        console.error("Proxy error:", error.message);
        if (error.response) {
            res.status(error.response.status).json(error.response.data);
        } else {
            res.status(500).json({ error: error.message });
        }
    }
});

// AUTENTICAZOINE
// Registrazione
app.post("/api/register", async (req, res) => {
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
app.post("/api/login", (req, res) => {
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

// Listen del server (inutile)
app.listen(3000, (err) => {
    if (err) {
        console.error(err)
    }
    else {
        console.log("Server started on port 3000")
    }
});