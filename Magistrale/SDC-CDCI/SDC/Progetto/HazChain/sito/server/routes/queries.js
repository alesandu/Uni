// --- Endpoint di sola lettura (query) ---
const express = require("express");
const bc = require("../services/blockchain");
const { authRequired } = require("../middleware/auth");

const router = express.Router();

// Tutti gli endpoint di query richiedono autenticazione
router.use(authRequired);

// GET /api/query/package/:id — Fetch singolo package
router.get("/package/:id", async (req, res) => {
    try {
        const pkg = await bc.getPackage(req.params.id);
        if (!pkg) return res.status(404).json({ error: "Package not found" });
        pkg.packageId = req.params.id;
        res.json(pkg);
    } catch (error) {
        console.error("Query package error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// GET /api/query/packages?creator=&custodian=&status=&waste=&risk=&weightMin=&weightMax=&transfer=
router.get("/packages", async (req, res) => {
    try {
        const keys = await bc.getKeys("PACKAGE", ["PKG"]);
        const promises = keys.map(k => {
            const id = k[1];
            return bc.getPackage(id).then(pkg => {
                if (pkg) { pkg.packageId = id; return pkg; }
                return null;
            });
        });
        let packages = (await Promise.all(promises)).filter(p => p !== null);

        // Filtri server-side
        const { creator, custodian, status, waste, risk, weightMin, weightMax, transfer } = req.query;

        if (creator) {
            packages = packages.filter(p => p.createdBy && p.createdBy.toLowerCase().includes(creator.toLowerCase()));
        }
        if (custodian) {
            packages = packages.filter(p => p.currentCustodian && p.currentCustodian.toLowerCase().includes(custodian.toLowerCase()));
        }
        if (status) {
            packages = packages.filter(p =>
                (p.state && p.state.toUpperCase() === status.toUpperCase()) ||
                (p.status && p.status.toUpperCase() === status.toUpperCase())
            );
        }
        if (waste) {
            packages = packages.filter(p => p.wasteType && p.wasteType.toLowerCase().includes(waste.toLowerCase()));
        }
        if (risk) {
            packages = packages.filter(p => {
                const codes = p.riskCodes || p.riskCode;
                if (!codes) return false;
                if (Array.isArray(codes)) {
                    return codes.some(r => r.toLowerCase().includes(risk.toLowerCase()));
                }
                return String(codes).toLowerCase().includes(risk.toLowerCase());
            });
        }
        if (weightMin) {
            packages = packages.filter(p => p.weightKg >= parseFloat(weightMin));
        }
        if (weightMax) {
            packages = packages.filter(p => p.weightKg <= parseFloat(weightMax));
        }
        if (transfer) {
            packages = packages.filter(p => p.lastTransferId && p.lastTransferId.includes(transfer));
        }

        res.json(packages);
    } catch (error) {
        console.error("Query packages error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// GET /api/query/actors?filter= — Lista attori (filtro opzionale per prefisso)
router.get("/actors", async (req, res) => {
    try {
        const filter = req.query.filter || "";
        const options = await bc.getActorOptions(filter);
        res.json(options);
    } catch (error) {
        console.error("Query actors error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// GET /api/query/actor/:id — Fetch singolo attore
router.get("/actor/:id", async (req, res) => {
    try {
        const actor = await bc.getActor(req.params.id);
        if (!actor) return res.status(404).json({ error: "Actor not found" });
        actor.actorId = req.params.id;
        res.json(actor);
    } catch (error) {
        console.error("Query actor error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// GET /api/query/alerts — Tutti gli alert attivi
router.get("/alerts", async (req, res) => {
    try {
        const keys = await bc.getKeys("ALERT", ["AL"]);
        const promises = keys.map(k => {
            const id = k[1];
            return bc.getKV("ALERT", ["AL", id]).then(alert => {
                if (alert) { alert.id = id; return alert; }
                return null;
            });
        });
        let alerts = (await Promise.all(promises)).filter(a => a !== null);
        // filtra alert dismissi (legacy)
        alerts = alerts.filter(a => !a.dismissed);
        res.json(alerts);
    } catch (error) {
        console.error("Query alerts error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// GET /api/query/package-history/:id — Storico completo package
router.get("/package-history/:id", async (req, res) => {
    try {
        const entries = await bc.getKeyHistory("PACKAGE", ["PKG", req.params.id]);
        res.json(entries);
    } catch (error) {
        console.error("Query history error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// GET /api/query/transfer/:id — Fetch singolo transfer
router.get("/transfer/:id", async (req, res) => {
    try {
        const transfer = await bc.getTransfer(req.params.id);
        if (!transfer) return res.status(404).json({ error: "Transfer not found" });
        res.json(transfer);
    } catch (error) {
        console.error("Query transfer error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// GET /api/query/next-id?class=PACKAGE&bucket=PKG&prefix=PKG-2026-
router.get("/next-id", async (req, res) => {
    try {
        const { class: className, bucket, prefix } = req.query;
        if (!className || !bucket || !prefix) {
            return res.status(400).json({ error: "Missing required params: class, bucket, prefix" });
        }
        const nextId = await bc.generateNextId(className, bucket, prefix);
        res.json({ nextId });
    } catch (error) {
        console.error("Next-id error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

module.exports = router;
