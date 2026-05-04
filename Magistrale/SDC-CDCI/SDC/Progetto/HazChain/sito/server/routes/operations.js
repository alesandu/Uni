// --- Endpoint operazioni (validati server-side) ---
const express = require("express");
const bc = require("../services/blockchain");
const val = require("../services/validators");
const { authRequired, requireRole } = require("../middleware/auth");

const router = express.Router();

// Tutti gli endpoint richiedono autenticazione
router.use(authRequired);

// ====================================================================
// 1. POST /api/operations/create-package
//    Body: { createdBy, riskCode[], wasteType, weightKg }
//    Ruolo: LAB
// ====================================================================
router.post("/create-package", requireRole("LAB"), async (req, res) => {
    try {
        const { createdBy, riskCode, wasteType, weightKg } = req.body;

        if (!createdBy || !riskCode || !wasteType || weightKg == null) {
            return res.status(400).json({ error: "Missing required fields: createdBy, riskCode, wasteType, weightKg" });
        }

        // Genera ID
        const newId = await bc.generateNextId("PACKAGE", "PKG", "PKG-2026-");

        // Controllo duplicato
        const dupCheck = await val.validateDuplicatePackage(newId);
        if (!dupCheck.valid) return res.status(409).json({ error: dupCheck.reason });

        const pkgValue = {
            createdBy,
            riskCode: Array.isArray(riskCode) ? riskCode : [riskCode],
            wasteType,
            weightKg: Number(weightKg),
            state: "IN_LAB",
            currentCustodian: createdBy,
            lastTransferId: "null",
            lastUpdateTs: new Date().toISOString()
        };

        await bc.addKV("PACKAGE", ["PKG", newId], pkgValue);
        console.log(`[OP] Package ${newId} created by ${createdBy}`);

        res.json({ success: true, packageId: newId, data: pkgValue });
    } catch (error) {
        console.error("create-package error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// ====================================================================
// 2. POST /api/operations/request-pickup  (Phase 1 - LAB → VAN)
//    Body: { packageId, to, weightKg, wasteType, riskCode[] }
//    Ruolo: LAB
// ====================================================================
router.post("/request-pickup", requireRole("LAB"), async (req, res) => {
    try {
        const { packageId, to, weightKg, wasteType, riskCode } = req.body;
        if (!packageId || !to) {
            return res.status(400).json({ error: "Missing required fields: packageId, to" });
        }

        // Sequenza: deve essere IN_LAB
        const seqCheck = await val.checkSequence(packageId, ["IN_LAB"], "Request Pickup (Phase 1)");
        if (!seqCheck.valid) return res.status(409).json({ error: seqCheck.reason });

        const pkgState = seqCheck.pkgState;
        const fromActor = pkgState.currentCustodian || pkgState.createdBy || "LAB-01";
        const newTrId = await bc.generateNextId("TRANSFER", "TR", "TR-F-");

        const transferData = {
            packageId,
            from: fromActor,
            to,
            weightKg: weightKg != null ? Number(weightKg) : pkgState.weightKg,
            wasteType: wasteType || pkgState.wasteType,
            riskCode: riskCode || pkgState.riskCode,
            action: "PICKUP",
            status: "WAITING",
            ts: new Date().toISOString()
        };

        // Validazioni
        const intCheck = await val.validateTransferIntegrity(packageId, transferData, newTrId);
        if (!intCheck.valid) return res.status(409).json({ error: intCheck.reason });

        const riskCheck = await val.validateRiskCodeCompatibility(packageId, to, newTrId);
        if (!riskCheck.valid) return res.status(409).json({ error: riskCheck.reason });

        await bc.addKV("TRANSFER", ["TR", newTrId], transferData);
        await val.autoUpdatePackageAfterTransfer(newTrId, transferData);

        console.log(`[OP] Transfer ${newTrId} created: WAITING pickup from ${fromActor}`);
        res.json({ success: true, transferId: newTrId, data: transferData });
    } catch (error) {
        console.error("request-pickup error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// ====================================================================
// 3. POST /api/operations/pickup-phase1  (VAN prende dal LAB)
//    Body: { packageId, to, weightKg, wasteType, riskCode[] }
//    Ruolo: TRANSPORT_LIGHT
// ====================================================================
router.post("/pickup-phase1", requireRole("TRANSPORT_LIGHT"), async (req, res) => {
    try {
        const { packageId, to, weightKg, wasteType, riskCode } = req.body;
        if (!packageId) {
            return res.status(400).json({ error: "Missing required field: packageId" });
        }

        // Sequenza: deve essere IN_LAB
        const seqCheck = await val.checkSequence(packageId, ["IN_LAB"], "Pickup from Lab (Phase 1)");
        if (!seqCheck.valid) return res.status(409).json({ error: seqCheck.reason });

        const pkgState = seqCheck.pkgState;

        // Deve avere un transfer pendente TR-F-
        const pendCheck = await val.validatePendingTransfer(packageId, pkgState, "TR-F-", "Pickup from Lab (Phase 1)");
        if (!pendCheck.valid) return res.status(409).json({ error: pendCheck.reason });

        const transferId = pkgState.lastTransferId;
        const existingTransfer = await bc.getTransfer(transferId);

        // fromActor = il van assegnato nella request-pickup
        let fromActor = existingTransfer && existingTransfer.to ? existingTransfer.to : "F-01";

        const transferData = {
            packageId,
            from: fromActor,
            to: to || (existingTransfer ? existingTransfer.to : "HUB-01"),
            weightKg: weightKg != null ? Number(weightKg) : pkgState.weightKg,
            wasteType: wasteType || pkgState.wasteType,
            riskCode: riskCode || pkgState.riskCode,
            action: "SHIPPING",
            status: "SHIPPING",
            ts: new Date().toISOString()
        };

        // Validazioni
        const intCheck = await val.validateTransferIntegrity(packageId, transferData, transferId);
        if (!intCheck.valid) return res.status(409).json({ error: intCheck.reason });

        const riskCheck = await val.validateRiskCodeCompatibility(packageId, transferData.to, transferId);
        if (!riskCheck.valid) return res.status(409).json({ error: riskCheck.reason });

        const capCheck = await val.validateVehicleCapacity(fromActor, packageId);
        if (!capCheck.valid) return res.status(409).json({ error: capCheck.reason });

        // Merge con dati esistenti del transfer
        const mergedTransfer = { ...(existingTransfer || {}), ...transferData };

        await bc.addKV("TRANSFER", ["TR", transferId], mergedTransfer);
        await val.autoUpdatePackageAfterTransfer(transferId, mergedTransfer);
        await val.updateTransporterLoad(fromActor, packageId, "load");

        console.log(`[OP] Transfer ${transferId}: SHIPPING by ${fromActor}`);
        res.json({ success: true, transferId, data: mergedTransfer });
    } catch (error) {
        console.error("pickup-phase1 error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// ====================================================================
// 4. POST /api/operations/arrive-hub  (VAN consegna a HUB)
//    Body: { packageId, weightKg?, wasteType?, riskCode[]? }
//    Ruolo: TRANSPORT_LIGHT
// ====================================================================
router.post("/arrive-hub", requireRole("TRANSPORT_LIGHT"), async (req, res) => {
    try {
        const { packageId, weightKg, wasteType, riskCode } = req.body;
        if (!packageId) {
            return res.status(400).json({ error: "Missing required field: packageId" });
        }

        // Sequenza: deve essere F_TRANSPORT
        const seqCheck = await val.checkSequence(packageId, ["F_TRANSPORT"], "Arrive at Hub (Phase 1)");
        if (!seqCheck.valid) return res.status(409).json({ error: seqCheck.reason });

        const pkgState = seqCheck.pkgState;
        const transferId = pkgState.lastTransferId;
        const existingTransfer = await bc.getTransfer(transferId);

        const transferData = {
            ...(existingTransfer || {}),
            weightKg: weightKg != null ? Number(weightKg) : (existingTransfer ? existingTransfer.weightKg : pkgState.weightKg),
            wasteType: wasteType || (existingTransfer ? existingTransfer.wasteType : pkgState.wasteType),
            riskCode: riskCode || (existingTransfer ? existingTransfer.riskCode : pkgState.riskCode),
            status: "COMPLETED",
            action: "DELIVER",
            ts: new Date().toISOString()
        };

        // Validazione integrità
        const intCheck = await val.validateTransferIntegrity(packageId, transferData, transferId);
        if (!intCheck.valid) return res.status(409).json({ error: intCheck.reason });

        await bc.addKV("TRANSFER", ["TR", transferId], transferData);
        await val.autoUpdatePackageAfterTransfer(transferId, transferData);

        // Scarica il veicolo
        if (existingTransfer && existingTransfer.from) {
            await val.updateTransporterLoad(existingTransfer.from, packageId, "unload");
        }

        console.log(`[OP] Transfer ${transferId}: COMPLETED at HUB`);
        res.json({ success: true, transferId, data: transferData });
    } catch (error) {
        console.error("arrive-hub error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// ====================================================================
// 5. POST /api/operations/request-haz-pickup  (Phase 2 - HUB → TRUCK)
//    Body: { packageId, to, weightKg?, wasteType?, riskCode[]? }
//    Ruolo: HUB
// ====================================================================
router.post("/request-haz-pickup", requireRole("HUB"), async (req, res) => {
    try {
        const { packageId, to, weightKg, wasteType, riskCode } = req.body;
        if (!packageId || !to) {
            return res.status(400).json({ error: "Missing required fields: packageId, to" });
        }

        // Sequenza: deve essere IN_HUB
        const seqCheck = await val.checkSequence(packageId, ["IN_HUB"], "Request Pickup (Phase 2)");
        if (!seqCheck.valid) return res.status(409).json({ error: seqCheck.reason });

        const pkgState = seqCheck.pkgState;
        const fromActor = pkgState.currentCustodian || "HUB-01";
        const newTrId = await bc.generateNextId("TRANSFER", "TR", "TR-C-");

        const transferData = {
            packageId,
            from: fromActor,
            to,
            weightKg: weightKg != null ? Number(weightKg) : pkgState.weightKg,
            wasteType: wasteType || pkgState.wasteType,
            riskCode: riskCode || pkgState.riskCode,
            action: "PICKUP",
            status: "WAITING",
            ts: new Date().toISOString()
        };

        // Validazioni
        const intCheck = await val.validateTransferIntegrity(packageId, transferData, newTrId);
        if (!intCheck.valid) return res.status(409).json({ error: intCheck.reason });

        const riskCheck = await val.validateRiskCodeCompatibility(packageId, to, newTrId);
        if (!riskCheck.valid) return res.status(409).json({ error: riskCheck.reason });

        await bc.addKV("TRANSFER", ["TR", newTrId], transferData);
        await val.autoUpdatePackageAfterTransfer(newTrId, transferData);

        console.log(`[OP] Transfer ${newTrId} created: WAITING haz pickup`);
        res.json({ success: true, transferId: newTrId, data: transferData });
    } catch (error) {
        console.error("request-haz-pickup error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// ====================================================================
// 6. POST /api/operations/pickup-phase2  (TRUCK prende da HUB)
//    Body: { packageId, to, weightKg?, wasteType?, riskCode[]? }
//    Ruolo: TRANSPORT_HAZ
// ====================================================================
router.post("/pickup-phase2", requireRole("TRANSPORT_HAZ"), async (req, res) => {
    try {
        const { packageId, to, weightKg, wasteType, riskCode } = req.body;
        if (!packageId) {
            return res.status(400).json({ error: "Missing required field: packageId" });
        }

        // Sequenza: deve essere IN_HUB
        const seqCheck = await val.checkSequence(packageId, ["IN_HUB"], "Pickup from Hub (Phase 2)");
        if (!seqCheck.valid) return res.status(409).json({ error: seqCheck.reason });

        const pkgState = seqCheck.pkgState;

        // Deve avere un transfer pendente TR-C-
        const pendCheck = await val.validatePendingTransfer(packageId, pkgState, "TR-C-", "Pickup from Hub (Phase 2)");
        if (!pendCheck.valid) return res.status(409).json({ error: pendCheck.reason });

        const transferId = pkgState.lastTransferId;
        const existingTransfer = await bc.getTransfer(transferId);

        let fromActor = existingTransfer && existingTransfer.to ? existingTransfer.to : "C-01";

        const transferData = {
            packageId,
            from: fromActor,
            to: to || (existingTransfer ? existingTransfer.to : "LANDFILL-01"),
            weightKg: weightKg != null ? Number(weightKg) : pkgState.weightKg,
            wasteType: wasteType || pkgState.wasteType,
            riskCode: riskCode || pkgState.riskCode,
            action: "SHIPPING",
            status: "SHIPPING",
            ts: new Date().toISOString()
        };

        // Validazioni
        const intCheck = await val.validateTransferIntegrity(packageId, transferData, transferId);
        if (!intCheck.valid) return res.status(409).json({ error: intCheck.reason });

        const riskCheck = await val.validateRiskCodeCompatibility(packageId, transferData.to, transferId);
        if (!riskCheck.valid) return res.status(409).json({ error: riskCheck.reason });

        const capCheck = await val.validateVehicleCapacity(fromActor, packageId);
        if (!capCheck.valid) return res.status(409).json({ error: capCheck.reason });

        const mergedTransfer = { ...(existingTransfer || {}), ...transferData };

        await bc.addKV("TRANSFER", ["TR", transferId], mergedTransfer);
        await val.autoUpdatePackageAfterTransfer(transferId, mergedTransfer);
        await val.updateTransporterLoad(fromActor, packageId, "load");

        console.log(`[OP] Transfer ${transferId}: SHIPPING by ${fromActor}`);
        res.json({ success: true, transferId, data: mergedTransfer });
    } catch (error) {
        console.error("pickup-phase2 error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// ====================================================================
// 7. POST /api/operations/arrive-landfill  (TRUCK consegna a LANDFILL)
//    Body: { packageId, weightKg?, wasteType?, riskCode[]? }
//    Ruolo: TRANSPORT_HAZ
// ====================================================================
router.post("/arrive-landfill", requireRole("TRANSPORT_HAZ"), async (req, res) => {
    try {
        const { packageId, weightKg, wasteType, riskCode } = req.body;
        if (!packageId) {
            return res.status(400).json({ error: "Missing required field: packageId" });
        }

        // Sequenza: deve essere C_TRANSPORT
        const seqCheck = await val.checkSequence(packageId, ["C_TRANSPORT"], "Arrive at Landfill (Phase 2)");
        if (!seqCheck.valid) return res.status(409).json({ error: seqCheck.reason });

        const pkgState = seqCheck.pkgState;
        const transferId = pkgState.lastTransferId;
        const existingTransfer = await bc.getTransfer(transferId);

        const transferData = {
            ...(existingTransfer || {}),
            weightKg: weightKg != null ? Number(weightKg) : (existingTransfer ? existingTransfer.weightKg : pkgState.weightKg),
            wasteType: wasteType || (existingTransfer ? existingTransfer.wasteType : pkgState.wasteType),
            riskCode: riskCode || (existingTransfer ? existingTransfer.riskCode : pkgState.riskCode),
            status: "COMPLETED",
            action: "DELIVER",
            ts: new Date().toISOString()
        };

        const intCheck = await val.validateTransferIntegrity(packageId, transferData, transferId);
        if (!intCheck.valid) return res.status(409).json({ error: intCheck.reason });

        await bc.addKV("TRANSFER", ["TR", transferId], transferData);
        await val.autoUpdatePackageAfterTransfer(transferId, transferData);

        if (existingTransfer && existingTransfer.from) {
            await val.updateTransporterLoad(existingTransfer.from, packageId, "unload");
        }

        console.log(`[OP] Transfer ${transferId}: COMPLETED at LANDFILL`);
        res.json({ success: true, transferId, data: transferData });
    } catch (error) {
        console.error("arrive-landfill error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// ====================================================================
// 8. POST /api/operations/dispose-package
//    Body: { packageId }
//    Ruolo: LANDFILL
// ====================================================================
router.post("/dispose-package", requireRole("LANDFILL"), async (req, res) => {
    try {
        const { packageId } = req.body;
        if (!packageId) {
            return res.status(400).json({ error: "Missing required field: packageId" });
        }

        // Sequenza: deve essere IN_LANDFILL
        const seqCheck = await val.checkSequence(packageId, ["IN_LANDFILL"], "Dispose Package");
        if (!seqCheck.valid) return res.status(409).json({ error: seqCheck.reason });

        const pkgState = seqCheck.pkgState;
        const newPkgValue = {
            ...pkgState,
            state: "DISPOSED",
            lastUpdateTs: new Date().toISOString()
        };

        await bc.addKV("PACKAGE", ["PKG", packageId], newPkgValue);

        console.log(`[OP] Package ${packageId}: DISPOSED`);
        res.json({ success: true, packageId, data: newPkgValue });
    } catch (error) {
        console.error("dispose-package error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// ====================================================================
// 9. POST /api/operations/create-actor
//    Body: { actorId, name, role, location?, company?, capacityKg?, riskCodesHandled[] }
//    Ruolo: SUPERUSER
// ====================================================================
router.post("/create-actor", requireRole("SUPERUSER"), async (req, res) => {
    try {
        const { actorId, name, role, location, company, capacityKg, riskCodesHandled } = req.body;
        if (!actorId || !name || !role) {
            return res.status(400).json({ error: "Missing required fields: actorId, name, role" });
        }

        // Validazione formato ID
        const fmtCheck = val.validateActorIdFormat(actorId, role);
        if (!fmtCheck.valid) return res.status(400).json({ error: fmtCheck.reason });

        // Controllo duplicato
        const dupCheck = await val.validateDuplicateActor(actorId);
        if (!dupCheck.valid) return res.status(409).json({ error: dupCheck.reason });

        const actorValue = { name, role };
        if (location) actorValue.location = location;
        if (company) actorValue.company = company;
        if (capacityKg != null) actorValue.capacityKg = Number(capacityKg);
        if (riskCodesHandled) {
            actorValue.riskCodesHandled = Array.isArray(riskCodesHandled) ? riskCodesHandled : [riskCodesHandled];
        }
        // Trasportatori hanno transportedKg
        if (role === "TRANSPORT_LIGHT" || role === "TRANSPORT_HAZ") {
            actorValue.transportedKg = 0;
        }

        await bc.addKV("ACTOR", ["ACT", actorId], actorValue);

        console.log(`[OP] Actor ${actorId} created (role: ${role})`);
        res.json({ success: true, actorId, data: actorValue });
    } catch (error) {
        console.error("create-actor error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// ====================================================================
// 10. POST /api/operations/dismiss-alert
//     Body: { alertId }
//     Ruolo: SUPERUSER
// ====================================================================
router.post("/dismiss-alert", requireRole("SUPERUSER"), async (req, res) => {
    try {
        const { alertId } = req.body;
        if (!alertId) {
            return res.status(400).json({ error: "Missing required field: alertId" });
        }

        await bc.delKV("ALERT", ["AL", alertId]);

        console.log(`[OP] Alert ${alertId} dismissed`);
        res.json({ success: true, alertId });
    } catch (error) {
        console.error("dismiss-alert error:", error.message);
        res.status(500).json({ error: error.message });
    }
});

module.exports = router;
