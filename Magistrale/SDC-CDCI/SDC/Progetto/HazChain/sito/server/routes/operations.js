// --- Endpoint operazioni (validati server-side) ---
const express = require("express");
const bc = require("../services/blockchain");
const val = require("../services/validators");
const logger = require("../services/logger");
const { requireRole } = require("../middleware/auth");

const router = express.Router();

const asyncHandler = (fn) => async (req, res, next) => {
    try {
        await fn(req, res, next);
    } catch (error) {
        console.error(`${req.path} error:`, error.message);
        if (req.intentLogId) await logger.logConfirm(req.intentLogId, "ERROR", error.message).catch(() => {});
        if (!res.headersSent) res.status(500).json({ error: error.message });
    }
};

// NOTA: authRequired è applicato in index.js prima di questo router

// ====================================================================
// 1. POST /api/operations/create-package
//    Body: { createdBy, riskCode[], wasteType, weightKg }
//    Ruolo: LAB
// ====================================================================
router.post("/create-package", requireRole("LAB", "OPERATOR"), asyncHandler(async (req, res) => {
        const { createdBy, riskCode, wasteType, weightKg } = req.body;

        if (!createdBy || !riskCode || !wasteType || weightKg == null) {
            if (req.intentLogId) await logger.logValidation(req.intentLogId, "BLOCKED", "Missing required fields");
            return res.status(400).json({ error: "Missing required fields: createdBy, riskCode, wasteType, weightKg" });
        }

        // Genera ID
        const newId = await bc.generateNextId("PACKAGE", "PKG", "PKG-2026-");

        // Controllo duplicato
        const dupCheck = await val.validateDuplicatePackage(newId, req.intentLogId);
        if (!dupCheck.valid) {
            if (req.intentLogId) await logger.logValidation(req.intentLogId, "BLOCKED", dupCheck.reason);
            return res.status(409).json({ error: dupCheck.reason });
        }

        if (req.intentLogId) await logger.logValidation(req.intentLogId, "SUCCESS", "All checks passed");

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
        if (req.intentLogId) await logger.logExecution(req.intentLogId, "PACKAGE", newId, "SUCCESS");

        console.log(`[OP] Package ${newId} created by ${createdBy}`);
        if (req.intentLogId) await logger.logConfirm(req.intentLogId, "SUCCESS", `Package ${newId} created`);

        res.json({ success: true, packageId: newId, data: pkgValue });
}));

// ====================================================================
// 2. POST /api/operations/request-pickup  (Phase 1 - LAB → VAN)
//    Body: { packageId, to, weightKg, wasteType, riskCode[] }
//    Ruolo: LAB
// ====================================================================
router.post("/request-pickup", requireRole("LAB", "OPERATOR"), asyncHandler(async (req, res) => {
        const { packageId, to, weightKg, wasteType, riskCode } = req.body;
        if (!packageId || !to) {
            return res.status(400).json({ error: "Missing required fields: packageId, to" });
        }

        // Sequenza: deve essere IN_LAB
        const seqCheck = await val.checkSequence(packageId, ["IN_LAB"], "Request Pickup (Phase 1)", req.intentLogId);
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
        const intCheck = await val.validateTransferIntegrity(packageId, transferData, newTrId, req.intentLogId);
        if (!intCheck.valid) return res.status(409).json({ error: intCheck.reason });

        const riskCheck = await val.validateRiskCodeCompatibility(packageId, to, newTrId, req.intentLogId);
        if (!riskCheck.valid) return res.status(409).json({ error: riskCheck.reason });

        if (req.intentLogId) await logger.logValidation(req.intentLogId, "SUCCESS", "All checks passed");

        await bc.addKV("TRANSFER", ["TR", newTrId], transferData);
        await val.autoUpdatePackageAfterTransfer(newTrId, transferData);
        if (req.intentLogId) await logger.logExecution(req.intentLogId, "TRANSFER", newTrId, "SUCCESS");

        console.log(`[OP] Transfer ${newTrId} created: WAITING pickup from ${fromActor}`);
        if (req.intentLogId) await logger.logConfirm(req.intentLogId, "SUCCESS", `Transfer ${newTrId} created`);
        res.json({ success: true, transferId: newTrId, data: transferData });
}));

// ====================================================================
// 3. POST /api/operations/pickup-phase1  (VAN prende dal LAB)
//    Body: { packageId, to, weightKg, wasteType, riskCode[] }
//    Ruolo: TRANSPORT_LIGHT
// ====================================================================
router.post("/pickup-phase1", requireRole("TRANSPORT_LIGHT", "OPERATOR"), asyncHandler(async (req, res) => {
        const { packageId, to, weightKg, wasteType, riskCode } = req.body;
        if (!packageId) {
            return res.status(400).json({ error: "Missing required field: packageId" });
        }

        // Sequenza: deve essere IN_LAB
        const seqCheck = await val.checkSequence(packageId, ["IN_LAB"], "Pickup from Lab (Phase 1)", req.intentLogId);
        if (!seqCheck.valid) return res.status(409).json({ error: seqCheck.reason });

        const pkgState = seqCheck.pkgState;

        // Deve avere un transfer pendente TR-F-
        const pendCheck = await val.validatePendingTransfer(packageId, pkgState, "TR-F-", "Pickup from Lab (Phase 1)", req.intentLogId);
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
        const intCheck = await val.validateTransferIntegrity(packageId, transferData, transferId, req.intentLogId);
        if (!intCheck.valid) return res.status(409).json({ error: intCheck.reason });

        const riskCheck = await val.validateRiskCodeCompatibility(packageId, transferData.to, transferId, req.intentLogId);
        if (!riskCheck.valid) return res.status(409).json({ error: riskCheck.reason });

        const capCheck = await val.validateVehicleCapacity(fromActor, packageId, req.intentLogId);
        if (!capCheck.valid) return res.status(409).json({ error: capCheck.reason });

        // Merge con dati esistenti del transfer
        const mergedTransfer = { ...(existingTransfer || {}), ...transferData };

        if (req.intentLogId) await logger.logValidation(req.intentLogId, "SUCCESS", "All checks passed");

        await bc.addKV("TRANSFER", ["TR", transferId], mergedTransfer);
        await val.autoUpdatePackageAfterTransfer(transferId, mergedTransfer);
        await val.updateTransporterLoad(fromActor, packageId, "load");
        if (req.intentLogId) await logger.logExecution(req.intentLogId, "TRANSFER", transferId, "SUCCESS");

        console.log(`[OP] Transfer ${transferId}: SHIPPING by ${fromActor}`);
        if (req.intentLogId) await logger.logConfirm(req.intentLogId, "SUCCESS", `Transfer ${transferId} SHIPPING`);
        res.json({ success: true, transferId, data: mergedTransfer });
}));

// ====================================================================
// 4. POST /api/operations/arrive-hub  (VAN consegna a HUB)
//    Body: { packageId, weightKg?, wasteType?, riskCode[]? }
//    Ruolo: TRANSPORT_LIGHT
// ====================================================================
router.post("/arrive-hub", requireRole("TRANSPORT_LIGHT", "OPERATOR"), asyncHandler(async (req, res) => {
        const { packageId, weightKg, wasteType, riskCode } = req.body;
        if (!packageId) {
            return res.status(400).json({ error: "Missing required field: packageId" });
        }

        // Sequenza: deve essere F_TRANSPORT
        const seqCheck = await val.checkSequence(packageId, ["F_TRANSPORT"], "Arrive at Hub (Phase 1)", req.intentLogId);
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
        const intCheck = await val.validateTransferIntegrity(packageId, transferData, transferId, req.intentLogId);
        if (!intCheck.valid) return res.status(409).json({ error: intCheck.reason });

        if (req.intentLogId) await logger.logValidation(req.intentLogId, "SUCCESS", "All checks passed");

        await bc.addKV("TRANSFER", ["TR", transferId], transferData);
        await val.autoUpdatePackageAfterTransfer(transferId, transferData);

        // Scarica il veicolo
        if (existingTransfer && existingTransfer.from) {
            await val.updateTransporterLoad(existingTransfer.from, packageId, "unload");
        }
        if (req.intentLogId) await logger.logExecution(req.intentLogId, "TRANSFER", transferId, "SUCCESS");

        console.log(`[OP] Transfer ${transferId}: COMPLETED at HUB`);
        if (req.intentLogId) await logger.logConfirm(req.intentLogId, "SUCCESS", `Transfer ${transferId} COMPLETED at HUB`);
        res.json({ success: true, transferId, data: transferData });
}));

// ====================================================================
// 5. POST /api/operations/request-haz-pickup  (Phase 2 - HUB → TRUCK)
//    Body: { packageId, to, weightKg?, wasteType?, riskCode[]? }
//    Ruolo: HUB
// ====================================================================
router.post("/request-haz-pickup", requireRole("HUB", "OPERATOR"), asyncHandler(async (req, res) => {
        const { packageId, to, weightKg, wasteType, riskCode } = req.body;
        if (!packageId || !to) {
            return res.status(400).json({ error: "Missing required fields: packageId, to" });
        }

        // Sequenza: deve essere IN_HUB
        const seqCheck = await val.checkSequence(packageId, ["IN_HUB"], "Request Pickup (Phase 2)", req.intentLogId);
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
        const intCheck = await val.validateTransferIntegrity(packageId, transferData, newTrId, req.intentLogId);
        if (!intCheck.valid) return res.status(409).json({ error: intCheck.reason });

        const riskCheck = await val.validateRiskCodeCompatibility(packageId, to, newTrId, req.intentLogId);
        if (!riskCheck.valid) return res.status(409).json({ error: riskCheck.reason });

        if (req.intentLogId) await logger.logValidation(req.intentLogId, "SUCCESS", "All checks passed");

        await bc.addKV("TRANSFER", ["TR", newTrId], transferData);
        await val.autoUpdatePackageAfterTransfer(newTrId, transferData);
        if (req.intentLogId) await logger.logExecution(req.intentLogId, "TRANSFER", newTrId, "SUCCESS");

        console.log(`[OP] Transfer ${newTrId} created: WAITING haz pickup`);
        if (req.intentLogId) await logger.logConfirm(req.intentLogId, "SUCCESS", `Transfer ${newTrId} created`);
        res.json({ success: true, transferId: newTrId, data: transferData });
}));

// ====================================================================
// 6. POST /api/operations/pickup-phase2  (TRUCK prende da HUB)
//    Body: { packageId, to, weightKg?, wasteType?, riskCode[]? }
//    Ruolo: TRANSPORT_HAZ
// ====================================================================
router.post("/pickup-phase2", requireRole("TRANSPORT_HAZ", "OPERATOR"), asyncHandler(async (req, res) => {
        const { packageId, to, weightKg, wasteType, riskCode } = req.body;
        if (!packageId) {
            return res.status(400).json({ error: "Missing required field: packageId" });
        }

        // Sequenza: deve essere IN_HUB
        const seqCheck = await val.checkSequence(packageId, ["IN_HUB"], "Pickup from Hub (Phase 2)", req.intentLogId);
        if (!seqCheck.valid) return res.status(409).json({ error: seqCheck.reason });

        const pkgState = seqCheck.pkgState;

        // Deve avere un transfer pendente TR-C-
        const pendCheck = await val.validatePendingTransfer(packageId, pkgState, "TR-C-", "Pickup from Hub (Phase 2)", req.intentLogId);
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
        const intCheck = await val.validateTransferIntegrity(packageId, transferData, transferId, req.intentLogId);
        if (!intCheck.valid) return res.status(409).json({ error: intCheck.reason });

        const riskCheck = await val.validateRiskCodeCompatibility(packageId, transferData.to, transferId, req.intentLogId);
        if (!riskCheck.valid) return res.status(409).json({ error: riskCheck.reason });

        const capCheck = await val.validateVehicleCapacity(fromActor, packageId, req.intentLogId);
        if (!capCheck.valid) return res.status(409).json({ error: capCheck.reason });

        const mergedTransfer = { ...(existingTransfer || {}), ...transferData };

        if (req.intentLogId) await logger.logValidation(req.intentLogId, "SUCCESS", "All checks passed");

        await bc.addKV("TRANSFER", ["TR", transferId], mergedTransfer);
        await val.autoUpdatePackageAfterTransfer(transferId, mergedTransfer);
        await val.updateTransporterLoad(fromActor, packageId, "load");
        if (req.intentLogId) await logger.logExecution(req.intentLogId, "TRANSFER", transferId, "SUCCESS");

        console.log(`[OP] Transfer ${transferId}: SHIPPING by ${fromActor}`);
        if (req.intentLogId) await logger.logConfirm(req.intentLogId, "SUCCESS", `Transfer ${transferId} SHIPPING`);
        res.json({ success: true, transferId, data: mergedTransfer });
}));

// ====================================================================
// 7. POST /api/operations/arrive-landfill  (TRUCK consegna a LANDFILL)
//    Body: { packageId, weightKg?, wasteType?, riskCode[]? }
//    Ruolo: TRANSPORT_HAZ
// ====================================================================
router.post("/arrive-landfill", requireRole("TRANSPORT_HAZ", "OPERATOR"), asyncHandler(async (req, res) => {
        const { packageId, weightKg, wasteType, riskCode } = req.body;
        if (!packageId) {
            return res.status(400).json({ error: "Missing required field: packageId" });
        }

        // Sequenza: deve essere C_TRANSPORT
        const seqCheck = await val.checkSequence(packageId, ["C_TRANSPORT"], "Arrive at Landfill (Phase 2)", req.intentLogId);
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

        const intCheck = await val.validateTransferIntegrity(packageId, transferData, transferId, req.intentLogId);
        if (!intCheck.valid) return res.status(409).json({ error: intCheck.reason });

        if (req.intentLogId) await logger.logValidation(req.intentLogId, "SUCCESS", "All checks passed");

        await bc.addKV("TRANSFER", ["TR", transferId], transferData);
        await val.autoUpdatePackageAfterTransfer(transferId, transferData);

        if (existingTransfer && existingTransfer.from) {
            await val.updateTransporterLoad(existingTransfer.from, packageId, "unload");
        }
        if (req.intentLogId) await logger.logExecution(req.intentLogId, "TRANSFER", transferId, "SUCCESS");

        console.log(`[OP] Transfer ${transferId}: COMPLETED at LANDFILL`);
        if (req.intentLogId) await logger.logConfirm(req.intentLogId, "SUCCESS", `Transfer ${transferId} COMPLETED at LANDFILL`);
        res.json({ success: true, transferId, data: transferData });
}));

// ====================================================================
// 8. POST /api/operations/dispose-package
//    Body: { packageId }
//    Ruolo: LANDFILL
// ====================================================================
router.post("/dispose-package", requireRole("LANDFILL", "OPERATOR"), asyncHandler(async (req, res) => {
        const { packageId } = req.body;
        if (!packageId) {
            return res.status(400).json({ error: "Missing required field: packageId" });
        }

        // Sequenza: deve essere IN_LANDFILL
        const seqCheck = await val.checkSequence(packageId, ["IN_LANDFILL"], "Dispose Package", req.intentLogId);
        if (!seqCheck.valid) return res.status(409).json({ error: seqCheck.reason });

        const pkgState = seqCheck.pkgState;

        if (req.intentLogId) await logger.logValidation(req.intentLogId, "SUCCESS", "All checks passed");

        const newPkgValue = {
            ...pkgState,
            state: "DISPOSED",
            lastUpdateTs: new Date().toISOString()
        };

        await bc.addKV("PACKAGE", ["PKG", packageId], newPkgValue);
        if (req.intentLogId) await logger.logExecution(req.intentLogId, "PACKAGE", packageId, "SUCCESS");

        console.log(`[OP] Package ${packageId}: DISPOSED`);
        if (req.intentLogId) await logger.logConfirm(req.intentLogId, "SUCCESS", `Package ${packageId} DISPOSED`);
        res.json({ success: true, packageId, data: newPkgValue });
}));

// ====================================================================
// 9. POST /api/operations/create-actor
//    Body: { actorId, name, role, location?, company?, capacityKg?, riskCodesHandled[] }
//    Ruolo: SUPERUSER
// ====================================================================
router.post("/create-actor", requireRole("SUPERUSER"), asyncHandler(async (req, res) => {
        const { actorId, name, role, location, company, capacityKg, riskCodesHandled } = req.body;
        if (!actorId || !name || !role) {
            return res.status(400).json({ error: "Missing required fields: actorId, name, role" });
        }

        // Validazione formato ID
        const fmtCheck = val.validateActorIdFormat(actorId, role);
        if (!fmtCheck.valid) return res.status(400).json({ error: fmtCheck.reason });

        // Controllo duplicato
        const dupCheck = await val.validateDuplicateActor(actorId, req.intentLogId);
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

        if (req.intentLogId) await logger.logValidation(req.intentLogId, "SUCCESS", "All checks passed");

        await bc.addKV("ACTOR", ["ACT", actorId], actorValue);
        if (req.intentLogId) await logger.logExecution(req.intentLogId, "ACTOR", actorId, "SUCCESS");

        console.log(`[OP] Actor ${actorId} created (role: ${role})`);
        if (req.intentLogId) await logger.logConfirm(req.intentLogId, "SUCCESS", `Actor ${actorId} created`);
        res.json({ success: true, actorId, data: actorValue });
}));

// ====================================================================
// 10. POST /api/operations/dismiss-alert
//     Body: { alertId }
//     Ruolo: SUPERUSER
// ====================================================================
router.post("/dismiss-alert", requireRole("SUPERUSER"), asyncHandler(async (req, res) => {
        const { alertId } = req.body;
        if (!alertId) {
            return res.status(400).json({ error: "Missing required field: alertId" });
        }

        if (req.intentLogId) await logger.logValidation(req.intentLogId, "SUCCESS", "Alert exists");

        await bc.delKV("ALERT", ["AL", alertId]);
        if (req.intentLogId) await logger.logExecution(req.intentLogId, "ALERT", alertId, "SUCCESS");

        console.log(`[OP] Alert ${alertId} dismissed`);
        if (req.intentLogId) await logger.logConfirm(req.intentLogId, "SUCCESS", `Alert ${alertId} dismissed`);
        res.json({ success: true, alertId });
}));

// ====================================================================
// 11. POST /api/operations/purge-all-alerts
//     Elimina TUTTI gli alert dalla blockchain
//     Ruolo: SUPERUSER
// ====================================================================
router.post("/purge-all-alerts", requireRole("SUPERUSER"), asyncHandler(async (req, res) => {
        const keys = await bc.getKeys("ALERT", ["AL"]);
        let deleted = 0;

        for (const k of keys) {
            await bc.delKV("ALERT", ["AL", k[1]]);
            deleted++;
        }

        if (req.intentLogId) await logger.logExecution(req.intentLogId, "ALERT", "ALL", "SUCCESS");
        if (req.intentLogId) await logger.logConfirm(req.intentLogId, "SUCCESS", `Purged ${deleted} alerts`);

        console.log(`[OP] Purged all alerts (${deleted} total)`);
        res.json({ success: true, deleted });
}));

// ====================================================================
// 12. POST /api/operations/purge-all-logs
//     Elimina TUTTI i log dalla blockchain (NORMAL, WARNING, CRITICAL)
//     Ruolo: SUPERUSER
// ====================================================================
router.post("/purge-all-logs", requireRole("SUPERUSER"), asyncHandler(async (req, res) => {
        const levels = ["LOG-NORMAL", "LOG-WARNING", "LOG-CRITICAL"];
        let deleted = 0;

        for (const bucket of levels) {
            const keys = await bc.getKeys("LOG", [bucket]);
            for (const k of keys) {
                await bc.delKV("LOG", [bucket, k[1]]);
                deleted++;
            }
        }

        if (req.intentLogId) await logger.logConfirm(req.intentLogId, "SUCCESS", `Purged ${deleted} logs`);

        console.log(`[OP] Purged all logs (${deleted} total)`);
        res.json({ success: true, deleted });
}));

module.exports = router;
