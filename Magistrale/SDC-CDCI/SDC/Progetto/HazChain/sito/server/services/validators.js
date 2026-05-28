// --- Validazioni di business (spostate dal client per sicurezza) ---
const bc = require("./blockchain");

// ====================================================================
// HELPER: crea un alert sulla blockchain
// ====================================================================
async function createAlert(alertRecord, logId) {
    const alertId = await bc.generateNextId("ALERT", "AL", "AL-");
    alertRecord.ts = new Date().toISOString();
    if (logId) {
        alertRecord.sourceLogId = logId;
    }
    if (!alertRecord.criticality) {
        alertRecord.criticality = "WARNING";
    }

    // Se l'alert è OPERATIONAL, scrivi anche un log WARNING sulla blockchain
    if (alertRecord.category === "OPERATIONAL") {
        const warningLogId = await bc.writeLog("WARNING", {
            phase: "ALERT",
            parentLogId: logId || null,
            result: "BLOCKED",
            reason: alertRecord.msg || alertRecord.type,
            alertType: alertRecord.type,
            details: {
                alertId,
                packageId: alertRecord.packageId || null,
                entityId: alertRecord.entityId || null,
                vehicleId: alertRecord.vehicleId || null,
                transferId: alertRecord.transferId || null
            }
        });
        alertRecord.sourceLogId = alertRecord.sourceLogId || warningLogId;
        console.log(`[VALIDATOR] Alert ${alertId} + LOG ${warningLogId} created: ${alertRecord.type}`);
    } else {
        console.log(`[VALIDATOR] Alert ${alertId} created: ${alertRecord.type} (source: ${logId || 'none'})`);
    }

    await bc.addKV("ALERT", ["AL", alertId], alertRecord);
    return alertId;
}

// ====================================================================
// 1. Validazione Ruolo Utente
// ====================================================================
async function validateUserRole(userRole, operationName, requiredRole, intentLogId) {
    if (userRole === "SUPERUSER") return { valid: true };
    if (requiredRole && userRole !== requiredRole) {
        const reason = `Unauthorized: logged as ${userRole}, but '${operationName}' requires ${requiredRole}.`;
        await createAlert({
            type: "UNAUTHORIZED_ACTION",
            category: "SECURITY",
            criticality: "CRITICAL",
            userRole,
            requiredRole,
            attemptedAction: operationName,
            msg: reason
        }, intentLogId);
        return { valid: false, reason };
    }
    return { valid: true };
}

// ====================================================================
// 2. Validazione Entità Duplicata
// ====================================================================
async function validateDuplicatePackage(pkgId, intentLogId) {
    const existing = await bc.getPackage(pkgId);
    if (existing) {
        const reason = `Duplicate entity blocked: Package ${pkgId} already exists (state: ${existing.state || 'unknown'}).`;
        await createAlert({
            type: "DUPLICATE_ENTITY",
            category: "OPERATIONAL",
            entityClass: "PACKAGE",
            entityId: pkgId,
            existingState: existing.state || "unknown",
            msg: reason
        }, intentLogId);
        return { valid: false, reason };
    }
    return { valid: true };
}

async function validateDuplicateActor(actorId, intentLogId) {
    const existing = await bc.getActor(actorId);
    if (existing) {
        const reason = `Duplicate entity blocked: Actor ${actorId} already exists (role: ${existing.role || 'unknown'}).`;
        await createAlert({
            type: "DUPLICATE_ENTITY",
            category: "OPERATIONAL",
            entityClass: "ACTOR",
            entityId: actorId,
            existingRole: existing.role || "unknown",
            msg: reason
        }, intentLogId);
        return { valid: false, reason };
    }
    return { valid: true };
}

// ====================================================================
// 3. Validazione Sequenza (Flow: LAB → VAN → HUB → TRUCK → LANDFILL)
// ====================================================================
async function checkSequence(pkgId, requiredStates, actionLabel, intentLogId) {
    const pkgState = await bc.getPackage(pkgId);
    if (!pkgState || !pkgState.state) {
        const reason = `Package ${pkgId} not found or has no state.`;
        return { valid: false, reason, pkgState: null };
    }

    const currentState = pkgState.state;
    if (requiredStates.includes(currentState)) {
        return { valid: true, pkgState };
    }

    const reason = `${actionLabel} blocked: Package ${pkgId} is in state ${currentState}, but must be ${requiredStates.join(" or ")}. Flow: LAB -> VAN -> HUB -> TRUCK -> LANDFILL`;
    await createAlert({
        type: "SEQUENCE_VIOLATION",
        category: "OPERATIONAL",
        packageId: pkgId,
        currentState,
        requiredStates,
        attemptedAction: actionLabel,
        msg: reason
    }, intentLogId);
    return { valid: false, reason, pkgState };
}

// ====================================================================
// 4. Validazione Integrità Transfer (confronto dati transfer vs package)
// ====================================================================
async function validateTransferIntegrity(packageId, transferData, transferId, intentLogId) {
    const pkgState = await bc.getPackage(packageId);
    if (!pkgState) return { valid: true }; // non c'è nulla da confrontare

    const mismatches = {};

    // weightKg
    const pkgWeight = Number(pkgState.weightKg);
    const trWeight = Number(transferData.weightKg);
    if (!isNaN(pkgWeight) && !isNaN(trWeight) && pkgWeight !== trWeight) {
        mismatches.weightKg = { expected: pkgWeight, got: trWeight };
    }

    // wasteType
    const pkgWaste = (pkgState.wasteType || "").trim();
    const trWaste = (transferData.wasteType || "").trim();
    if (pkgWaste && trWaste && pkgWaste !== trWaste) {
        mismatches.wasteType = { expected: pkgWaste, got: trWaste };
    }

    // riskCode
    const pkgRisk = [...(pkgState.riskCode || [])].sort();
    const trRisk = [...(transferData.riskCode || [])].sort();
    if (pkgRisk.length > 0 && trRisk.length > 0 &&
        (pkgRisk.length !== trRisk.length || pkgRisk.some((v, i) => v !== trRisk[i]))) {
        mismatches.riskCode = { expected: pkgRisk, got: trRisk };
    }

    if (Object.keys(mismatches).length === 0) {
        return { valid: true };
    }

    const changedFields = Object.keys(mismatches).join(", ");
    const reason = `Transfer ${transferId} blocked: ${changedFields} mismatch with Package ${packageId}`;
    let detailParts = [];
    for (const field in mismatches) {
        const m = mismatches[field];
        const exp = Array.isArray(m.expected) ? m.expected.join(",") : m.expected;
        const got = Array.isArray(m.got) ? m.got.join(",") : m.got;
        detailParts.push(`${field}: expected [${exp}] got [${got}]`);
    }

    await createAlert({
        type: "INTEGRITY_MISMATCH",
        category: "OPERATIONAL",
        packageId,
        transferId,
        mismatches,
        msg: `${reason}. Details: ${detailParts.join("; ")}`
    }, intentLogId);
    return { valid: false, reason };
}

// ====================================================================
// 5. Validazione Compatibilità Risk Codes
// ====================================================================
async function validateRiskCodeCompatibility(packageId, destinationActorId, transferId, intentLogId) {
    const pkgState = await bc.getPackage(packageId);
    if (!pkgState) return { valid: true };

    const pkgRiskCodes = pkgState.riskCode || [];

    const actorState = await bc.getActor(destinationActorId);
    if (!actorState) return { valid: true };

    let actorRiskCodes = actorState.riskCodesHandled;
    if (!Array.isArray(actorRiskCodes)) {
        actorRiskCodes = [actorRiskCodes];
    }

    const overlap = pkgRiskCodes.filter(code => actorRiskCodes.includes(code));

    if (overlap.length > 0) {
        return { valid: true };
    }

    const reason = `Transfer ${transferId} blocked: Package ${packageId} riskCodes [${pkgRiskCodes.join(", ")}] are NOT handled by ${destinationActorId} [${actorRiskCodes.join(", ")}]`;
    await createAlert({
        type: "RISK_CODE_INCOMPATIBLE",
        category: "OPERATIONAL",
        packageId,
        transferId,
        actorId: destinationActorId,
        packageRiskCodes: pkgRiskCodes,
        actorRiskCodesHandled: actorRiskCodes,
        msg: reason
    }, intentLogId);
    return { valid: false, reason };
}

// ====================================================================
// 6. Validazione Capacità Veicolo
// ====================================================================
async function validateVehicleCapacity(transporterId, packageId, intentLogId) {
    const actorState = await bc.getActor(transporterId);
    if (!actorState) return { valid: true };

    const capacityKg = Number(actorState.capacityKg);
    const currentLoad = Number(actorState.transportedKg) || 0;

    const pkgState = await bc.getPackage(packageId);
    const packageWeight = pkgState ? Number(pkgState.weightKg) || 0 : 0;

    const newLoad = currentLoad + packageWeight;

    if (capacityKg > 0 && newLoad > capacityKg) {
        const reason = `Vehicle ${transporterId} capacity exceeded: ${newLoad}kg would exceed ${capacityKg}kg limit (current load: ${currentLoad}kg, package: ${packageWeight}kg)`;
        await createAlert({
            type: "CAPACITY_EXCEEDED",
            category: "OPERATIONAL",
            vehicleId: transporterId,
            packageId: packageId || "unknown",
            capacityKg,
            currentLoadKg: currentLoad,
            packageWeightKg: packageWeight,
            attemptedLoadKg: newLoad,
            msg: reason
        }, intentLogId);
        return { valid: false, reason };
    }
    return { valid: true };
}

// ====================================================================
// 7. Validazione Richiesta Transfer Pendente
// ====================================================================
async function validatePendingTransfer(pkgId, pkgState, requiredPrefix, actionLabel, intentLogId) {
    const lastTr = pkgState ? pkgState.lastTransferId : null;
    if (!lastTr || !lastTr.startsWith(requiredPrefix)) {
        const reason = `${actionLabel} blocked: Package ${pkgId} has no pending ${requiredPrefix} request. lastTransferId=${lastTr || 'none'}.`;
        await createAlert({
            type: "SEQUENCE_VIOLATION",
            category: "OPERATIONAL",
            packageId: pkgId,
            currentState: pkgState ? pkgState.state : "unknown",
            requiredTransferPrefix: requiredPrefix,
            attemptedAction: actionLabel,
            msg: reason
        }, intentLogId);
        return { valid: false, reason };
    }
    return { valid: true };
}

// ====================================================================
// 8. Aggiornamento automatico Package dopo Transfer
// ====================================================================
async function autoUpdatePackageAfterTransfer(transferId, transferData) {
    const packageId = transferData.packageId;
    if (!packageId) return;

    const pkgData = await bc.getPackage(packageId);
    if (!pkgData) return;

    let newCustodian = pkgData.currentCustodian;
    let newState = pkgData.state;
    const status = transferData.status;

    // Recupera il ruolo dell'attore destinazione dalla blockchain
    // invece di usare string matching fragile sull'ID
    const destActor = await bc.getActor(transferData.to);
    const destRole = destActor ? destActor.role : null;

    if (status === "WAITING") {
        newCustodian = transferData.from;
    } else if (status === "SHIPPING") {
        newCustodian = transferData.from;
        if (destRole === "HUB") {
            newState = "F_TRANSPORT";
        } else if (destRole === "LANDFILL") {
            newState = "C_TRANSPORT";
        }
    } else if (status === "COMPLETED" || status === "DELIVERED") {
        newCustodian = transferData.to;
        if (destRole === "HUB") {
            newState = "IN_HUB";
        } else if (destRole === "LANDFILL") {
            newState = "IN_LANDFILL";
        }
    }

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

    await bc.addKV("PACKAGE", ["PKG", packageId], newPkgValue);
    console.log(`[AUTO-UPDATE] Package ${packageId}: state=${newState}, custodian=${newCustodian}`);
}

// ====================================================================
// 9. Aggiornamento carico trasportatore
// ====================================================================
async function updateTransporterLoad(transporterId, packageId, action) {
    // action: "load" (SHIPPING) o "unload" (COMPLETED)
    const transporterState = await bc.getActor(transporterId);
    const pkgState = await bc.getPackage(packageId);
    if (!transporterState || !pkgState) return;

    const prevLoad = Number(transporterState.transportedKg) || 0;
    const pkgWeight = Number(pkgState.weightKg) || 0;

    if (action === "load") {
        transporterState.transportedKg = prevLoad + pkgWeight;
    } else if (action === "unload") {
        transporterState.transportedKg = Math.max(0, prevLoad - pkgWeight);
    }

    await bc.addKV("ACTOR", ["ACT", transporterId], transporterState);
    console.log(`[AUTO-SYNC] ${transporterId} transportedKg: ${prevLoad} -> ${transporterState.transportedKg}`);
}

// ====================================================================
// 10. Validazione formato ID attore
// ====================================================================
function validateActorIdFormat(actorId, role) {
    const prefixes = {
        LAB: "LAB-",
        TRANSPORT_LIGHT: "F-",
        HUB: "HUB-",
        TRANSPORT_HAZ: "C-",
        LANDFILL: "LANDFILL-"
    };
    const expected = prefixes[role];
    if (expected && !actorId.startsWith(expected)) {
        return { valid: false, reason: `Invalid Actor ID format for role ${role}. Expected '${expected}xxx'.` };
    }
    return { valid: true };
}

module.exports = {
    createAlert,
    validateUserRole,
    validateDuplicatePackage,
    validateDuplicateActor,
    checkSequence,
    validateTransferIntegrity,
    validateRiskCodeCompatibility,
    validateVehicleCapacity,
    validatePendingTransfer,
    autoUpdatePackageAfterTransfer,
    updateTransporterLoad,
    validateActorIdFormat
};
