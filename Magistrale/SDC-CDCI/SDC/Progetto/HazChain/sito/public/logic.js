// --- AUTENTICAZIONE ---
// prendi token per autenticazione
const token = localStorage.getItem('token');
const userStr = localStorage.getItem('user');
// se non cè token reindirizza a login.html
if (!token || !userStr) {
    if (!window.location.pathname.endsWith('login.html')) {
        window.location.href = 'login.html';
    }
}
// altrimenti prendi i dati di quell user
let currentUser = null;
if (userStr) {
    currentUser = JSON.parse(userStr);

    // Aggiornamento UI
    window.addEventListener('DOMContentLoaded', () => {
        const authDiv = document.querySelector('.nav-auth');
        if (authDiv) {
            authDiv.innerHTML = `<button id="btn-logout" class="btn text" style="color: white;"> Logout (${currentUser.username})</button>`;
            document.getElementById('btn-logout').addEventListener('click', () => {
                localStorage.removeItem('token');
                localStorage.removeItem('user');
                window.location.href = 'login.html';
            });
        }
    });
}
// utente loggato reindirizza alla pagina corretta
if (window.location.pathname.endsWith('login.html')) {
    const _loginToken = localStorage.getItem('token');
    const _loginUser = localStorage.getItem('user');
    if (_loginToken && _loginUser) {
        const _u = JSON.parse(_loginUser);
        const _rolePages = {
            'LAB': 'lab.html',
            'TRANSPORT_LIGHT': 'van.html',
            'HUB': 'hub.html',
            'TRANSPORT_HAZ': 'truck.html',
            'LANDFILL': 'landfill.html',
            'SUPERUSER': 'alerts.html'
        };
        window.location.href = _rolePages[_u.role] || 'alerts.html';
    }
}
// helper switch tra login e register
function switchTab(tab) {
    document.getElementById('tab-login').classList.remove('active');
    document.getElementById('tab-register').classList.remove('active');
    document.getElementById('form-login').classList.remove('active');
    document.getElementById('form-register').classList.remove('active');

    document.getElementById(`tab-${tab}`).classList.add('active');
    document.getElementById(`form-${tab}`).classList.add('active');

    clearMessages();
}
// helper clear messaggi
function clearMessages() {
    document.getElementById('error-msg').style.display = 'none';
    document.getElementById('success-msg').style.display = 'none';
}
// helper mostra errore login
function showError(msg) {
    const el = document.getElementById('error-msg');
    el.textContent = msg;
    el.style.display = 'block';
    document.getElementById('success-msg').style.display = 'none';
}
// helper mostra succsso login
function showSuccess(msg) {
    const el = document.getElementById('success-msg');
    el.textContent = msg;
    el.style.display = 'block';
    document.getElementById('error-msg').style.display = 'none';
}
// login
async function handleLogin(e) {
    e.preventDefault();
    clearMessages();
    const btn = e.target.querySelector('button[type="submit"]');
    const originalText = btn.textContent;
    btn.textContent = 'Authenticating...';
    btn.disabled = true;

    const username = document.getElementById('login-username').value;
    const password = document.getElementById('login-password').value;

    try {
        const res = await fetch('http://localhost:3000/api/login', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ username, password })
        });

        const data = await res.json();

        if (res.ok) {
            localStorage.setItem('token', data.token);
            localStorage.setItem('user', JSON.stringify(data.user));

            const rolePages = {
                'LAB': 'lab.html',
                'TRANSPORT_LIGHT': 'van.html',
                'HUB': 'hub.html',
                'TRANSPORT_HAZ': 'truck.html',
                'LANDFILL': 'landfill.html',
                'SUPERUSER': 'alerts.html'
            };
            const targetPage = rolePages[data.user.role] || 'alerts.html';

            btn.textContent = 'Access Granted. Redirecting...';
            btn.style.background = 'var(--primary)';

            setTimeout(() => {
                window.location.href = targetPage;
            }, 500);
        } else {
            showError(data.error || 'Login failed');
            btn.textContent = originalText;
            btn.disabled = false;
        }
    } catch (err) {
        showError('Network error connecting to auth server.');
        btn.textContent = originalText;
        btn.disabled = false;
    }
}
// registrazione
async function handleRegister(e) {
    e.preventDefault();
    clearMessages();
    const btn = e.target.querySelector('button[type="submit"]');
    const originalText = btn.textContent;
    btn.textContent = 'Registering...';
    btn.disabled = true;

    const username = document.getElementById('reg-username').value;
    const password = document.getElementById('reg-password').value;
    const role = document.getElementById('reg-role').value;

    try {
        const res = await fetch('http://localhost:3000/api/register', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ username, password, role })
        });

        const data = await res.json();

        if (res.ok) {
            showSuccess('Registration successful! You can now access the network.');
            document.getElementById('form-register').reset();
            btn.textContent = 'Success!';

            setTimeout(() => {
                switchTab('login');
                document.getElementById('login-username').value = username;
                document.getElementById('login-password').focus();
                btn.textContent = originalText;
                btn.disabled = false;
            }, 1000);
        } else {
            showError(data.error || 'Registration failed');
            btn.textContent = originalText;
            btn.disabled = false;
        }
    } catch (err) {
        showError('Network error connecting to auth server.');
        btn.textContent = originalText;
        btn.disabled = false;
    }
}

// --- CHIAMATA API PROXY ---
const PROXY_URL = "http://localhost:3000/api/proxy";
// richiesta POST all'endpoint proxy con un payload JSON e restituisce la risposta.
async function callBlockchain(payload, actionName) {
    log(`Executing: ${actionName}...`, "system");

    const token = localStorage.getItem("token");
    const response = await fetch(PROXY_URL, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${token}`
        },
        body: JSON.stringify(payload)
    });

    const data = await response.json();

    if (response.ok) {
        log(`Success: ${actionName}`, "success");
        log(`Response: ${JSON.stringify(data)}`, "system");
        return data;
    } else {
        log(`Error: ${actionName} failed`, "error");
        log(`Details: ${JSON.stringify(data)}`, "error");
        return null;
    }
}

// --- AIUTI BACKEND ---
// recupera package attraverso ID (GetKV)
async function fetchPackageState(packageId) {
    log(`Fetching state for ${packageId}...`, "info");
    const payload = { "cmd": "GetKV", "class": "PACKAGE", "key": ["PKG", packageId] };
    const data = await callBlockchain(payload, `Get PKG ${packageId}`);
    if (data && data.answer && data.answer.value) {
        return data.answer.value;
    }
    return null;
}
// recupera transfer attraverso ID (GetKV)
async function fetchTransferState(packageId, transferId) {
    log(`Fetching transfer ${transferId} for pkg ${packageId}...`, "info");
    const payload = { "cmd": "GetKV", "class": "TRANSFER", "key": ["TR", transferId] };
    const data = await callBlockchain(payload, `Get TR ${transferId}`);
    if (data && data.answer && data.answer.value) {
        return data.answer.value;
    }
    return null;
}
// recupera attore attraverso ID (GetKV)
async function fetchActorState(actorId) {
    log(`Fetching actor state for ${actorId}...`, "info");
    const payload = { "cmd": "GetKV", "class": "ACTOR", "key": ["ACT", actorId] };
    const data = await callBlockchain(payload, `Get Actor ${actorId}`);
    if (data && data.answer && data.answer.value) {
        return data.answer.value;
    }
    return null;
}
// recupera lista dropdown di attori filtrata (GetKeys)
async function fetchActorOptions(filterStr) {
    const payload = { "cmd": "GetKeys", "class": "ACTOR", "key": ["ACT"] };
    const data = await callBlockchain(payload, `Get Actors (${filterStr})`);
    let options = [];
    if (data && data.answer && data.answer.keys) {
        options = data.answer.keys
            .filter(k => k[1].startsWith(filterStr))
            .map(k => k[1]);
    }
    return options;
}
// recupera prossimo ID disponibile (GetKeys)
async function generateNextId(className, bucket, prefix) {
    // className: "PACKAGE", "TRANSFER"
    // bucket: "PKG", "TR" (first part of composite key)
    // prefix: "PKG-2026-", "TR-F-", "TR-C-"
    log(`Generating ID for ${prefix}...`, "info");
    const payload = { "cmd": "GetKeys", "class": className, "key": [bucket] };
    const data = await callBlockchain(payload, `GenID Scan ${bucket}`);

    let maxNum = 0;
    if (data && data.answer && data.answer.keys) {
        data.answer.keys.forEach(k => {
            // k structure: [bucket, ID] e.g. ["PKG", "PKG-2026-0001"]
            const id = k[1];
            if (id && id.startsWith(prefix)) {
                const numPart = parseInt(id.replace(prefix, ""));
                if (!isNaN(numPart) && numPart > maxNum) {
                    maxNum = numPart;
                }
            }
        });
    }

    const nextNum = maxNum + 1;
    return prefix + String(nextNum).padStart(4, '0');
}
// recupera lista Alerts (GetKeys)
async function fetchAlerts() {
    const container = document.getElementById("alerts-container");
    if (!container) return;
    const payload = { "cmd": "GetKeys", "class": "ALERT", "key": ["AL"] };
    const data = await callBlockchain(payload, "Get All Alerts");

    let alerts = [];

    if (data && data.answer && data.answer.keys) {
        const allKeys = data.answer.keys;

        const promises = allKeys.map(k => {
            const id = k[1];
            return callBlockchain({ "cmd": "GetKV", "class": "ALERT", "key": ["AL", id] }, `Fetch ${id}`)
                .then(res => {
                    if (res && res.answer && res.answer.value) {
                        const alertObj = res.answer.value;
                        alertObj.id = id;
                        return alertObj;
                    }
                    return null;
                });
        });

        const results = await Promise.all(promises);
        alerts = results.filter(a => a !== null);
    }

    // filtra allert dismissi (ormai inutile perche abbiamo DelKV)
    alerts = alerts.filter(a => !a.dismissed);
    renderAlerts(alerts, container);
}
// recupera storico completo di un pacchetto (GetKeyHistory)
async function fetchPackageHistory(packageId) {
    const timeline = document.getElementById('history-timeline');
    if (!timeline) return; // not on history page

    timeline.innerHTML = '<div class="history-empty"><div class="big-icon">⏳</div>Loading blockchain history…</div>';

    const domElements = {
        resultsCard: document.getElementById('history-results-card'),
        timeline,
        titleEl: document.getElementById('history-results-title'),
        countEl: document.getElementById('history-entry-count'),
        summaryBar: document.getElementById('history-pkg-summary')
    };

    const payload = { "cmd": "GetKeyHistory", "class": "PACKAGE", "key": ["PKG", packageId] };
    const data = await callBlockchain(payload, `GetKeyHistory ${packageId}`);

    const entries = (data && data.success && Array.isArray(data.answer)) ? data.answer : [];
    renderHistoryTimeline(packageId, entries, domElements);
}

// --- ALERT ---
// --- Alert: Mismatch --- (AddKV)
async function validateTransferIntegrity(payload) {

    const packageId = payload.value.packageId;
    const transferValue = payload.value;
    const transferId = payload.key[1];

    const pkgState = await fetchPackageState(packageId);

    const mismatches = {};

    // weightKg confronto
    const pkgWeight = Number(pkgState.weightKg);
    const trWeight = Number(transferValue.weightKg);
    if (!isNaN(pkgWeight) && !isNaN(trWeight) && pkgWeight !== trWeight) {
        mismatches.weightKg = { expected: pkgWeight, got: trWeight };
    }

    // wasteType confronto
    const pkgWaste = (pkgState.wasteType || "").trim();
    const trWaste = (transferValue.wasteType || "").trim();
    if (pkgWaste && trWaste && pkgWaste !== trWaste) {
        mismatches.wasteType = { expected: pkgWaste, got: trWaste };
    }

    // riskCode confronto
    const pkgRisk = [...pkgState.riskCode].sort();
    const trRisk = [...transferValue.riskCode].sort();
    if (pkgRisk.length > 0 && trRisk.length > 0 &&
        (pkgRisk.length !== trRisk.length || pkgRisk.some((v, i) => v !== trRisk[i]))) {
        mismatches.riskCode = { expected: pkgRisk, got: trRisk };
    }
    // nessun errore
    if (Object.keys(mismatches).length === 0) {
        log(`Integrity Check PASSED for ${packageId}.`, "success");
        return { valid: true };
    }
    // errore trovato
    const changedFields = Object.keys(mismatches).join(", ");
    const reason = `Transfer ${transferId} blocked: ${changedFields} mismatch with Package ${packageId}`;
    log(`INTEGRITY ALERT: ${reason}`, "error");
    let detailParts = [];
    for (const field in mismatches) {
        const m = mismatches[field];
        const exp = Array.isArray(m.expected) ? m.expected.join(",") : m.expected;
        const got = Array.isArray(m.got) ? m.got.join(",") : m.got;
        detailParts.push(`${field}: expected [${exp}] got [${got}]`);
    }
    // crea alert id e record
    const alertId = await generateNextId("ALERT", "AL", "AL-");
    const alertRecord = {
        "type": "INTEGRITY_MISMATCH",
        "packageId": packageId,
        "transferId": transferId,
        "mismatches": mismatches,
        "msg": `${reason}. Details: ${detailParts.join("; ")}`,
        "ts": new Date().toISOString()
    };
    // aggiungi alert sulla blockchain
    await callBlockchain(
        { "cmd": "AddKV", "class": "ALERT", "key": ["AL", alertId], "value": alertRecord },
        `Create Alert ${alertId}`
    );
    log(`Alert ${alertId} recorded on blockchain.`, "error");

    return { valid: false, reason: reason };
}
// --- Alert: Risk Code Compatibility --- (AddKV)
async function validateRiskCodeCompatibility(payload) {

    const packageId = payload.value.packageId;
    const destinationActorId = payload.value.to;
    const transferId = payload.key[1];

    const pkgState = await fetchPackageState(packageId);

    const pkgRiskCodes = pkgState.riskCode;

    // prendi attore destinazione
    const actorState = await fetchActorState(destinationActorId);

    let actorRiskCodes = actorState.riskCodesHandled;
    if (!Array.isArray(actorRiskCodes)) {
        actorRiskCodes = [actorRiskCodes];
    }

    // verifica almeno un riskcode del pacchetto che sia gestito dall'attore
    const overlap = pkgRiskCodes.filter(code => actorRiskCodes.includes(code));

    // nessun errore
    if (overlap.length > 0) {
        log(`Risk Check PASSED: Package [${pkgRiskCodes}] ∩ Actor [${actorRiskCodes}] = [${overlap}]`, "success");
        return { valid: true };
    }
    // errore trovato
    const reason = `Transfer ${transferId} blocked: Package ${packageId} riskCodes [${pkgRiskCodes.join(", ")}] are NOT handled by ${destinationActorId} [${actorRiskCodes.join(", ")}]`;
    log(`RISK ALERT: ${reason}`, "error");

    const alertId = await generateNextId("ALERT", "AL", "AL-");
    const alertRecord = {
        "type": "RISK_CODE_INCOMPATIBLE",
        "packageId": packageId,
        "transferId": transferId,
        "actorId": destinationActorId,
        "packageRiskCodes": pkgRiskCodes,
        "actorRiskCodesHandled": actorRiskCodes,
        "msg": reason,
        "ts": new Date().toISOString()
    };

    await callBlockchain(
        { "cmd": "AddKV", "class": "ALERT", "key": ["AL", alertId], "value": alertRecord },
        `Create Risk Alert ${alertId}`
    );
    log(`Alert ${alertId} recorded on blockchain.`, "error");

    return { valid: false, reason: reason };
}
// --- Alert: Transfer Sequence --- (AddKV)
async function checkSequence(pkgId, pkgState, requiredStates, actionLabel) {

    if (!pkgState || !pkgState.state) {
        // Package not found — can't validate, allow through (will fail elsewhere)
        return true;
    }

    const currentState = pkgState.state;

    // nessun errore
    if (requiredStates.includes(currentState)) {
        log(`Sequence OK: ${pkgId} is ${currentState}, allowed for ${actionLabel}.`, "success");
        return true;
    }

    // errore trovato
    const reason = `${actionLabel} blocked: Package ${pkgId} is in state ${currentState}, but must be ${requiredStates.join(" or ")}. Flow: LAB -> VAN -> HUB -> TRUCK -> LANDFILL`;
    log(`SEQUENCE ALERT: ${reason}`, "error");
    const alertId = await generateNextId("ALERT", "AL", "AL-");
    const alertRecord = {
        "type": "SEQUENCE_VIOLATION",
        "packageId": pkgId,
        "currentState": currentState,
        "requiredStates": requiredStates,
        "attemptedAction": actionLabel,
        "msg": reason,
        "ts": new Date().toISOString()
    };
    await callBlockchain(
        { "cmd": "AddKV", "class": "ALERT", "key": ["AL", alertId], "value": alertRecord },
        `Create Sequence Alert ${alertId}`
    );
    log(`Alert ${alertId} recorded. Operation cancelled.`, "error");
    return false;
}
// --- Alert: Capacità veicolo --- (AddKV)
async function validateVehicleCapacity(payload) {

    const transferValue = payload.value;
    const transporterId = transferValue.from;
    const actorState = await fetchActorState(transporterId);

    const capacityKg = Number(actorState.capacityKg);
    const currentLoad = Number(actorState.transportedKg);

    const packageId = transferValue.packageId;
    let packageWeight = 0;
    const pkgState = await fetchPackageState(packageId);
    packageWeight = Number(pkgState.weightKg);

    const newLoad = currentLoad + packageWeight;
    log(`CAPACITY CHECK: ${transporterId} — current ${currentLoad}kg + package ${packageWeight}kg = ${newLoad}kg / ${capacityKg}kg capacity`, "info");

    // errore trovato
    if (capacityKg > 0 && newLoad > capacityKg) {
        const reason = `Vehicle ${transporterId} capacity exceeded: ${newLoad}kg would exceed ${capacityKg}kg limit (current load: ${currentLoad}kg, package: ${packageWeight}kg)`;
        log(`CAPACITY ALERT: ${reason}`, "error");

        const alertId = await generateNextId("ALERT", "AL", "AL-");
        const alertRecord = {
            "type": "CAPACITY_EXCEEDED",
            "vehicleId": transporterId,
            "packageId": packageId || "unknown",
            "capacityKg": capacityKg,
            "currentLoadKg": currentLoad,
            "packageWeightKg": packageWeight,
            "attemptedLoadKg": newLoad,
            "msg": reason,
            "ts": new Date().toISOString()
        };

        await callBlockchain(
            { "cmd": "AddKV", "class": "ALERT", "key": ["AL", alertId], "value": alertRecord },
            `Create Capacity Alert ${alertId}`
        );
        log(`Alert ${alertId} recorded. Operation cancelled.`, "error");
        return { valid: false, reason: reason };
    }

    return { valid: true };
}
// --- Alert: Ruolo diverso --- (AddKV)
async function validateUserRole(payload) {
    if (currentUser.role === "SUPERUSER") return { valid: true };

    let requiredRole = null;
    let operationName = "Unknown Operation";

    if (payload.class === "ACTOR") {
        return { valid: false, reason: "Actor creation is restricted to SUPERUSER." };
    } else if (payload.class === "PACKAGE") {
        if (payload.value && payload.value.state === "IN_LAB") {
            requiredRole = "LAB";
            operationName = "Create Package (Lab)";
        } else if (payload.value && (payload.value.state === "DISPOSED" || payload.value.state === "IN_LANDFILL")) {
            requiredRole = "LANDFILL";
            operationName = "Dispose Package (Landfill)";
        }
    } else if (payload.class === "TRANSFER") {
        const transferId = payload.key[1];
        const status = payload.value.status;

        if (status === "WAITING") {
            if (transferId.startsWith("TR-F")) {
                requiredRole = "LAB";
                operationName = "Request Light Transport (Phase 1)";
            } else if (transferId.startsWith("TR-C")) {
                requiredRole = "HUB";
                operationName = "Request Haz Transport (Phase 2)";
            }
        } else if (status === "SHIPPING" || status === "COMPLETED") {
            if (transferId.startsWith("TR-F")) {
                requiredRole = "TRANSPORT_LIGHT";
                operationName = "Process Light Transport (Phase 1)";
            } else if (transferId.startsWith("TR-C")) {
                requiredRole = "TRANSPORT_HAZ";
                operationName = "Process Haz Transport (Phase 2)";
            }
        }
    }

    // errore trovato
    if (requiredRole && currentUser.role !== requiredRole) {
        const reason = `Unauthorized Action: You are logged in as ${currentUser.role}, but '${operationName}' requires ${requiredRole} access.`;
        log(`AUTH ALERT: ${reason}`, "error");

        const alertId = await generateNextId("ALERT", "AL", "AL-");
        const alertRecord = {
            "type": "UNAUTHORIZED_ACTION",
            "userId": currentUser.username,
            "userRole": currentUser.role,
            "requiredRole": requiredRole,
            "attemptedAction": operationName,
            "msg": reason,
            "ts": new Date().toISOString()
        };

        await callBlockchain(
            { "cmd": "AddKV", "class": "ALERT", "key": ["AL", alertId], "value": alertRecord },
            `Create Auth Alert ${alertId}`
        );
        log(`Alert ${alertId} recorded. Operation blocked.`, "error");

        return { valid: false, reason: reason };
    }

    return { valid: true };
}
// --- Alert: Entità duplicata --- (AddKV)
async function validateDuplicateEntity(payload) {
    if (payload.class === "PACKAGE" && payload.value && payload.value.state === "IN_LAB") {
        const pkgId = payload.key[1];
        const existing = await fetchPackageState(pkgId);
        if (existing) {
            const reason = `Duplicate entity blocked: Package ${pkgId} already exists (state: ${existing.state || 'unknown'}).`;
            log(`DUPLICATE ALERT: ${reason}`, "error");
            const alertId = await generateNextId("ALERT", "AL", "AL-");
            await callBlockchain(
                {
                    "cmd": "AddKV", "class": "ALERT", "key": ["AL", alertId], "value": {
                        "type": "DUPLICATE_ENTITY",
                        "entityClass": "PACKAGE",
                        "entityId": pkgId,
                        "existingState": existing.state || "unknown",
                        "msg": reason,
                        "ts": new Date().toISOString()
                    }
                },
                `Create Duplicate Alert ${alertId}`
            );
            log(`Alert ${alertId} recorded. Operation cancelled.`, "error");
            return { valid: false, reason };
        }
    }

    if (payload.class === "ACTOR") {
        const actorId = payload.key[1];
        const existing = await fetchActorState(actorId);
        if (existing) {
            const reason = `Duplicate entity blocked: Actor ${actorId} already exists (role: ${existing.role || 'unknown'}).`;
            log(`DUPLICATE ALERT: ${reason}`, "error");
            const alertId = await generateNextId("ALERT", "AL", "AL-");
            await callBlockchain(
                {
                    "cmd": "AddKV", "class": "ALERT", "key": ["AL", alertId], "value": {
                        "type": "DUPLICATE_ENTITY",
                        "entityClass": "ACTOR",
                        "entityId": actorId,
                        "existingRole": existing.role || "unknown",
                        "msg": reason,
                        "ts": new Date().toISOString()
                    }
                },
                `Create Duplicate Alert ${alertId}`
            );
            log(`Alert ${alertId} recorded. Operation cancelled.`, "error");
            return { valid: false, reason };
        }
    }

    return { valid: true };
}
// --- Dismiss Alert (DelKV) ---
async function dismissAlert(alertId) {
    log(`Deleting alert ${alertId}...`, "info");

    await callBlockchain(
        { "cmd": "DelKV", "class": "ALERT", "key": ["AL", alertId] },
        `Delete Alert ${alertId}`
    );
    log(`Alert ${alertId} deleted from blockchain.`, "success");

    const card = document.querySelector(`.alert-detail-card[data-alert-id="${alertId}"]`);
    if (card) {
        card.style.transition = "opacity 0.3s ease, transform 0.3s ease";
        card.style.opacity = "0";
        card.style.transform = "translateX(30px)";
        setTimeout(() => card.remove(), 300);
    }
    setTimeout(() => fetchAlerts(), 400);
}
// --- Alert Polling ---
if (window.location.pathname.endsWith("alerts.html")) {
    fetchAlerts();
    setInterval(() => {
        fetchAlerts();
    }, 10000);
}
// refresh per alert 
addSafeEventListener("btn-refresh-alerts", "click", () => {
    fetchAlerts();
});

// --- ESECUZIONE OPERAZIONI---
// 1. Builder: Create Package
addSafeEventListener("btn-p1-create", "click", async () => {
    const newId = await generateNextId("PACKAGE", "PKG", "PKG-2026-");

    log("Fetching LAB actors...", "info");
    const payload = {
        "cmd": "GetKeys",
        "class": "ACTOR",
        "key": ["ACT"]
    };

    const data = await callBlockchain(payload, "Get Actors");
    let options = ["LAB-01"]; // Default fallback

    if (data && data.answer && data.answer.keys) {
        const labKeys = data.answer.keys
            .filter(k => k[1].includes("LAB"))
            .map(k => k[1]);

        if (labKeys.length > 0) options = labKeys;
    }

    const visibleAttrs = {
        "createdBy": { type: 'select', options: options, selected: options[0] },
        "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: [''] },
        "wasteType": "",
        "weightKg": { type: 'number', value: '' }
    };

    const hiddenAttrs = {
        "state": "IN_LAB",
        "currentCustodian": "",
        "lastTransferId": "null",
        "lastUpdateTs": new Date().toISOString()
    };

    setBuilderState("PACKAGE", `PKG, ${newId}`, visibleAttrs, hiddenAttrs);
    log(`Tpl: Create Package (LAB) [New ID: ${newId}]`, "info");
});
// 2. Builder: Request Pickup (Waiting) (dev essere IN_LAB)
addSafeEventListener("btn-p1-start", "click", async () => {
    const options = await fetchActorOptions("F");

    let pkgId = getBuilderPackageId();
    if (!pkgId) {
        pkgId = await requestPackageId();
        if (!pkgId) return;
    }
    const pkgState = await fetchPackageState(pkgId);
    let fromActor = "LAB-01";
    if (pkgState) {
        if (pkgState.currentCustodian) fromActor = pkgState.currentCustodian;
        else if (pkgState.createdBy) fromActor = pkgState.createdBy;
    }
    // check sequence integrity
    if (!(await checkSequence(pkgId, pkgState, ["IN_LAB"], "Request Pickup (Phase 1)"))) return;

    log(`DEBUG: Request Pickup From: ${fromActor} (PkgId: ${pkgId})`, "info");

    const newTrId = await generateNextId("TRANSFER", "TR", "TR-F-");

    const visibleAttrs = {
        "packageId": pkgId,
        "to": { type: 'select', options: options, selected: options[0] },
        "weightKg": { type: 'number', value: pkgState ? pkgState.weightKg : '' },
        "wasteType": pkgState ? pkgState.wasteType : '',
        "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState ? pkgState.riskCode : [] }
    };
    const hiddenAttrs = {
        "from": fromActor,
        "action": "PICKUP",
        "status": "WAITING",
        "ts": new Date().toISOString()
    };
    setBuilderState("TRANSFER", `TR, ${newTrId}`, visibleAttrs, hiddenAttrs);
    log(`Tpl P1: Start Transfer [ID: ${newTrId}]`, "info");
});
// 3. Builder: Transport Pickup (Shipping) (deve essere IN_LAB)
addSafeEventListener("btn-p1-pickup", "click", async () => {
    let pkgId = getBuilderPackageId();
    log(`PICKUP DEBUG [1]: Package ID = ${pkgId}`, "info");
    if (!pkgId) {
        pkgId = await requestPackageId();
        if (!pkgId) return;
    }
    const pkgState = await fetchPackageState(pkgId);
    log(`PICKUP DEBUG [2]: Package State = ${pkgState ? JSON.stringify(pkgState) : 'NULL'}`, "info");

    // --- sequence integrity check ---
    if (!(await checkSequence(pkgId, pkgState, ["IN_LAB"], "Pickup from Lab (Phase 1)"))) return;

    // --- Generazione Sequence Alert in caso di non richeista ---
    const lastTr = pkgState ? pkgState.lastTransferId : null;
    if (!lastTr || !lastTr.startsWith("TR-F-")) {
        const reason = `Pickup from Lab blocked: Package ${pkgId} has no pending Van (F) request. lastTransferId=${lastTr || 'none'}. You must first do 'Request Pickup (Phase 1)'.`;
        log(`SEQUENCE ALERT: ${reason}`, "error");
        const alertId = await generateNextId("ALERT", "AL", "AL-");
        await callBlockchain(
            {
                "cmd": "AddKV", "class": "ALERT", "key": ["AL", alertId], "value": {
                    "type": "SEQUENCE_VIOLATION", "packageId": pkgId,
                    "currentState": pkgState.state, "requiredTransferPrefix": "TR-F-",
                    "attemptedAction": "Pickup from Lab (Phase 1)",
                    "msg": reason, "ts": new Date().toISOString()
                }
            },
            `Create Sequence Alert ${alertId}`
        );
        return;
    }

    let fromActor = null; // Will be resolved below
    let transferId = "TR-F-0001"; // Ultimate Fallback

    if (pkgState && pkgState.lastTransferId && pkgState.lastTransferId !== "null") {
        transferId = pkgState.lastTransferId;
        log(`PICKUP DEBUG [3]: lastTransferId = ${transferId}`, "info");

        const transferData = await fetchTransferState(pkgId, transferId);
        log(`PICKUP DEBUG [4]: TransferData = ${transferData ? JSON.stringify(transferData) : 'NULL'}`, "info");

        if (transferData && transferData.to) {
            fromActor = transferData.to; // The Van assigned in "Request Pickup"
            log(`PICKUP DEBUG [5]: fromActor derived from transfer.to = ${fromActor}`, "success");
        } else {
            log(`PICKUP DEBUG [5]: Transfer fetch OK but no 'to' field.`, "warning");
        }
    } else {
        log(`PICKUP DEBUG [3]: No lastTransferId found on Package (value: ${pkgState ? pkgState.lastTransferId : 'pkg is null'})`, "warning");
    }

    if (!fromActor) {
        const allRows = document.querySelectorAll(".attr-row");
        allRows.forEach(row => {
            const keyEl = row.querySelector(".attr-key");
            const valEl = row.querySelector(".attr-value");
            if (keyEl && valEl && keyEl.value === "to" && valEl.value) {
                fromActor = valEl.value.trim();
                log(`PICKUP DEBUG [5b]: fromActor read from builder form 'to' field: ${fromActor}`, "info");
            }
        });
    }

    // fallback
    if (!fromActor) {
        fromActor = "F-01";
        log(`PICKUP DEBUG [5c]: Using hardcoded fallback: F-01`, "warning");
    }

    log(`PICKUP DEBUG [6]: FINAL fromActor = ${fromActor}`, "success");

    const options = await fetchActorOptions("HUB");
    const visibleAttrs = {
        "packageId": pkgId,
        "to": { type: 'select', options: options, selected: options[0] },
        "weightKg": { type: 'number', value: pkgState ? pkgState.weightKg : '' },
        "wasteType": pkgState ? pkgState.wasteType : '',
        "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState ? pkgState.riskCode : [] }
    };
    const hiddenAttrs = {
        "from": fromActor,
        "action": "SHIPPING",
        "status": "SHIPPING",
        "ts": new Date().toISOString()
    };

    setBuilderState("TRANSFER", `TR, ${transferId}`, visibleAttrs, hiddenAttrs);
    log(`Tpl P1: Transport Pickup [TR ID: ${transferId}, From: ${fromActor}]`, "info");
});
// 4. Builder: Arrive Hub (Completed) (deve essere F_TRANSPORT)
addSafeEventListener("btn-p1-arrive", "click", async () => {
    let pkgId = getBuilderPackageId();
    if (!pkgId) {
        pkgId = await requestPackageId();
        if (!pkgId) return;
    }
    const pkgState = await fetchPackageState(pkgId);

    // --- check sequence integrity ---
    if (!(await checkSequence(pkgId, pkgState, ["F_TRANSPORT"], "Arrive at Hub (Phase 1)"))) return;

    if (pkgState && pkgState.lastTransferId) {
        const transferData = await fetchTransferState(pkgId, pkgState.lastTransferId);
        if (transferData) {
            const visibleAttrs = {
                "packageId": pkgId,
                "weightKg": { type: 'number', value: pkgState ? pkgState.weightKg : '' },
                "wasteType": pkgState ? pkgState.wasteType : '',
                "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState ? pkgState.riskCode : [] }
            };
            const hiddenAttrs = {
                ...transferData,
                "status": "COMPLETED",
                "action": "DELIVER",
                "ts": new Date().toISOString()
            };
            setBuilderState("TRANSFER", `TR, ${pkgState.lastTransferId}`, visibleAttrs, hiddenAttrs);
            log(`Tpl P1: Hub Arrival [Closing T-ID: ${pkgState.lastTransferId}]`, "info");
        }
        else {
            log("Warning: Could not fetch Transfer data, using fallback.", "warning");
            // Fallback 
            const options = await fetchActorOptions("HUB");
            const visibleAttrs = {
                "packageId": pkgId,
                "to": { type: 'select', options: options, selected: options[0] },
                "weightKg": { type: 'number', value: pkgState ? pkgState.weightKg : '' },
                "wasteType": pkgState ? pkgState.wasteType : '',
                "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState ? pkgState.riskCode : [] }
            };
            const hiddenAttrs = { "from": "F-01", "action": "DELIVER", "status": "COMPLETED", "ts": new Date().toISOString() };
            setBuilderState("TRANSFER", `TR, ${pkgState.lastTransferId}`, visibleAttrs, hiddenAttrs);
        }
    }
    else {
        log("Error: Package has no lastTransferId or not found", "error");
        // Fallback 
        const options = await fetchActorOptions("HUB");
        const visibleAttrs = {
            "packageId": pkgId,
            "to": { type: 'select', options: options, selected: options[0] },
            "weightKg": { type: 'number', value: pkgState ? pkgState.weightKg : '' },
            "wasteType": pkgState ? pkgState.wasteType : ''
        };
        const hiddenAttrs = { "from": "F-01", "action": "DELIVER", "status": "COMPLETED", "ts": new Date().toISOString() };
        setBuilderState("TRANSFER", `TR, TR-F-0001`, visibleAttrs, hiddenAttrs);
    }
});
// 5. Builder: Start Haz Transfer (Waiting) (deve essere IN_HUB)
addSafeEventListener("btn-p2-start", "click", async () => {
    const options = await fetchActorOptions("C");
    let pkgId = getBuilderPackageId();
    if (!pkgId) {
        pkgId = await requestPackageId();
        if (!pkgId) return;
    }
    const pkgState = await fetchPackageState(pkgId);

    // --- check sequence integrity ---
    if (!(await checkSequence(pkgId, pkgState, ["IN_HUB"], "Request Pickup (Phase 2)"))) return;
    const fromActor = pkgState ? pkgState.currentCustodian : "HUB-01";

    const newTrId = await generateNextId("TRANSFER", "TR", "TR-C-");

    const visibleAttrs = {
        "packageId": pkgId,
        "to": { type: 'select', options: options, selected: options[0] },
        "weightKg": { type: 'number', value: pkgState ? pkgState.weightKg : '' },
        "wasteType": pkgState ? pkgState.wasteType : '',
        "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState ? pkgState.riskCode : [] }
    };
    const hiddenAttrs = {
        "from": fromActor,
        "action": "PICKUP",
        "status": "WAITING",
        "ts": new Date().toISOString()
    };
    setBuilderState("TRANSFER", `TR, ${newTrId}`, visibleAttrs, hiddenAttrs);
    log(`Tpl P2: Hub requests Haz pickup [Using ID: ${newTrId}]`, "info");
});
// 6. Builder: Transport Haz Pickup (Shipping) (deve essere IN_HUB)
addSafeEventListener("btn-p2-pickup", "click", async () => {
    let pkgId = getBuilderPackageId();
    if (!pkgId) {
        pkgId = await requestPackageId();
        if (!pkgId) return;
    }
    const pkgState = await fetchPackageState(pkgId);

    // --- check sequence integrity ---
    if (!(await checkSequence(pkgId, pkgState, ["IN_HUB"], "Pickup from Hub (Phase 2)"))) return;

    // --- Generazione Sequence Alert in caso di non aver fatto richeista preventiva ---
    const lastTr = pkgState ? pkgState.lastTransferId : null;
    if (!lastTr || !lastTr.startsWith("TR-C-")) {
        const reason = `Pickup from Hub blocked: Package ${pkgId} has no pending Truck (C) request. lastTransferId=${lastTr || 'none'}. You must first do 'Request Pickup (Phase 2)'.`;
        log(`SEQUENCE ALERT: ${reason}`, "error");
        const alertId = await generateNextId("ALERT", "AL", "AL-");
        await callBlockchain(
            {
                "cmd": "AddKV", "class": "ALERT", "key": ["AL", alertId], "value": {
                    "type": "SEQUENCE_VIOLATION", "packageId": pkgId,
                    "currentState": pkgState.state, "requiredTransferPrefix": "TR-C-",
                    "attemptedAction": "Pickup from Hub (Phase 2)",
                    "msg": reason, "ts": new Date().toISOString()
                }
            },
            `Create Sequence Alert ${alertId}`
        );
        return;
    }
    let fromActor = null;
    let transferId = "TR-C-0001";

    if (pkgState && pkgState.lastTransferId && pkgState.lastTransferId !== "null") {
        transferId = pkgState.lastTransferId;
        const transferData = await fetchTransferState(pkgId, transferId);
        if (transferData && transferData.to) {
            fromActor = transferData.to;
        }
    }

    // FALLBACK
    if (!fromActor) {
        const allRows = document.querySelectorAll(".attr-row");
        allRows.forEach(row => {
            const keyEl = row.querySelector(".attr-key");
            const valEl = row.querySelector(".attr-value");
            if (keyEl && valEl && keyEl.value === "to" && valEl.value) {
                fromActor = valEl.value.trim();
                log(`P2 PICKUP: fromActor read from builder form 'to' field: ${fromActor}`, "info");
            }
        });
    }

    // LAST RESORT: hardcoded fallback
    if (!fromActor) {
        fromActor = "C-01";
        log(`P2 PICKUP: Using hardcoded fallback: C-01`, "warning");
    }

    const options = await fetchActorOptions("LANDFILL");
    const visibleAttrs = {
        "packageId": pkgId,
        "to": { type: 'select', options: options, selected: options[0] },
        "weightKg": { type: 'number', value: pkgState ? pkgState.weightKg : '' },
        "wasteType": pkgState ? pkgState.wasteType : '',
        "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState ? pkgState.riskCode : [] }
    };
    const hiddenAttrs = {
        "from": fromActor,
        "action": "SHIPPING",
        "status": "SHIPPING",
        "ts": new Date().toISOString()
    };
    setBuilderState("TRANSFER", `TR, ${transferId}`, visibleAttrs, hiddenAttrs);
    log(`Tpl P2: Haz Pickup [TR ID: ${transferId}]`, "info");
});
// 7. Builder: Arrive Landfill (Completed) (deve essere C_TRANSPORT)
addSafeEventListener("btn-p2-arrive", "click", async () => {
    let pkgId = getBuilderPackageId();
    if (!pkgId) {
        pkgId = await requestPackageId();
        if (!pkgId) return;
    }
    const pkgState = await fetchPackageState(pkgId);

    // --- check sequence integrity ---
    if (!(await checkSequence(pkgId, pkgState, ["C_TRANSPORT"], "Arrive at Landfill (Phase 2)"))) return;

    if (pkgState && pkgState.lastTransferId) {
        const transferData = await fetchTransferState(pkgId, pkgState.lastTransferId);
        if (transferData) {
            const visibleAttrs = {
                "packageId": pkgId,
                "weightKg": { type: 'number', value: pkgState ? pkgState.weightKg : '' },
                "wasteType": pkgState ? pkgState.wasteType : '',
                "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState ? pkgState.riskCode : [] }
            };
            const hiddenAttrs = {
                ...transferData,
                "status": "COMPLETED",
                "action": "DELIVER",
                "ts": new Date().toISOString()
            };
            setBuilderState("TRANSFER", `TR, ${pkgState.lastTransferId}`, visibleAttrs, hiddenAttrs);
            log(`Tpl P2: Landfill Arrival [Closing T-ID: ${pkgState.lastTransferId}]`, "info");
        } else {
            log("Warning: Could not fetch Transfer data, using fallback.", "warning");
            // Fallback 
            const options = await fetchActorOptions("LANDFILL");
            const visibleAttrs = {
                "packageId": pkgId,
                "to": { type: 'select', options: options, selected: options[0] },
                "weightKg": { type: 'number', value: pkgState ? pkgState.weightKg : '' },
                "wasteType": pkgState ? pkgState.wasteType : '',
                "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState ? pkgState.riskCode : [] }
            };
            const hiddenAttrs = { "from": "C-01", "action": "DELIVER", "status": "COMPLETED", "ts": new Date().toISOString() };
            setBuilderState("TRANSFER", `TR, ${pkgState.lastTransferId}`, visibleAttrs, hiddenAttrs);
        }
    } else {
        log("Error: Package has no lastTransferId or not found", "error");
        // Fallback 
        const options = await fetchActorOptions("LANDFILL");
        const visibleAttrs = {
            "packageId": pkgId,
            "to": { type: 'select', options: options, selected: options[0] },
            "weightKg": { type: 'number', value: pkgState ? pkgState.weightKg : '' },
            "wasteType": pkgState ? pkgState.wasteType : ''
        };
        const hiddenAttrs = { "from": "C-01", "action": "DELIVER", "status": "COMPLETED", "ts": new Date().toISOString() };
        setBuilderState("TRANSFER", `TR, TR-C-0002`, visibleAttrs, hiddenAttrs);
    }
});
// 8. Builder: Dispose Package (Landfill) — Package must be IN_LANDFILL
addSafeEventListener("btn-p2-dispose", "click", async () => {
    let packageId = getBuilderPackageId();
    if (!packageId) {
        packageId = await requestPackageId();
        if (!packageId) return;
    }

    // --- seqeunce integrity check ---
    const disposePkgState = await fetchPackageState(packageId);
    if (!(await checkSequence(packageId, disposePkgState, ["IN_LANDFILL"], "Dispose Package"))) return;

    let visibleAttrs = {};
    let hiddenAttrs = {
        "state": "DISPOSED",
        "lastUpdateTs": new Date().toISOString()
    };
    let keyString = "PKG, PKG-2026-0001"; // default fallback

    if (packageId) {
        log(`Fetching current state for ${packageId}...`, "info");
        const payload = { "cmd": "GetKV", "class": "PACKAGE", "key": ["PKG", packageId] };
        const data = await callBlockchain(payload, "Get Package State");

        if (data && data.answer && data.answer.value) {
            const currentPkg = data.answer.value;
            visibleAttrs = {
                "weightKg": { type: 'number', value: currentPkg.weightKg },
                "wasteType": currentPkg.wasteType,
                "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: currentPkg.riskCode || [] }
            };
            hiddenAttrs = {
                ...currentPkg,
                "state": "DISPOSED",
                "lastUpdateTs": new Date().toISOString()
            };
            keyString = `PKG, ${packageId}`;
            log(`Tpl P2: Dispose Package (Dynamic) [State -> Disposed]`, "success");
        }
        else {
            // Fallback
            visibleAttrs = {
                "weightKg": { type: 'number', value: '' },
                "wasteType": "",
                "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: [] }
            };
            keyString = `PKG, ${packageId}`;
        }
    }
    else {
        visibleAttrs = {
            "weightKg": { type: 'number', value: '' },
            "wasteType": "",
            "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: [] }
        };
        log("Tpl P2: Dispose Package (Empty) [Enter ID in Builder]", "info");
    }

    setBuilderState("PACKAGE", keyString, visibleAttrs, hiddenAttrs);
});
// Bottone Esecuzione vero e proprio
addSafeEventListener("btn-execute-builder", "click", async () => {
    const execBtn = document.getElementById("btn-execute-builder");
    if (execBtn && execBtn.disabled) return; // already running
    if (execBtn) { execBtn.disabled = true; execBtn.textContent = "Executing…"; }

    try {
        let payload = getBuilderPayload(); // prende i valori del builder html

        // se stiamo creando un PACKAGE, il custodian deve corrispondere al lab selezionato
        if (payload.class === "PACKAGE" && payload.value && payload.value.state === "IN_LAB") {
            if (payload.value.createdBy) {
                payload.value.currentCustodian = payload.value.createdBy;
                log(`Sync: currentCustodian -> ${payload.value.currentCustodian}`, "info");
            }
        }

        // role alert check
        const roleCheck = await validateUserRole(payload);
        if (!roleCheck.valid) {
            log(`OPERATION CENCELLED: ${roleCheck.reason}`, "error");
            return;
        }

        if (["PACKAGE", "ACTOR", "TRANSFER"].includes(payload.class)) {
            const keyParts = payload.key; // e.g. ["PKG", "PKG-xxx"]

            // controllo formato key
            if (payload.class === "PACKAGE") {
                if (!keyParts || keyParts.length !== 2 || keyParts[0] !== "PKG" || !keyParts[1].startsWith("PKG-")) {
                    log(`OPERATION BLOCKED: Invalid Package Key format. Expected 'PKG, PKG-xxx'.`, "error");
                    return;
                }
            }

            if (payload.class === "ACTOR") {

                if (!keyParts || keyParts.length !== 2 || keyParts[0] !== "ACT") {
                    log(`OPERATION BLOCKED: Invalid Actor Key format. Expected 'ACT, <PREFIX>-xxx'.`, "error");
                    return;
                }

                const actorRole = payload.value.role;
                const actorId = keyParts[1];

                let expectedPrefix = "";
                let expectedPatternStr = "";

                if (actorRole === "LAB") { expectedPrefix = "LAB-"; expectedPatternStr = "LAB-xxx"; }
                else if (actorRole === "TRANSPORT_LIGHT") { expectedPrefix = "F-"; expectedPatternStr = "F-xxx"; }
                else if (actorRole === "HUB") { expectedPrefix = "HUB-"; expectedPatternStr = "HUB-xxx"; }
                else if (actorRole === "TRANSPORT_HAZ") { expectedPrefix = "C-"; expectedPatternStr = "C-xxx"; }
                else if (actorRole === "LANDFILL") { expectedPrefix = "LANDFILL-"; expectedPatternStr = "LANDFILL-xxx"; }

                if (expectedPrefix && !actorId.startsWith(expectedPrefix)) {
                    log(`OPERATION BLOCKED: Invalid Actor ID format for role ${actorRole}. Expected '${expectedPatternStr}'.`, "error");
                    return;
                }
            }

            // fetchPayload per prendere i valori correnti per fare dopo il merge
            const fetchPayload = {
                "cmd": "GetKV",
                "class": payload.class,
                "key": keyParts
            };

            try {
                const response = await callBlockchain(fetchPayload, `Pre-flight Fetch ${keyParts.join("-")}`);
                if (response && response.answer && response.answer.value) {
                    const existingData = response.answer.value;
                    const newData = payload.value;

                    // merge dati
                    const mergedData = { ...existingData };

                    for (const prop in newData) {
                        const newVal = newData[prop];
                        const oldVal = existingData[prop];

                        if (newVal === "" && oldVal !== undefined && oldVal !== null) {
                        }
                        else {
                            mergedData[prop] = newVal;
                        }
                    }
                    payload.value = mergedData;
                    log(`Smart Merge Applied: Preserved existing attributes for ${keyParts.join("-")}`, "success");
                }
            } catch (e) {
                console.warn("Pre-flight fetch failed, proceeding with raw payload.", e);
            }
        }

        // mismatch alert check
        if (payload.class === "TRANSFER") {
            const integrityCheck = await validateTransferIntegrity(payload);
            if (!integrityCheck.valid) {
                log(`OPERATION BLOCKED: ${integrityCheck.reason}`, "error");
                log("Fix the mismatched fields and try again.", "warning");
                return;
            }
        }

        // risk code alert check
        if (payload.class === "TRANSFER") {
            const riskCheck = await validateRiskCodeCompatibility(payload);
            if (!riskCheck.valid) {
                log(`OPERATION BLOCKED: ${riskCheck.reason}`, "error");
                log("The destination actor does not handle any of this package's risk codes.", "warning");
                return;
            }
        }

        // capacità alert check
        if (payload.class === "TRANSFER" && payload.value && payload.value.status === "SHIPPING") {
            const capCheck = await validateVehicleCapacity(payload);
            if (!capCheck.valid) {
                log(`OPERATION BLOCKED: ${capCheck.reason}`, "error");
                return;
            }
        }

        // duplicate alert check
        const dupCheck = await validateDuplicateEntity(payload);
        if (!dupCheck.valid) {
            log(`OPERATION BLOCKED: ${dupCheck.reason}`, "error");
            return;
        }

        // se non ci sono alert allora eseguiamo l'operazione
        await callBlockchain(payload, `Custom Op: ${payload.class}`);

        // in caso fosse un operazione di transfer, aggiorna solo i dati dell'attore trasportatore (capacità)
        // l'aggiornamento del Package avviene già automaticamente nel proxy backend, quindi NON va rifatto altrimenti TX duplicate.
        if (payload.class === "TRANSFER" && payload.value && payload.value.packageId) {
            const transferValue = payload.value;
            const trStatus = transferValue.status;

            if (trStatus === "SHIPPING" && transferValue.from) {
                // Aumenta il carico del mezzo di trasporto
                try {
                    const pkgState = await fetchPackageState(transferValue.packageId);
                    const transporterState = await fetchActorState(transferValue.from);
                    if (transporterState && pkgState) {
                        const prevLoad = Number(transporterState.transportedKg) || 0;
                        const pkgWeight = Number(pkgState.weightKg) || 0;
                        transporterState.transportedKg = prevLoad + pkgWeight;
                        await callBlockchain(
                            { "cmd": "AddKV", "class": "ACTOR", "key": ["ACT", transferValue.from], "value": transporterState },
                            `Auto-Sync: Update ${transferValue.from} transportedKg -> ${transporterState.transportedKg}`
                        );
                        log(`AUTO-SYNC: ${transferValue.from} carico: ${prevLoad} + ${pkgWeight} = ${transporterState.transportedKg} kg`, "success");
                    }
                } catch (e) {
                    console.warn("Auto-sync transportedKg (SHIPPING) failed.", e);
                }
            } else if (trStatus === "COMPLETED" && transferValue.from) {
                // Diminuisce il carico del mezzo di trasporto
                try {
                    const pkgState = await fetchPackageState(transferValue.packageId);
                    const transporterState = await fetchActorState(transferValue.from);
                    if (transporterState && pkgState) {
                        const prevLoad = Number(transporterState.transportedKg) || 0;
                        const pkgWeight = Number(pkgState.weightKg) || 0;
                        transporterState.transportedKg = Math.max(0, prevLoad - pkgWeight);
                        await callBlockchain(
                            { "cmd": "AddKV", "class": "ACTOR", "key": ["ACT", transferValue.from], "value": transporterState },
                            `Auto-Sync: Update ${transferValue.from} transportedKg -> ${transporterState.transportedKg}`
                        );
                        log(`AUTO-SYNC: ${transferValue.from} carico: ${prevLoad} - ${pkgWeight} = ${transporterState.transportedKg} kg`, "success");
                    }
                } catch (e) {
                    console.warn("Auto-sync transportedKg (COMPLETED) failed.", e);
                }
            }
        }
    } finally {
        if (execBtn) { execBtn.disabled = false; execBtn.textContent = "Execute Operation"; }
    }
});

// --- PACKAGE INSPECTOR ---
addSafeEventListener("btn-inspect", "click", async () => {
    const pkgIdInput = document.getElementById("inspector-pkg-id");
    const creatorInput = document.getElementById("inspector-creator");
    const custodianInput = document.getElementById("inspector-custodian");
    const statusInput = document.getElementById("inspector-status");
    const wasteInput = document.getElementById("inspector-waste");
    const riskInput = document.getElementById("inspector-risk");
    const weightMinInput = document.getElementById("inspector-weight-min");
    const weightMaxInput = document.getElementById("inspector-weight-max");
    const transferInput = document.getElementById("inspector-transfer");
    const resultDiv = document.getElementById("inspector-result");

    const pkgId = pkgIdInput.value.trim();
    const creator = creatorInput ? creatorInput.value.trim() : "";
    const custodian = custodianInput ? custodianInput.value.trim() : "";
    const status = statusInput ? statusInput.value : "";
    const waste = wasteInput ? wasteInput.value.trim() : "";
    const risk = riskInput ? riskInput.value.trim() : "";
    const weightMin = weightMinInput ? parseFloat(weightMinInput.value) : null;
    const weightMax = weightMaxInput ? parseFloat(weightMaxInput.value) : null;
    const transfer = transferInput ? transferInput.value.trim() : "";

    resultDiv.classList.add("hidden");
    resultDiv.innerHTML = '<div class="log-entry info">Searching...</div>';
    resultDiv.classList.remove("hidden");

    let packages = [];

    // Cerca per PKG ID (GetKV, GetKeys)
    if (pkgId) {
        log(`Searching by ID: ${pkgId}`, "info");
        const payload = { "cmd": "GetKV", "class": "PACKAGE", "key": ["PKG", pkgId] };
        const data = await callBlockchain(payload, `Inspect PKG ${pkgId}`);
        if (data && data.answer && data.answer.value) {
            const pkg = data.answer.value;
            pkg.packageId = pkgId;
            packages.push(pkg);
        }
    } else {
        // cerca tra tutti i pacchetti quelli con i filtri corretti
        log("Scanning all packages for filters...", "info");
        const payload = { "cmd": "GetKeys", "class": "PACKAGE", "key": ["PKG"] };
        const data = await callBlockchain(payload, "Get All PKG Keys");

        if (data && data.answer && data.answer.keys) {
            const allKeys = data.answer.keys;

            const promises = allKeys.map(k => {
                const id = k[1];
                return callBlockchain({ "cmd": "GetKV", "class": "PACKAGE", "key": ["PKG", id] }, `Fetch ${id}`)
                    .then(res => {
                        if (res && res.answer && res.answer.value) {
                            const pkg = res.answer.value;
                            pkg.packageId = id;
                            return pkg;
                        }
                        return null;
                    });
            });

            const results = await Promise.all(promises);
            packages = results.filter(p => p !== null);
        }
    }
    // Filtri vari
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
    if (weightMin !== null && !isNaN(weightMin)) {
        packages = packages.filter(p => p.weightKg >= weightMin);
    }
    if (weightMax !== null && !isNaN(weightMax)) {
        packages = packages.filter(p => p.weightKg <= weightMax);
    }
    if (transfer) {
        packages = packages.filter(p => p.lastTransferId && p.lastTransferId.includes(transfer));
    }

    // HTML risultati
    renderInspectorResults(packages, resultDiv);
});
