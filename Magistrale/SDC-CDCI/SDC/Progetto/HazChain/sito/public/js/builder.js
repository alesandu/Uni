// --- Builder: Form rendering + Operation execution ---
// Pura UI — il server gestisce validazioni e costruzione payload blockchain.

// ====================================================================
// DOM Helpers per il builder
// ====================================================================

function addAttributeRow(key = "", value = "") {
    const container = document.getElementById("attributes-container");
    if (!container) return;
    const row = document.createElement("div");
    row.className = "attr-row";

    const keyInput = document.createElement("input");
    keyInput.type = "text";
    keyInput.placeholder = "Key (e.g. weightKg)";
    keyInput.className = "input-field attr-key";
    keyInput.value = key;
    keyInput.disabled = true;
    keyInput.classList.add("disabled-input");

    let valueElement;

    if (typeof value === 'object' && value !== null && value.type === 'select') {
        valueElement = document.createElement("select");
        valueElement.className = "input-field attr-value";
        if (value.multiple) {
            valueElement.multiple = true;
            valueElement.style.height = "100px";
        }
        value.options.forEach(opt => {
            const option = document.createElement("option");
            option.value = opt;
            option.textContent = opt;
            if (Array.isArray(value.selected)) {
                if (value.selected.includes(opt)) option.selected = true;
            } else {
                if (opt === value.selected) option.selected = true;
            }
            valueElement.appendChild(option);
        });
    } else if (typeof value === 'object' && value !== null && value.type === 'checkbox-group') {
        valueElement = document.createElement("div");
        valueElement.className = "checkbox-group attr-value";
        valueElement.dataset.type = "checkbox-group";
        value.options.forEach(opt => {
            const label = document.createElement("label");
            label.style.marginRight = "10px";
            label.style.display = "inline-flex";
            label.style.alignItems = "center";
            label.style.cursor = "pointer";
            const checkbox = document.createElement("input");
            checkbox.type = "checkbox";
            checkbox.value = opt;
            checkbox.style.marginRight = "5px";
            if (Array.isArray(value.selected) && value.selected.includes(opt)) {
                checkbox.checked = true;
            }
            label.appendChild(checkbox);
            label.appendChild(document.createTextNode(opt));
            valueElement.appendChild(label);
        });
    } else if (typeof value === 'object' && value !== null && value.type === 'number') {
        valueElement = document.createElement("input");
        valueElement.type = "number";
        valueElement.placeholder = "Value (Number)";
        valueElement.className = "input-field attr-value";
        valueElement.value = value.value !== undefined ? value.value : "";
    } else {
        valueElement = document.createElement("input");
        valueElement.type = "text";
        valueElement.placeholder = "Value";
        valueElement.className = "input-field attr-value";
        valueElement.value = value;
    }

    // Read-only per campi di sistema
    const readOnlyKeys = ["currentCustodian", "lastTransferId", "lastUpdateTs", "ts", "from", "action", "status"];
    if (readOnlyKeys.includes(key)) {
        valueElement.disabled = true;
        valueElement.classList.add("disabled-input");
        keyInput.disabled = true;
    }

    row.appendChild(keyInput);
    row.appendChild(valueElement);
    container.appendChild(row);
}

function clearAttributes() {
    const container = document.getElementById("attributes-container");
    if (container) container.innerHTML = "";
}

// Stato corrente del builder
let _builderOperation = null; // quale operazione è stata caricata
let _builderContext = {};     // dati di contesto (packageId, transferId, ecc.)

/**
 * Imposta lo stato del builder per un'operazione.
 * @param {string} operation - Nome operazione API (es. "create-package")
 * @param {string} title - Titolo da mostrare
 * @param {Object} visibleAttrs - Attributi editabili {key: value|config}
 * @param {Object} context - Dati di contesto non editabili (packageId, transferId, ecc.)
 */
function setBuilderState(operation, title, visibleAttrs, context = {}) {
    _builderOperation = operation;
    _builderContext = context;

    // Mostra il builder
    const builderSection = document.querySelector('.builder-section');
    if (builderSection) {
        builderSection.style.display = 'block';
    }

    // Mostra la key blockchain nel campo key parts
    const builderKeyEl = document.getElementById("builder-key");
    if (builderKeyEl) {
        builderKeyEl.value = context.keyString || title;
        builderKeyEl.readOnly = true;
        builderKeyEl.classList.add("disabled-input");
    }

    // Mostra la classe nel selettore (read-only)
    const classSelect = document.getElementById("builder-class");
    if (classSelect) {
        classSelect.parentElement.style.display = '';
        if (context.entityClass) {
            classSelect.value = context.entityClass;
        }
        classSelect.disabled = true;
    }
    const customInput = document.getElementById("builder-class-custom");
    if (customInput) customInput.style.display = 'none';

    // Attributi visibili
    clearAttributes();
    for (const [k, v] of Object.entries(visibleAttrs)) {
        let valStr = v;
        if (typeof v === 'object' && v !== null && v.type) {
            valStr = v;
        } else if (typeof v === 'object' && v !== null) {
            valStr = JSON.stringify(v);
        }
        addAttributeRow(k, valStr);
    }

    if (builderSection) {
        builderSection.scrollIntoView({ behavior: 'smooth' });
    }
    log(`Ready: ${title}`, "info");
}

/**
 * Raccoglie i valori editabili dal form builder.
 */
function getBuilderFormValues() {
    const valueObj = {};
    const rows = document.querySelectorAll(".attr-row");

    rows.forEach(row => {
        const k = row.querySelector(".attr-key").value.trim();
        const valueEl = row.querySelector(".attr-value");
        let v;

        if (valueEl.tagName === "SELECT" && valueEl.multiple) {
            v = Array.from(valueEl.selectedOptions).map(opt => opt.value);
        } else if (valueEl.classList.contains("checkbox-group")) {
            const checkedBoxes = valueEl.querySelectorAll("input[type=checkbox]:checked");
            v = Array.from(checkedBoxes).map(cb => cb.value);
        } else {
            v = valueEl.value.trim();
            try {
                if (v.startsWith("[") || v.startsWith("{")) {
                    v = JSON.parse(v);
                } else if (!isNaN(v) && v !== "") {
                    v = Number(v);
                }
            } catch (e) { /* keep as string */ }
        }

        if (!k) return;
        if (k === "riskCodesHandled" && !Array.isArray(v)) {
            v = [v];
        }
        valueObj[k] = v;
    });

    return valueObj;
}

// ====================================================================
// Package ID Lookup Form
// ====================================================================
function requestPackageId() {
    return new Promise((resolve) => {
        const existing = document.getElementById("pkg-lookup-form");
        if (existing) existing.remove();

        const card = document.createElement("section");
        card.id = "pkg-lookup-form";
        card.className = "card";
        card.style.cssText = "border: 2px solid var(--primary); animation: fadeIn 0.3s ease;";

        card.innerHTML = `
            <h2><span class="step-num">📦</span> Select Package</h2>
            <p style="color: var(--text-dim); margin-bottom: 1rem;">Enter the Package ID to continue with this operation.</p>
            <div style="display: flex; gap: 10px; align-items: center; flex-wrap: wrap;">
                <input type="text" id="pkg-lookup-input" class="input-field"
                       placeholder="e.g. PKG-2026-0001"
                       style="flex: 1; min-width: 200px;">
                <button id="pkg-lookup-confirm" class="btn primary">Fetch & Continue</button>
                <button id="pkg-lookup-cancel" class="btn text">Cancel</button>
            </div>
        `;

        const builderSection = document.querySelector('.builder-section');
        if (builderSection) {
            builderSection.parentNode.insertBefore(card, builderSection);
        } else {
            const main = document.querySelector("main");
            if (main) main.prepend(card);
        }

        card.scrollIntoView({ behavior: 'smooth' });
        const input = document.getElementById("pkg-lookup-input");
        input.focus();

        const confirmBtn = document.getElementById("pkg-lookup-confirm");
        const cancelBtn = document.getElementById("pkg-lookup-cancel");
        const cleanup = () => { card.remove(); };

        confirmBtn.addEventListener("click", () => {
            const val = input.value.trim();
            if (val && val.startsWith("PKG-")) {
                log(`Package ID selected: ${val}`, "success");
                cleanup();
                resolve(val);
            } else {
                input.style.borderColor = "#ff4444";
                input.placeholder = "Must start with PKG- (e.g. PKG-2026-0001)";
                log("Invalid Package ID format. Must start with PKG-.", "error");
            }
        });

        input.addEventListener("keydown", (e) => {
            if (e.key === "Enter") confirmBtn.click();
        });

        cancelBtn.addEventListener("click", () => {
            log("Package lookup cancelled.", "warning");
            cleanup();
            resolve(null);
        });
    });
}

// ====================================================================
// Actor Template Buttons (SUPERUSER)
// ====================================================================
addSafeEventListener("btn-tpl-lab", "click", () => {
    setBuilderState("create-actor", "Create LAB Actor", {
        "name": "Laboratorio Chimica",
        "location": "Lab-Edificio-A",
        "actorId": "LAB-01"
    }, { role: "LAB" });
});
addSafeEventListener("btn-tpl-f1", "click", () => {
    setBuilderState("create-actor", "Create VAN Actor", {
        "name": "Furgone 1",
        "capacityKg": { type: 'number', value: 1000 },
        "company": "Trasporti Locali SRL",
        "riskCodesHandled": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'] },
        "actorId": "F-01"
    }, { role: "TRANSPORT_LIGHT" });
});
addSafeEventListener("btn-tpl-hub", "click", () => {
    setBuilderState("create-actor", "Create HUB Actor", {
        "name": "Centro Smistamento",
        "location": "Zona Industriale",
        "riskCodesHandled": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'] },
        "actorId": "HUB-01"
    }, { role: "HUB" });
});
addSafeEventListener("btn-tpl-c1", "click", () => {
    setBuilderState("create-actor", "Create TRUCK Actor", {
        "name": "Camion Hazard",
        "company": "HazTrans SpA",
        "capacityKg": { type: 'number', value: 1000 },
        "riskCodesHandled": { type: 'select', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: "R1" },
        "actorId": "C-01"
    }, { role: "TRANSPORT_HAZ" });
});
addSafeEventListener("btn-tpl-landfill", "click", () => {
    setBuilderState("create-actor", "Create LANDFILL Actor", {
        "name": "Discarica Autorizzata",
        "location": "Provincia-X",
        "riskCodesHandled": { type: 'select', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: "R1" },
        "actorId": "LANDFILL-01"
    }, { role: "LANDFILL" });
});

// Nascondi "Add Field" button
document.addEventListener("DOMContentLoaded", () => {
    const addBtn = document.getElementById("btn-add-attr");
    if (addBtn) addBtn.style.display = "none";
});

// ====================================================================
// Operation Buttons
// ====================================================================

// 1. Create Package
addSafeEventListener("btn-p1-create", "click", async () => {
    try {
        log("Fetching LAB actors and next ID...", "info");
        const [options, idData] = await Promise.all([
            API.getActors("LAB"),
            API.getNextId("PACKAGE", "PKG", "PKG-2026-")
        ]);
        const labOptions = options.length > 0 ? options : ["LAB-01"];
        const nextId = idData.nextId;

        setBuilderState("create-package", "Create New Package", {
            "createdBy": { type: 'select', options: labOptions, selected: labOptions[0] },
            "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: [] },
            "wasteType": "",
            "weightKg": { type: 'number', value: '' }
        }, { entityClass: "PACKAGE", keyString: `PKG, ${nextId}` });
    } catch (err) {
        log(`Error: ${err.message}`, "error");
    }
});

// 2. Request Pickup (Phase 1)
addSafeEventListener("btn-p1-start", "click", async () => {
    try {
        let pkgId = await requestPackageId();
        if (!pkgId) return;

        const [pkgState, options, idData] = await Promise.all([
            API.getPackage(pkgId),
            API.getActors("F"),
            API.getNextId("TRANSFER", "TR", "TR-F-")
        ]);
        const vanOptions = options.length > 0 ? options : ["F-01"];
        const nextTrId = idData.nextId;

        setBuilderState("request-pickup", `Request Pickup — ${pkgId}`, {
            "to": { type: 'select', options: vanOptions, selected: vanOptions[0] },
            "weightKg": { type: 'number', value: pkgState.weightKg || '' },
            "wasteType": pkgState.wasteType || '',
            "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState.riskCode || [] }
        }, { packageId: pkgId, entityClass: "TRANSFER", keyString: `TR, ${nextTrId}` });
    } catch (err) {
        log(`Error: ${err.message}`, "error");
    }
});

// 3. Transport Pickup (Phase 1 - SHIPPING)
addSafeEventListener("btn-p1-pickup", "click", async () => {
    try {
        let pkgId = await requestPackageId();
        if (!pkgId) return;

        const pkgState = await API.getPackage(pkgId);
        const options = await API.getActors("HUB");
        const hubOptions = options.length > 0 ? options : ["HUB-01"];
        const trId = pkgState.lastTransferId || 'TR-F-????';

        setBuilderState("pickup-phase1", `Pickup from Lab — ${pkgId}`, {
            "to": { type: 'select', options: hubOptions, selected: hubOptions[0] },
            "weightKg": { type: 'number', value: pkgState.weightKg || '' },
            "wasteType": pkgState.wasteType || '',
            "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState.riskCode || [] }
        }, { packageId: pkgId, entityClass: "TRANSFER", keyString: `TR, ${trId}` });
    } catch (err) {
        log(`Error: ${err.message}`, "error");
    }
});

// 4. Arrive Hub (Phase 1 - COMPLETED)
addSafeEventListener("btn-p1-arrive", "click", async () => {
    try {
        let pkgId = await requestPackageId();
        if (!pkgId) return;

        const pkgState = await API.getPackage(pkgId);
        const trId = pkgState.lastTransferId || 'TR-F-????';

        setBuilderState("arrive-hub", `Arrive at Hub — ${pkgId}`, {
            "weightKg": { type: 'number', value: pkgState.weightKg || '' },
            "wasteType": pkgState.wasteType || '',
            "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState.riskCode || [] }
        }, { packageId: pkgId, entityClass: "TRANSFER", keyString: `TR, ${trId}` });
    } catch (err) {
        log(`Error: ${err.message}`, "error");
    }
});

// 5. Request Haz Pickup (Phase 2)
addSafeEventListener("btn-p2-start", "click", async () => {
    try {
        let pkgId = await requestPackageId();
        if (!pkgId) return;

        const [pkgState, options, idData] = await Promise.all([
            API.getPackage(pkgId),
            API.getActors("C"),
            API.getNextId("TRANSFER", "TR", "TR-C-")
        ]);
        const truckOptions = options.length > 0 ? options : ["C-01"];
        const nextTrId = idData.nextId;

        setBuilderState("request-haz-pickup", `Request Haz Pickup — ${pkgId}`, {
            "to": { type: 'select', options: truckOptions, selected: truckOptions[0] },
            "weightKg": { type: 'number', value: pkgState.weightKg || '' },
            "wasteType": pkgState.wasteType || '',
            "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState.riskCode || [] }
        }, { packageId: pkgId, entityClass: "TRANSFER", keyString: `TR, ${nextTrId}` });
    } catch (err) {
        log(`Error: ${err.message}`, "error");
    }
});

// 6. Transport Haz Pickup (Phase 2 - SHIPPING)
addSafeEventListener("btn-p2-pickup", "click", async () => {
    try {
        let pkgId = await requestPackageId();
        if (!pkgId) return;

        const pkgState = await API.getPackage(pkgId);
        const options = await API.getActors("LANDFILL");
        const lfOptions = options.length > 0 ? options : ["LANDFILL-01"];
        const trId = pkgState.lastTransferId || 'TR-C-????';

        setBuilderState("pickup-phase2", `Haz Pickup from Hub — ${pkgId}`, {
            "to": { type: 'select', options: lfOptions, selected: lfOptions[0] },
            "weightKg": { type: 'number', value: pkgState.weightKg || '' },
            "wasteType": pkgState.wasteType || '',
            "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState.riskCode || [] }
        }, { packageId: pkgId, entityClass: "TRANSFER", keyString: `TR, ${trId}` });
    } catch (err) {
        log(`Error: ${err.message}`, "error");
    }
});

// 7. Arrive Landfill (Phase 2 - COMPLETED)
addSafeEventListener("btn-p2-arrive", "click", async () => {
    try {
        let pkgId = await requestPackageId();
        if (!pkgId) return;

        const pkgState = await API.getPackage(pkgId);
        const trId = pkgState.lastTransferId || 'TR-C-????';

        setBuilderState("arrive-landfill", `Arrive at Landfill — ${pkgId}`, {
            "weightKg": { type: 'number', value: pkgState.weightKg || '' },
            "wasteType": pkgState.wasteType || '',
            "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState.riskCode || [] }
        }, { packageId: pkgId, entityClass: "TRANSFER", keyString: `TR, ${trId}` });
    } catch (err) {
        log(`Error: ${err.message}`, "error");
    }
});

// 8. Dispose Package
addSafeEventListener("btn-p2-dispose", "click", async () => {
    try {
        let pkgId = await requestPackageId();
        if (!pkgId) return;

        const pkgState = await API.getPackage(pkgId);

        setBuilderState("dispose-package", `Dispose Package — ${pkgId}`, {
            "weightKg": { type: 'number', value: pkgState.weightKg || '' },
            "wasteType": pkgState.wasteType || '',
            "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: pkgState.riskCode || [] }
        }, { packageId: pkgId, entityClass: "PACKAGE", keyString: `PKG, ${pkgId}` });
    } catch (err) {
        log(`Error: ${err.message}`, "error");
    }
});

// ====================================================================
// Execute Button — Sends data to the correct API endpoint
// ====================================================================
addSafeEventListener("btn-execute-builder", "click", async () => {
    const execBtn = document.getElementById("btn-execute-builder");
    if (execBtn && execBtn.disabled) return;
    if (execBtn) { execBtn.disabled = true; execBtn.textContent = "Executing…"; }

    try {
        if (!_builderOperation) {
            log("No operation selected. Click an operation button first.", "error");
            return;
        }

        const formValues = getBuilderFormValues();
        let result;

        switch (_builderOperation) {
            case "create-package":
                result = await API.createPackage({
                    createdBy: formValues.createdBy,
                    riskCode: formValues.riskCode,
                    wasteType: formValues.wasteType,
                    weightKg: formValues.weightKg
                });
                log(`Package ${result.packageId} created successfully!`, "success");
                break;

            case "request-pickup":
                result = await API.requestPickup({
                    packageId: _builderContext.packageId,
                    to: formValues.to,
                    weightKg: formValues.weightKg,
                    wasteType: formValues.wasteType,
                    riskCode: formValues.riskCode
                });
                log(`Transfer ${result.transferId} created (WAITING)`, "success");
                break;

            case "pickup-phase1":
                result = await API.pickupPhase1({
                    packageId: _builderContext.packageId,
                    to: formValues.to,
                    weightKg: formValues.weightKg,
                    wasteType: formValues.wasteType,
                    riskCode: formValues.riskCode
                });
                log(`Transfer ${result.transferId} updated (SHIPPING)`, "success");
                break;

            case "arrive-hub":
                result = await API.arriveHub({
                    packageId: _builderContext.packageId,
                    weightKg: formValues.weightKg,
                    wasteType: formValues.wasteType,
                    riskCode: formValues.riskCode
                });
                log(`Transfer ${result.transferId} completed at HUB`, "success");
                break;

            case "request-haz-pickup":
                result = await API.requestHazPickup({
                    packageId: _builderContext.packageId,
                    to: formValues.to,
                    weightKg: formValues.weightKg,
                    wasteType: formValues.wasteType,
                    riskCode: formValues.riskCode
                });
                log(`Transfer ${result.transferId} created (WAITING)`, "success");
                break;

            case "pickup-phase2":
                result = await API.pickupPhase2({
                    packageId: _builderContext.packageId,
                    to: formValues.to,
                    weightKg: formValues.weightKg,
                    wasteType: formValues.wasteType,
                    riskCode: formValues.riskCode
                });
                log(`Transfer ${result.transferId} updated (SHIPPING)`, "success");
                break;

            case "arrive-landfill":
                result = await API.arriveLandfill({
                    packageId: _builderContext.packageId,
                    weightKg: formValues.weightKg,
                    wasteType: formValues.wasteType,
                    riskCode: formValues.riskCode
                });
                log(`Transfer ${result.transferId} completed at LANDFILL`, "success");
                break;

            case "dispose-package":
                result = await API.disposePackage({
                    packageId: _builderContext.packageId
                });
                log(`Package ${result.packageId} DISPOSED`, "success");
                break;

            case "create-actor":
                result = await API.createActor({
                    actorId: formValues.actorId,
                    name: formValues.name,
                    role: _builderContext.role,
                    location: formValues.location,
                    company: formValues.company,
                    capacityKg: formValues.capacityKg,
                    riskCodesHandled: formValues.riskCodesHandled
                });
                log(`Actor ${result.actorId} created (${_builderContext.role})`, "success");
                break;

            default:
                log(`Unknown operation: ${_builderOperation}`, "error");
        }

    } catch (err) {
        log(`OPERATION FAILED: ${err.message}`, "error");
    } finally {
        if (execBtn) { execBtn.disabled = false; execBtn.textContent = "Execute Operation"; }
    }
});
