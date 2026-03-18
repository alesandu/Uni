// TUTTA ROBA INUTILE DI HTML E CSS, NON CENTRA NULLA CON LE API CALLS

// --- Event Binding ---
function addSafeEventListener(id, event, handler) {
    const el = document.getElementById(id);
    if (el) {
        el.addEventListener(event, handler);
    }
}

// --- Console Log sotto ---
function log(message, type = "info") {
    const consoleDiv = document.getElementById("log-console");
    const entry = document.createElement("div");
    entry.className = `log-entry ${type}`;

    const time = new Date().toLocaleTimeString();
    entry.innerHTML = `<span class="time">[${time}]</span> ${message}`;

    consoleDiv.appendChild(entry);
    consoleDiv.scrollTop = consoleDiv.scrollHeight;
}
// clear log
addSafeEventListener("btn-clear-log", "click", () => {
    document.getElementById("log-console").innerHTML = '<div class="log-entry system">System ready. Waiting for commands...</div>';
});

// --- Builder DOM Helpers HTML E CSS---
function addAttributeRow(key = "", value = "") {
    const container = document.getElementById("attributes-container");
    const row = document.createElement("div");
    row.className = "attr-row";

    const keyInput = document.createElement("input");
    keyInput.type = "text";
    keyInput.placeholder = "Key (e.g. weightKg)";
    keyInput.className = "input-field attr-key";
    keyInput.value = key;

    // If options specificed, use Select, otherwise Input
    let valueElement;

    // Keys are always read-only in builder templates
    keyInput.disabled = true;
    keyInput.classList.add("disabled-input");

    if (Array.isArray(value) && value.length > 0 && value[0].__isOption) {
        // Heuristic: if value passed is a special array indicating options
        // Actually cleaner to pass options as a separate arg to addAttributeRow, 
        // but setBuilderState iterates objects.
        // Let's assume if the 'value' in the object is an Array with a special flag or just handle it in setBuilderState pass.
        // Better approach: setBuilderState logic change.
        // But for now, let's look at the parameters of this function: key, value.
    }

    // Let's change the function signature slightly to support options if passed
    // But setBuilderState calls it in a loop.

    // Simpler hack: If 'value' is an object with { type: 'select', options: [], selected: '' }, render select.

    if (typeof value === 'object' && value !== null && value.type === 'select') {
        valueElement = document.createElement("select");
        valueElement.className = "input-field attr-value";
        if (value.multiple) {
            valueElement.multiple = true;
            // Optionally increase height for better UX
            valueElement.style.height = "100px";
        }

        value.options.forEach(opt => {
            const option = document.createElement("option");
            option.value = opt;
            option.textContent = opt;
            // Handle multiple selection (array) or single (string)
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
        // Tag it for the payload extractor
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

            // Handle pre-selection
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

    // Read-only logic for system fields
    const readOnlyKeys = ["currentCustodian", "lastTransferId", "lastUpdateTs", "ts", "from", "action", "status"];
    // Note: User only asked for creation fields, but making transfer fields read-only in templates makes sense too? 
    // User said: "some fileds cannot be modified, like when package is created..."
    // Let's stick strictly to what was asked + obvious system fields (ts).
    // Actually, "from", "to", "action", "status" in Transfer should probably be modifiable if the user wants to test edge cases?
    // But for "Create Package", `createdBy` etc must be fixed.
    // Let's enforce it for the Package lifecycle fields.

    if (readOnlyKeys.includes(key)) {
        valueElement.disabled = true;
        valueElement.classList.add("disabled-input");
        keyInput.disabled = true;
    }

    row.appendChild(keyInput);
    row.appendChild(valueElement);
    container.appendChild(row);
}
// Helper clear attributes
function clearAttributes() {
    document.getElementById("attributes-container").innerHTML = "";
}

// Vedere bene queste tre funzioni

let currentHiddenAttributes = {};

function setBuilderState(className, keyString, attributes, hiddenAttributes = {}) {
    // Set Class
    const classSelect = document.getElementById("builder-class");
    const customInput = document.getElementById("builder-class-custom");

    if (["ACTOR", "PACKAGE", "TRANSFER", "ALERT"].includes(className)) {
        classSelect.value = className;
        customInput.classList.add("hidden");
    } else {
        classSelect.value = "OTHER";
        customInput.classList.remove("hidden");
        customInput.value = className;
    }

    // Hide Class Field Container (Parent of select)
    // Assuming structure: <div class="form-group"> <label>Class</label> <select>... 
    // We can try to hide the select itself and label? Or just the select.
    // User request: "make the CLASS field hidden in very form"

    // Try to find the parent form-group if possible, or just hide the elements directly.
    classSelect.parentElement.style.display = 'none'; // Heuristic
    if (classSelect.previousElementSibling) classSelect.previousElementSibling.style.display = 'none'; // Label

    // Also hide custom input if present
    customInput.style.display = 'none';

    // Set Key
    const builderKeyEl = document.getElementById("builder-key");
    builderKeyEl.value = keyString;

    // Disable Key modification except for 'Create Package' and 'Create Actor'
    if ((className === "PACKAGE" && hiddenAttributes && hiddenAttributes.state === "IN_LAB" && hiddenAttributes.currentCustodian === "") || className === "ACTOR") {
        builderKeyEl.readOnly = false;
        builderKeyEl.classList.remove("disabled-input");
    } else {
        builderKeyEl.readOnly = true;
        builderKeyEl.classList.add("disabled-input");
    }

    // Set Attributes
    clearAttributes();

    // Display Visible Attributes
    for (const [k, v] of Object.entries(attributes)) {
        let valStr = v;
        // Don't stringify if it's our special config object (has 'type')
        if (typeof v === 'object' && v !== null && v.type) {
            valStr = v;
        } else if (typeof v === 'object' && v !== null) {
            valStr = JSON.stringify(v);
        }
        addAttributeRow(k, valStr);
    }
    currentHiddenAttributes = hiddenAttributes;
    const builderSection = document.querySelector('.builder-section');
    if (builderSection) {
        builderSection.style.display = 'block';
    }
    builderSection.scrollIntoView({ behavior: 'smooth' });
}

function getBuilderPackageId() {
    const keyInput = document.getElementById("builder-key").value;
    const parts = keyInput.split(",").map(p => p.trim());
    // Heuristic: Find part starting with PKG-
    let pkg = parts.find(p => p.startsWith("PKG-"));

    if (!pkg) {
        // Try to find it in the builder attribute inputs
        const inputs = document.querySelectorAll(".attr-value");
        inputs.forEach(input => {
            if (input.value && input.value.startsWith && input.value.startsWith("PKG-")) {
                pkg = input.value;
            }
        });
    }

    return pkg;
}

function getBuilderPayload() {
    const classSelect = document.getElementById("builder-class");
    let className = classSelect.value;
    if (className === "OTHER") {
        className = document.getElementById("builder-class-custom").value;
    }

    const keyRaw = document.getElementById("builder-key").value;
    // Split key by comma, trim whitespace
    const keyParts = keyRaw.split(",").map(s => s.trim()).filter(s => s.length > 0);

    const valueObj = {};
    const rows = document.querySelectorAll(".attr-row");

    rows.forEach(row => {
        const k = row.querySelector(".attr-key").value.trim();
        const valueEl = row.querySelector(".attr-value");
        let v;

        if (valueEl.tagName === "SELECT" && valueEl.multiple) {
            // Handle Multi-Select (Legacy)
            v = Array.from(valueEl.selectedOptions).map(opt => opt.value);
        } else if (valueEl.classList.contains("checkbox-group")) {
            // Handle Checkbox Group
            const checkedBoxes = valueEl.querySelectorAll("input[type=checkbox]:checked");
            v = Array.from(checkedBoxes).map(cb => cb.value);
        } else {
            v = valueEl.value.trim();
            // Auto-type inference
            // 1. Try JSON parse (for arrays/objects)
            try {
                // Only if it looks like array or object
                if (v.startsWith("[") || v.startsWith("{")) {
                    v = JSON.parse(v);
                } else {
                    // 2. Try Number
                    if (!isNaN(v) && v !== "") {
                        v = Number(v);
                    }
                }
            } catch (e) {
                // Keep as string
            }
        }

        if (!k) return;

        // Constraint Fix: riskCodesHandled should always be an array
        if (k === "riskCodesHandled" && !Array.isArray(v)) {
            v = [v];
        }

        valueObj[k] = v;
    });

    // Merge Hidden Attributes - Hidden is BASE, Value (Visual) is OVERRIDE
    const mergedValue = { ...currentHiddenAttributes, ...valueObj };


    let finalValue = mergedValue;
    if (mergedValue.hasOwnProperty("_VAL_")) {
        finalValue = mergedValue["_VAL_"];
    }

    return {
        "cmd": "AddKV",
        "class": className,
        "key": keyParts,
        "value": finalValue
    };
}





// --- Visual Helpers (Search, Cards) ---
function renderPackageCard(pkgData, container) {
    let html = '<div class="inspector-grid" style="margin-bottom: 1rem; border-bottom: 1px solid var(--border); padding-bottom: 1rem;">';

    // Explicitly show Package ID first
    if (pkgData.packageId) {
        html += `
            <div class="inspector-item" style="border-bottom: 1px dashed var(--border); padding-bottom:5px; margin-bottom:5px; width:100%;">
                <span class="inspector-label">Package ID</span>
                <span class="inspector-value" style="font-weight:bold; color:var(--primary);">${pkgData.packageId}</span>
            </div>
        `;
    }

    // Custom sort/display
    for (const [key, value] of Object.entries(pkgData)) {
        if (key === "packageId") continue; // Already shown

        let displayValue = value;
        let badgeClass = "";

        if (key === "state" || key === "status") {
            const badgeType = String(value).toLowerCase();
            badgeClass = `badge ${badgeType}`;
            displayValue = `<span class="${badgeClass}">${value}</span>`;
        } else if (key.endsWith("Ts") || key === "ts") {
            displayValue = new Date(value).toLocaleString();
        } else if (key === "weightKg") {
            displayValue = `${value} kg`;
        } else if (typeof value === 'object') {
            displayValue = JSON.stringify(value);
        }

        html += `
            <div class="inspector-item">
                <span class="inspector-label">${key}</span>
                <span class="inspector-value">${displayValue}</span>
            </div>
        `;
    }
    html += '</div>';

    const wrapper = document.createElement("div");
    wrapper.innerHTML = html;
    container.appendChild(wrapper);
}

// --- Event Bindings for UI-only logic (Templates) ---
// TEMPLATE PER LA CREAZIONE HTML E CSS
addSafeEventListener("btn-tpl-lab", "click", () => {
    const visibleAttrs = {
        "name": "Laboratorio Chimica",
        "location": "Lab-Edificio-A"
    };
    const hiddenAttrs = {
        "role": "LAB"
    };
    setBuilderState("ACTOR", "ACT, LAB-01", visibleAttrs, hiddenAttrs);
    log("Template Loaded: LAB Actor [Hidden: role]", "info");
});
addSafeEventListener("btn-tpl-f1", "click", () => {
    const visibleAttrs = {
        "name": "Furgone 1",
        "capacityKg": { type: 'number', value: 1000 },
        "company": "Trasporti Locali SRL",
        "riskCodesHandled": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'] }
    };
    const hiddenAttrs = {
        "role": "TRANSPORT_LIGHT",
        "transportedKg": 0
    };
    setBuilderState("ACTOR", "ACT, F-01", visibleAttrs, hiddenAttrs);
    log("Template Loaded: F Actor [Hidden: role]", "info");
});
addSafeEventListener("btn-tpl-hub", "click", () => {
    const visibleAttrs = {
        "name": "Centro Smistamento",
        "location": "Zona Industriale",
        "riskCodesHandled": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'] }
    };
    const hiddenAttrs = {
        "role": "HUB"
    };
    setBuilderState("ACTOR", "ACT, HUB-01", visibleAttrs, hiddenAttrs);
    log("Template Loaded: HUB Actor [Hidden: role]", "info");
});
addSafeEventListener("btn-tpl-c1", "click", () => {
    const visibleAttrs = {
        "name": "Camion Hazard",
        "company": "HazTrans SpA",
        "capacityKg": { type: 'number', value: 1000 },
        "riskCodesHandled": { type: 'select', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: "R1" }
    };
    const hiddenAttrs = {
        "role": "TRANSPORT_HAZ",
        "transportedKg": 0
    };
    setBuilderState("ACTOR", "ACT, C-01", visibleAttrs, hiddenAttrs);
    log("Template Loaded: C1 Actor [Hidden: role]", "info");
});
addSafeEventListener("btn-tpl-landfill", "click", () => {
    const visibleAttrs = {
        "name": "Discarica Autorizzata",
        "location": "Provincia-X",
        "riskCodesHandled": { type: 'select', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: "R1" }
    };
    const hiddenAttrs = {
        "role": "LANDFILL"
    };
    setBuilderState("ACTOR", "ACT, LANDFILL-01", visibleAttrs, hiddenAttrs);
    log("Template Loaded: LANDFILL Actor [Hidden: role]", "info");
});
document.addEventListener("DOMContentLoaded", () => {
    const addBtn = document.getElementById("btn-add-attr");
    if (addBtn) addBtn.style.display = "none";
});


// --- Package ID Lookup Form ---
// Shows an inline mini-form for entering a Package ID when the builder is empty.
// Returns a Promise that resolves with the Package ID or null if cancelled.
function requestPackageId() {
    return new Promise((resolve) => {
        // Remove any existing lookup form
        const existing = document.getElementById("pkg-lookup-form");
        if (existing) existing.remove();

        // Create the lookup card
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

        // Insert before the builder section
        const builderSection = document.querySelector('.builder-section');
        if (builderSection) {
            builderSection.parentNode.insertBefore(card, builderSection);
        } else {
            // Fallback: insert into main
            const main = document.querySelector("main");
            if (main) main.prepend(card);
        }

        card.scrollIntoView({ behavior: 'smooth' });

        // Focus the input
        const input = document.getElementById("pkg-lookup-input");
        input.focus();

        // Handle confirm
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

        // Also confirm on Enter key
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

addSafeEventListener("builder-class", "change", (e) => {
    const customInput = document.getElementById("builder-class-custom");
    if (e.target.value === "OTHER") {
        customInput.classList.remove("hidden");
    } else {
        customInput.classList.add("hidden");
    }
});

// --- Alert Rendering HTML E CSS---
// Global store of categorized alerts for detail panel
let _categorizedAlerts = { time: [], sequence: [], risk: [], integrity: [], duplicate: [], capacity: [] };

function categorizeAlert(alert) {
    // Use the actual alert type field for precise categorization
    const type = (alert.type || "").toUpperCase();
    if (type === "SEQUENCE_VIOLATION") return "sequence";
    if (type === "RISK_CODE_INCOMPATIBLE") return "risk";
    if (type === "INTEGRITY_MISMATCH") return "integrity";
    if (type === "DUPLICATE_ENTITY" || type === "DUPLICATE_PACKAGE") return "duplicate";
    if (type === "CAPACITY_EXCEEDED") return "capacity";
    if (type === "UNAUTHORIZED_ACTION") return "auth";
    // Fallback for unknown types
    return "time";
}

function renderAlertDetailPanel(category, alerts) {
    // Remove existing panel
    const existing = document.getElementById("alert-detail-panel");
    if (existing) existing.remove();

    if (!alerts || alerts.length === 0) return;

    const categoryLabels = {
        time: "Time Compliance",
        sequence: "Sequence Integrity",
        risk: "Risk Compatibility",
        integrity: "Data Integrity",
        duplicate: "Duplicate Entity",
        capacity: "Vehicle Capacity",
        auth: "Role Authorization"
    };

    const panel = document.createElement("section");
    panel.id = "alert-detail-panel";
    panel.className = "card";
    panel.style.cssText = "border: 2px solid var(--danger); animation: fadeIn 0.3s ease; margin-top: 1.5rem;";

    // Header with close button
    let html = `
        <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 1.5rem;">
            <h2 style="margin-bottom: 0;"><span class="step-num">⚠️</span> ${categoryLabels[category] || category} — ${alerts.length} Alert(s)</h2>
            <button id="btn-close-alert-panel" class="btn text" style="font-size: 1.5rem; padding: 0.25rem 0.5rem;">✕</button>
        </div>
    `;

    // Render each alert as a card
    alerts.forEach(alert => {
        html += `<div class="alert-detail-card" data-alert-id="${alert.id}">`;
        html += `<div class="alert-detail-header">`;
        html += `<span class="alert-detail-id">${alert.id}</span>`;
        html += `<span class="badge disposed">${alert.type || 'ALERT'}</span>`;
        html += `</div>`;

        // Message
        if (alert.msg || alert.description) {
            html += `<div class="alert-detail-msg">${alert.msg || alert.description}</div>`;
        }

        // Info grid
        html += `<div class="alert-detail-grid">`;
        if (alert.packageId) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Package</span><span class="inspector-value">${alert.packageId}</span></div>`;
        }
        if (alert.entityClass) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Entity Class</span><span class="inspector-value">${alert.entityClass}</span></div>`;
        }
        if (alert.entityId) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Entity ID</span><span class="inspector-value" style="color: var(--danger);">${alert.entityId}</span></div>`;
        }
        if (alert.existingState) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Existing State</span><span class="inspector-value"><span class="badge ${alert.existingState.toLowerCase()}">${alert.existingState}</span></span></div>`;
        }
        if (alert.existingRole) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Existing Role</span><span class="inspector-value">${alert.existingRole}</span></div>`;
        }
        if (alert.transferId) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Transfer</span><span class="inspector-value">${alert.transferId}</span></div>`;
        }
        if (alert.ts) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Timestamp</span><span class="inspector-value">${new Date(alert.ts).toLocaleString()}</span></div>`;
        }
        if (alert.currentState) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Package State</span><span class="inspector-value"><span class="badge ${alert.currentState.toLowerCase()}">${alert.currentState}</span></span></div>`;
        }
        if (alert.attemptedDestination) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Attempted Dest.</span><span class="inspector-value" style="color: var(--danger);">${alert.attemptedDestination}</span></div>`;
        }
        if (alert.attemptedAction) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Attempted Action</span><span class="inspector-value" style="color: var(--danger);">${alert.attemptedAction}</span></div>`;
        }
        if (alert.requiredStates && Array.isArray(alert.requiredStates)) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Required State</span><span class="inspector-value" style="color: var(--primary);">${alert.requiredStates.join(" or ")}</span></div>`;
        }
        if (alert.actorId) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Destination Actor</span><span class="inspector-value" style="color: var(--danger);">${alert.actorId}</span></div>`;
        }
        if (alert.packageRiskCodes && Array.isArray(alert.packageRiskCodes)) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Package Risk Codes</span><span class="inspector-value" style="color: var(--primary);">${alert.packageRiskCodes.join(", ")}</span></div>`;
        }
        if (alert.actorRiskCodesHandled && Array.isArray(alert.actorRiskCodesHandled)) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Actor Handles</span><span class="inspector-value" style="color: var(--danger);">${alert.actorRiskCodesHandled.join(", ")}</span></div>`;
        }

        // Capacity alert details
        if (alert.vehicleId) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Vehicle</span><span class="inspector-value" style="color: var(--danger);">${alert.vehicleId}</span></div>`;
        }
        if (alert.capacityKg !== undefined) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Vehicle Capacity</span><span class="inspector-value">${alert.capacityKg} kg</span></div>`;
        }
        if (alert.currentLoadKg !== undefined) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Current Load</span><span class="inspector-value">${alert.currentLoadKg} kg</span></div>`;
        }
        if (alert.packageWeightKg !== undefined) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Package Weight</span><span class="inspector-value">${alert.packageWeightKg} kg</span></div>`;
        }
        if (alert.attemptedLoadKg !== undefined) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Attempted Load</span><span class="inspector-value" style="color: var(--danger);">${alert.attemptedLoadKg} kg</span></div>`;
        }

        // Mismatches detail
        if (alert.mismatches && typeof alert.mismatches === 'object') {
            for (const [field, vals] of Object.entries(alert.mismatches)) {
                const exp = Array.isArray(vals.expected) ? vals.expected.join(", ") : vals.expected;
                const got = Array.isArray(vals.got) ? vals.got.join(", ") : vals.got;
                html += `<div class="alert-detail-field alert-detail-mismatch">
                    <span class="inspector-label">${field}</span>
                    <span class="inspector-value"><span style="color: var(--primary);">Expected:</span> ${exp} → <span style="color: var(--danger);">Got:</span> ${got}</span>
                </div>`;
            }
        }

        // Auth details
        if (alert.userRole) {
            html += `<div class="alert-detail-field"><span class="inspector-label">User Role</span><span class="inspector-value" style="color: var(--danger);">${alert.userRole}</span></div>`;
        }
        if (alert.requiredRole) {
            html += `<div class="alert-detail-field"><span class="inspector-label">Required Role</span><span class="inspector-value" style="color: var(--primary);">${alert.requiredRole}</span></div>`;
        }

        html += `</div>`; // close grid

        // Dismiss button
        html += `<div style="text-align: right; margin-top: 0.75rem;">
            <button class="btn danger-dismiss" onclick="dismissAlert('${alert.id}')">🗑 Dismiss Alert</button>
        </div>`;
        html += `</div>`; // close card
    });

    panel.innerHTML = html;

    // Insert after the alerts-grid card
    const gridCard = document.getElementById("alerts-grid");
    if (gridCard && gridCard.closest(".card")) {
        gridCard.closest(".card").insertAdjacentElement("afterend", panel);
    } else {
        const main = document.querySelector("main");
        if (main) main.appendChild(panel);
    }

    // Scroll to panel
    panel.scrollIntoView({ behavior: 'smooth', block: 'start' });

    // Close button
    document.getElementById("btn-close-alert-panel").addEventListener("click", () => {
        panel.remove();
    });
}

function renderAlerts(alerts, container) {
    // 1. Reset Monitors to Hidden State
    const monitors = {
        time: { id: "monitor-time", count: 0 },
        sequence: { id: "monitor-sequence", count: 0 },
        risk: { id: "monitor-risk", count: 0 },
        integrity: { id: "monitor-integrity", count: 0 },
        duplicate: { id: "monitor-duplicate", count: 0 },
        capacity: { id: "monitor-capacity", count: 0 },
        auth: { id: "monitor-auth", count: 0 }
    };

    // Reset categorized alerts
    _categorizedAlerts = { time: [], sequence: [], risk: [], integrity: [], duplicate: [], capacity: [], auth: [] };

    // Hide all initially
    Object.values(monitors).forEach(m => {
        const el = document.getElementById(m.id);
        if (el) {
            el.style.display = "none";
            el.classList.remove("alert-active");
            el.style.cursor = "default";
            // Reset text (though hidden)
            el.querySelector(".monitor-status").innerText = "";
        }
    });

    // Handle "System Healthy" message visibility
    let healthyMsg = document.getElementById("system-healthy-msg");
    if (!healthyMsg) {
        // Create if missing
        const grid = document.getElementById("alerts-grid");
        if (grid) {
            healthyMsg = document.createElement("div");
            healthyMsg.id = "system-healthy-msg";
            healthyMsg.className = "log-entry success";
            healthyMsg.style.gridColumn = "1 / -1"; // distinct span
            healthyMsg.style.textAlign = "center";
            healthyMsg.style.fontSize = "1.5rem";
            healthyMsg.style.padding = "2rem";
            healthyMsg.innerText = "System Healthy. No active alerts.";
            grid.appendChild(healthyMsg);
        }
    }

    if (alerts.length === 0) {
        if (healthyMsg) healthyMsg.style.display = "block";
        return;
    }

    // 2. Categorize Alerts
    alerts.forEach(alert => {
        const cat = categorizeAlert(alert);
        monitors[cat].count++;
        _categorizedAlerts[cat].push(alert);
    });

    // 3. Update UI for Active Monitors
    let hasActive = false;
    Object.entries(monitors).forEach(([cat, m]) => {
        if (m.count > 0) {
            hasActive = true;
            const el = document.getElementById(m.id);
            if (el) {
                el.style.display = "flex"; // Show it
                el.classList.add("alert-active");
                el.style.cursor = "pointer";

                const statusEl = el.querySelector(".monitor-status");
                statusEl.innerText = `${m.count} ERRORS`;
                statusEl.style.color = "#ff0000";

                el.querySelector(".monitor-desc").innerText = "Click to view details";

                // Clickable: show detail panel
                // Remove old listeners by cloning
                const newEl = el.cloneNode(true);
                el.parentNode.replaceChild(newEl, el);
                newEl.addEventListener("click", () => {
                    renderAlertDetailPanel(cat, _categorizedAlerts[cat]);
                });
            }
        }
    });

    // Toggle Healthy Message
    if (healthyMsg) {
        healthyMsg.style.display = hasActive ? "none" : "block";
    }

    // Optional: Dump to verbose log
    const logConsole = document.getElementById("log-console");
    if (logConsole) {
        logConsole.innerHTML = `<div class="log-entry system">Scan complete. ${alerts.length} active signals.</div>`;
        alerts.forEach(a => {
            logConsole.innerHTML += `<div class="log-entry error">[${a.id}] ${a.type || 'Alert'}: ${a.msg || a.description}</div>`;
        });
    }
}

// --- History Timeline Rendering ---
// Riceve i dati già fetchati da fetchPackageHistory in logic.js e costruisce il DOM.
function renderHistoryTimeline(packageId, entries, elements) {
    const { resultsCard, timeline, titleEl, countEl, summaryBar } = elements;

    if (titleEl) titleEl.textContent = `History — ${packageId}`;
    if (resultsCard) resultsCard.style.display = '';

    if (!entries || entries.length === 0) {
        timeline.innerHTML = '<div class="history-empty"><div class="big-icon">📭</div>No history found for this Package ID.</div>';
        if (countEl) countEl.textContent = '';
        if (summaryBar) summaryBar.style.display = 'none';
        return;
    }

    if (countEl) countEl.textContent = `${entries.length} transaction${entries.length !== 1 ? 's' : ''}`;

    // Summary bar — latest non-deleted entry
    const latestEntry = entries.find(e => !e.isDelete);
    if (summaryBar && latestEntry) {
        try {
            const latestVal = JSON.parse(latestEntry.data).value;
            summaryBar.innerHTML = `
                <div class="pill">🆔 ID: <span>${packageId}</span></div>
                <div class="pill">📦 State: <span>${latestVal.state || '—'}</span></div>
                <div class="pill">⚖️ Weight: <span>${latestVal.weightKg != null ? latestVal.weightKg + ' kg' : '—'}</span></div>
                <div class="pill">☣ Waste: <span>${latestVal.wasteType || '—'}</span></div>
                <div class="pill">⚠️ Risk: <span>${Array.isArray(latestVal.riskCode) ? latestVal.riskCode.join(', ') : (latestVal.riskCode || '—')}</span></div>
                <div class="pill">🏭 Custodian: <span>${latestVal.currentCustodian || '—'}</span></div>
            `;
            summaryBar.style.display = '';
        } catch (_) {
            summaryBar.style.display = 'none';
        }
    }

    const stateColors = {
        'IN_LAB': '#6366f1', 'F_TRANSPORT': '#f59e0b', 'IN_HUB': '#3b82f6',
        'C_TRANSPORT': '#f97316', 'IN_LANDFILL': '#8b5cf6', 'DISPOSED': '#10b981'
    };

    timeline.innerHTML = '';
    entries.forEach((entry, idx) => {
        const dot = document.createElement('div');
        dot.className = 'timeline-dot' + (entry.isDelete ? ' deleted' : '');

        const el = document.createElement('div');
        el.className = 'timeline-entry';

        let parsedValue = null;
        try { parsedValue = JSON.parse(entry.data).value; } catch (_) { }

        const ts = entry.timestamp
            ? new Date(entry.timestamp).toLocaleString('it-IT', { dateStyle: 'short', timeStyle: 'medium' })
            : '—';
        const txShort = entry.txId ? entry.txId.substring(0, 20) + '…' : '—';
        const state = parsedValue ? (parsedValue.state || '') : '';
        const stateColor = stateColors[state] || 'var(--primary)';
        const badgeClass = entry.isDelete ? 'timeline-state-badge deleted-badge' : 'timeline-state-badge';
        const badgeText = entry.isDelete ? 'DELETED' : (state || 'UPDATE');

        let fieldsHtml = '';
        if (parsedValue && !entry.isDelete) {
            const fields = [
                ['Custodian', parsedValue.currentCustodian],
                ['Transfer', parsedValue.lastTransferId],
                ['CreatedBy', parsedValue.createdBy],
                ['Updated', parsedValue.lastUpdateTs ? new Date(parsedValue.lastUpdateTs).toLocaleString('it-IT', { dateStyle: 'short', timeStyle: 'medium' }) : null]
            ].filter(([_, v]) => v && v !== 'null');
            fieldsHtml = fields.map(([k, v]) => `<div class="timeline-field">${k}: <span>${v}</span></div>`).join('');
        }

        el.innerHTML = `
            <div class="timeline-header">
                <div>
                    <div class="timeline-ts">🕐 ${ts}${idx === 0 ? ' <em style="color:#10b981;font-size:0.7rem;">(latest)</em>' : ''}</div>
                    <div class="timeline-txid" title="${entry.txId || ''}">TX: ${txShort}</div>
                </div>
                <span class="${badgeClass}" style="${entry.isDelete ? '' : `background:${stateColor}22;color:${stateColor};border-color:${stateColor}55;`}">${badgeText}</span>
            </div>
            ${fieldsHtml ? `<div class="timeline-fields">${fieldsHtml}</div>` : ''}
        `;
        el.prepend(dot);
        timeline.appendChild(el);
    });
}

// --- Inspector Results Rendering ---
// Riceve i packages già filtrati da btn-inspect in logic.js e li renderizza.
function renderInspectorResults(packages, resultDiv) {
    resultDiv.innerHTML = '';
    if (packages.length === 0) {
        resultDiv.innerHTML = '<div class="log-entry error">No packages found matching criteria.</div>';
    } else {
        packages.forEach(pkg => renderPackageCard(pkg, resultDiv));
    }
}
