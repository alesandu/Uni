const PROXY_URL = "http://localhost:3000/api/proxy";

// --- Logger Utility ---
function log(message, type = "info") {
    const consoleDiv = document.getElementById("log-console");
    const entry = document.createElement("div");
    entry.className = `log-entry ${type}`;

    const time = new Date().toLocaleTimeString();
    entry.innerHTML = `<span class="time">[${time}]</span> ${message}`;

    consoleDiv.appendChild(entry);
    consoleDiv.scrollTop = consoleDiv.scrollHeight;
}

// --- API Interaction ---
async function callBlockchain(payload, actionName) {
    log(`Executing: ${actionName}...`, "system");
    try {
        const response = await fetch(PROXY_URL, {
            method: "POST",
            headers: {
                "Content-Type": "application/json"
            },
            body: JSON.stringify(payload)
        });

        const data = await response.json();

        if (response.ok) {
            log(`Success: ${actionName}`, "success");
            log(`Response: ${JSON.stringify(data)}`, "system");
            return data; // Return data for use
        } else {
            log(`Error: ${actionName} failed`, "error");
            log(`Details: ${JSON.stringify(data)}`, "error");
            return null;
        }
    } catch (error) {
        log(`Network Error: ${error.message}`, "error");
        return null;
    }
}

// --- Builder Logic ---



// --- Builder Logic ---

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

    if (Array.isArray(value) && value.length > 0 && value[0].__isOption) {
        // Heuristic: if value passed is a special array indicating options
        // Actually cleaner to pass options as a separate arg to addAttributeRow, 
        // but setBuilderState iterates objects.
        // Let's assume if the 'value' in the object is an Array with a special flag or just handle it in setBuilderState pass.

        // Better approach: setBuilderState logic change.
        // But for now, let's look at the parameters of this function: key, value.
        // If I change setBuilderState to pass a "value" that is actually a config obj?
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

    // Detect if value is JSON (object/array) or number for better UX? 
    // For now simple text, parse later if needed? 
    // Actually our value inputs need to support types. 
    // We'll stick to string/number auto-detection in constructPayload.

    // const removeBtn = document.createElement("button");
    // removeBtn.className = "btn danger-text";
    // removeBtn.innerHTML = "&times;";
    // removeBtn.title = "Remove Field";
    // removeBtn.onclick = () => container.removeChild(row);

    row.appendChild(keyInput);
    row.appendChild(valueElement);
    // row.appendChild(removeBtn); // Removed per user request
    container.appendChild(row);
}

function clearAttributes() {
    document.getElementById("attributes-container").innerHTML = "";
}

// Store hidden attributes globally or attached to the builder state
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

    // Try to find the parent form-group if possible, or just hide the inputs.
    // Since we don't have easy DOM traversal here without IDs on parents, let's hide the elements directly.
    classSelect.parentElement.style.display = 'none'; // Heuristic
    if (classSelect.previousElementSibling) classSelect.previousElementSibling.style.display = 'none'; // Label

    // Also hide custom input if present
    customInput.style.display = 'none';

    // Set Key
    document.getElementById("builder-key").value = keyString;

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

    // Store Hidden Attributes
    currentHiddenAttributes = hiddenAttributes;

    // Scroll to builder
    document.querySelector('.builder-section').scrollIntoView({ behavior: 'smooth' });
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

    // Merge Hidden Attributes
    const mergedValue = { ...valueObj, ...currentHiddenAttributes };

    // SYNC Logic: If creating package, ensure currentCustodian == createdBy
    if (className === "PACKAGE" && mergedValue.createdBy && mergedValue.state === "IN_LAB") {
        mergedValue.currentCustodian = mergedValue.createdBy;
    }

    // Special handling: if valueObj is empty and there's a specific simple value requirement... 
    // But for AddKV usually value is an object. 
    // Exception: ALERT schemaVersion is just "1" (string) in the example.
    // If the builder has ONE attribute with EMPTY key? No, easier to just support obj.
    // Let's look at ALERT example: "key": ["_META", ...], "value": "1".
    // I will add a check: if exactly one row has key "_VAL_", use its value directly.

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

// --- Event Listeners ---

// Builder Actions
// Builder Actions
// document.getElementById("btn-add-attr").addEventListener("click", () => addAttributeRow());
// Hide the button via CSS or just disable listener? User said "remove". 
// Best to hide it if we can't edit HTML. But I can't edit HTML from here easily without view_file.
// Wait, I can try to find the button in DOM and hide it via script on load.

document.addEventListener("DOMContentLoaded", () => {
    const addBtn = document.getElementById("btn-add-attr");
    if (addBtn) addBtn.style.display = "none";
});

document.getElementById("builder-class").addEventListener("change", (e) => {
    const customInput = document.getElementById("builder-class-custom");
    if (e.target.value === "OTHER") {
        customInput.classList.remove("hidden");
    } else {
        customInput.classList.add("hidden");
    }
});

document.getElementById("btn-execute-builder").addEventListener("click", async () => {
    const payload = getBuilderPayload();
    await callBlockchain(payload, `Custom Op: ${payload.class}`);


});


// --- Templates ---

// Actors
document.getElementById("btn-tpl-lab").addEventListener("click", () => {
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

document.getElementById("btn-tpl-f1").addEventListener("click", () => {
    const visibleAttrs = {
        "name": "Furgone 1",
        "capacityKg": { type: 'number', value: 1000 },
        "company": "Trasporti Locali SRL",
        "riskCodesHandled": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: ["R1", "R2"] }
    };
    const hiddenAttrs = {
        "role": "TRANSPORT_LIGHT"
    };
    setBuilderState("ACTOR", "ACT, F-01", visibleAttrs, hiddenAttrs);
    log("Template Loaded: F Actor [Hidden: role]", "info");
});

document.getElementById("btn-tpl-hub").addEventListener("click", () => {
    const visibleAttrs = {
        "name": "Centro Smistamento",
        "location": "Zona Industriale 3",
        "riskCodesHandled": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: ["R1", "R2"] }
    };
    const hiddenAttrs = {
        "role": "HUB"
    };
    setBuilderState("ACTOR", "ACT, HUB-01", visibleAttrs, hiddenAttrs);
    log("Template Loaded: HUB Actor [Hidden: role]", "info");
});

document.getElementById("btn-tpl-c1").addEventListener("click", () => {
    const visibleAttrs = {
        "name": "Camion Hazard",
        "company": "HazTrans SpA",
        "capacityKg": { type: 'number', value: 1000 },
        "riskCodesHandled": { type: 'select', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: "R1" }
    };
    const hiddenAttrs = {
        "role": "TRANSPORT_HAZ"
    };
    setBuilderState("ACTOR", "ACT, C-01", visibleAttrs, hiddenAttrs);
    log("Template Loaded: C1 Actor [Hidden: role]", "info");
});

document.getElementById("btn-tpl-landfill").addEventListener("click", () => {
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


// --- Helpers for Dynamic Logic ---

function getBuilderPackageId() {
    const keyInput = document.getElementById("builder-key").value;
    const parts = keyInput.split(",").map(p => p.trim());
    // Heuristic: Find part starting with PKG-
    let pkg = parts.find(p => p.startsWith("PKG-"));
    return pkg || "PKG-2026-0001"; // Fallback
}

async function fetchPackageState(packageId) {
    if (!packageId) return null;
    log(`Fetching state for ${packageId}...`, "info");
    const payload = { "cmd": "GetKV", "class": "PACKAGE", "key": ["PKG", packageId] };
    const data = await callBlockchain(payload, `Get PKG ${packageId}`);
    if (data && data.answer && data.answer.value) {
        return data.answer.value;
    }
    return null;
}

async function fetchTransferState(packageId, transferId) {
    if (!packageId || !transferId) return null;
    log(`Fetching transfer ${transferId} for pkg ${packageId}...`, "info");
    const payload = { "cmd": "GetKV", "class": "TRANSFER", "key": ["TR", packageId, transferId] };
    const data = await callBlockchain(payload, `Get TR ${transferId}`);
    if (data && data.answer && data.answer.value) {
        return data.answer.value;
    }
    return null;
}

// --- Phase 1: Lab -> Hub ---

// 1. Create Package
document.getElementById("btn-p1-create").addEventListener("click", async () => {
    // Fetch Actors logic
    log("Fetching LAB actors...", "info");
    const payload = {
        "cmd": "GetKeys",
        "class": "ACTOR",
        "key": ["ACT"]
    };

    const data = await callBlockchain(payload, "Get Actors");
    let options = ["LAB-01"]; // Default fallback

    if (data && data.answer && data.answer.keys) {
        // Filter for keys containing "LAB"
        // key structure: ["ACT", "LAB-01"]
        const labKeys = data.answer.keys
            .filter(k => k[1].includes("LAB"))
            .map(k => k[1]); // Extract ID

        if (labKeys.length > 0) options = labKeys;
    }

    // Visible fields
    const visibleAttrs = {
        "createdBy": { type: 'select', options: options, selected: options[0] },
        "riskCode": { type: 'checkbox-group', options: ['R1', 'R2', 'R3', 'R4', 'R5'], selected: [] },
        "wasteType": "",
        "weightKg": { type: 'number', value: '' }
    };

    // Hidden fields (unchangeable):
    const hiddenAttrs = {
        "state": "IN_LAB",
        "currentCustodian": "",
        "lastTransferId": "null",
        "lastUpdateTs": new Date().toISOString()
    };

    setBuilderState("PACKAGE", "PKG, PKG-2026-0001", visibleAttrs, hiddenAttrs);
    log("Tpl: Create Package (LAB) [Dynamic LAB list]", "info");
});

// Helper to fetch keys
async function fetchActorOptions(filterStr) {
    const payload = { "cmd": "GetKeys", "class": "ACTOR", "key": ["ACT"] };
    const data = await callBlockchain(payload, `Get Actors (${filterStr})`);

    let options = [];
    if (data && data.answer && data.answer.keys) {
        options = data.answer.keys
            .filter(k => k[1].startsWith(filterStr))
            .map(k => k[1]);
    }

    if (options.length === 0) options = [`${filterStr}-01`]; // Fallback
    return options;
}

// 2. Start Transfer (Waiting)
document.getElementById("btn-p1-start").addEventListener("click", async () => {
    // Target: Light Transport (F)
    const options = await fetchActorOptions("F");

    // Dynamic From: Custodian
    const pkgId = getBuilderPackageId();
    const pkgState = await fetchPackageState(pkgId);
    const fromActor = pkgState ? pkgState.currentCustodian : "LAB-01";

    const visibleAttrs = {
        "to": { type: 'select', options: options, selected: options[0] }
    };
    const hiddenAttrs = {
        "from": fromActor,
        "action": "PICKUP",
        "status": "WAITING",
        "ts": new Date().toISOString()
    };
    setBuilderState("TRANSFER", `TR, ${pkgId}, T-0001`, visibleAttrs, hiddenAttrs);
    log(`Tpl P1: Start Transfer [From: ${fromActor}]`, "info");
});

// 3. Transport Pickup (Shipping) -> Chain: Package Update
document.getElementById("btn-p1-pickup").addEventListener("click", async () => {
    const pkgId = getBuilderPackageId();
    const pkgState = await fetchPackageState(pkgId);
    let fromActor = "F-01"; // Fallback
    let transferId = "T-0001";

    if (pkgState && pkgState.lastTransferId) {
        transferId = pkgState.lastTransferId;
        const transferData = await fetchTransferState(pkgId, transferId);
        if (transferData && transferData.to) {
            fromActor = transferData.to; // The actor who was assigned
        }
    } else {
        log("Warning: Could not fetch Package state for Pickup inference.", "warning");
    }

    const options = await fetchActorOptions("HUB");
    const visibleAttrs = {
        "to": { type: 'select', options: options, selected: options[0] }
    };
    const hiddenAttrs = {
        "from": fromActor,
        "action": "SHIPPING",
        "status": "SHIPPING",
        "ts": new Date().toISOString()
    };

    setBuilderState("TRANSFER", `TR, ${pkgId}, ${transferId}`, visibleAttrs, hiddenAttrs);
    log(`Tpl P1: Transport Pickup [From: ${fromActor}]`, "info");
});

// 4. Arrive Hub (Completed) -> Chain: Package Update
document.getElementById("btn-p1-arrive").addEventListener("click", async () => {
    const pkgId = getBuilderPackageId();
    const pkgState = await fetchPackageState(pkgId);

    if (pkgState && pkgState.lastTransferId) {
        const transferData = await fetchTransferState(pkgId, pkgState.lastTransferId);
        if (transferData) {
            const visibleAttrs = {}; // Empty
            const hiddenAttrs = {
                ...transferData,
                "status": "COMPLETED",
                "action": "DELIVER",
                "ts": new Date().toISOString()
            };
            setBuilderState("TRANSFER", `TR, ${pkgId}, ${pkgState.lastTransferId}`, visibleAttrs, hiddenAttrs);
            log(`Tpl P1: Hub Arrival [Closing T-ID: ${pkgState.lastTransferId}]`, "info");
        } else {
            log("Error: Could not fetch Transfer data", "error");
        }
    } else {
        log("Error: Package has no lastTransferId or not found", "error");
        // Fallback legacy
        const options = await fetchActorOptions("HUB");
        const visibleAttrs = { "to": { type: 'select', options: options, selected: options[0] } };
        const hiddenAttrs = { "from": "F-01", "action": "DELIVER", "status": "COMPLETED", "ts": new Date().toISOString() };
        setBuilderState("TRANSFER", `TR, ${pkgId}, T-0001`, visibleAttrs, hiddenAttrs);
    }
});


// --- Phase 2: Hub -> Landfill ---

// 1. Start Haz Transfer (Waiting)
document.getElementById("btn-p2-start").addEventListener("click", async () => {
    const options = await fetchActorOptions("C"); // Matches C-01, C-02...

    // Dynamic From: Package Custodian (should be HUB now)
    const pkgId = getBuilderPackageId();
    const pkgState = await fetchPackageState(pkgId);
    const fromActor = pkgState ? pkgState.currentCustodian : "HUB-01";

    const visibleAttrs = {
        "to": { type: 'select', options: options, selected: options[0] }
    };
    const hiddenAttrs = {
        "from": fromActor,
        "action": "PICKUP",
        "status": "WAITING",
        "ts": new Date().toISOString()
    };
    setBuilderState("TRANSFER", `TR, ${pkgId}, T-0002`, visibleAttrs, hiddenAttrs);
    log(`Tpl P2: Hub requests Haz pickup [From: ${fromActor}]`, "info");
});

// 2. Transport Haz Pickup (Shipping) -> Chain: Package Update
document.getElementById("btn-p2-pickup").addEventListener("click", async () => {
    const pkgId = getBuilderPackageId();
    const pkgState = await fetchPackageState(pkgId);
    let fromActor = "C-01"; // Fallback
    let transferId = "T-0002";

    if (pkgState && pkgState.lastTransferId) {
        transferId = pkgState.lastTransferId;
        const transferData = await fetchTransferState(pkgId, transferId);
        if (transferData && transferData.to) {
            fromActor = transferData.to; // The actor who was assigned
        }
    }

    const options = await fetchActorOptions("LANDFILL");
    const visibleAttrs = {
        "to": { type: 'select', options: options, selected: options[0] }
    };
    const hiddenAttrs = {
        "from": fromActor,
        "action": "SHIPPING",
        "status": "SHIPPING",
        "ts": new Date().toISOString()
    };
    setBuilderState("TRANSFER", `TR, ${pkgId}, ${transferId}`, visibleAttrs, hiddenAttrs);
    log(`Tpl P2: Haz Pickup [From: ${fromActor}]`, "info");
});

// 3. Arrive Landfill (Completed) -> Chain: Package Update
document.getElementById("btn-p2-arrive").addEventListener("click", async () => {
    const pkgId = getBuilderPackageId();
    const pkgState = await fetchPackageState(pkgId);

    if (pkgState && pkgState.lastTransferId) {
        const transferData = await fetchTransferState(pkgId, pkgState.lastTransferId);
        if (transferData) {
            const visibleAttrs = {}; // Empty
            const hiddenAttrs = {
                ...transferData,
                "status": "COMPLETED",
                "action": "DELIVER",
                "ts": new Date().toISOString()
            };
            setBuilderState("TRANSFER", `TR, ${pkgId}, ${pkgState.lastTransferId}`, visibleAttrs, hiddenAttrs);
            log(`Tpl P2: Landfill Arrival [Closing T-ID: ${pkgState.lastTransferId}]`, "info");
        } else {
            log("Error: Could not fetch Transfer data", "error");
        }
    } else {
        log("Error: Package has no lastTransferId or not found", "error");
        // Fallback legacy
        const options = await fetchActorOptions("LANDFILL");
        const visibleAttrs = { "to": { type: 'select', options: options, selected: options[0] } };
        const hiddenAttrs = { "from": "C-01", "action": "DELIVER", "status": "COMPLETED", "ts": new Date().toISOString() };
        setBuilderState("TRANSFER", `TR, ${pkgId}, T-0002`, visibleAttrs, hiddenAttrs);
    }
});

// 4. Dispose Package (Landfill)
document.getElementById("btn-p2-dispose").addEventListener("click", async () => {
    // Attempt to parse Package ID from the Key Input field
    // Format is usually "TR, PKG-2026-0001, T-0002" or "PKG, PKG-2026-0001"
    const keyInput = document.getElementById("builder-key").value;
    const parts = keyInput.split(",").map(p => p.trim());

    // Heuristic: Find the part starting with "PKG-"
    let packageId = parts.find(p => p.startsWith("PKG-"));

    if (!packageId) {
        // Fallback for demo purposes if field is empty or weird
        packageId = "PKG-2026-0001";
        log(`Warning: Could not infer PKG ID from key '${keyInput}'. Defaulting to ${packageId}`, "warning");
    }

    log(`Fetching current state for ${packageId}...`, "info");
    const payload = {
        "cmd": "GetKV",
        "class": "PACKAGE",
        "key": ["PKG", packageId]
    };

    const data = await callBlockchain(payload, "Get Package State");

    if (data && data.answer && data.answer.value) {
        const currentPkg = data.answer.value;

        // Construct new state
        const visibleAttrs = {};

        const hiddenAttrs = {
            ...currentPkg,
            "state": "Disposed",
            "lastUpdateTs": new Date().toISOString()
        };

        setBuilderState("PACKAGE", `PKG, ${packageId}`, visibleAttrs, hiddenAttrs);
        log(`Tpl P2: Dispose Package (Dynamic) [State -> Disposed]`, "success");
    } else {
        log(`Error: Could not fetch package ${packageId}`, "error");
        alert(`Package ${packageId} not found on chain.`);
    }
});

document.getElementById("btn-clear-log").addEventListener("click", () => {
    document.getElementById("log-console").innerHTML = '<div class="log-entry system">System ready. Waiting for commands...</div>';
});
