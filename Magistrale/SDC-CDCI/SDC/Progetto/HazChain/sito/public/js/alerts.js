// --- Alerts: Rendering dashboard allarmi ---
// Pura UI — i dati arrivano da GET /api/query/alerts

// Global store of categorized alerts for detail panel
let _categorizedAlerts = { time: [], sequence: [], risk: [], integrity: [], duplicate: [], capacity: [], auth: [] };

function categorizeAlert(alert) {
    const type = (alert.type || "").toUpperCase();
    if (type === "SEQUENCE_VIOLATION") return "sequence";
    if (type === "RISK_CODE_INCOMPATIBLE") return "risk";
    if (type === "INTEGRITY_MISMATCH") return "integrity";
    if (type === "DUPLICATE_ENTITY" || type === "DUPLICATE_PACKAGE") return "duplicate";
    if (type === "CAPACITY_EXCEEDED") return "capacity";
    if (type === "UNAUTHORIZED_ACTION") return "auth";
    return "time";
}

// ====================================================================
// Dismiss Alert
// ====================================================================
async function dismissAlert(alertId) {
    try {
        log(`Deleting alert ${alertId}...`, "info");
        await API.dismissAlert({ alertId });
        log(`Alert ${alertId} deleted.`, "success");

        const card = document.querySelector(`.alert-detail-card[data-alert-id="${alertId}"]`);
        if (card) {
            card.style.transition = "opacity 0.3s ease, transform 0.3s ease";
            card.style.opacity = "0";
            card.style.transform = "translateX(30px)";
            setTimeout(() => card.remove(), 300);
        }
        setTimeout(() => fetchAlerts(), 400);
    } catch (err) {
        log(`Failed to dismiss alert: ${err.message}`, "error");
    }
}

// ====================================================================
// Fetch & Render Alerts
// ====================================================================
async function fetchAlerts() {
    const container = document.getElementById("alerts-container");
    if (!container) return;

    try {
        const alerts = await API.getAlerts();
        renderAlerts(alerts, container);
    } catch (err) {
        log(`Failed to fetch alerts: ${err.message}`, "error");
    }
}

function renderAlertDetailPanel(category, alerts) {
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

    let html = `
        <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 1.5rem;">
            <h2 style="margin-bottom: 0;"><span class="step-num">⚠️</span> ${categoryLabels[category] || category} — ${alerts.length} Alert(s)</h2>
            <button id="btn-close-alert-panel" class="btn text" style="font-size: 1.5rem; padding: 0.25rem 0.5rem;">✕</button>
        </div>
    `;

    alerts.forEach(alert => {
        html += `<div class="alert-detail-card" data-alert-id="${alert.id}">`;
        html += `<div class="alert-detail-header">`;
        html += `<span class="alert-detail-id">${alert.id}</span>`;
        html += `<span class="badge disposed">${alert.type || 'ALERT'}</span>`;
        html += `</div>`;

        if (alert.msg || alert.description) {
            html += `<div class="alert-detail-msg">${alert.msg || alert.description}</div>`;
        }

        html += `<div class="alert-detail-grid">`;
        if (alert.packageId) html += `<div class="alert-detail-field"><span class="inspector-label">Package</span><span class="inspector-value">${alert.packageId}</span></div>`;
        if (alert.entityClass) html += `<div class="alert-detail-field"><span class="inspector-label">Entity Class</span><span class="inspector-value">${alert.entityClass}</span></div>`;
        if (alert.entityId) html += `<div class="alert-detail-field"><span class="inspector-label">Entity ID</span><span class="inspector-value" style="color: var(--danger);">${alert.entityId}</span></div>`;
        if (alert.existingState) html += `<div class="alert-detail-field"><span class="inspector-label">Existing State</span><span class="inspector-value"><span class="badge ${alert.existingState.toLowerCase()}">${alert.existingState}</span></span></div>`;
        if (alert.existingRole) html += `<div class="alert-detail-field"><span class="inspector-label">Existing Role</span><span class="inspector-value">${alert.existingRole}</span></div>`;
        if (alert.transferId) html += `<div class="alert-detail-field"><span class="inspector-label">Transfer</span><span class="inspector-value">${alert.transferId}</span></div>`;
        if (alert.ts) html += `<div class="alert-detail-field"><span class="inspector-label">Timestamp</span><span class="inspector-value">${new Date(alert.ts).toLocaleString()}</span></div>`;
        if (alert.currentState) html += `<div class="alert-detail-field"><span class="inspector-label">Package State</span><span class="inspector-value"><span class="badge ${alert.currentState.toLowerCase()}">${alert.currentState}</span></span></div>`;
        if (alert.attemptedDestination) html += `<div class="alert-detail-field"><span class="inspector-label">Attempted Dest.</span><span class="inspector-value" style="color: var(--danger);">${alert.attemptedDestination}</span></div>`;
        if (alert.attemptedAction) html += `<div class="alert-detail-field"><span class="inspector-label">Attempted Action</span><span class="inspector-value" style="color: var(--danger);">${alert.attemptedAction}</span></div>`;
        if (alert.requiredStates && Array.isArray(alert.requiredStates)) html += `<div class="alert-detail-field"><span class="inspector-label">Required State</span><span class="inspector-value" style="color: var(--primary);">${alert.requiredStates.join(" or ")}</span></div>`;
        if (alert.actorId) html += `<div class="alert-detail-field"><span class="inspector-label">Destination Actor</span><span class="inspector-value" style="color: var(--danger);">${alert.actorId}</span></div>`;
        if (alert.packageRiskCodes && Array.isArray(alert.packageRiskCodes)) html += `<div class="alert-detail-field"><span class="inspector-label">Package Risk Codes</span><span class="inspector-value" style="color: var(--primary);">${alert.packageRiskCodes.join(", ")}</span></div>`;
        if (alert.actorRiskCodesHandled && Array.isArray(alert.actorRiskCodesHandled)) html += `<div class="alert-detail-field"><span class="inspector-label">Actor Handles</span><span class="inspector-value" style="color: var(--danger);">${alert.actorRiskCodesHandled.join(", ")}</span></div>`;
        if (alert.vehicleId) html += `<div class="alert-detail-field"><span class="inspector-label">Vehicle</span><span class="inspector-value" style="color: var(--danger);">${alert.vehicleId}</span></div>`;
        if (alert.capacityKg !== undefined) html += `<div class="alert-detail-field"><span class="inspector-label">Vehicle Capacity</span><span class="inspector-value">${alert.capacityKg} kg</span></div>`;
        if (alert.currentLoadKg !== undefined) html += `<div class="alert-detail-field"><span class="inspector-label">Current Load</span><span class="inspector-value">${alert.currentLoadKg} kg</span></div>`;
        if (alert.packageWeightKg !== undefined) html += `<div class="alert-detail-field"><span class="inspector-label">Package Weight</span><span class="inspector-value">${alert.packageWeightKg} kg</span></div>`;
        if (alert.attemptedLoadKg !== undefined) html += `<div class="alert-detail-field"><span class="inspector-label">Attempted Load</span><span class="inspector-value" style="color: var(--danger);">${alert.attemptedLoadKg} kg</span></div>`;

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

        if (alert.userRole) html += `<div class="alert-detail-field"><span class="inspector-label">User Role</span><span class="inspector-value" style="color: var(--danger);">${alert.userRole}</span></div>`;
        if (alert.requiredRole) html += `<div class="alert-detail-field"><span class="inspector-label">Required Role</span><span class="inspector-value" style="color: var(--primary);">${alert.requiredRole}</span></div>`;

        html += `</div>`; // close grid
        html += `<div style="text-align: right; margin-top: 0.75rem;">
            <button class="btn danger-dismiss" onclick="dismissAlert('${alert.id}')">🗑 Dismiss Alert</button>
        </div>`;
        html += `</div>`; // close card
    });

    panel.innerHTML = html;

    const gridCard = document.getElementById("alerts-grid");
    if (gridCard && gridCard.closest(".card")) {
        gridCard.closest(".card").insertAdjacentElement("afterend", panel);
    } else {
        const main = document.querySelector("main");
        if (main) main.appendChild(panel);
    }

    panel.scrollIntoView({ behavior: 'smooth', block: 'start' });

    document.getElementById("btn-close-alert-panel").addEventListener("click", () => {
        panel.remove();
    });
}

function renderAlerts(alerts, container) {
    const monitors = {
        time: { id: "monitor-time", count: 0 },
        sequence: { id: "monitor-sequence", count: 0 },
        risk: { id: "monitor-risk", count: 0 },
        integrity: { id: "monitor-integrity", count: 0 },
        duplicate: { id: "monitor-duplicate", count: 0 },
        capacity: { id: "monitor-capacity", count: 0 },
        auth: { id: "monitor-auth", count: 0 }
    };

    _categorizedAlerts = { time: [], sequence: [], risk: [], integrity: [], duplicate: [], capacity: [], auth: [] };

    Object.values(monitors).forEach(m => {
        const el = document.getElementById(m.id);
        if (el) {
            el.style.display = "none";
            el.classList.remove("alert-active");
            el.style.cursor = "default";
            const statusEl = el.querySelector(".monitor-status");
            if (statusEl) statusEl.innerText = "";
        }
    });

    let healthyMsg = document.getElementById("system-healthy-msg");
    if (!healthyMsg) {
        const grid = document.getElementById("alerts-grid");
        if (grid) {
            healthyMsg = document.createElement("div");
            healthyMsg.id = "system-healthy-msg";
            healthyMsg.className = "log-entry success";
            healthyMsg.style.gridColumn = "1 / -1";
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

    alerts.forEach(alert => {
        const cat = categorizeAlert(alert);
        monitors[cat].count++;
        _categorizedAlerts[cat].push(alert);
    });

    let hasActive = false;
    Object.entries(monitors).forEach(([cat, m]) => {
        if (m.count > 0) {
            hasActive = true;
            const el = document.getElementById(m.id);
            if (el) {
                el.style.display = "flex";
                el.classList.add("alert-active");
                el.style.cursor = "pointer";

                const statusEl = el.querySelector(".monitor-status");
                statusEl.innerText = `${m.count} ERRORS`;
                statusEl.style.color = "#ff0000";

                el.querySelector(".monitor-desc").innerText = "Click to view details";

                const newEl = el.cloneNode(true);
                el.parentNode.replaceChild(newEl, el);
                newEl.addEventListener("click", () => {
                    renderAlertDetailPanel(cat, _categorizedAlerts[cat]);
                });
            }
        }
    });

    if (healthyMsg) {
        healthyMsg.style.display = hasActive ? "none" : "block";
    }

    const logConsole = document.getElementById("log-console");
    if (logConsole) {
        logConsole.innerHTML = `<div class="log-entry system">Scan complete. ${alerts.length} active signals.</div>`;
        alerts.forEach(a => {
            logConsole.innerHTML += `<div class="log-entry error">[${a.id}] ${a.type || 'Alert'}: ${a.msg || a.description}</div>`;
        });
    }
}

// ====================================================================
// Alert Polling (solo su alerts.html)
// ====================================================================
if (window.location.pathname.endsWith("alerts.html")) {
    fetchAlerts();
    setInterval(() => { fetchAlerts(); }, 10000);
}

addSafeEventListener("btn-refresh-alerts", "click", () => { fetchAlerts(); });
