// --- Inspector: Package search & display ---
// Pura UI — il filtraggio avviene server-side tramite GET /api/query/packages

function renderPackageCard(pkgData, container) {
    let html = '<div class="inspector-grid" style="margin-bottom: 1rem; border-bottom: 1px solid var(--border); padding-bottom: 1rem;">';

    if (pkgData.packageId) {
        html += `
            <div class="inspector-item" style="border-bottom: 1px dashed var(--border); padding-bottom:5px; margin-bottom:5px; width:100%;">
                <span class="inspector-label">Package ID</span>
                <span class="inspector-value" style="font-weight:bold; color:var(--primary);">${pkgData.packageId}</span>
            </div>
        `;
    }

    for (const [key, value] of Object.entries(pkgData)) {
        if (key === "packageId") continue;

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

function renderInspectorResults(packages, resultDiv) {
    resultDiv.innerHTML = '';
    if (packages.length === 0) {
        resultDiv.innerHTML = '<div class="log-entry error">No packages found matching criteria.</div>';
    } else {
        packages.forEach(pkg => renderPackageCard(pkg, resultDiv));
    }
}

// ====================================================================
// Inspector Search Button
// ====================================================================
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

    if (!resultDiv) return;

    resultDiv.classList.add("hidden");
    resultDiv.innerHTML = '<div class="log-entry info">Searching...</div>';
    resultDiv.classList.remove("hidden");

    const pkgId = pkgIdInput ? pkgIdInput.value.trim() : "";

    try {
        let packages;

        if (pkgId) {
            // Ricerca per ID singolo
            log(`Searching by ID: ${pkgId}`, "info");
            const pkg = await API.getPackage(pkgId);
            packages = pkg ? [pkg] : [];
        } else {
            // Ricerca con filtri server-side
            log("Scanning packages with filters...", "info");
            const filters = {};
            if (creatorInput && creatorInput.value.trim()) filters.creator = creatorInput.value.trim();
            if (custodianInput && custodianInput.value.trim()) filters.custodian = custodianInput.value.trim();
            if (statusInput && statusInput.value) filters.status = statusInput.value;
            if (wasteInput && wasteInput.value.trim()) filters.waste = wasteInput.value.trim();
            if (riskInput && riskInput.value.trim()) filters.risk = riskInput.value.trim();
            if (weightMinInput && weightMinInput.value) filters.weightMin = weightMinInput.value;
            if (weightMaxInput && weightMaxInput.value) filters.weightMax = weightMaxInput.value;
            if (transferInput && transferInput.value.trim()) filters.transfer = transferInput.value.trim();

            packages = await API.getPackages(filters);
        }

        renderInspectorResults(packages, resultDiv);
        log(`Found ${packages.length} package(s).`, packages.length > 0 ? "success" : "warning");
    } catch (err) {
        resultDiv.innerHTML = `<div class="log-entry error">Search failed: ${err.message}</div>`;
        log(`Inspector error: ${err.message}`, "error");
    }
});
