// --- History: Timeline rendering per storico package ---
// Pura UI — i dati arrivano da GET /api/query/package-history/:id

async function fetchPackageHistory(packageId) {
    const timeline = document.getElementById('history-timeline');
    if (!timeline) return;

    timeline.innerHTML = '<div class="history-empty"><div class="big-icon">⏳</div>Loading blockchain history…</div>';

    const domElements = {
        resultsCard: document.getElementById('history-results-card'),
        timeline,
        titleEl: document.getElementById('history-results-title'),
        countEl: document.getElementById('history-entry-count'),
        summaryBar: document.getElementById('history-pkg-summary')
    };

    try {
        log(`Fetching history for ${packageId}...`, "info");
        const entries = await API.getPackageHistory(packageId);
        renderHistoryTimeline(packageId, entries, domElements);
        log(`History loaded: ${entries.length} entries for ${packageId}`, "success");
    } catch (err) {
        timeline.innerHTML = `<div class="history-empty"><div class="big-icon">❌</div>Error: ${err.message}</div>`;
        log(`History error: ${err.message}`, "error");
    }
}

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
