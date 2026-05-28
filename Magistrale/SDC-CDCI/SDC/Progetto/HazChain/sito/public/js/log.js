// --- Utility: Console Log + Event Binding ---

/**
 * Aggiunge un event listener solo se l'elemento esiste.
 */
function addSafeEventListener(id, event, handler) {
    const el = document.getElementById(id);
    if (el) {
        el.addEventListener(event, handler);
    }
}

/**
 * Funzione di escaping per prevenire XSS
 */
function escapeHtml(str) {
    if (str == null) return "";
    const div = document.createElement('div');
    div.textContent = String(str);
    return div.innerHTML;
}

/**
 * Scrive un messaggio nella console log visuale.
 */
function log(message, type = "info") {
    const consoleDiv = document.getElementById("log-console");
    if (!consoleDiv) return;
    const entry = document.createElement("div");
    entry.className = `log-entry ${type}`;

    const time = new Date().toLocaleTimeString();
    const safeMessage = escapeHtml(message);
    entry.innerHTML = `<span class="time">[${time}]</span> ${safeMessage}`;

    consoleDiv.appendChild(entry);
    consoleDiv.scrollTop = consoleDiv.scrollHeight;
}

// Clear log
addSafeEventListener("btn-clear-log", "click", () => {
    const consoleDiv = document.getElementById("log-console");
    if (consoleDiv) {
        consoleDiv.innerHTML = '<div class="log-entry system">System ready. Waiting for commands...</div>';
    }
});
