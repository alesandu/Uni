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
 * Scrive un messaggio nella console log visuale.
 */
function log(message, type = "info") {
    const consoleDiv = document.getElementById("log-console");
    if (!consoleDiv) return;
    const entry = document.createElement("div");
    entry.className = `log-entry ${type}`;

    const time = new Date().toLocaleTimeString();
    entry.innerHTML = `<span class="time">[${time}]</span> ${message}`;

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
