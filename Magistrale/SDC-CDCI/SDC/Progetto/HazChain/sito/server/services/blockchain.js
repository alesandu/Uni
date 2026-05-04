// --- Wrapper centralizzato per Hyperledger Fabric (porta 9999) ---
const axios = require("axios");

const FABRIC_URL = "http://localhost:9999/api";

/**
 * Esegue una chiamata generica al chaincode Hyperledger.
 * @param {Object} payload - Il payload JSON da inviare.
 * @returns {Object|null} - La risposta del chaincode o null in caso di errore.
 */
async function fabricCall(payload) {
    const response = await axios.post(FABRIC_URL, payload);
    return response.data;
}

/**
 * AddKV — Aggiunge o aggiorna una coppia chiave/valore.
 */
async function addKV(className, key, value) {
    return fabricCall({ cmd: "AddKV", class: className, key, value });
}

/**
 * GetKV — Recupera il valore di una chiave.
 * @returns {Object|null} Il valore associato alla chiave, o null se non trovato.
 */
async function getKV(className, key) {
    const data = await fabricCall({ cmd: "GetKV", class: className, key });
    if (data && data.answer && data.answer.value) {
        return data.answer.value;
    }
    // fallback
    if (data && data.value) {
        return data.value;
    }
    return null;
}

/**
 * GetKeys — Recupera tutte le chiavi di una classe.
 * @returns {Array} Array di chiavi, es. [["PKG","PKG-2026-0001"], ...]
 */
async function getKeys(className, key) {
    const data = await fabricCall({ cmd: "GetKeys", class: className, key });
    if (data && data.answer && data.answer.keys) {
        return data.answer.keys;
    }
    return [];
}

/**
 * DelKV — Cancella una coppia chiave/valore.
 */
async function delKV(className, key) {
    return fabricCall({ cmd: "DelKV", class: className, key });
}

/**
 * GetKeyHistory — Recupera lo storico di una chiave.
 * @returns {Array} Array di entry storiche.
 */
async function getKeyHistory(className, key) {
    const data = await fabricCall({ cmd: "GetKeyHistory", class: className, key });
    if (data && data.success && Array.isArray(data.answer)) {
        return data.answer;
    }
    return [];
}

// --- Helper di alto livello ---

/** Recupera un package per ID. */
async function getPackage(packageId) {
    return getKV("PACKAGE", ["PKG", packageId]);
}

/** Recupera un transfer per ID. */
async function getTransfer(transferId) {
    return getKV("TRANSFER", ["TR", transferId]);
}

/** Recupera un attore per ID. */
async function getActor(actorId) {
    return getKV("ACTOR", ["ACT", actorId]);
}

/** Recupera lista di attori filtrata per prefisso. */
async function getActorOptions(filterStr) {
    const keys = await getKeys("ACTOR", ["ACT"]);
    return keys
        .filter(k => k[1].startsWith(filterStr))
        .map(k => k[1]);
}

/**
 * Genera il prossimo ID sequenziale.
 * @param {string} className - "PACKAGE", "TRANSFER", "ALERT"
 * @param {string} bucket - "PKG", "TR", "AL"
 * @param {string} prefix - "PKG-2026-", "TR-F-", "TR-C-", "AL-"
 * @returns {string} Il prossimo ID, es. "PKG-2026-0001"
 */
async function generateNextId(className, bucket, prefix) {
    const keys = await getKeys(className, [bucket]);
    let maxNum = 0;
    keys.forEach(k => {
        const id = k[1];
        if (id && id.startsWith(prefix)) {
            const numPart = parseInt(id.replace(prefix, ""));
            if (!isNaN(numPart) && numPart > maxNum) {
                maxNum = numPart;
            }
        }
    });
    const nextNum = maxNum + 1;
    return prefix + String(nextNum).padStart(4, '0');
}

module.exports = {
    fabricCall,
    addKV,
    getKV,
    getKeys,
    delKV,
    getKeyHistory,
    getPackage,
    getTransfer,
    getActor,
    getActorOptions,
    generateNextId
};
