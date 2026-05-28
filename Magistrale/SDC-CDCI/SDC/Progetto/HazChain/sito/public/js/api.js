// --- Client HTTP per comunicare col backend ---
// Nessuna logica di business, solo fetch wrapper.

const API_BASE = "";

function getAuthHeaders() {
    return {
        "Content-Type": "application/json"
    };
}

async function apiCall(method, path, body = null) {
    const options = {
        method,
        headers: getAuthHeaders(),
        credentials: "include"
    };
    if (body) {
        options.body = JSON.stringify(body);
    }
    const response = await fetch(`${API_BASE}${path}`, options);
    const data = await response.json();

    if (!response.ok) {
        throw { status: response.status, message: data.error || "Request failed", data };
    }
    return data;
}

// --- Operations API ---
const API = {
    // Operazioni
    createPackage: (data) => apiCall("POST", "/api/operations/create-package", data),
    requestPickup: (data) => apiCall("POST", "/api/operations/request-pickup", data),
    pickupPhase1: (data) => apiCall("POST", "/api/operations/pickup-phase1", data),
    arriveHub: (data) => apiCall("POST", "/api/operations/arrive-hub", data),
    requestHazPickup: (data) => apiCall("POST", "/api/operations/request-haz-pickup", data),
    pickupPhase2: (data) => apiCall("POST", "/api/operations/pickup-phase2", data),
    arriveLandfill: (data) => apiCall("POST", "/api/operations/arrive-landfill", data),
    disposePackage: (data) => apiCall("POST", "/api/operations/dispose-package", data),
    createActor: (data) => apiCall("POST", "/api/operations/create-actor", data),
    dismissAlert: (data) => apiCall("POST", "/api/operations/dismiss-alert", data),
    purgeAllAlerts: () => apiCall("POST", "/api/operations/purge-all-alerts"),
    purgeAllLogs: () => apiCall("POST", "/api/operations/purge-all-logs"),

    // Query
    getPackage: (id) => apiCall("GET", `/api/query/package/${encodeURIComponent(id)}`),
    getPackages: (filters = {}) => {
        const params = new URLSearchParams();
        for (const [k, v] of Object.entries(filters)) {
            if (v) params.append(k, v);
        }
        const qs = params.toString();
        return apiCall("GET", `/api/query/packages${qs ? '?' + qs : ''}`);
    },
    getActors: (filter = "") => apiCall("GET", `/api/query/actors?filter=${encodeURIComponent(filter)}`),
    getActor: (id) => apiCall("GET", `/api/query/actor/${encodeURIComponent(id)}`),
    getAlerts: () => apiCall("GET", "/api/query/alerts"),
    getPackageHistory: (id) => apiCall("GET", `/api/query/package-history/${encodeURIComponent(id)}`),
    getTransfer: (id) => apiCall("GET", `/api/query/transfer/${encodeURIComponent(id)}`),
    getNextId: (className, bucket, prefix) => apiCall("GET", `/api/query/next-id?class=${encodeURIComponent(className)}&bucket=${encodeURIComponent(bucket)}&prefix=${encodeURIComponent(prefix)}`),

    // LOG
    getLogs: (level = "NORMAL") => apiCall("GET", `/api/query/logs?level=${encodeURIComponent(level)}`),
    getLogChain: (logId) => apiCall("GET", `/api/query/log-chain/${encodeURIComponent(logId)}`),

    // Auth
    login: (username, password) => apiCall("POST", "/api/login", { username, password }),
    register: (data) => apiCall("POST", "/api/register", data),
    logout: () => apiCall("POST", "/api/logout"),
    me: () => apiCall("GET", "/api/me"),

    // Admin (SUPERUSER only)
    createUser: (data) => apiCall("POST", "/api/admin/create-user", data),
    getUsers: () => apiCall("GET", "/api/admin/users"),
    updateUser: (data) => apiCall("PUT", "/api/admin/update-user", data),
    deleteUser: (username) => apiCall("DELETE", `/api/admin/delete-user/${encodeURIComponent(username)}`)
};
