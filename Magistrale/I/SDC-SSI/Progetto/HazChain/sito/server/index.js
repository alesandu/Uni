const express = require("express")
const fs = require("fs");
const path = require("path");
const cors = require("cors");
const axios = require("axios");

const app = express()

app.use(express.json());
app.use(cors());
app.use(express.static(path.join(__dirname, "../public")));


// Helper to get current package state
async function getPackage(packageId) {
    try {
        console.log(`[DEBUG] Fetching package state for ${packageId}...`);

        // Use GetKV with the required class parameter
        try {
            const payload = {
                cmd: "GetKV",
                class: "PACKAGE", // Added as required by API
                key: ["PKG", packageId]
            };
            console.log("[DEBUG] Sending GetKV:", JSON.stringify(payload));

            const response = await axios.post("http://localhost:9999/api", payload);
            console.log(`[DEBUG] GetKV response status: ${response.status}`);

            const data = response.data;

            // CORRECT STRATEGY based on logs: data.answer.value
            if (data && data.answer && data.answer.value) {
                console.log("[DEBUG] Found data.answer.value");
                return data.answer.value;
            }

            // Fallbacks
            if (data && data.value) {
                console.log("[DEBUG] Found data.value");
                return data.value;
            }

            console.log("[DEBUG] Could not extract Package attributes from response:", JSON.stringify(data));

        } catch (e) {
            console.log(`[DEBUG] GetKV failed: ${e.message}`);
            if (e.response) console.log(`[DEBUG] Server responded:`, e.response.data);
        }

        return null;

    } catch (error) {
        console.error(`[DEBUG] Failed to fetch package ${packageId}:`, error.message);
        return null;
    }
}

app.post("/api/proxy", async (req, res) => {
    try {
        const payload = req.body;
        console.log(`[DEBUG] Proxying request: ${payload.cmd} ${payload.class}`);

        // 1. Execute the original request (The Transfer)
        const response = await axios.post("http://localhost:9999/api", payload);

        // 2. Check if it's a TRANSFER operation that needs a Package update
        if (response.status === 200 && payload.cmd === "AddKV" && payload.class === "TRANSFER") {
            console.log("Intercepted TRANSFER. Attempting automatic Package update...");

            // Extract IDs from Key: ["TR", "PKG-ID", "TRANSFER-ID"]
            const keyParts = payload.key;
            console.log(`[DEBUG] Key Parts:`, keyParts);

            if (Array.isArray(keyParts) && keyParts.length >= 3) {
                const packageId = keyParts[1];
                const transferId = keyParts[2];
                const transferData = payload.value;

                // 3. Get current Package data
                const pkgData = await getPackage(packageId);

                // If we get attributes
                if (pkgData) {
                    console.log(`[DEBUG] Current Package State:`, pkgData);

                    // 4. Determine new Current Custodian AND Package State
                    let newCustodian = pkgData.currentCustodian;
                    let newState = pkgData.state;

                    // Status check
                    const status = transferData.status;

                    if (status === "WAITING") {
                        newCustodian = transferData.from;
                        // State generally remains IN_LAB or IN_HUB (wherever it is waiting)
                        // If we strictly follow "from": 
                        // If from LAB -> IN_LAB presumably already.
                        // If from HUB -> IN_HUB presumably already.
                    } else if (status === "SHIPPING") {
                        newCustodian = transferData.from; // Transporter

                        // Infer Transport State
                        // Logic: Lab -> Hub (F_TRANSPORT), Hub -> Landfill (C_TRANSPORT)
                        if (transferData.to.includes("HUB")) {
                            newState = "F_TRANSPORT";
                        } else if (transferData.to.includes("LANDFILL")) {
                            newState = "C_TRANSPORT";
                        }
                    } else if (status === "COMPLETED" || status === "DELIVERED") {
                        newCustodian = transferData.to;

                        // Infer Location State
                        if (transferData.to.includes("HUB")) {
                            newState = "IN_HUB";
                        } else if (transferData.to.includes("LANDFILL")) {
                            newState = "IN_LANDFILL";
                        }
                    }

                    console.log(`[DEBUG] Updating Custodian: ${pkgData.currentCustodian} -> ${newCustodian}`);
                    console.log(`[DEBUG] Updating State: ${pkgData.state} -> ${newState}`);

                    // 5. Construct new Package Payload
                    const newPkgValue = {
                        ...pkgData, // Copy all existing
                        currentCustodian: newCustodian,
                        state: newState, // Update state
                        lastTransferId: transferId,
                        lastUpdateTs: new Date().toISOString()
                    };

                    const updatePayload = {
                        cmd: "AddKV",
                        class: "PACKAGE",
                        key: ["PKG", packageId],
                        value: newPkgValue
                    };

                    console.log("Auto-updating Package:", JSON.stringify(updatePayload, null, 2));

                    // 6. Send the update
                    try {
                        await axios.post("http://localhost:9999/api", updatePayload);
                        console.log("Package auto-update successful.");
                    } catch (updateErr) {
                        console.error("Package auto-update failed:", updateErr.message);
                    }
                } else {
                    console.log("Could not retrieve current package data. Skipping update.");
                }
            } else {
                console.log("[DEBUG] Key doesn't match expected format ['TR', 'PKG-ID', 'T-ID']");
            }
        }

        res.status(response.status).json(response.data);
    } catch (error) {
        console.error("Proxy error:", error.message);
        if (error.response) {
            res.status(error.response.status).json(error.response.data);
        } else {
            res.status(500).json({ error: error.message });
        }
    }
});


app.listen(3000, (err) => {
    if (err) {
        console.error(err)
    }
    else {
        console.log("Server started on port 3000")
    }
});