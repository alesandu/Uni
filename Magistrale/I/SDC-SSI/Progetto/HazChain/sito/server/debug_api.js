const axios = require('axios');

async function debugGet() {
    const packageId = "PKG-2026-0001"; // Assuming this exists from previous runs
    console.log(`Testing GetKV for ${packageId}...`);

    try {
        const payload = {
            cmd: "GetKV",
            class: "PACKAGE",
            key: ["PKG", packageId]
        };
        console.log("Payload:", JSON.stringify(payload, null, 2));

        const response = await axios.post("http://localhost:9999/api", payload);
        console.log("Response Status:", response.status);
        console.log("Response Data (Full):", JSON.stringify(response.data, null, 2));

        if (response.data.value) {
            console.log("Found .value property");
        } else {
            console.log("NO .value property found");
        }

    } catch (e) {
        console.error("Request failed:", e.message);
        if (e.response) {
            console.log("Error Response:", e.response.data);
        }
    }
}

debugGet();
