const axios = require('axios');

async function testGetKeys() {
    try {
        const payload = {
            "cmd": "GetKeys",
            "class": "ACTOR",
            "key": ["ACT"] // Try with prefix "ACT" as used in "ACT, LAB-01"
        };
        console.log("Sending GetKeys payload:", JSON.stringify(payload));
        const response = await axios.post("http://localhost:9999/api", payload);
        console.log("Response:", JSON.stringify(response.data, null, 2));
    } catch (error) {
        console.error("Error:", error.message);
        if (error.response) console.error("Response Data:", error.response.data);
    }
}

testGetKeys();
