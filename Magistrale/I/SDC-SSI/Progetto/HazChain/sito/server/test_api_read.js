const axios = require('axios');

async function testGet() {
    const url = "http://localhost:9999/api";
    // Try different command names for reading
    const commands = ["GetKV", "ReadKV", "Query", "query", "get"];

    for (const cmd of commands) {
        try {
            console.log(`Testing cmd: ${cmd}...`);
            const payload = {
                "cmd": cmd,
                "key": ["ACT", "LAB-01"] // Should exist if seeded
            };
            const res = await axios.post(url, payload);
            console.log(`Success with ${cmd}:`, JSON.stringify(res.data));
            return; // Found it
        } catch (e) {
            console.log(`Failed with ${cmd}:`, e.message);
            if (e.response) {
                console.log(`Status: ${e.response.status}, Data:`, e.response.data);
            }
        }
    }
}

testGet();
