import { ethers } from "ethers";

const RPC_URL = "https://api.avax-test.network/ext/bc/C/rpc";
const CONTRACT_ADDRESS = "0xe762d953d7564b7c8c8742e0f56bf7f329806aab";
const ABI = [
    "function owner() view returns (address)"
];

const provider = new ethers.JsonRpcProvider(RPC_URL);
const contract = new ethers.Contract(CONTRACT_ADDRESS, ABI, provider);

async function checkOwner() {
    try {
        const owner = await contract.owner();
        console.log("Contract Address:", CONTRACT_ADDRESS);
        console.log("Contract Owner:", owner);

        const key1Address = "0xC7B0644072848930965A6c81fAC190B6416c2Dd0";
        const key2Address = "0x5e347A07A5e2C4ecbfB50333933fEdc3a5582119";

        if (owner.toLowerCase() === key1Address.toLowerCase()) {
            console.log("MATCH: Owner is Key 1 (0xd752...)");
        } else if (owner.toLowerCase() === key2Address.toLowerCase()) {
            console.log("MATCH: Owner is Key 2 (0xde5d...)");
        } else {
            console.log("MISMATCH: Owner is neither!");
        }
    } catch (error) {
        console.error("Error fetching owner:", error);
    }
}

checkOwner();
