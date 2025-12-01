import { ethers } from "ethers";

const key1 = "0xd752dd43e9bbee3a817ebee055b7681b54f20be5dceeca775a3a7e6f17a3557f";
const key2 = "0xde5d7db4e4a6e57a7166b6d32e71df230b130b469539df93ac0820f8cfda4131";

const wallet1 = new ethers.Wallet(key1);
const wallet2 = new ethers.Wallet(key2);

console.log("Key 1 (0xd752...):", wallet1.address);
console.log("Key 2 (0xde5d...):", wallet2.address);
