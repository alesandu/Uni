// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "forge-std/Script.sol";
import "../src/SkillWager.sol";

contract DeploySkillWager is Script {
    function run() external {
        uint256 deployerPrivateKey = vm.envUint("PRIVATE_KEY");
        
        vm.startBroadcast(deployerPrivateKey);

        SkillWager skillWager = new SkillWager();
        
        console.log("SkillWager deployed to:", address(skillWager));
        
        // Add deployer as mock juror
        skillWager.addMockJuror(vm.addr(deployerPrivateKey));
        console.log("Added deployer as mock juror");

        vm.stopBroadcast();
    }
}
