/bin/rm -rf crypto-config
mkdir crypto-config
/bin/rm -rf channel-artifacts
mkdir channel-artifacts
docker run -v "$PWD":/this hyperledger/fabric-tools:2.4.9 /bin/bash -c  "cd /this; cryptogen generate --config=crypto-config.yaml --output=crypto-config"
docker run -v "$PWD":/this hyperledger/fabric-tools:2.4.9 /bin/bash -c  "cd /this; configtxgen -configPath /this/ -profile SampleMultiNodeEtcdRaft -channelID systemchannel -outputBlock ./channel-artifacts/genesis.block"
docker run -v "$PWD":/this hyperledger/fabric-tools:2.4.9 /bin/bash -c  "cd /this; configtxgen -configPath /this/ -profile TwoOrgsChannel -outputCreateChannelTx ./channel-artifacts/kvdevel.tx -channelID kvdevel"
docker run -v "$PWD":/this hyperledger/fabric-tools:2.4.9 /bin/bash -c  "cd /this; configtxgen -configPath /this/ -profile TwoOrgsChannel -outputAnchorPeersUpdate ./channel-artifacts/Org1MSPanchors.tx -channelID kvdevel -asOrg Org1MSP"
docker run -v "$PWD":/this hyperledger/fabric-tools:2.4.9 /bin/bash -c  "cd /this; configtxgen -configPath /this/ -profile TwoOrgsChannel -outputAnchorPeersUpdate ./channel-artifacts/Org2MSPanchors.tx -channelID kvdevel -asOrg Org2MSP"
sudo chown -R $USER:$USER crypto-config channel-artifacts
/bin/cp -f ccp-generate.sh  crypto-config/
/bin/cp -f ccp-template.json  crypto-config/
/bin/cp -f ccp-template.yaml  crypto-config/
chmod a+x crypto-config/ccp-generate.sh
./crypto-config/ccp-generate.sh
