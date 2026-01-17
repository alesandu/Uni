docker exec cli peer channel create -o orderer.kvdevel.com:7050 -c kvdevel -f ./channel-artifacts/kvdevel.tx --tls --cafile /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations/kvdevel.com/orderers/orderer.kvdevel.com/msp/tlscacerts/tlsca.kvdevel.com-cert.pem
sleep 5
docker exec cli peer channel join -b kvdevel.block
docker exec -e CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org2.kvdevel.com/users/Admin@org2.kvdevel.com/msp -e CORE_PEER_ADDRESS=peer0.org2.kvdevel.com:7151 -e CORE_PEER_LOCALMSPID="Org2MSP" -e CORE_PEER_TLS_ROOTCERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org2.kvdevel.com/peers/peer0.org2.kvdevel.com/tls/ca.crt cli peer channel join -b kvdevel.block

docker exec cli peer channel update -o orderer.kvdevel.com:7050 -c kvdevel -f ./channel-artifacts/Org1MSPanchors.tx --tls --cafile /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations/kvdevel.com/orderers/orderer.kvdevel.com/msp/tlscacerts/tlsca.kvdevel.com-cert.pem
docker exec -e CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org2.kvdevel.com/users/Admin@org2.kvdevel.com/msp -e CORE_PEER_ADDRESS=peer0.org2.kvdevel.com:7151 -e CORE_PEER_LOCALMSPID="Org2MSP" -e CORE_PEER_TLS_ROOTCERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org2.kvdevel.com/peers/peer0.org2.kvdevel.com/tls/ca.crt cli peer channel update -o orderer.kvdevel.com:7050 -c kvdevel -f ./channel-artifacts/Org2MSPanchors.tx --tls --cafile /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations/kvdevel.com/orderers/orderer.kvdevel.com/msp/tlscacerts/tlsca.kvdevel.com-cert.pem



