#pushd ./chaincode

#GO111MODULE=on go mod vendor

#popd

docker exec cli peer lifecycle chaincode package fabcar.tar.gz --path /opt/gopath/src/github.com/chaincode/ --label fabcar_1
docker exec -e CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.kvdevel.com/users/Admin@org1.kvdevel.com/msp -e CORE_PEER_ADDRESS=peer0.org1.kvdevel.com:7051 -e CORE_PEER_LOCALMSPID="Org1MSP" -e CORE_PEER_TLS_ROOTCERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.kvdevel.com/peers/peer0.org1.kvdevel.com/tls/ca.crt cli peer lifecycle chaincode install fabcar.tar.gz
docker exec -e CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org2.kvdevel.com/users/Admin@org2.kvdevel.com/msp -e CORE_PEER_ADDRESS=peer0.org2.kvdevel.com:7151 -e CORE_PEER_LOCALMSPID="Org2MSP" -e CORE_PEER_TLS_ROOTCERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org2.kvdevel.com/peers/peer0.org2.kvdevel.com/tls/ca.crt cli peer lifecycle chaincode install fabcar.tar.gz
export CHAINCODEID=`docker exec -e CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.kvdevel.com/users/Admin@org1.kvdevel.com/msp -e CORE_PEER_ADDRESS=peer0.org1.kvdevel.com:7051 -e CORE_PEER_LOCALMSPID="Org1MSP" -e CORE_PEER_TLS_ROOTCERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.kvdevel.com/peers/peer0.org1.kvdevel.com/tls/ca.crt cli peer lifecycle chaincode queryinstalled -O json|jq -r ".installed_chaincodes[0].package_id"`

echo Recuperato il chaincode id: $CHAINCODEID



#Esiste jq?

jq --version >/dev/null 2>/dev/null

jqok=$?

if [[ "$jqok" != "0" ]]

then

  sudo apt update & sudo apt install -y jq

fi



function GiveLatestChaincodeSequence () {

    sequence=`docker exec cli peer lifecycle chaincode queryapproved --channelID kvdevel --name fabcar -O json 2>/dev/null`

    if [ $? -eq 0 ];

    then

        echo `echo $sequence | jq -r ".sequence"`

    else

        echo 0

    fi

}

export SEQUENCE=$(GiveLatestChaincodeSequence)

SEQUENCE=$((SEQUENCE + 1))

echo "Sequence calcolato: " $SEQUENCE


docker exec -e CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.kvdevel.com/users/Admin@org1.kvdevel.com/msp -e CORE_PEER_ADDRESS=peer0.org1.kvdevel.com:7051 -e CORE_PEER_LOCALMSPID="Org1MSP" -e CORE_PEER_TLS_ROOTCERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.kvdevel.com/peers/peer0.org1.kvdevel.com/tls/ca.crt cli peer lifecycle chaincode approveformyorg --tls --cafile /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations/kvdevel.com/orderers/orderer.kvdevel.com/msp/tlscacerts/tlsca.kvdevel.com-cert.pem --channelID kvdevel --name fabcar --version 1.0 --sequence ${SEQUENCE} --waitForEvent --package-id ${CHAINCODEID}
docker exec -e CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org2.kvdevel.com/users/Admin@org2.kvdevel.com/msp -e CORE_PEER_ADDRESS=peer0.org2.kvdevel.com:7151 -e CORE_PEER_LOCALMSPID="Org2MSP" -e CORE_PEER_TLS_ROOTCERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org2.kvdevel.com/peers/peer0.org2.kvdevel.com/tls/ca.crt cli peer lifecycle chaincode approveformyorg --tls --cafile /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations/kvdevel.com/orderers/orderer.kvdevel.com/msp/tlscacerts/tlsca.kvdevel.com-cert.pem --channelID kvdevel --name fabcar --version 1.0 --sequence ${SEQUENCE} --waitForEvent --package-id ${CHAINCODEID}
docker exec cli peer lifecycle chaincode checkcommitreadiness --channelID kvdevel --name fabcar --version 1.0 --sequence ${SEQUENCE}

docker exec cli peer lifecycle chaincode commit -o orderer.kvdevel.com:7050 --tls --cafile /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations/kvdevel.com/orderers/orderer.kvdevel.com/msp/tlscacerts/tlsca.kvdevel.com-cert.pem \--peerAddresses peer0.org1.kvdevel.com:7051 --tlsRootCertFiles /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.kvdevel.com/peers/peer0.org1.kvdevel.com/tls/ca.crt \--peerAddresses peer0.org2.kvdevel.com:7151 --tlsRootCertFiles /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org2.kvdevel.com/peers/peer0.org2.kvdevel.com/tls/ca.crt \--channelID kvdevel --name fabcar --version 1.0 --sequence ${SEQUENCE}


docker exec cli peer lifecycle chaincode querycommitted --channelID kvdevel --name fabcar
