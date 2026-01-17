
#!/bin/bash

function one_line_pem {
    echo "`awk 'NF {sub(/\\n/, ""); printf "%s\\\\\\\n",$0;}' $1`"
}

function json_ccp {
    local PP=$(one_line_pem $4)
    local CP=$(one_line_pem $5)
    sed -e "s/\${ORG}/$1/" \
        -e "s/\${P0PORT}/$2/" \
        -e "s/\${CAPORT}/$3/" \
        -e "s#\${PEERPEM}#$PP#" \
        -e "s#\${CAPEM}#$CP#" \
        crypto-config/ccp-template.json
}
			  
function yaml_ccp {
    local PP=$(one_line_pem $4)
    local CP=$(one_line_pem $5)
    sed -e "s/\${ORG}/$1/" \
        -e "s/\${P0PORT}/$2/" \
        -e "s/\${CAPORT}/$3/" \
        -e "s#\${PEERPEM}#$PP#" \
        -e "s#\${CAPEM}#$CP#" \
        crypto-config/ccp-template.yaml | sed -e $'s/\\\\n/\\\n          /g'
}


ORG=1
P0PORT=7051
CAPORT=7054
PEERPEM=crypto-config/peerOrganizations/org1.kvdevel.com/tlsca/tlsca.org1.kvdevel.com-cert.pem
CAPEM=crypto-config/peerOrganizations/org1.kvdevel.com/ca/ca.org1.kvdevel.com-cert.pem

echo "$(json_ccp $ORG $P0PORT $CAPORT $PEERPEM $CAPEM)" > crypto-config/peerOrganizations/org1.kvdevel.com/connection-org1.json
echo "$(yaml_ccp $ORG $P0PORT $CAPORT $PEERPEM $CAPEM)" > crypto-config/peerOrganizations/org1.kvdevel.com/connection-org1.yaml


ORG=2
P0PORT=7151
CAPORT=7154
PEERPEM=crypto-config/peerOrganizations/org2.kvdevel.com/tlsca/tlsca.org2.kvdevel.com-cert.pem
CAPEM=crypto-config/peerOrganizations/org2.kvdevel.com/ca/ca.org2.kvdevel.com-cert.pem

echo "$(json_ccp $ORG $P0PORT $CAPORT $PEERPEM $CAPEM)" > crypto-config/peerOrganizations/org2.kvdevel.com/connection-org2.json
echo "$(yaml_ccp $ORG $P0PORT $CAPORT $PEERPEM $CAPEM)" > crypto-config/peerOrganizations/org2.kvdevel.com/connection-org2.yaml

