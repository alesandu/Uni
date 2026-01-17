echo 'Beware, this script cleans all the blockchain data, scritps and local folder'
read -n 1 -s -r -p "Press any key to continue, CTRL-C to stop"
./host1down.sh -v --remove-orphans

sudo /bin/rm -f ./host1down.sh

sudo /bin/rm -f ./host1up.sh

sudo /bin/rm -f ./host1.yaml
./host2down.sh -v --remove-orphans

sudo /bin/rm -f ./host2down.sh

sudo /bin/rm -f ./host2up.sh

sudo /bin/rm -f ./host2.yaml
sudo /bin/rm -rf keystore wallet ##Se ricominci allora cancella anche il wallet

sudo /bin/rm -rf base channel-artifacts crypto-config scripts   ##Per pulire per benino

sudo /bin/rm -f ccp-generate.sh ccp-template.json ccp-template.yaml configtx.yaml create-and-join-channel.sh crypto-config.yaml make-cert-and-genesis.sh output.log.1

