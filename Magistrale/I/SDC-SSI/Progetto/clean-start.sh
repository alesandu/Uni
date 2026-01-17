echo 'Beware, this script cleans all data and rewrites all'
echo 'using the configuration files starting from cgeneratore.json contents'
read -n 1 -s -r -p "Press any key to continue, CTRL-C to stop"


./host1down.sh -v --remove-orphans
./host2down.sh -v --remove-orphans
./host1down.sh -v --remove-orphans

sudo /bin/rm -f ./host[0-9]*down.sh

sudo /bin/rm -f ./host[0-9]*up.sh

sudo /bin/rm -f ./host[0-9]*.yaml
./host2down.sh -v --remove-orphans

sudo /bin/rm -f ./host[0-9]*down.sh

sudo /bin/rm -f ./host[0-9]*up.sh

sudo /bin/rm -f ./host[0-9]*.yaml
sudo /bin/rm -rf keystore wallet ##Se ricominci allora cancella anche il wallet

sudo /bin/rm -rf base channel-artifacts crypto-config scripts   ##Per pulire per benino

sudo /bin/rm -f ccp-generate.sh ccp-template.json ccp-template.yaml configtx.yaml create-and-join-channel.sh crypto-config.yaml make-cert-and-genesis.sh output.log.1
./RunMeFirst

read -n 1 -s -r -p "archive these ip numbers and press any key to continue, CTRL-C to stop"



sudo chown -R ${USER}:${USER} *
docker network create kvdevel

./make-cert-and-genesis.sh
./host1up.sh
./host2up.sh
sleep 3
./create-and-join-channel.sh
sleep 2
 docker exec peer0.org1.kvdevel.com peer channel getinfo -c kvdevel
 docker exec peer0.org2.kvdevel.com peer channel getinfo -c kvdevel
