echo 'Beware, this script cleans all the blockchain data'
read -n 1 -s -r -p "Press any key to continue, CTRL-C to stop"
./host1down.sh -v --remove-orphans
./host2down.sh -v --remove-orphans
