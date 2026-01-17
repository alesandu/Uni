./host1up.sh
./host2up.sh
./create-and-join-channel.sh
docker exec peer0.org1.kvdevel.com peer channel getinfo -c kvdevel
docker exec peer0.org2.kvdevel.com peer channel getinfo -c kvdevel
