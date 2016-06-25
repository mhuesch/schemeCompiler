ABSOLUTE_PATH=$(cd `dirname "${BASH_SOURCE[0]}"` && pwd)

cd $ABSOLUTE_PATH
./genL1.sh
./genL2.sh
./genL3.sh
./genL4.sh
./genL5.sh
