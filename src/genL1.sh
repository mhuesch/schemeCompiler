ABSOLUTE_PATH=$(cd `dirname "${BASH_SOURCE[0]}"` && pwd)

cd $ABSOLUTE_PATH
stack exec -- bnfc -m -haskell -p L1 L1/L1.cf
