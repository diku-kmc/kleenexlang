#!/bin/bash
cd "$(dirname "$0")"

echo "Checking if DReX was already installed..."
if [ -d "$" ] ; then
    echo "DReX was already installed.  Aborting."
    exit 0
fi
echo "DReX is not installed.  Fetching and installing."

repg compile fix_dep.kex --out fix_dep
wget http://www.seas.upenn.edu/~rmukund/drex/DReX.zip
unzip DReX.zip
rm DReX.zip
cd DReX

cat drex-front/pom.xml | ../fix_dep > drex-front/pom_new.xml
mv drex-front/pom_new.xml drex-front/pom.xml

./build

cd ..
ln -s "$(pwd)/DReX/drex-front/target/appassembler/bin/drex-front"
