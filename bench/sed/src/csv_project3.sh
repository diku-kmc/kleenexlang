#! env bash

# (GNU) sed version of the csv_project3 program

# If we are on OSX, the GNU sed is called gsed if installed with
# Macports.
if [[ $(uname) == "Linux" ]] ; then
    sed="sed"
else
    sed="gsed"
fi

regex="([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*)"

cmd="${sed} -E -n s/${regex}/\2\x09\5/p"
$cmd
