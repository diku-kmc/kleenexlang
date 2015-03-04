#! env bash

# sed version of the patho2 program

# If we are on OSX, the GNU sed is called gsed if installed with
# Macports.
if [[ $(uname) == "Linux" ]] ; then
    sed="sed"
else
    sed="gsed"
fi

# - We have converted the POSIX non-capturing groups (?: .. ) to normal parens.
# - We have added the trailing "|^$" in order to also match and output the empty
#   lines, as the semantics of the "patho2" program dictates.
regex="^([a-z]*a)|([a-z]*b)$|^$"


cmd="${sed} -r -n s/${regex}/\2/p"

$cmd
