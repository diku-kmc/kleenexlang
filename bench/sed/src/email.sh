#! env bash

# sed version of the email validator

# If we are on OSX, the GNU sed is called gsed if installed with
# Macports.
if [[ $(uname) == "Linux" ]] ; then
    sed="sed"
else
    sed="gsed"
fi

# Here we must escape all the backtick ` so bash won't try to interpret them as
# commands.
# We have also converted the POSIX non-capturing groups (?: .. ) to normal parens.
regex="[a-z0-9!#$%&'*+/=?^_\`{|}~-]+(\.[a-z0-9!#$%&'*+/=?^_\`{|}~-]+)*@([a-z0-9]([a-z0-9-]*[a-z0-9])?\.)+[a-z0-9]([a-z0-9-]*[a-z0-9])?"


cmd="${sed} -r /^${regex}\$/!d"
$cmd


