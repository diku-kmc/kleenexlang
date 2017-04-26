#!/bin/bash
UPPER=8
PROG="./email"
BASE="emails_1gb"
FILE="$BASE.txt"
LOG="$BASE.log"

if [ ! -f scaletest.py ]; then
    echo "Unable to find \"scaletest.py\"!"
    exit 1
fi

if [ ! -f plottiming.py ]; then
    echo "Unable to find \"plottiming.py\"!"
    exit 1
fi

if [ ! -f $FILE ]; then
    echo "Unable to find \"$FILE\"!"
    exit 1
fi

echo "1:$UPPER\n" > ${LOG}
for i in `seq 1 ${UPPER}`
do
    printf "$i\n" >> ${LOG}
    { time -p python -O scaletest.py ${PROG} ${FILE} -c ${i} ; } 2>> ${LOG}
    echo "" >> ${LOG}
done

python -O plottiming.py ${LOG}
