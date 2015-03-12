#!/bin/bash

red="\033[0;31m"
green='\033[0;32m'
NC='\033[0m' # No Color


if [ -f $3 ] ; then
	printf "$1"
	$6 $3 < $4 > $5 2> tmp_timing
	ms=$(cat tmp_timing | utils/extracttime 2> /dev/null)
	shasum -c $2.sha &> /dev/null && echo -e "${green}OK${NC} (${ms}ms)" || echo -e "${red}FAIL${NC}"
else
	echo "Can't find $3.  Skipping."
fi

