#!/bin/bash
# Measure run time
# Run after compiletime.sh, so the binaries are built.

test_case_table="${BASH_SOURCE%/*}/benchmarks.txt"
input_table="${BASH_SOURCE%/*}/inputs.txt"
all_test_cases=$(cat ${test_case_table} | awk '$1 !~ /#/ { print $1 }')


# Get invocation command
function invocation {
    prog=$2
    if   [ $1 == "hased"     ]; then cmd="hased/bin/$prog -t"
    elif [ $1 == "re2"       ]; then cmd="re2/bin/$prog"
    elif [ $1 == "re2j"      ]; then cmd="java -jar re2j/build/jar/$prog.jar"
    elif [ $1 == "gawk"      ]; then cmd="gawk/src/$prog.awk"
    elif [ $1 == "python"    ]; then cmd="python python/src/$prog.py"
    elif [ $1 == "perl"      ]; then cmd="perl perl/src/$prog.pl"
    elif [ $1 == "tcl"       ]; then cmd="tcl/src/$prog.tcl"
    elif [ $1 == "sed"       ]; then cmd="sed/src/$prog.sh"
    elif [ $1 == "grep"      ]; then cmd="grep/src/$prog.sh"
    elif [ $1 == "cpp11"     ]; then cmd="cpp11/bin/$prog"
    elif [ $1 == "oniguruma" ]; then cmd="oniguruma/bin/$prog"
    else
        echo "Could not find invocation for $1"
        exit 1
    fi
    printf "$cmd"
}


time_postfix=".runningtime"
data_dir="../test/data/"
time_dir="times/"
warmup_reps=0
reps=1 # number of repetitions of each run

function areyousure {
    while true; do
        read -p "$1" yn
        case $yn in
            [Yy]* ) break;;
            [Nn]* ) exit;;
            * ) echo "Please answer yes or no.";;
        esac
    done
}


prefix=""
cleardata=false
dryrun=true
only_case=""
only_prog=""

while getopts ":fc:p:w:r:" opt; do
    case $opt in
        c)
            echo "# Only doing case $OPTARG"
            only_case=$OPTARG
            ;;
        p)
            echo "# Only doing program $OPTARG"
            only_prog=$OPTARG
            ;;
        f)
            dryrun=false
            echo "# NOT making a dry-run..."
            ;;
        w)
            echo "# Doing $OPTARG warm-up runs."
            warmup_reps=$OPTARG
            ;;
        r)
            echo "# Doing $OPTARG benchmark runs."
            reps=$OPTARG
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit
            ;;
    esac
done

function run {
    testname=$1
    flavor=$2
    # Get names of input files.
    IFS=';' read -a inputs <<< $(cat ${input_table} | awk "\$1 ~ /${testname}/ { print \$2 }")
    out_dir="$flavor/$time_dir"
    mkdir -p $out_dir
    printf "# Called with %s %s\n" $testname $flavor
    for input in ${inputs[@]}; do
        pf=$(echo $input | sed 's/\//_/g')
        outfile="${out_dir}${prefix}${testname}-${pf}${time_postfix}"
        _cmd="$(invocation $flavor $testname) < ${data_dir}${input} > /dev/null"
        warmup_cmd="$_cmd 2> /dev/null"
        cmd="$_cmd 2>> ${outfile}"
        if [ $warmup_reps -gt 0 ]; then
            for i in $(seq 1 $warmup_reps); do
                echo "# WARMUP #$i: "
                echo $warmup_cmd
                if [ "$dryrun" = false ]; then eval $warmup_cmd ; fi
            done
        fi
        for i in $(seq 1 $reps); do
            echo "# Run #$i: "
            echo $cmd
            if [ "$dryrun" = false ]; then eval $cmd ; fi
        done
    done
}


for test_case in $all_test_cases; do
    IFS=',' read -a progs <<< $(cat ${test_case_table} | awk "\$1 ~ /${test_case}/ { print \$2 }")
    if [ "$only_case" != "" ] && [ "$only_case" == "$test_case" ] ||
           [ "$only_case" == "" ]; then
        for prog in $progs; do
            if [ "$only_prog" != "" ] && [ "$only_prog" == $prog ] ||
                   [ "$only_prog" == "" ] ; then
                run $test_case $prog
            fi
        done
    fi
done
