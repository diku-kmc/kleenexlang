#!/bin/bash
# Measure run time
# Run after compiletime.sh, so the binaries are built.

test_case_table="${BASH_SOURCE%/*}/benchmarks.txt"
input_table="${BASH_SOURCE%/*}/inputs.txt"
all_test_cases=$(cat ${test_case_table} | awk '$1 !~ /#/ { print $1 }')

time_suffix=".runningtime"

data_dir="../test/data"
time_dir="times"

# Default settings
warmup_reps=0   # Number of warm-up runs
reps=1          # Number of measured runs
dryrun=true     # Do a dry-run (i.e., only show what to do)
only_case=""    # If set, do /only/ this test case
only_prog=""    # If set, do /only/ this program


invocation_cmds=()
invocation_cmds_names=()
# Get invocation commands and names
function set_invocation_names {
    prog=$2
    a=()
    b=()
    if   [ $1 == "hased"     ]; then
        # Many different versions!
        i=0
        for p in $(ls hased/bin/${prog}*); do
            a[$i]="$p -t"
            b[$i]=$(basename $p)
            i=$(expr $i + 1)
        done
    elif [ $1 == "re2"       ]; then a=("re2/bin/$prog")
    elif [ $1 == "re2j"      ]; then a=("java -jar re2j/build/jar/$prog.jar")
    elif [ $1 == "gawk"      ]; then a=("gawk/src/$prog.awk")
    elif [ $1 == "python"    ]; then a=("python python/src/$prog.py")
    elif [ $1 == "perl"      ]; then a=("perl perl/src/$prog.pl")
    elif [ $1 == "tcl"       ]; then a=("tcl/src/$prog.tcl")
    elif [ $1 == "sed"       ]; then a=("sed/src/$prog.sh")
    elif [ $1 == "grep"      ]; then a=("grep/src/$prog.sh")
    elif [ $1 == "cpp11"     ]; then a=("cpp11/bin/$prog")
    elif [ $1 == "oniguruma" ]; then a=("oniguruma/bin/$prog")
    else
        echo "Could not find invocation for $1"
        exit 1
    fi
    # Now we "return" the arrays by copying them into the global vars!
    invocation_cmds=( "${a[@]}" )
    if [ ${#b[@]} -eq 0 ] ; then
        invocation_cmds_names=( $prog )
    else
        invocation_cmds_names=( "${b[@]}" )
    fi
}


function run {
    testname=$1
    flavor=$2
    # Get names of input files.
    IFS=';' read -a inputs <<< $(cat ${input_table} | awk "\$1 ~ /${testname}/ { print \$2 }")
    # Set prog names and invocation commands.
    set_invocation_names $flavor $testname
    for input in ${inputs[@]}; do
        for i in $(seq 0 $(expr ${#invocation_cmds[@]} - 1)); do
            inv=${invocation_cmds[i]}
            inv_name=${invocation_cmds_names[i]}
            # Stitch together the actual command to run
            pf=$(echo $input | sed 's/\//_/g')
            out_dir="${flavor}/${time_dir}/${inv_name}"
            mkdir -p "$out_dir"
            outfile="${out_dir}/${pf}${time_suffix}"
            _cmd="${inv} < ${data_dir}/${input} > /dev/null"
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
    done
}

function usage {
    echo "Usage: ${BASH_SOURCE} [-c test_case] [-p program] [-fh] [-w n] [-r n]"
    echo "  -c TC: only do the case \"TC\""
    echo "  -p P:  only run the program \"P\""
    echo "  -f:    force -- actually run the tests instead of printing what to do"
    echo "  -h:    print this message"
    echo "  -w n:  do n warm-up runs of each test (default 0)"
    echo "  -r n:  do n repetitions of each test (default 1)"
    exit 1
}

# Parse command-line parameters.
while getopts ":fhc:p:w:r:" opt; do
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
        h)
            usage
            ;;
        \?)
            echo "Invalid option: -$OPTARG"
            usage
            ;;
    esac
done

# Run!
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
