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


invocation_cmds=()
invocation_cmds_names=()
# Get invocation commands and names
function set_invocation_names {
    prog=$2
    a=()
    b=()
    if   [ $1 == "kleenex"     ]; then
        # Many different versions!
        i=0
        for p in $(ls kleenex/bin/${prog}*); do
            a[$i]="$p -t"
            b[$i]=$(basename $p)
            i=$(expr $i + 1)
        done
    elif [ $1 == "re2"       ]; then a=("re2/bin/$prog")
    elif [ $1 == "re2j"      ]; then a=("java -jar re2j/build/jar/$prog.jar")
    elif [ $1 == "gawk"      ]; then
        i=0
        for p in $(ls gawk/src/${prog}*.awk); do
            a[$i]=$p
            b[$i]=$(basename $p)
            i=$(expr $i + 1)
        done
    elif [ $1 == "python"    ]; then a=("python python/src/$prog.py")
    elif [ $1 == "perl"      ]; then a=("perl perl/src/$prog.pl")
    elif [ $1 == "tcl"       ]; then a=("tcl/tclsh tcl/src/$prog.tcl")
    elif [ $1 == "sed"       ]; then a=("sed/src/$prog.sh")
    elif [ $1 == "grep"      ]; then a=("grep/src/$prog.sh")
    elif [ $1 == "cpp11"     ]; then a=("cpp11/bin/$prog")
    elif [ $1 == "oniguruma" ]; then a=("oniguruma/bin/$prog")
    elif [ $1 == "cat"       ]; then a=("cat/src/$prog.sh")
    elif [ $1 == "cut"       ]; then a=("cut/src/$prog.sh")
    elif [ $1 == "tr"        ]; then a=("tr/src/$prog.sh")
    elif [ $1 == "ragel"     ]; then
        i=0
        for p in $(ls ragel/bin/${prog}*); do
            a[$i]=$p
            b[$i]=$(basename $p)
            i=$(expr $i + 1)
        done
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
    tmp_file="output.tmp"
    testname=$1
    flavor=$2
    # Get names of input files.
    IFS=';' read -a inputs <<< $(cat ${input_table} | awk "\$1 ~ /${testname}/ { print \$2 }")
    # Set prog names and invocation commands.
    set_invocation_names $flavor $testname
    if [ ${#invocation_cmds[@]} == 0 ]; then
        printf "# No %s binaries for %s!  Skipping.\n" $flavor $testname
        return
    fi
    for input in ${inputs[@]}; do
        c=$(contains "${only_inputdata[@]}" $input)
        if [ "$only_inputdata" != "" ] && [ "$c" != "y" ]; then
            continue;
        fi
        for i in $(seq 0 $(expr ${#invocation_cmds[@]} - 1)); do
            inv=${invocation_cmds[i]}
            inv_name=${invocation_cmds_names[i]}
            # Stitch together the actual command to run
            pf=$(basename $input)
            out_dir="${flavor}/${time_dir}/${inv_name}"
            outfile="${out_dir}/${pf}${time_suffix}"
            _cmd="${inv} < ${data_dir}/${input} > /dev/null"
            warmup_cmd="$_cmd 2>> $tmp_file"
            cmd="$_cmd 2>> ${outfile}"
            if [ $warmup_reps -gt 0 ]; then
                for i in $(seq 1 $warmup_reps); do
                    echo "# WARMUP $i of $warmup_reps: "
                    echo $warmup_cmd
                    if [ "$dryrun" = false ]; then
                        eval $warmup_cmd
                        if [ $? != 0 ]; then
                            echo "# Hm, some sort of error occurred!"
                            echo "# Here are the last five lines of stderr output:"
                            tail -n 5 $tmp_file | while read el; do echo "#    $el"; done
                            break
                        fi
                    fi
                done
            fi
            for i in $(seq 1 $reps); do
                echo "# Run $i of $reps: "
                echo $cmd
                if [ "$dryrun" = false ]; then
                    mkdir -p "$out_dir"
                    eval $cmd
                    if [ $? != 0 ]; then
                        echo "# Hm, some sort of error occurred!"
                        echo "# Here are the last five lines of stderr output:"
                        tail -n 5 $outfile | while read el; do echo "#    $el"; done
                        break
                    fi
                fi
            done
        done
    done
    rm -f $tmp_file
}

function usage {
    echo "Usage: ${BASH_SOURCE} [-c test_case] [-p program] [-fh] [-w n] [-r n]"
    echo "  -c TC: only do the programs \"TC\" (comma-separated)"
    echo "  -i P:  only run the implementations \"P\" (comma-separated)"
    echo "  -d F:  only run programs that read input files in \"F\" (comma-separated)"
    echo "  -f:    force -- actually run the tests instead of printing what to do"
    echo "  -h:    print this message"
    echo "  -w n:  do n warm-up runs of each test (default 0)"
    echo "  -r n:  do n repetitions of each test (default 1)"
    exit 1
}

# Parse command-line parameters.
while getopts ":fhi:p:w:r:d:" opt; do
    case $opt in
        p)
            IFS=',' read -a only_cases <<< $OPTARG
            echo "# Only doing programs $only_cases"
            ;;
        i)
            IFS=',' read -a only_progs <<< $OPTARG
            echo "# Only doing implementations $only_progs"
            ;;
        d)
            IFS=',' read -a only_inputdata <<< $OPTARG
            echo "# Only doing programs that read input files $only_inputdata"
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

function contains() {
    arr=$1
    value=$2
    case "${arr}" in
        *$value*) echo "y"
                  return 0
                  ;;
    esac
    echo "n"
    return 1
}

# Run!
for test_case in $all_test_cases; do
    IFS=',' read -a progs <<< $(cat ${test_case_table} | awk "\$1 ~ /${test_case}/ { print \$2 }")
    c=$(contains "${only_cases[@]}" $test_case)
    if [ "$only_cases" != "" ] && [ "$c" == "y" ] ||
           [ "$only_cases" == "" ]; then
        for prog in $progs; do
            d=$(contains "${only_progs[@]}" $prog)
            if [ "$only_progs" != "" ] && [ "$d" == "y" ] ||
                   [ "$only_progs" == "" ] ; then
                run $test_case $prog
            fi
        done
    fi
done
