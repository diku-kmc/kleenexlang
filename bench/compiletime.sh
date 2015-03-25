#! /bin/bash
# Measure compile times.

repgc=../dist/build/repg/repg # Location of our compiler

# 'nola' is "no lookahead" and 'la' is "lookahead"
opt_levels=(0-nola 3-nola 3-la)
compiler_conf_file="${BASH_SOURCE%/*}/compilers.txt"
reps=1
src_dir="kleenex/src"
bin_dir="kleenex/bin"
time_dir="kleenex/compiletimes"
compiletime_postfix=".compiletime"
name=""
timeoutseconds=30

if [ "$(uname)" = "Linux" ]; then
    timeoutcmd="timeout"
else
    timeoutcmd="gtimeout"
fi

ccs=()
ccs_names=()
# Sets the "ccs" and "ccs_names" arrays to contain a list of compiler commands
# and a corresponding list of "pretty" compiler names.
function set_compiler_names {
    unm=$(uname)
    IFS=$'\n' ccs=($(cat ${compiler_conf_file} | awk "\$1~/${unm}/ { print \$2 }"))
    IFS=$'\n' ccs_names=($(cat ${compiler_conf_file} | awk "\$1~/${unm}/ { print \$3 }"))
}

# Args: file name, opt level, C compiler name
function setname {
    name="${1}__${2}__${3}"
}
function usage {
    echo "usage: $0 [-fh] [-p P] [-t N]"
    echo " -f: force, perform compilation instead of just printing what to do."
    echo " -h: print this usage and exit."
    echo " -p P: only compile program P."
    echo " -t N: set timeout to N seconds.  If N==0 there is no timeout."
}

dryrun=true
only_do=""

while getopts ":fhp:t:" opt; do
  case $opt in
  p)
      echo "# Only doing $OPTARG"
      only_do=$OPTARG
      ;;
  t)
      if [[ $OPTARG =~ ^[0-9]+$ ]] && (( $OPTARG >= 0 )); then
          echo "# Setting timeout value to $OPTARG seconds"
          timeoutseconds=$OPTARG
          if [ "$timeoutseconds" == 0 ]; then
              echo "# Timeout set to 0; disabling timeout."
              timeoutcmd=""
              timeoutseconds=""
          fi
      else
          echo "# Illegal argument: '$OPTARG'.  Must be an integer."
          exit 1
      fi
      ;;
  f)
      dryrun=false
      echo "# NOT making a dry-run..."
      ;;
  h)
      usage
      exit 2
      ;;
  \?)
      echo "Invalid option: -$OPTARG" >&2
      usage
      exit 1
      ;;
  esac
done


function compile_ini2json {
    echo "# Handling ini2json as a special case!"
    opt_la_level=$1
    la_on_off=$2
    cc_name=$3    
    setname "ini2json" $opt_la_level $cc_name
    timingdata1="${time_dir}/BIN1-${name}${compiletime_postfix}"
    binary1="${bin_dir}/BIN1-${name}"
    timingdata2="${time_dir}/BIN2-${name}${compiletime_postfix}"
    binary2="${bin_dir}/BIN2-${name}"
    scriptbin="${bin_dir}/${name}"    

    precmd1="$repgc compile ${src_dir}/ini2json_PART1.kex --out $binary1 $la_on_off --opt $opt_level --cc $cc >> $timingdata1"
    precmd2="$repgc compile ${src_dir}/ini2json_PART2.kex --out $binary2 $la_on_off --opt $opt_level --cc $cc >> $timingdata2"
    if [ "$timeoutcmd" == "" ]; then
        cmd1=$precmd1
        cmd2=$precmd2
    else
        cmd1="$timeoutcmd $timeoutseconds $precmd1"
        cmd2="$timeoutcmd $timeoutseconds $precmd2"
    fi
    for i in `seq 1 $reps`; do
        echo "#$i"
        echo $cmd1
        echo $cmd2
        if [ "$dryrun" = false ]; then
            eval "$cmd1"
            if [ $? == 124 ]; then
                echo "# TIMED OUT!"
            fi
            eval "$cmd2"
            if [ $? == 124 ]; then
                echo "# TIMED OUT!"
            fi
        fi
    done
    echo "# Writing script $scriptbin"
    echo '#!/bin/bash' > $scriptbin
    echo 'DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )' >> $scriptbin
    echo 'source "${BASH_SOURCE%/*}"/../timinghelper.sh' >> $scriptbin
    echo "strip_comments=\"\$DIR/BIN1-${name}\"" >> $scriptbin
    echo "tojson=\"\$DIR/BIN2-${name}\"" >> $scriptbin
    echo 'start=$(get_millisecond_time)' >> $scriptbin
    echo '$strip_comments | $tojson' >> $scriptbin
    echo 'end=$(get_millisecond_time)' >> $scriptbin
    echo 'elaps=$(expr $end - $start)' >> $scriptbin
    echo 'printf "matching (ms): %d\n" $elaps >> /dev/stderr' >> $scriptbin
    chmod +x $scriptbin
}

# Read in the compiler config file "compilers.txt" and set the names
set_compiler_names
mkdir -p $time_dir
mkdir -p $bin_dir

for opt_la_level in ${opt_levels[@]}; do # for each SST optimization level
    opt_level=${opt_la_level%-*}
    lookahead=${opt_la_level#*-}
    for i in $(seq 0 $(expr ${#ccs[@]} - 1)); do # for each C compiler available
        cc=${ccs[i]}
        cc_name=${ccs_names[i]}
        for n in $(ls $src_dir); do         # for each Kleenex source file
            if [[ ${n} != *".kex" ]]; then  #
                continue
            fi
            if [ "$only_do" != "" ]; then
                if [[ ${n} != $only_do* ]]; then
                    continue;
                fi
            fi
            if [ "$lookahead" == "la" ]; then
                la_on_off="--la=true"
            else
                la_on_off="--la=false"
            fi
            
            if [ $n == "ini2json_PART1.kex" ]; then
                # This is a special case!
                compile_ini2json $opt_la_level $la_on_off $cc_name
                continue
            fi
            if [ $n == "ini2json_PART2.kex" ]; then
                # this one is always handled by PART1
                continue
            fi
            setname $n $opt_la_level $cc_name
            
            timingdata="${time_dir}/${name}${compiletime_postfix}"
            binary="${bin_dir}/${name}"
            precmd="$repgc compile ${src_dir}/$n --out $binary $la_on_off --opt $opt_level --cc $cc >> $timingdata"
            if [ "$timeoutcmd" == "" ]; then
                cmd=$precmd
            else
                cmd="$timeoutcmd $timeoutseconds $precmd"
            fi
            for i in `seq 1 $reps`; do
                echo "#$i"
                echo $cmd
                if [ "$dryrun" = false ]; then
                    eval "$cmd"
                    if [ $? == 124 ]; then
                        echo "# TIMED OUT!"
                    fi
                fi
                echo ""
            done
        done
    done
done
