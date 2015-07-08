#! /bin/bash
# Measure compile times.

repgc=../dist/build/repg/repg # Location of our compiler

# 'nola' is "no lookahead" and 'la' is "lookahead"
opt_levels=(0-nola 0-la 3-nola 3-la)
compiler_conf_file="${BASH_SOURCE%/*}/compilers.txt"
reps=1
src_dir="kleenex/src"
bin_dir="kleenex/bin"
timestamp="$(date "+%Y_%m_%d__%k_%M_%S")"
time_base_dir="kleenex/compiletimes"
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
    if [ "${4}" = true ]; then
        dfa="__wDFA"
    else
        dfa=""
    fi
    if [ "${5}" = false ]; then
        action="__woACT"
    else
        action=""
    fi
    name="${1}__${2}__${3}${dfa}${action}"
}
function usage {
    echo "usage: $0 [-fh] [-p P] [-t N]"
    echo " -f: force, perform compilation instead of just printing what to do."
    echo " -h: print this usage and exit."
    echo " -p P: only compile program P."
    echo " -t N: set timeout to N seconds.  If N==0 there is no timeout."
    echo " -d: enable DFA optimization"
}

dryrun=true
use_dfa=false
only_do=""

while getopts ":fdhp:t:l:" opt; do
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
  d)
      use_dfa=true
      echo "# Using DFA optimization"
      ;;
  l)
      echo "# Setting label of this run to $OPTARG."
      timestamp="$OPTARG"
      ;;
  \?)
      echo "Invalid option: -$OPTARG" >&2
      usage
      exit 1
      ;;
  esac
done

time_dir="$time_base_dir/$timestamp"

# Read in the compiler config file "compilers.txt" and set the names
set_compiler_names
mkdir -p $time_dir
mkdir -p $bin_dir

if [ "$dryrun" = true ]; then
    echo "# Executing dry-run; use -f to perform an actual run."
fi

for opt_la_level in ${opt_levels[@]}; do # for each SST optimization level
    opt_level=${opt_la_level%-*}
    lookahead=${opt_la_level#*-}
    for i in $(seq 0 $(expr ${#ccs[@]} - 1)); do # for each C compiler available
        cc=${ccs[i]}
        cc_name=${ccs_names[i]}
        for act in {true,false}; do
            if [[ "$act" == "false" && "$opt_la_level" != "3-la" ]]; then # Run once without actions on the highest optimization level
                continue
            fi
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
                if [ "$use_dfa" = true ]; then
                    dfa_on_off="--dfa=true"
                else
                    dfa_on_off="--dfa=false"
                fi

                setname $n $opt_la_level $cc_name $use_dfa $act

                timingdata="${time_dir}/${name}${compiletime_postfix}"
                binary="${bin_dir}/${name}"
                precmd="$repgc compile ${src_dir}/$n --out $binary $la_on_off $dfa_on_off --opt $opt_level --cc $cc --act=$act >> $timingdata"
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
done
