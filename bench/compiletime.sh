#! /bin/bash
# Measure compile times.

repgc=../dist/build/repg/repg # Location of our compiler

opt_levels=(0 3)
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

dryrun=true
only_do=""

while getopts ":fo:t:" opt; do
  case $opt in
  o)
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
  \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done

# Read in the compiler config file "compilers.txt" and set the names
set_compiler_names
mkdir -p $time_dir
mkdir -p $bin_dir

for opt_level in ${opt_levels[@]}; do # for each SST optimization level
    for i in $(seq 0 $(expr ${#ccs[@]} - 1)); do # for each C compiler available
        cc=${ccs[i]}
        cc_name=${ccs_names[i]}
        for n in $(ls $src_dir); do         # for each Kleenex source file
            if [[ ${n} != *".kex" ]]; then  #
                continue
            fi
            if [ "$only_do" != "" ]; then
                if [ $n != $only_do ]; then
                    continue;
                fi
            fi
            setname $n $opt_level $cc_name
            timingdata="${time_dir}/${name}${compiletime_postfix}"
            if [ "$cleardata" = true ]; then
                cat /dev/null > $timingdata
            fi
            for i in `seq 1 $reps`; do
                binary="${bin_dir}/${name}"
                precmd="$repgc compile ${src_dir}/$n --out $binary --opt $opt_level --cc $cc >> $timingdata"
                if [ "$timeoutcmd" == "" ]; then
                    cmd=$precmd
                else
                    cmd="$timeoutcmd $timeoutseconds $precmd"
                fi
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
