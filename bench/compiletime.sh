#! /bin/bash
# Measure compile times.

repgc=../dist/build/repg/repg # Location of our compiler

opt_levels=(3) # (1 2 3)
compiler_conf_file="${BASH_SOURCE%/*}/compilers.txt"
reps=1
src_dir="kleenex/src"
bin_dir="kleenex/bin"
time_dir="kleenex/compiletimes"
compiletime_postfix=".compiletime"
name=""
skip="issuu"
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

cleardata=false
dryrun=true
only_do=""
while getopts ":fco:" opt; do
  case $opt in
  c)
      cleardata=true
      areyousure "This will clear old data in $out_dir.  Proceed? "
      ;;
  o)
      echo "# Only doing $OPTARG"
      only_do=$OPTARG
      ;;
  f)
      dryrun=false
      echo "# NOT making a dry-run..."
      ;;
  \?)
      echo "Invalid option: -$OPTARG" >&2
      exit
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
                cmd="$timeoutcmd $timeoutseconds $precmd"
                echo "#$i"
                echo $cmd
                if [ "$dryrun" = false ]; then
                    eval "$cmd"
                fi
                echo ""
            done
        done
    done
done
