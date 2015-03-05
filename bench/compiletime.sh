#! /bin/bash
# Measure compile times.

repgc=../dist/build/repg/repg # Location of our compiler
has_dir=hased_src/
opt_levels=(3) # (1 2 3)
ccs=(gcc gcc-mp-4.9)
reps=1
bin_dir="bin/"
out_dir="compiletimes/"
compiletime_postfix=".compiletime"
name=""
skip="issuu"
timeoutseconds=30
timeoutcmd="gtimeout"

if [ -n $(hash gcc-mp-4.9) ]; then # Linux stuff
    ccs=gcc
    timeoutcmd="timeout"
fi

# Args: file name, opt level, C compiler
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

prefix=""
cleardata=false
dryrun=false
only_do=""
while getopts ":dp:cn:o:" opt; do
  case $opt in
  c)
      cleardata=true
      areyousure "This will clear old data in $out_dir.  Proceed? "
      ;;
  p)
      prefix=$OPTARG
      ;;
  o)
      echo "# Only doing $OPTARG"
      only_do=$OPTARG
      ;;
  d)
      dryrun=true
      echo "# Making a dry-run..."
      ;;
  n)
      areyousure "This will delete anything in $bin_dir and $out_dir prefixed by $OPTARG.  Proceed? "
      rm $bin_dir$OPTARG*
      rm $bin_dir$OPTARG*
      exit
      ;;
  \?)
      echo "Invalid option: -$OPTARG" >&2
      exit
      ;;
  esac
done

for opt_level in ${opt_levels[@]}; do # for each SST optimization level
    for cc in ${ccs[@]}; do # for each C compiler available
        for n in $(ls $has_dir); do         # for each hased source file
            if [[ ${n} != *".has" ]]; then  #
                continue
            fi
            if [ "$only_do" != "" ]; then
                if [ $n != $only_do ]; then
                    continue;
                fi
            fi
            setname $n $opt_level $cc
            timingdata=$out_dir$prefix$name$compiletime_postfix
            if [ "$cleardata" = true ]; then
                cat /dev/null > $timingdata
            fi
            for i in `seq 1 $reps`; do
                binary=$bin_dir$prefix$name
                precmd="$repgc compile $has_dir$n --out $binary --opt $opt_level --cc $cc >> $timingdata"
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
