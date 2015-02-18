#! /bin/sh
# Measure compile times.

# Assumes a link to the binary in ../repgbin
REPGC=../repgbin
HASDIR=hased_src/
OPT_LEVELS=(1 2 3)
CCS=(gcc gcc-mp-4.9)
REPS=5
NAME=""
SKIP="issuu"
# SKIP="NONE"

# Args: file name, opt level, C compiler
function setname {
    NAME="${1}_${2}_${3}"
}

for opt_level in ${OPT_LEVELS[@]}; do
    for cc in ${CCS[@]}; do
        for n in $(ls $HASDIR); do
            if [[ ${n} !=  ${SKIP}* ]]; then
                setname $n $opt_level $cc
                cat /dev/null > $NAME.out # Make sure it's there and empty
                for i in `seq 1 $REPS`; do
                    CMD="$REPGC compile $HASDIR$n --out $NAME --opt $opt_level --cc $cc"
                    echo $i
                    echo $CMD
                    $CMD >> $NAME.out
                    echo ""
                done
            fi
        done
    done
done
