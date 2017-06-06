#!/bin/bash

test_dir=src
test_approx=approx_src
if [ -f ../../dist/build/kexc/kexc ]; then
     kexc=../../dist/build/kexc/kexc
elif which kexc >/dev/null; then
     kexc="kexc"
elif which stack >/dev/null; then
     kexc="stack exec kexc --"
fi
binsimul=../../dist/build/sim/sim

echo "Using executable '${kexc}'"

getinput="sed -n 's/\/\/ IN: \(.*\)/\1/p'"
getoutput="sed -n 's/\/\/ OUT: \(.*\)/\1/p'"
failed=0
for tst in $(ls $test_dir); do
    if [[ $tst != *".kex" ]]; then  #
        continue
    fi
    compile="${kexc} simul --out ${tst}_csv $test_dir/${tst}"
    eval "$compile > /dev/null"
    input=$(sed -n 's/\/\/ IN:\(.*\)/\1/p' < $test_dir/$tst)
    exp_out=$(sed -n 's/\/\/ OUT:\(.*\)/\1/p' < $test_dir/$tst)
    out=$(echo "$input" | ${binsimul} "$tst"_csv)
    if [[ $out == $exp_out ]]; then
        echo "$tst Pass"
    else
        echo "$tst FAIL!"
        echo "  Output: $out"
        echo "  Expected: $exp_out"
        echo "  Cmd: $compile"
        ((failed++))
    fi
done

for tst in $(ls $test_approx); do
    if [[ $tst != *".kex" ]]; then  #
        continue
    fi
    compile1="${kexc} simul --out ${tst}_csv $test_approx/${tst}"
    compile2="${kexc} --counters simul --out ${tst}_counters_csv $test_approx/${tst}"
    eval "$compile1 > /dev/null"
    eval "$compile2 > /dev/null"
    input=$(sed -n 's/\/\/ IN:\(.*\)/\1/p' < $test_approx/$tst)
    exp_out=$(sed -n 's/\/\/ OUT:\(.*\)/\1/p' < $test_approx/$tst)
    out=$(echo "$input" | ${binsimul} "$tst"_csv)
    if [[ $out == $exp_out ]]; then
        echo "$tst Pass"
    else
        echo "$tst FAIL!"
        echo "  Output: $out"
        echo "  Expected: $exp_out"
        echo "  Cmd: $compile1"
        ((failed++))
    fi
    out=$(echo "$input" | ${binsimul} "$tst"_counters_csv)
    if [[ $out == $exp_out ]]; then
        echo "$tst Pass"
    else
        echo "$tst FAIL!"
        echo "  Output: $out"
        echo "  Expected: $exp_out"
        echo "  Cmd: $compile2"
        ((failed++))
    fi
done

exit $failed

