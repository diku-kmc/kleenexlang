#!/bin/bash

test_dir=src
if [ -f ../../dist/build/kexc/kexc ]; then
     kexc=../../dist/build/kexc/kexc
elif which kexc >/dev/null; then
     kexc="kexc"
elif which stack >/dev/null; then
     kexc="stack exec kexc --"
fi

echo "Using executable '${kexc}'"

getinput="sed -n 's/\/\/ IN: \(.*\)/\1/p'"
getoutput="sed -n 's/\/\/ OUT: \(.*\)/\1/p'"
failed=0
for tst in $(ls $test_dir); do
    if [[ $tst != *".kex" ]]; then  #
        continue
    fi
    for simtype in lockstep sst; do
        run="${kexc} simulate --sb=false --sim=${simtype} --quiet $test_dir/${tst}"
        input=$(sed -n 's/\/\/ IN:\(.*\)/\1/p' < $test_dir/$tst)
        exp_out=$(sed -n 's/\/\/ OUT:\(.*\)/\1/p' < $test_dir/$tst)
        out=$(echo "$input" | eval "${run}")
        if [[ $out == $exp_out ]]; then
            echo "$tst-${simtype} Pass"
        else
            echo "$tst-${simtype} FAIL!"
            echo "  Sim type: ${simtype}"
            echo "  Output: $out"
            echo "  Expected: $exp_out"
            echo "  Cmd: $run"
            ((failed++))
        fi
    done
done

exit $failed
   
