#!/bin/bash

test_dir=src
if [ -f ../../dist/build/kexc/kexc ]; then
    kexc=../../dist/build/kexc/kexc
else
    kexc="stack exec kexc --"
fi

getinput="sed -n 's/\/\/ IN: \(.*\)/\1/p'"
getoutput="sed -n 's/\/\/ OUT: \(.*\)/\1/p'"
failed=0
for tst in $(ls $test_dir); do
    if [[ $tst != *".kex" ]]; then  #
        continue
    fi
    run="${kexc} simulate --quiet $test_dir/${tst}"
    input=$(sed -n 's/\/\/ IN:\(.*\)/\1/p' < $test_dir/$tst)
    exp_out=$(sed -n 's/\/\/ OUT:\(.*\)/\1/p' < $test_dir/$tst)
    out=$(echo "$input" | eval "${run}")
    if [[ $out == $exp_out ]]; then
        echo "$tst Pass"
    else
        echo "$tst FAIL!"
        echo "  Output: $out"
        echo "  Expected: $exp_out"
        echo "  Cmd: $run"
        ((failed++))
    fi
done

exit $failed
   