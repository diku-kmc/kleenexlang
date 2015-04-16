#! env bash

test_dir=src
repgc=../../dist/build/repg/repg

getinput="sed -n 's/\/\/ IN: \(.*\)/\1/p'"
getoutput="sed -n 's/\/\/ OUT: \(.*\)/\1/p'"

for tst in $(ls $test_dir); do
    if [[ $tst != *".kex" ]]; then  #
        continue
    fi
    compile="${repgc} compile --opt 0 --la=false --out ${tst}_bin $test_dir/${tst}"
    eval "$compile > /dev/null"
    input=$(sed -n 's/\/\/ IN: \(.*\)/\1/p' < $test_dir/$tst)
    exp_out=$(sed -n 's/\/\/ OUT: \(.*\)/\1/p' < $test_dir/$tst)
    out=$(echo "$input" | ./"$tst"_bin)
    if [[ $out == $exp_out ]]; then
        echo "$tst Pass"
    else
        echo "$tst FAIL!"
        echo "  Output: $out"
        echo "  Expected: $exp_out"
        echo "  Cmd: $compile"
    fi
done
