if [ -z "$1" ]; then
    echo usage $0 "{irc, apache, as}"
    exit
fi
if [ -z "$2" ]; then
    out=0
else
    out=$2
fi
exedir="../../dist/build"
datadir="../../test/data"
mkdir -p build
mkdir -p output
case $1 in
    irc)
        csv="build/irc.csv"
        $exedir/kexc/kexc simul --out $csv src/irc.kex
        time $exedir/sim/sim $csv < $datadir/irc/irc_250mb.txt > output/$1-${out}.txt
        ;;
    apache)
        csv="build/apache.csv"
        $exedir/kexc/kexc simul --out build/apache.csv src/apache_log.kex
        time $exedir/sim/sim $csv < $datadir/apache_log/example_big.log > output/$1-${out}.txt
        ;;
    as)
        csv="build/as.csv"
        $exedir/kexc/kexc simul --out build/as.csv src/as.kex
        time $exedir/sim/sim $csv < $datadir/strings/as_200mb.txt > output/$1-${out}.txt
        ;;
    dna)
        csv="build/dna.csv"
        $exedir/kexc/kexc simul --out $csv src/dna_regex.kex
        time $exedir/sim/sim $csv < $datadir/dna/fasta.txt > output/$1-${out}.txt
        ;;
    approx)
        csv="build/approx.csv"
        $exedir/kexc/kexc simul --out $csv approx.kex
        time $exedir/sim/sim $csv < data_bigger.txt > output/$1-${out}.txt
        ;;
    approx-counters)
        csv="build/approx-counters.csv"
        $exedir/kexc/kexc simul --counters --out $csv approx.kex
        time $exedir/sim/sim $csv < data.txt > output/$1-${out}.txt
        ;;
    approx-data)
        rm data_big data_bigger
        for i in {1..200}; do cat data.txt >> data_big.txt ; done
        for i in {1..100}; do cat data_big.txt >> data_bigger.txt ; done
        ;;
    *)
        echo Unkown usage
        exit 1
esac

