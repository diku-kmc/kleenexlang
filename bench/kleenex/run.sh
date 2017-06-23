if [ -z "$1" ]; then
    echo usage $0 "{irc, apache, as}"
    exit
fi
if [ -z "$2" ]; then
    out="a"
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
        time $exedir/sim/sim $csv < $datadir/irc/irc_250mb.txt > output/$1-${2}.txt
        ;;
    apache)
        csv="build/apache.csv"
        $exedir/kexc/kexc simul --out build/apache.csv src/apache_log.kex
        time $exedir/sim/sim $csv < $datadir/apache_log/example_big.log > output/$1-${2}.txt
        ;;
    as)
        csv="build/as.csv"
        $exedir/kexc/kexc simul --out build/as.csv src/as.kex
        time $exedir/sim/sim $csv < $datadir/strings/as_200mb.txt > output/$1-${2}.txt
        ;;
    dna)
        csv="build/dna.csv"
        $exedir/kexc/kexc simul --out $csv src/dna_regex.kex
        time $exedir/sim/sim $csv < $datadir/dna/fasta.txt > output/$1-${2}.txt
        ;;
    *)
        echo Unkown usage
        exit 1
esac

