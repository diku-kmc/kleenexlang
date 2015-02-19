#! /opt/local/bin/bash
# Measure run time
# Run after compiletime.sh, so the binaries are built.


declare -A inputdata=(
    ["simple_id.has"]="issuu/sample.json" #";issuu/medium.json"
    ["id_ints.has"]="ints.txt"
    ["patho1.has"]="abs_8.txt;abs9.txt"
    ["flip_ab.has"]="abs_8.txt;abs9.txt"
    ["issuu_fallback.has"]="issuu/big.json"
    ["issuu_nofallback.has"]="issuu/big.json"
    ["csv2json.has"]="csv/csv_format1.csv"
    ["csv2json_nows.has"]="csv/csv_format1.csv"
    ["json2csv.has"]="csv/csv_format1.csv.ws.json;csv/csv_format1.csv.nows.json"
)


RUNTIME_POSTFIX=".runningtime"
DATA_DIR="../test/data/"
BIN_DIR="bin/"
OUT_DIR="runningtimes/"
REPS=2
PRIMNAME=""

# Set PRIMNAME to first component its argument
function setprimname {
    PRIMNAME=$(echo $1 | sed 's/^\(.*\)__\(.*\)__\(.*\)/\1/')
}

function areyousure {
    while true; do
        read -p "This will clear old data in $OUT_DIR.  Proceed? " yn
        case $yn in
            [Yy]* ) break;;
            [Nn]* ) exit;;
            * ) echo "Please answer yes or no.";;
        esac
    done
}

cleardata=false
while getopts ":c" opt; do
  case $opt in
  c)
      cleardata=true
      areyousure
      ;;
  \?)
      echo "Invalid option: -$OPTARG" >&2
      exit
      ;;
  esac
done


for bin in $(ls $BIN_DIR); do
    setprimname $bin
    IFS=';' read -a inputs <<< ${inputdata[$PRIMNAME]}
    for input in ${inputs[@]}; do
        outfile=$OUT_DIR$bin${RUNTIME_POSTFIX}
        if [ "$cleardata" = true ]; then
            cat /dev/null > $outfile
        fi
        for i in `seq 1 $REPS`; do
            CMD="$BIN_DIR$bin -t < ${DATA_DIR}${input} > /dev/null 2>> $outfile"
            echo $i
            echo $CMD
            eval "$CMD"
            echo ""
        done
    done
done
