#! /opt/local/bin/bash
# Measure run time
# Run after compiletime.sh, so the binaries are built.


declare -A inputdata=(
    ["simple_id.has"]="issuu/sample.json" #";issuu/medium.json"
    ["id_ints.has"]="ints.txt"
    ["patho1.has"]="ab_strings/ab1.txt"
    ["flip_ab.has"]="ab_strings/ab1.txt"
    ["issuu_fallback.has"]="issuu/big.json"
    ["issuu_nofallback.has"]="issuu/big.json"
    ["csv2json.has"]="csv/csv_format1.csv"
    ["csv2json_nows.has"]="csv/csv_format1.csv"
    ["json2csv.has"]="csv/csv_format1.csv.ws.json;csv/csv_format1.csv.nows.json"
    ["as.has"]="a_strings/as.txt"
)


RUNTIME_POSTFIX=".runningtime"
DATA_DIR="../test/data/"
BIN_DIR="bin/"
OUT_DIR="runningtimes/"
REPS=1 # number of repetitions of each run
PRIMNAME=""


# Set PRIMNAME to first component its argument
function setprimname {
    PRIMNAME=$(echo $1 | sed 's/^\(.*\)__\(.*\)__\(.*\)/\1/')
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
      areyousure "This will clear old data in $OUT_DIR.  Proceed? "
      ;;
  p)
      prefix=$OPTARG
      ;;
  o)
      echo "# Only doing $OPTARG"
      only_do=$OPTARG
      ;;
  n)      
      areyousure "This will delete anything in $OUT_DIR prefixed by $OPTARG.  Proceed? "
      rm $OUT_DIR$OPTARG*
      exit
      ;;
  d)
      dryrun=true
      echo "# Making a dry-run..."
      ;;
  \?)
      echo "Invalid option: -$OPTARG" >&2
      exit
      ;;
  esac
done


for bin in $(ls $BIN_DIR); do
    setprimname $bin
    if [ $only_do != "" ]; then
        if [ $only_do != $PRIMNAME ]; then
            continue;
        fi
    fi
    IFS=';' read -a inputs <<< ${inputdata[$PRIMNAME]}
    for input in ${inputs[@]}; do
        pf=$(echo $input | sed 's/\//_/g')
        outfile=$OUT_DIR$prefix$bin-$pf${RUNTIME_POSTFIX}
        if [ "$cleardata" = true ]; then
            cat /dev/null > $outfile
        fi
        for i in `seq 1 $REPS`; do
            CMD="$BIN_DIR$bin -t < ${DATA_DIR}${input} > /dev/null 2>> $outfile"
            echo "#$i"
            echo $CMD
            if [ "$dryrun" = false ]; then
                eval "$CMD"
            fi
            echo ""
        done
    done
done
