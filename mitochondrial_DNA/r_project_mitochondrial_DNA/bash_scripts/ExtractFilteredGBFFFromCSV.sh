#!/usr/bin/env bash
# The following function extracts the entries given in the filtered.csv file to 
# separate .gbff files and save them in the ./gbff_filtered_files folder
# The following function is intended to be used in R. 
function ExtractFilteredGBFFFromCSV () {
# $1 is the path to the file filtered.csv which is the output of an R function 
# named FilterParsedGBFF()
# $2 is the path to the .gbff file which contains the entries to be extracted
  if [ ! -d "./gbff_filtered_files" ]; then 
    mkdir ./gbff_filtered_files 
  fi
  let lineNumber=1
  totalEntries=$(($(cat $1 | wc -l)-1))
  cat $1 | while read linesFromFilteredCSV; do 
#   skip header
  if (( $lineNumber > 1)); then
    echo Extracting gbbf entry: "$(($lineNumber - 1))" "of" "$totalEntries"
    accessionNumber=$(echo "$linesFromFilteredCSV" | awk 'BEGIN{FS=","}; { print $7 }')
    organism=$(echo "$linesFromFilteredCSV" | awk 'BEGIN{FS=","}; { print $4 }' | sed -e 's/ /_/g')
    outputFileName=$(echo ./gbff_filtered_files/"$organism"_"$accessionNumber".gbff)
    fromLine=$(echo "$linesFromFilteredCSV" | awk 'BEGIN{FS=","}; { print $1 }')
    toLine=$(echo $linesFromFilteredCSV | awk 'BEGIN{FS=","}; { print $2 }')
    diffLine=$(($toLine - $fromLine))
    head -n "$toLine" "$2" | tail -n "$diffLine" >> "$outputFileName"
    let "outputFileNumber++"
  fi 
  let "lineNumber++"
  done
}
ExtractFilteredGBFFFromCSV "$1" "$2"