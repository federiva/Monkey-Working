#!/usr/bin/env bash
function ExtractFastaSeqsFromGBFF () {
#   $1 is the folder which has the .gbff files 
  currentDir=$(echo ${PWD##*/}/)
  # check if we are in the directory of the sequences or not and then moves into it
  if [[ "$currentDir" != "$1" ]]; then 
   cd "$1"
  fi
  if [ ! -d "./fasta_seqs" ]; then 
    mkdir ./fasta_seqs
  fi
  ls *.gbff | while read gbbfFile; do 
    outputFilename=$(echo ./fasta_seqs/"$gbbfFile".fasta)
    fastaDefline=$(echo "$gbbfFile" | grep -oPe '.*(?=(.gbff))' | sed -e 's/\s/_/g')
    echo ">""$fastaDefline" > "$outputFilename"
    fromLine=$(cat "$gbbfFile" | grep -ne "^ORIGIN" |grep -oe '[[:digit:]]*')
    toLine=$(wc -l "$gbbfFile" | grep -oPe '^[[:digit:]]*')
    tail -n "$(($toLine - $fromLine))" "$gbbfFile" | sed -e 's/[[:digit:]]//g' \
-e 's/ *//g' >> "$outputFilename"
  done
}
ExtractFastaSeqsFromGBFF "$1"
