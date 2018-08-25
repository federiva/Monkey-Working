#!/bin/sh
function TestNumberOfTargets() {
# $1 is the .gbff file
# The function has two exit codes 0 for a positive result and 1 for a 
# negative result of the test
# This function test if the number of each target to be extracted with 
# CreateCSVFromGBFF() is the same. If this number is the same then the .gbff
# file can be parsed.
  echo "Testing if the number of fields in the .gbff file are consistent"
  numberOfDashes=$(grep -c "^//" $1)
  numberOfTaxons=$(grep -c "\/db_xref=\"taxon:" $1)
  numberOfOrganisms=$(grep -c "^  ORGANISM" $1)
  numberOfDefinitions=$(grep -c "^DEFINITION" $1)
  numberOfLocus=$(grep -c "^LOCUS" $1)
  numberOfAccessions=$(grep -c "^ACCESSION" $1)
  numberOfAccessVersions=$(grep -c "VERSION" $1)
  numbersArray=( $numberOfDashes $numberOfTaxons $numberOfOrganisms
                 $numberOfDefinitions $numberOfLocus $numberOfAccessions
                 $numberOfAccessVersions
  )
  let testCounter=1
  for variable in "${numbersArray[@]}" ; do
    if (( $testCounter > 1 )); then
      if (( $variable != $previousVariable )); then
        echo -e "There is a problem with the number of fields tested. \nThe gbff file cannot be parsed\n"
        return 1 
      fi
    fi
    previousVariable=$(echo $variable)
    let "testCounter++"
  done
  echo -e "The number of fields in the .gbff file are consistent.\nThe file can be parsed\nThe number of accessions in the file is: "$numberOfAccessions"\n"
  return 0
}
###############################################################################
###############################################################################
# Each entry is delimited by a // tag so each line number where the //
# tag is could be used to extract information for each entry
function CreateCSVFromGBFF () {
# $1 is the *.gbff target file to parse 
# The function calls another function named TestNumberOfTargets
# The function creates a .csv file in the working directory named 
# indexes_summary.csv
###############################################################################
# Checking if the temporal folder exists
  if [ ! -d "../tmp_parsed_files" ]; then
    mkdir ../tmp_parsed_files
  fi
  if [ ! -d "../csv_files" ]; then
    mkdir ../csv_files
  fi
#   Checking if the target file can be parsed
  echo "Running TestNumberOfTargets..."
  test=$(TestNumberOfTargets "$1")
  if [ $? -eq 0 ]; then 
    echo "TestNumberOfTargets is OK" 
    else if [ $? -eq 1 ]; then 
      echo "TestNumberOfTargets is not OK"
      echo $test
      exit
      else 
        echo unexpected: Exit status of TestNumberOfTargets is incorrect 
        echo $test
        exit
    fi
  fi 
###############################################################################
##########Indexing
###############################################################################
  # Start the indexing of the sequences present in the gbff file ($1)
  # Creating and initialiazing output .csv file
  outputFileName=$(echo ../csv_files/indexes_summary.csv)
  echo Initialiazing "$outputFileName"
  > "$outputFileName"
  # First index
  echo Extracting line delimiters...
  echo 1 > ../tmp_parsed_files/dashes_indexes.tmp
  echo fromLine,toLine > ../tmp_parsed_files/indexes_summary.tmp
  indexesTmpFile=$(echo "../tmp_parsed_files/indexes_summary.tmp")
  fileDashesIndexes=$(echo "../tmp_parsed_files/dashes_indexes.tmp")
# Getting the middle and last indexes
  cat "$1" | grep -n "^//" | grep -oPe "[[:digit:]]*(?=(:))" >> "$fileDashesIndexes"
# Getting the indexes for each of the sequences
  let indexCounter=1
  let seqNumber=1
  cat $fileDashesIndexes | while read index; do 
    if (( $indexCounter > 1 )); then
      echo $previousIndex,$(( $index - 1 )) >> $indexesTmpFile
      let "seqNumber++"
    fi
    previousIndex=$(echo $index)
    let "indexCounter++"
  done
  ##### Extracting taxonId's
  echo "Extracting taxonid's..." 
  taxonIdTmpFile=$(echo "../tmp_parsed_files/taxonIdTmpFile.tmp")
  echo "taxonId" > "$taxonIdTmpFile"
  grep  "\/db_xref=\"taxon:" $1 | grep -oPe "(?<=(\/db_xref=\"taxon:)).*(?=(\"))" \
  >> "$taxonIdTmpFile"
  ##### Extracting organisms
  echo "Extracting organisms..." 
  organismTmpFile=$(echo "../tmp_parsed_files/organismTmpFile.tmp")
  echo "organism" > "$organismTmpFile"
  grep "^  ORGANISM" $1 | grep -oPe "(?<=(^  ORGANISM  )).*" >> "$organismTmpFile"
  ##### Extracting definitions
  echo "Extracting definitions..." 
  definitionTmpFile=$(echo "../tmp_parsed_files/definitionTmpFile.tmp")
  echo "definition" > "$definitionTmpFile"
  grep "^DEFINITION" $1 | grep -oPe "(?<=(^DEFINITION  )).*" \
  | sed -e 's/[[:punct:]]//g'  >> "$definitionTmpFile"
  ##### Extracting locus
  echo "Extracting locus..." 
  locusTmpFile=$(echo "../tmp_parsed_files/locusTmpFile.tmp")
  echo "locus" > "$locusTmpFile"
  grep "^LOCUS" $1 | grep -oPe "(?<=(^LOCUS)).*" | sed -e "s/  */ /g" \
  -e "s/^\s//g"  >> "$locusTmpFile"
  ##### Extracting accession numbers
  echo "Extracting accession numbers..." 
  accessionTmpFile=$(echo "../tmp_parsed_files/accessionTmpFile.tmp")
  echo "accession" > "$accessionTmpFile"
  grep "^ACCESSION" $1 |  grep -oPe "(?<=(^ACCESSION   )).*" \
  >> "$accessionTmpFile"
  ##### Extracting accession version numbers
  echo "Extracting accession version numbers..." 
  accessionVersionTmpFile=$(echo "../tmp_parsed_files/accessionVersionTmpFile.tmp")
  echo "accession_version" > "$accessionVersionTmpFile"
  grep "^VERSION" $1 |  grep -oPe "(?<=(VERSION\s{5})).*" \
  >> "$accessionVersionTmpFile"
  ##### FINISHING
  echo "Compiling csv file ..." 
  paste -d "," "$indexesTmpFile" "$taxonIdTmpFile" "$organismTmpFile" \
  "$definitionTmpFile" "$locusTmpFile" "$accessionTmpFile" \
  "$accessionVersionTmpFile" >> "$outputFileName"
}
###############################################################################
###############################################################################
# This part of the script is intended to be used with R!  
# Running the function 
CreateCSVFromGBFF "$1"
exit