#!/bin/sh
################################################################################
function anyIsTrue () {
# Description: This function takes two arguments and returns 0 if the argument 1 
#   is present in the argument 2
# $1 is the object to be compared
# $2 is an array
#   for eachElement in $1; do
  inputArray=("$@")
  compare=$(echo "${inputArray[0]}")
  unset inputArray[0]
  for eachElement in "${inputArray[@]}"; do
    if [[ "$eachElement" == "$compare" ]]; then
#       echo is True
      return 0
    fi
  done
#   echo is False
  return 1
}
# arrayPrueba=( a b c d e f g 8 9 0 )
# anyIsTrue "15" "${arrayPrueba[@]}" 
# echo $?
################################################################################
function getNCBITaxonomyChildNodes () {
# $1 is the path to nodes.dmp as it is downloaded from ftp .... 
# $2 is the number that corresponds to the parent node
#########################
# Check if the output tmp_folder directory exists
  if [ ! -d "../tmp_folder" ]; then
    mkdir ../tmp_folder
  fi
  echo "Searching for the division Id..."
  divisionId=$(awk -F"|" '{if ($1 == '$2') {print $5} }' "$1" \
  | sed 's/[[:space:]]//g')
  echo The division Id is: "$divisionId" 
# Subset the nodes.dmp file
  echo "Subsetting the dataset..."
  nodesTmpFile=$(echo "../tmp_folder/subset_nodes.tmp")
  > "$nodesTmpFile"
  awk -F'|' '{ if ($5 == '$divisionId') {print} }' "$1" >> "$nodesTmpFile"
  echo Done
# Extracting the child nodes for the input $2 parent node
  echo "Getting the child nodes..."
  childNodesSubset=$(echo "../tmp_folder/child_subset_nodes.child")
  > "$childNodesSubset"
  awk -F'|' '{ if ($2 == '$2') {print} }' "$nodesTmpFile" >> "$childNodesSubset"
  echo "Done"
}
# getNCBITaxonomyChildNodes nodes.dmp 33550
################################################################################
function ExhaustiveChildNodeSearch () {
# $1 is a file like "child_subset_nodes.child" the output of getNCBITaxonomyChildNodes()
# $2 is the previous "subset_nodes.tmp" also output of getNCBITaxonomyChildNodes()
# This function searchs iteratively comparing the obtained child nodes as 
# parent nodes of another ones. So, locally returning child nodes that will be 
# again compared against the same database. 
# Once the function doesn't get any new result, it is assumed that every child 
# node is then found. 
#########################
# Create an array of the parents taxids from child_subset_nodes.child
  while read parentTaxIds; do 
    arrayParents+=($parentTaxIds)
  done < <(awk '{FS="|"; print $1}' "$1")
#######
  let iterator=1
  let isFound=1 
  if [ ! -d "../csv_files" ]; then
    mkdir ../csv_files
  fi
  while [ $isFound == 1 ]; do
    echo Round: "$iterator"
    # Creating a new array of the given taxids for the second iteration
    if (( $iterator > 1 )); then
      while read parentTaxIds; do 
        arrayParents+=($parentTaxIds)
      done < <(awk '{FS="|"; print $1}' "$outputChildsFilename")
      #makes a copy for further comparisons
      previousOutputFilename=$(echo "../tmp_folder/previous_output_childs.tmp")
      cp "$outputChildsFilename" "$previousOutputFilename"
    fi
      #Create the output file
    outputChildsFilename=$(echo ../tmp_folder/childs_level_"$iterator".child)
    > "$outputChildsFilename"
#     Read the division-subsetted database searching for child nodes
    cat "$2" | while read lines; do
#     Defining parentTaxIdsFromChilds 
      parentTaxIdsFromChilds=$(echo $lines | awk 'BEGIN{FS="|"}; {print $2}')
#   Run anyIsTrue, the first argument is un-quoted because it is a number 
# The following line means that if any of the parent taxid given by the currently
# printed line is present like a child taxid of the previous iteration that only 
# means that the current line is a child taxid because it has any of the previous one 
# as a parent taxid. More clearly: compares if the line has a parent taxid of 
# the previous iteration given as an array
      anyIsTrue $parentTaxIdsFromChilds "${arrayParents[@]}"
      # If only one correct result is obtained then it proceeds to the following 
#       iteration
      if [ $? -eq 0 ]; then
        echo "$lines" >> "$outputChildsFilename"
      fi
    done
#     Compare if the last two output files are the same, if they are the same 
#     then the function returns
    if (( $iterator > 1 )); then
        # compare the current and the last output
        cmp -s "$previousOutputFilename" "$outputChildsFilename"
        if [ $? -eq 0 ]; then
          let "isFound++"
          #compile the generated files and generate .csv "|" delimited file 
          #  named child_nodes.csv
          #Initialize .csv file headers
          echo "tax_id|parent_tax_id|rank|embl_code|division_id|inherited_div_flag|\
genetic_code_id|inherited_GC_flag|mitochondrial_genetic_code_id|\
inherited_MGC_flag|GenBank_hidden_flag|hidden_subtree_root|comments"> ../csv_files/child_nodes.csv
          cat ../tmp_folder/*.child | sort | uniq | sed -e 's/\t//g' -e 's/.$//g'\
>> ../csv_files/child_nodes.csv
        fi
      fi
    let "iterator++"
  done
  exit
}
####Running the function
getNCBITaxonomyChildNodes "$1" "$2"
ExhaustiveChildNodeSearch ../tmp_folder/child_subset_nodes.child ../tmp_folder/subset_nodes.tmp
exit
