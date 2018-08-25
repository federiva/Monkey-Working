GetNCBITaxonomyChildNodes <- function(nodes.dmp.path, target.taxid) {
  # nodes.dmp.path is a STRING which is the path to the nodes.dmp
  #  file downloaded from NCBI:
  #  ftp://ftp.ncbi.nlm.nih.gov/pub/taxonomy/taxdmp.zip 
  # target.taxid is a  STRING which is the upper NCBI taxonomy
  #  taxid from which we want to get the child's taxids
  # The output of the function is a .csv file ("|" delimited) 
  #  saved in the current working directory named child_nodes.csv
  # The function assumes that the .sh script 
  #  GetNCBITaxonomyChildNodes.sh is located in the 
  #  working directory. 
  path.to.script <- paste(getwd(), "/GetNCBITaxonomyChildNodes.sh", sep="")
  arguments.to.bash <- paste(path.to.script, nodes.dmp.path, target.taxid, sep=" ")
  system2(command = "/bin/bash", args = arguments.to.bash)
  cat("The resulting .csv file is saved in the working directory as child_nodes.csv")
}