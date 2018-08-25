ExtractFilteredGBFFFromCSV <- function(path.to.filtered.CSV, path.to.gbff.sourcefile) {
  # The bash_scripts folder must be located in the working directory
  path.to.script <- paste(getwd(), "/bash_scripts/ExtractFilteredGBFFFromCSV.sh", sep="")
  arguments.to.bash <- paste(path.to.script, path.to.filtered.CSV,path.to.gbff.sourcefile, sep=" ")
  system2(command = "/bin/bash", args = arguments.to.bash)
  cat("The individual .gbff sequences were saved in ./gbff_filtered_files/")
}