ExtractFastaSeqsFromGBFF <- function(path.to.folder.with.gbff.files) {
  # The .sh script must be located in the working directory
  path.to.script <- paste(getwd(), "/bash_scripts/ExtractFastaSeqsFromGBFF.sh", sep="")
  arguments.to.bash <- paste(path.to.script, path.to.folder.with.gbff.files, sep=" ")
  system2(command = "/bin/bash", args = arguments.to.bash)
  cat("The individual .fasta sequences were saved in ./gbff_filtered_files/fasta_seqs")
}