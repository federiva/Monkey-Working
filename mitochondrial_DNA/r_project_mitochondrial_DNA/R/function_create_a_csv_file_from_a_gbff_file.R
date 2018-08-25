CreateCSVFromGBFF <- function(input.gbff){
  #The .sh script must be saved in the working directory
  path.to.script <- paste(getwd(), "/bash_scripts/CreateCSVFromGBFF.sh", sep="")
  arguments.to.bash <- paste(path.to.script, input.gbff, sep=" ")
  
  system2(command = "/bin/bash", args = arguments.to.bash)
  print("The resulting .csv file is saved in the working
        directory as indexes_summary.csv")
}