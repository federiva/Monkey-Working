#' @title GenerateDataset. Generates a random lineal transect data set
#' @name GenerateDataset
#' @description This function returns a random dataset of distances measured over lineal transets. It uses parameters fixed within. The only parameter is num.obs which is a numeric and corresponds to the number of observations of the dataset. 
#' @usage GenerateDataset(num.obs)
#' @param num.obs Number of observations desired (numeric).
#' @return Returns a dataset with the columns: subject, track, distance, transectID and num.obs observations.
#' @author Edgar Federico Rivadeneira, email = federivadeira@gmail.com
#' @export
GenerateDataset <- function(num.obs) {
  # This function returns a random dataset based on the parameters fixed within.
  # The only parameter is num.obs which is a numeric and corresponds to the 
  # number of observations of the dataset. 
  # Returns a dataframe. 
  # Set the column names
  names.cols <- c("subject", "track", "distance", "transectID")
  # Set the subjects
  subjects <- c("NatDen", "FedRiv", "PeqPon")
  # Set the types of tracks recorded
  tracks <- c("Donkey", "Bird", "Poop", "Other")
  # Set the distance
  distance <- c(0:2000)
  result.data <- c()
  # Loop: For every iteration step sample through the previously defined vectors
  for (i in c(1:num.obs)) {
    subj.i  <- sample(x = subjects, size = 1)
    track.i <- sample(x = tracks, size = 1)
    distance.i <- as.numeric(sample(x = distance, size = 1))
    if (subj.i == "NatDen") {
      transect.i <- "A"
    } else if (subj.i == "FedRiv") {
      transect.i <- "B"
    } else if (subj.i == "PeqPon") {
      transect.i <- "C"
    }
    observation.i <- cbind(subj.i, track.i, distance.i, transect.i)
    result.data <- rbind(result.data, observation.i)
  }
  # Set the names 
  colnames(result.data) <- names.cols
  # Convert to a data.frame
  result.data <- as.data.frame(result.data)
  # Set the distances as numeric
  result.data$Distance <- as.numeric(as.character(result.data$Distance))
  return(result.data)
}