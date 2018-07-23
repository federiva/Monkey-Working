generateDataset <- function(numObs){
  # This function returns a random dataset based on the parameters fixed within. 
  # The only parameter is numObs which is a numeric and corresponds to the 
  # number of observations of the dataset. 
  # Returns a dataframe. 
  # Set the column names
  names_cols <- c("Subject", "Track", "Distance", "TransectID")
  # Set the subjects
  subjects <- c("NatDen", "FedRiv", "PeqPon")
  # Set the types of tracks recorded
  tracks <- c("Donkey", "Bird", "Poop", "Other")
  # Set the distance
  distance <- c(0:2000)
  resultData <- c()
  # Loop: For every iteration step sample through the previously defined vectors
  for (i in c(1:numObs)){
    subj_i <- sample(x = subjects, size = 1)
    track_i <- sample(x = tracks, size = 1)
    distance_i <- as.numeric(sample(x = distance, size = 1))
    if (subj_i == "NatDen") {
      transect_i <- "A"
    } else if (subj_i == "FedRiv") {
      transect_i <- "B"
    } else if (subj_i == "PeqPon") {
      transect_i <- "C"
    }
    observation_i <- cbind(subj_i, track_i, distance_i, transect_i)
    resultData <- rbind(resultData, observation_i)
  }
  # Set the names 
  colnames(resultData) <- names_cols
  # Convert to a data.frame
  resultData <- as.data.frame(resultData)
  # Set the distances as numeric
  resultData$Distance <- as.numeric(as.character(resultData$Distance))
  return(resultData)
}