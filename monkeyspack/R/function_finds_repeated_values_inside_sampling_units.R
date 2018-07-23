#' @title FindRepetitionsSU. Function that finds repeated values in sampling units and calculates their frequency
#' @name FindRepetitionsSU
#' @description The imput of this function is a data frame of sampling units with their corresponding data or observations. It finds repeated values inside sample units and calculates their frequency.
#' @usage FindRepetitionsSU(data.x, ncol.names.su, ncol.observation, result = "frequency")
#' @param data.x It's a data.frame with observations (numeric or factor) per sampling unit or some category
#' @param ncol.names.su Index of the column which has sampling units's names, or cathegories names.
#' @param ncol.observation Column's index of the observations's column (factor or numerical).
#' @param result If result is equal to "frequency", then FindRepetitionsSU returns a data.frame with 3 columns: $names.su, $observation, $frequency. Last column has the observation frequency per sampling unit. If result is equal to "is.repeated" function adds a new column to the original data set naming with FALSE unique values or observations that have been registered for first time and TRUE repeated observations.
#' @return see parameter result in Arguments
#' @author Natalia Mariana Denkiewicz, email = nataliamdenk11@gmail.com
#' @examples 
#' # data.set example number 1. 
#' 
#' # It has 5 sampling units, each one with 4 observations of class factor.
#' col.names.su.1 <- c(rep("M1",4),rep("M2",4),rep("M3",4),rep("M4",4), rep("M5", 4))  # create sample units
#' simulated.species <- c(rep("sp1",20), rep("sp2",20), rep("sp3",20), rep("sp4",20), rep("sp5",20), rep("sp6",20))  # create a pool of species. From there, obtain a sample of size = 20.
#' col.factor.observation <- sample(simulated.species, size = 20)
#' ej1 <- cbind(col.names.su.1, col.factor.observation)
#' ej1 <-as.data.frame(ej1)
#' ej1.result.is.repeated <- FindRepetitionsSU(data.x = ej1, ncol.names.su = 1, ncol.observation = 2, result = "is.repeated")
#' ej1.result.frequencies <- FindRepetitionsSU(data.x = ej1, ncol.names.su = 1, ncol.observation = 2, result = "frequency")
#' 
#' # data.set example number 2. 
#' 
#' # It has 5 different sample units, each one with 4 observations of class numeric.
#' col.names.su.2 <- c(rep("M1", 1),rep("M2",1),rep("M3",1),rep("M4",1), rep("M5", 1), rep("M1", 2),rep("M2",2),rep("M3",2),rep("M4",2), rep("M5", 2), rep("M1", 1),rep("M2",1),rep("M3",1),rep("M4",1), rep("M5", 1))  # sample units are mixed.
#' simulated.values <- c(rep(1,20), rep(2,20), rep(3,20), rep(4,20), rep(5,20), rep(6,20))  # create a pool of species. From there, obtain a sample of size = 20.
#' col.numeric.observation <- sample(simulated.values, size = 20)
#' ej2 <- cbind(col.names.su.2, col.numeric.observation)
#' ej2 <-as.data.frame(ej2)
#' ej2.result.is.repeated <- FindRepetitionsSU(data.x = ej2, ncol.names.su = 1, ncol.observation = 2, result = "is.repeated")
#' ej2.result.frequencies <- FindRepetitionsSU(data.x = ej2, ncol.names.su = 1, ncol.observation = 2, result = "frequency")
#' @export
FindRepetitionsSU <- function(data.x, ncol.names.su, ncol.observation, 
                              result = "frequency") {
  # FIRST PART: It returns the original data.x with a new colsun. 
  # For each Sampling Unit it assings TRUE if the observations are repeated, 
  # and FALSE if not.
  data.x[ ,(ncol(data.x) + 1)] <- 1:nrow(data.x)
  n <- ncol(data.x)
  names(data.x)[n] <- "order"
  frequen <- list()
  nom <- list()
  logical_operation <- list()
  is.rep.total <- list()
  for (su in 1:length(unique(data.x[ , ncol.names.su]))) {
    order_su <- list()
    is.rep.su <- list()
    names.su <- unique(data.x[ , ncol.names.su])[su]
    t <- subset.data.frame(x = data.x, 
                           subset = data.x[ , ncol.names.su] == names.su)   
    # t is a data frame for each sampling unit or category
    observations <- t[ , ncol.observation]
    order_su <- t$order
    # logical operation
    logical_operation[[1]] <- FALSE
    is.rep.su[1] <- FALSE
    if (length(observations) >= 2) {
      for (k in 2:length(observations)) {
        logical_operation[[k]] <- observations[k] == observations[1:(k - 1)]
        is.rep.su[k] <- any(logical_operation[[k]])
      }
    } 
    is.rep.total[order_su] <- is.rep.su
  }
  is.rep.total <- unlist(is.rep.total)
  data.x[ , (ncol(data.x) + 1)] <- is.rep.total
  n <- ncol(data.x)
  names(data.x)[n] <- "repeated.value"
  #
  # SECOND PART: Function calculates the frequency to 
  # each non-repeated value (FALSES)
  # Uses a new data.frame with those rows with $repeated.value == FALSE
  simple.observations <- data.x[data.x$repeated.value == "FALSE", ]
  simple.observations$order <- 1:nrow(simple.observations)
  # Calculates the observations's frequency for each SU 
  # using the entire data set (with TRUES and FALSES).
  for (su in 1:length(unique(data.x[ , ncol.names.su]))) {
    order.obs <- list()
    names.su <- unique(data.x[ , ncol.names.su])[su]
    # t.obs is a data.frame for each sampling unit
    t.obs   <- subset.data.frame(x = data.x, 
                                 subset = data.x[ , ncol.names.su] == names.su)   
    observations_r <- t.obs[ , ncol.observation]
    freq <- as.data.frame(table(observations_r))
    # Cero frequencies aren't needed
    freq <- subset.data.frame(x = freq, subset = freq[ , 2] != 0)     
    # FALSES values for each sampling unit
    subset.simple.observations <- subset.data.frame(x = simple.observations, 
      subset =  simple.observations[ , ncol.names.su] == names.su)
    order.obs <- subset.simple.observations$order
    index     <- list()
    variable  <- list()
    for(i in 1:nrow(subset.simple.observations)) {
      index      <- which(x = freq[ , 1] == 
                             subset.simple.observations[i, ncol.observation])
      variable[i] <- freq[index, 2]
      variable    <- unlist(variable)
      nombre      <- rep(names.su, nrow(subset.simple.observations))
    }
    observ <- simple.observations[ , ncol.observation]
    frequen[order.obs] <- variable
    nom[order.obs]  <- as.character.factor(nombre)
  }
  frequen <- unlist(frequen)
  freq.data.frame <- cbind.data.frame(observ, frequen)
  nom <- unlist(nom)
  freq.data.frame <- cbind.data.frame(as.factor(nom), freq.data.frame)
  colnames(freq.data.frame) <- c("names.su", "observation", "frequency")
  # Delete data.x$order. It's not useful for users.
  data.x <- data.x[ ,-(which(colnames(data.x) == "order"))]
  if (result == "is.repeated") {
    return(data.x)
  } else if (result == "frequency") {
    return(freq.data.frame)
  } else {
    print("result can only be equal to 'is.repeated' or 'frequency'")
  }
}
