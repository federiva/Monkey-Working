#' @title MonthlyValue. Calculates monthly means or monthly sums from daily data
#' @name MonthlyValue
#' @description Using daily data, calculates monthly values (mean or accumulated value) for all the given years. The imput of this function is a data frame with daily data for some variable. A column with the date or colums with the number of the month and year are needed.
#' @usage MonthlyValue(data.x, ncol.interest, value = "mean", ncol.date = NULL, ncol.year = NULL, ncol.month = NULL, na.presence = FALSE).
#' @param data.x It's a data.frame with daily data for some variable.
#' @param ncol.interest Column's index of the variable x (numeric).
#' @param value If value = "mean", functions returns monthly mean. If value = "accumulated", it returns the sum. Default is "mean".
#' @param na.presence If TRUE, variable has NAs in it. If FALSE, it hasn't. Default is TRUE.
#' @return It returns a data.frame with three columns. 1) $month.order: Column with the number of the month (from 1 to 12) corresponding to each value, 2) $year: Column with the year, and 3) $mean.value or $accumulated.value: Column with the monthly value of the observation
#' @author Natalia Mariana Denkiewicz, email = nataliamdenk11@gmail.com
#' @import lubridate
#' @import reshape2
#' @export
MonthlyValue <- function(data.x, ncol.obs, value = "mean", 
                           na.presence = TRUE) {
  #
  if (!any(value == c("mean", "accumulated"))) {
    stop(" value must to be 'mean' or 'accumulated'")
  }
  #
  # Calculate the mean of the variable of interest
  first.year <- min(data.x$year, na.rm = na.presence)
  last.year  <- max(data.x$year, na.rm = na.presence)
  x.per.month <- list()
  name.month.tot <- list()
  name.year.tot  <- list() 
  c = 1
  #  Creates a temporary i data.frame for each year == i
  for (i in first.year:last.year) {
    x.anual <- data.x[data.x$year == i, ]
    # It takes the i year df and creates a j df for each month == j
    first.month = min(x.anual$month, na.rm = na.presence)
    last.month  = max(x.anual$month, na.rm = na.presence)
    name.month <- list()
    name.year  <- list() 
    x.value <- list()
    for (j in first.month:last.month) {
      x.month <- x.anual[x.anual$month == j, ]
        # Calculates the mean value or the accumulated value
          if (value == "mean") {
            x.value[j] <- mean(x = x.month[ , ncol.obs], na.rm = na.presence)
            name.month[j] <- j
            name.year[j]  <- i
          } else {
            x.value[j] <- sum(x = x.month[ , ncol.obs], na.rm = na.presence)
            name.month[j] <- j
            name.year[j]  <- i
          }
    } # end cicle month
      x.per.month[[c]] <- unlist(x.value)
      name.month.tot[[c]]    <- unlist(name.month)
      name.year.tot[[c]]     <- unlist(name.year)
      c = c + 1
  } # end year
    monthly.data <- cbind(unlist(name.month.tot), unlist(name.year.tot),
                        unlist(x.per.month))
    if (value == "mean") {
      colnames(monthly.data) <- c("month.order", "year", "mean.value")
    } else {
      colnames(monthly.data) <- c("month.order", "year", "accumulated.value")
    }
    return(monthly.data)
}

  