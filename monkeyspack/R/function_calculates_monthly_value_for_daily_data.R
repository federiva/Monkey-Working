#' @title MonthlyValue. Calculates monthly means or monthly sums from daily data
#' @name MonthlyValue
#' @description Using daily data, calculates monthly values (mean or accumulated value) for all the given years. The imput of this function is a data frame with daily data for some variable. A column with the date or colums with the number of the month and year are needed.
#' @usage MonthlyValue(data.x, ncol.interest, value = "mean", ncol.date = NULL, ncol.year = NULL, ncol.month = NULL, na.presence = FALSE).
#' @param data.x It's a data.frame with daily data for some variable.
#' @param ncol.interest Column's index of the variable x (numeric).
#' @param value If value = "mean", functions returns monthly mean. If value = "accumulated", it returns the sum. Default is "mean".
#' @param ncol.date Column's index of the dates's column. Default is NULL.
#' @param ncol.year Index of the column which has the year of each x[i, ]. Default is NULL.
#' @param ncol.month Index of the column which has the number of the month of each x[i, ]. Default is NULL (numeric).
#' @param na.presence If TRUE, variable has NAs in it. If FALSE, it hasn't. Default is FALSE.
#' @return It returns a data.frame with three columns. 1) $month.order: Column with the number of the month (from 1 to 12) corresponding to each value, 2) $year: Column with the year, and 3) $mean.value or $accumulated.value: Column with the monthly value of the observation
#' @author Natalia Mariana Denkiewicz, email = nataliamdenk11@gmail.com
#' @import lubridate
#' @import reshape2
#' @export
MonthlyValue <- function(data.x, ncol.interest, value = "mean", 
                         ncol.date = NULL, ncol.year = NULL,
                         ncol.month = NULL, na.presence = FALSE) {
  # Evaluates "value"
  if (!any(value == c("mean", "accumulated"))) {
    stop(" value must to be 'mean' or 'accumulated'")
  }
  # 
  # It requires lubridate for organize dates
  # It requires reshape2 for stack columns
  # It tests which columns are present and creates new columns when it's 
  # necessary.
  # 
  if (is.null(ncol.year) == TRUE & is.null(ncol.month) == TRUE) {
    data.x[ , ncol(data.x) + 1] <- lubridate::month(data.x[ , ncol.date])
    ncol.month <- ncol(data.x)
    data.x[ , ncol(data.x) + 1] <- lubridate::year(data.x[ , ncol.date])
    ncol.year <- ncol(data.x)
  } else if (is.null(ncol.year) == TRUE & is.null(ncol.month) == FALSE) {
    data.x[ , ncol(data.x) + 1] <- lubridate::year(data.x[ , ncol.date])
    ncol.year <- ncol(data.x)
  } else if (is.null(ncol.year) == FALSE & is.null(ncol.month) == TRUE) {
    data.x[ , ncol(data.x) + 1] <- lubridate::month(data.x[ , ncol.date])
    ncol.month <- ncol(data.x)
  } else {
    ncol.year  <- ncol.year
    ncol.month <- ncol.month
  }
  # Calculate the mean of the variable of interest
  x.value <- list()
  x.value.year <- data.frame()
  x.value.year[1:12, 1] <- 1:12
  first.year = min(data.x[ , ncol.year])
  last.year  = max(data.x[ , ncol.year])
  # It takes a data's sample and creates a temporary i data.frame 
  # for year == i
  for (i in first.year:last.year) {
    x.anual <- data.x[data.x[ , ncol.year] == i, ]
    # It takes the i year data.frame and creates a temporary j data.frame 
    # for each month == j
    for (j in 1:12) {
      x.month <- x.anual[x.anual[ , ncol.month] == j, ]
      # Calculates the mean value or the accumulated value
      if (value == "mean") {
      x.value[j] <- mean(x.month[ , ncol.interest], na.rm = na.presence)
      } else {
      x.value[j] <- sum(x.month[ , ncol.interest], na.rm = na.presence)
      }
    }
    value.month <- unlist(x.value)
    value.month <- as.vector(value.month)
    x.value.year <- cbind(x.value.year, value.month)
  }
  # It renames the columns and stacks them
  colnames(x.value.year) <- c("month.order", first.year:last.year)
  x.value.year <- reshape2::melt(data = x.value.year, id.vars = 'month.order', 
                       variable_name = 'year')
  if (value == "mean") {
    colnames(x.value.year) <- c("month.order", "year", "mean.value")
  } else {
    colnames(x.value.year) <- c("month.order", "year", "accumulated.value")
  }
  return(x.value.year)
}
