#' @title HourlyToDaily. Function that returns a data.frame with accumulated or average daily data from a data frame with hourly data.
#' @name HourlyToDaily
#' @description This function takes a data.frame with hourly data and returns another data.frame with average or accumulated daily data. It needs columns with day, month and year corresponding to each observation, and they can be easily obtained using OrganizeDateColumns function.
#' @usage HourlyToDaily(data.x, ncol.obs, na.presence = TRUE, value = "mean")
#' @param data.x Data.frame with data recorded every hour.
#' @param ncol.obs Index of the column which has hourly observations (numeric).
#' @param value If value = "mean", functions returns monthly mean. If value = "accumulated", it returns the sum. Default is "mean".
#' @param na.presence If TRUE, variable has NAs in it. If FALSE, it hasn't. Default is TRUE.
#' @return Returns a data.frame with accumulated or avarage daily data.
#' @author Natalia Mariana Denkiewicz, email = nataliamdenk11@gmail.com
#' @export  
HourlyToDaily <- function(data.x, ncol.obs, na.presence = TRUE, 
                          value = "mean") {
  #
  if (!any(value == c("mean", "accumulated"))) {
    stop(" value must to be 'mean' or 'accumulated'")
  }
  # data.x$year  <- data.x$year
  # data.x$month <- data.x$month
  # data.x$day   <- data.x$day
    # Calculate the mean of the variable of interest
    first.year <- min(data.x$year, na.rm = na.presence)
  last.year  <- max(data.x$year, na.rm = na.presence)
  x.daily.per.years <- list()
  name.month.tot <- list()
  name.year.tot  <- list() 
  name.day.tot  <- list() 
  c = 1
  #  Creates a temporary i data.frame for each year == i
  for (i in first.year:last.year) {
    x.anual <- data.x[data.x$year == i, ]
    # It takes the i year df and creates a j df for each month == j
    first.month = min(x.anual$month, na.rm = na.presence)
    last.month  = max(x.anual$month, na.rm = na.presence)
    x.daily.per.month <- list()
    name.month.list <- list()
    name.year.list  <- list() 
    name.day.list   <- list()
    for (j in first.month:last.month) {
      x.month <- x.anual[x.anual$month == j, ]
      # It takes the j month df and creates a k df for each day == k
      first.day = min(x.month$day, na.rm = na.presence)
      last.day  = max(x.month$day, na.rm = na.presence)
      x.value.max <- list()
      x.value.min <- list()
      x.value <- list()
      name.month <- list()
      name.year <- list()
      name.day <- list()
      for (k in first.day:last.day) {
        x.day <- x.month[x.month$day == k, ]
        # Calculates the mean value or the accumulated value
        if (all(unlist(lapply(X = x.day[ , ncol.obs], FUN = is.na)))) {
          x.value[k] <- NA
        } else {
          if (value == "mean") {
            x.value.max[k] <- max(x.day[ , ncol.obs], na.rm = na.presence)
            x.value.min[k] <- min(x.day[ , ncol.obs], na.rm = na.presence)
            x.value[k] <- mean(x = c(x.value.max[[k]], x.value.min[[k]]), na.rm = na.presence)
          } else {
            x.value[k] <- sum(x.day[ , ncol.obs], na.rm = na.presence)
          }
        }
        name.day[k] <- k 
        name.month[k] <- j
        name.year[k] <- i
      } # end cicle day
      x.daily.per.month[[j]] <- unlist(x.value)
      name.month.list[[j]]   <- name.month
      name.year.list[[j]]    <- name.year
      name.day.list[[j]]     <- name.day
    } # end cicle month
    x.daily.per.years[[c]] <- unlist(x.daily.per.month)
    name.month.tot[[c]]    <- unlist(name.month.list)
    name.year.tot[[c]]     <- unlist(name.year.list)
    name.day.tot[[c]]      <- unlist(name.day.list)
    c = c + 1
    daily.data <- cbind(unlist(name.day.tot), unlist(name.month.tot), 
                        unlist(name.year.tot), unlist(x.daily.per.years))
    colnames(daily.data) <- c("day", "month", "year", "value")
  }
  return(daily.data)
}
