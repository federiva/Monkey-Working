#' @title HourlyToDailyAccum. Function that returns a data.frame with accumulated daily data from a data frame with hourly data.
#' @name HourlyToDailyAccum
#' @description This function takes a data.frame with hourly data and returns another data.frame with accumulated daily data. It needs columns with day, month and year corresponding to each observation, and they can be easily obtained using OrganizeDateColumns function.
#' @usage HourlyToDailyAccum(data.x, ncol.obs)
#' @param data.x Data.frame with data recorded every hour.
#' @param ncol.obs Index of the column which has hourly observations (numeric).
#' @return Returns a data.frame with accumulated daily data.
#' @author Edgar Federico Rivadeneira, email = federivadeneira@gmail.com
#' @export  
HourlyToDailyAccum <- function(data.x, ncol.obs) {
#
# creates list where add daily data
# 
  col.day <- data.x$day
  col.month <- data.x$month
  col.year <- data.x$year
  col.observation <- data.x[ , ncol.obs]
  initial.day <- list(col.day[1])
  if (is.na(col.observation[1]) == TRUE) {
    obs <- 0
  } else {
    obs <- col.observation[1]
  }
  obs.list <- list()
  month.day <- list(col.month[1])
  year.day <- list(col.year[1])
  i <- 1
  j <- 1
  for (each.day in col.day) {
    if (i >= length(col.day)) {
      obs.list[[j]] <- obs
      break
    }
    if (each.day == initial.day[length(initial.day)]) {
      if (is.na(col.observation[i]) == TRUE) {
        i <- i + 1
        next
      } else {
        obs.accu <- col.observation[i]
        obs <- obs.accu + obs
        i <- i + 1
        next
      }
    } else {
      if (each.day != initial.day[length(initial.day)]) {
        obs.list[[j]] <- obs
        j <- j + 1
        initial.day[[j]] <- each.day
        month.day[[j]] <- col.month[i]
        year.day[[j]] <- col.year[i]
        if (is.na(col.observation[i]) == TRUE){
          obs <- 0
          i <- i + 1
          next
        } else {
          obs <- col.observation[i]
          i <- i + 1
          next
        }
      }
    }
  }
  month.day <- unlist(month.day)
  year.day <- unlist(year.day)
  initial.day <- as.factor(unlist(initial.day))
  obs.list <- as.numeric(unlist(obs.list))
  result <- cbind.data.frame(initial.day, month.day, year.day, obs.list)
  colnames(result) <- c("day", "month", "year", "daily.accumulated")
  return(result)
}

