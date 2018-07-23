#' @title HourlyToDailyAccum. Function that returns a data.frame with accumulated daily data from a data frame with hourly data.
#' @name HourlyToDailyAccum
#' @description The imput of this function is a data frame with hourly observations. A column with the day number or the date are needed.
#' @usage HourlyToDailyAccum(data.x, ncol.obs, ncol.date = NULL, orders = NULL, ncol.day = NULL, ncol.month = NULL, ncol.year = NULL)
#' @param data.x Data.frame with data registered every hour.
#' @param ncol.obs Index of the column which has hourly observations (numeric).
#' @param ncol.date Column's index for date's column. Default is NULL (numeric).
#' @param orders Only when data.frame has a date column. It's the order of parameters in date, where firts letter represents the component of the date (e.g. day is d). See help(topic = parse_date_time, package = lubridate) (character).
#' @param ncol.day Index of the column which has the number of the day. Default is NULL (numeric).
#' @param ncol.month Index of the column which has the number of the month. Default is NULL (numeric).
#' @param ncol.year Index of the column which has the year. Default is NULL (numeric).
#' @return Returns a data.frame with accumulated daily data.
#' @author Edgar Federico Rivadeneira, email = federivadeneira@gmail.com
#' @export  
HourlyToDailyAccum <- function(data.x, ncol.obs, ncol.date = NULL, orders = NULL, ncol.day = NULL, ncol.month = NULL, ncol.year = NULL) {
# Organizes dates's columns using lubridate
# If columns with days, months or years are NULL, creates these columns.
  if (is.null(ncol.day) == TRUE) {
    if (lubridate::is.POSIXt(data.x[ , ncol.date]) == TRUE) {
      col.day <-  lubridate::day(data.x[ , ncol.date])
    } else {
      data.x[ , ncol.date] <- lubridate::parse_date_time(x = data.x[ , ncol.date], 
                                              orders = orders)
      n <- ncol(data.x)
      data.x[ , n + 1] <- lubridate::day(data.x[ , ncol.date])
      col.day <- data.x[ , ncol(data.x)]  
    } 
  } else {
    col.day <- data.x[ , ncol.day]
  }
  if (is.null(ncol.month) == TRUE) {
    if (lubridate::is.POSIXt(data.x[ , ncol.date]) == TRUE) {
      col.month <- lubridate::month(data.x[ , ncol.date])
    } else {
      data.x[ , ncol.date] <- lubridate::parse_date_time(x = data.x[ , ncol.date], 
                                              orders = orders)
      n <- ncol(data.x)
      data.x[ , n + 1] <- lubridate::month(data.x[ , ncol.date])
      col.month <- data.x[ , ncol(data.x)]  
    } 
  } else {
    col.month <- data.x[ , ncol.month]
  }
  if (is.null(ncol.year) == TRUE) {
    if (lubridate::is.POSIXt(data.x[ , ncol.date]) == TRUE) {
      col.year <-  lubridate::year(data.x[ , ncol.date])
    } else {
      data.x[ , ncol.date] <- lubridate::parse_date_time(x = data.x[ , ncol.date], 
                                              orders = orders)
      n <- ncol(data.x)
      data.x[ , n + 1] <- lubridate::year(data.x[ , ncol.date])
      col.year <- data.x[ , ncol(data.x)]  
    } 
  } else {
    col.year <- data.x[ , ncol.year]
  }
  # creates list where summarize data
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
