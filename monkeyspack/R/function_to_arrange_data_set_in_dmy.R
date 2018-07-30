#' @title OrganizeDateColumns. Function that returns a data.frame with columns of day, month and year.
#' @name OrganizeDateColumns
#' @description The input of this function is a data frame with observations through time. This function organizes date data and adds (or renames) columns with the day, the month and the year corresponding to each observation. 
#' @usage OrganizeDateColumns(data.x, ncol.date = 1, orders = "mdyHMS", ncol.day = NULL, ncol.month = NULL, ncol.year = NULL)
#' @param data.x Data.frame with observations across time.
#' @param ncol.date Index of the column which has date data (numeric). Default is NULL.
#' @param orders Only when data.frame has a date column. It's the order of parameters in date. It's formed by the firt letter of each component of the date (e.g. day is d). Day, month, and year are written in lower case. Hour, minute and second are written in capital letters (e.g. hour is H). See help(topic = parse_date_time, package = lubridate) (character).
#' @param ncol.day Index of the column which has the number of the day. Default is NULL (numeric).
#' @param ncol.month Index of the column which has the number of the month. Default is NULL (numeric).
#' @param ncol.year Index of the column which has the year. Default is NULL (numeric).
#' @return Returns a data.frame with columns with the day, month and year corresponding to each observation.
#' @details This function is useful for organizing data before its use in functions that make daily or monthly calculations.
#' @import lubridate
#' @author Natalia Mariana Denkiewicz, email = nataliamdenk11@gmail.com
#' @export
OrganizeDateColumns <- function(data.x, ncol.date = 1, orders = "mdyHMS", ncol.day = NULL, ncol.month = NULL, ncol.year = NULL) {
  #
  require(lubridate)
  #
  # Organizes dates's columns using lubridate
  # If columns with days, months or years are NULL, creates these columns with a 
  # function inside de function
  #
  OrganizeDate <- function(ncol.time, fun) {
    names.col <- colnames(data.x)
    if (is.null(ncol.time) == TRUE) {
      if (lubridate::is.POSIXt(data.x[ , ncol.date]) == TRUE) {
        col.time <-  match.fun(fun)(data.x[ , ncol.date])
        data.x[ ,(ncol(data.x) + 1)] <- col.time
        colnames(data.x) <- c(names.col, fun)
      } else {
        data.x[ , ncol.date] <- parse_date_time(x = data.x[ , ncol.date], 
                                                orders = orders)
        col.time <- match.fun(fun)(data.x[ , ncol.date])
        data.x[ ,(ncol(data.x) + 1)] <- col.time
        colnames(data.x) <- c(names.col, fun)
      } 
    } else {
      col.time <- data.x[ , ncol.time]
      colnames(data.x)[ncol.time] <- fun 
    }
    data.x <- data.x
  }
#  
# Apply internal function to data
#
data.x <- OrganizeDate(ncol.time = ncol.day, fun = "day")
data.x <- OrganizeDate(ncol.time = ncol.month, fun = "month")
data.x <- OrganizeDate(ncol.time = ncol.year, fun = "year")
  return(data.x)
}
