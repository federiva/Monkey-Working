#### Analysing meteorological data from weather stations
#
# First we read the .csv with the data from La Calera
#
calera_raw <- read.csv(".../meteorologicos_la_calera.csv")
head(calera_raw)
#
#> head(calera)
#                 fecha mm temp humedad
# 1 04/27/2018 17:00:00  0 22.8    49.8
# 2 04/27/2018 16:00:00  0 24.0    47.9
# 3 04/27/2018 15:00:00  0 26.4    45.5
# 4 04/27/2018 14:00:00  0 25.9    49.7
# 5 04/27/2018 13:00:00  0 26.0    56.7
# 6 04/27/2018 12:00:00  0 25.6    58.5
#
# How many observations are recorded? 
nrow(calera_raw) # 91421
#
calera_raw[91421, 1] # [1] 10/16/2007 00:00:00
calera_raw[1, 1] # 04/27/2018 17:00:00
#
str(calera_raw)
#
# > str(calera)
# 'data.frame':	91421 obs. of  4 variables:
#   $ fecha  : Factor w/ 91421 levels "01/01/2008 00:00:00",..: 30473 30472 30471 30470 30469 30468 30467 30466 30465 30464 ...
# $ mm     : num  0 0 0 0 0 0 0 0 0 0 ...
# $ temp   : num  22.8 24 26.4 25.9 26 25.6 25.7 25.3 23.9 20.7 ...
# $ humedad: num  49.8 47.9 45.5 49.7 56.7 58.5 57.1 58.3 62 73.6 ...
#
# Function that creates day, month and year columns.
# 
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
  data.x[ , ncol.date] <- as.Date(paste(data.x$year, data.x$month, data.x$day, sep = "-"))
  return(data.x)
}
# Apply the function
# 
calera_organized <- OrganizeDateColumns(data.x = calera_raw, ncol.date = 1, orders = "mdyHMS", ncol.day = NULL, ncol.month = NULL, ncol.year = NULL)
head(calera_organized)
#
# > head(calera_organized)
#        fecha mm temp humedad day month year
# 1 2018-04-27  0 22.8    49.8  27     4 2018
# 2 2018-04-27  0 24.0    47.9  27     4 2018
# 3 2018-04-27  0 26.4    45.5  27     4 2018
# 4 2018-04-27  0 25.9    49.7  27     4 2018
# 5 2018-04-27  0 26.0    56.7  27     4 2018
# 6 2018-04-27  0 25.6    58.5  27     4 2018
# 
# 1) We obtain daily data from hourly data
# 
HourlyToDaily <- function(data.x, ncol.obs, ncol.date, na.presence = TRUE, value = "mean") {
  #
  if (!any(value == c("mean", "accumulated"))) {
    stop(" value must to be 'mean' or 'accumulated'")
  }
  # Calculate the mean of the variable of interest
  first.year <- min(data.x$year, na.rm = na.presence)
  last.year  <- max(data.x$year, na.rm = na.presence)
  x.daily.per.years <- list()
  date.tot <- list()
  c = 1
  #  Creates a temporary i data.frame for each year == i
  for (i in first.year:last.year) {
    x.anual <- data.x[data.x$year == i, ]
    # It takes the i year df and creates a j df for each month == j
    first.month = min(x.anual$month, na.rm = na.presence)
    last.month  = max(x.anual$month, na.rm = na.presence)
    x.daily.per.month <- list()
    date.list <- list()
    for (j in first.month:last.month) {
      x.month <- x.anual[x.anual$month == j, ]
      # It takes the j month df and creates a k df for each day == k
      first.day = min(x.month$day, na.rm = na.presence)
      last.day  = max(x.month$day, na.rm = na.presence)
      x.value.max <- list()
      x.value.min <- list()
      x.value <- list()
      date.x <- list()
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
        date.x[k] <- x.day[ , ncol.date][1] 
      } # end cicle day
      x.daily.per.month[[j]] <- unlist(x.value)
      date.list[[j]]         <- date.x     
    } # end cicle month
    x.daily.per.years[[c]] <- unlist(x.daily.per.month)
    date.tot[[c]]          <- unlist(date.list)    
    c = c + 1
    daily.data <- as.data.frame(unlist(x.daily.per.years))
    daily.data <- cbind.data.frame(unlist(date.tot), daily.data)
    colnames(daily.data) <- c("date", "value")
    daily.data$date <- as.Date.numeric(daily.data$date, origin = "1970-01-01")
  }
  return(daily.data)
}
#
# 1) a) Daily accumulated rainfall
d_mm_ac <- HourlyToDaily(data.x = calera_organized, ncol.obs = 2, na.presence = TRUE, value = "accumulated", ncol.date = 1)
head(d_mm_ac)
#
plot(x = d_mm_ac$date,
     y = d_mm_ac$value, 
     type = "l", 
     xlab = "Date", 
     ylab = "daily accumulated precipitation (mm)")
# 
# 1) b) Daily mean temperature (°C)
d_t_mean <- HourlyToDaily(data.x = calera_organized, ncol.obs = 3, na.presence = TRUE, value = "mean", ncol.date = 1)
head(d_t_mean)
#
plot(x = d_t_mean$date, y = d_t_mean$value, 
     type = "l", xlab = "Date", 
     ylab = "daily mean temperature (°C)")
#
# 2) Getting monthly data from daily data
# 
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
  monthly.data <- as.data.frame(cbind(unlist(name.month.tot), 
                                      unlist(name.year.tot),
                                      unlist(x.per.month)))
  if (value == "mean") {
    colnames(monthly.data) <- c("month.order", "year", "mean.value")
  } else {
    colnames(monthly.data) <- c("month.order", "year", "accumulated.value")
  }
  return(monthly.data)
}
# 2) a) Monthly accumulated rainfall
d_mm_ac <- OrganizeDateColumns(data.x = d_mm_ac, ncol.date = 1, orders = "ymd", ncol.day = NULL, ncol.month = NULL, ncol.year = NULL)
# 
m_mm_ac <- MonthlyValue(data.x = d_mm_ac, ncol.obs = 2, value = "accumulated", na.presence = TRUE)
head(m_mm_ac)
# 
# Add ceros so we can make a plot
data.ceros <- as.data.frame(cbind(1:9, rep(x = 2007, times = 9), rep(x = 0, times = 9)))
colnames(data.ceros) <- c("month.order", "year", "accumulated.value")
m_mm_ac <- rbind.data.frame(data.ceros, m_mm_ac)
head(m_mm_ac)
#
# 2) a) Monthly mean temperature
d_t_mean <- OrganizeDateColumns(data.x = d_t_mean, ncol.date = 1, orders = "ymd", ncol.day = NULL, ncol.month = NULL, ncol.year = NULL) 
#
m_t_mean <- MonthlyValue(data.x = d_t_mean, ncol.obs = 2, value = "mean", na.presence = TRUE)
head(m_t_mean)
#
## Add NA so we can make a plot 
data.nas <- as.data.frame(cbind(1:9, rep(x = 2007, times = 9), rep(x = NA, times = 9)))
colnames(data.nas) <- c("month.order", "year", "mean.value")
m_t_mean <- rbind.data.frame(data.nas, m_t_mean)
head(m_t_mean)
#
#
# plot with monthly data
plot.calera <- function() {
  parameters.1 <- par(mfrow = c(4, 3),
                    mar = c(3,3,3,1.5),
                    oma = c(3,3,3,3))
  for (i in seq(from = unique(m_mm_ac$year)[1], 
                to = unique(m_mm_ac$year)[length(unique(m_mm_ac$year))])) {
    barplot(height = m_mm_ac[m_mm_ac$year == i, 3], 
            names.arg = m_mm_ac[m_mm_ac$year == i, 1], 
            col = "lightgreen", 
            axes = TRUE, 
            main = i,
            cex.main = 0.8,
            ylim = c(0, 140), 
            xlim = c(0,12), 
            space = 0)
    lines(y = (m_t_mean[m_t_mean$year == i, 3]), 
          ylim = c(0, 70), 
          x = ((m_mm_ac[m_mm_ac$year == i, 1]) - 0.5), 
          type = "o", 
          col = "darkorange", 
          lwd = 2, 
          pch = 16)
    axis(side = 4)
  }
  title(main = "Accumulated rainfalls and temperature per month in La Calera", 
        cex.main = 1.3,
        xlab = "Months", 
        cex.lab = 1.5,
        ylab = "Monthly accumulated precipitation",
        outer = TRUE,
        line = 0)
  mtext(text = "Monthly mean temperature °C",
        side = 4,
        cex = 1,
        outer = TRUE,
        line = 1.2)
  par(parameters.1)
}
plot_calera <- plot.calera()

