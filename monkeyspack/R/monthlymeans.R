monthly.means <- function(DataX, ncol.interest,
                          ncol.date = NULL, ncol.year = NULL,
                          ncol.month = NULL, NA.PRESENCE = FALSE) {
  require(lubridate) # for organize dates
  require(reshape2) # for stack columns
  # To test which columns are present and create new columns when it's necessary.
  if (is.null(ncol.year) == TRUE & is.null(ncol.month) == TRUE) {
    DataX[ , ncol(DataX) + 1] <- month(DataX[ , ncol.date])
    ncol.month <- ncol(DataX)
    DataX[ , ncol(DataX) + 1] <- year(DataX[ , ncol.date])
    ncol.year <- ncol(DataX)
  } else if (is.null(ncol.year) == TRUE & is.null(ncol.month) == FALSE) {
    DataX[ , ncol(DataX) + 1] <- year(DataX[ , ncol.date])
    ncol.year <- ncol(DataX)
  } else if (is.null(ncol.year) == FALSE & is.null(ncol.month) == TRUE) {
    DataX[ , ncol(DataX) + 1] <- month(DataX[ , ncol.date])
    ncol.month <- ncol(DataX)
  } else {
    ncol.year <- ncol.year
    ncol.month <- ncol.month
  }
  # Calculate the mean of the variable of interest
  media_x <- list()
  x_media_year <- data.frame()
  x_media_year[1:12, 1] <- 1:12
  first_year = min(DataX[ , ncol.year])
  last_year  = max(DataX[ , ncol.year])
  # It takes a data's sample and creates a temporary i data.frame for year == i
  for (i in first_year:last_year) {
    x_anual <- DataX[DataX[ , ncol.year] == i, ]
    # It takes a data's sample and creates a temporary i data.frame for year == i
    for (j in 1:12) {
      x_mes <- x_anual[x_anual[ , ncol.month] == j, ]
      # Calculates the mean
      media_x[j] <- mean(x_mes[ , ncol.interest], na.rm = NA.PRESENCE)
    }
    media_men <- unlist(media_x)
    media_men <- as.vector(media_men)
    x_media_year <- cbind(x_media_year, media_men)
  }
  # Renames the columns and stacks them
  colnames(x_media_year) <- c("month.order", first_year:last_year)
  x_media_year <- melt(data = x_media_year, id.vars = 'month.order', variable_name = 'year')
  colnames(x_media_year) <- c("month.order", "year", "mean.value")
  return(x_media_year)
}
