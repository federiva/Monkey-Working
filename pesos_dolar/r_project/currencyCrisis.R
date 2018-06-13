load("currencyCrisis.RData")

pesoDollarDataSet <- read.csv("../data/pesoDollarDataSet.csv")
# Converting to an object of class date 
pesoDollarDataSet$fecha <- as.Date(pesoDollarDataSet$fecha, "%Y-%m-%d")
str(pesoDollarDataSet)
# Daily Percentage Change ----------------------------------------------------
# What's the daily percentage change of the exchange rate ARS/USD? 
DPC <- function(input_data, index){
  # input_data: is a data.frame that contains the data used to calculate the 
  # Daily Percentage Change
  # index: is a Numeric equal to the column number of the dataframe which
  # contains the data with the values 
  # Returns a duplicated of the input dataframe plus one column called variation
  # with the result of the applied formula for every pair of values
  ####
  # Set an empty list 
  variation <- list()
  # For every value from (i=1 to k=N-1) in the input vector apply the following
  # formula ((Pi+1 - Pi) / Pi) * 100 
  for (i in seq(1,length(input_data[,index])-1,1)) {
    variation[[i]] <- ((input_data[i+1, index]-input_data[i, index])/input_data[i, index])*100
  }
  # In order to get the same length as the input data we add an NA in the last value that can't be computed 
  variation[[i+1]] <- NA
  #duplicate the input dataframe, modify and return a dataframe
  output_data <- input_data
  output_data$variation <- unlist(variation)
  return(output_data)
}
# Run the function  
dailyPercentageChange <- DPC(input_data = pesoDollarDataSet,index = 2)


# Plotting ----------------------------------------------------------------
# Choose the colors
colores <- c("orange3",  "steelblue4", "gray20", "red","yellowgreen")
# Set the colors
palette(colores)
# save the plot as a png file
png(filename = "scatterplot_DPC_1.png",
    width    = 17,
    height   = 13,
    units    = "cm",
    res      = 300 )
# Scatterplot
plot(x        = dailyPercentageChange$fecha,
     y        = dailyPercentageChange$variation,
     ylab     = "Percentage Change (%)",
     xlab     = "Date",
     main     = "Daily percentage change (DPC) for the exchange rate ARS/USD \n 
     in the period 2002-01-11 to 2018-06-05",
     cex.main = 1.0,
     type     = "o",
     lty      = 3,
     col      = dailyPercentageChange$Presidencia,
     cex      = 0.7)
# Add legend
legend("topleft",
       legend  = levels(dailyPercentageChange$Presidencia),
       col     = 1:5,
       lwd     = c(3,3,3,3),
       cex     = 0.6,
       box.lty = 0)
dev.off()


# Top ten  ----------------------------------------------------------------

topTenIncrease <- dailyPercentageChange[order(dailyPercentageChange$variation, decreasing = T),][1:10,]

# How much is the percentage of these increases? 
# save the plot as a png file
png(filename = "HistogramTopTenDPC.png",
    width    = 17,
    height   = 13,
    units    = "cm",
    res      = 300 )
#Plot the histogram
hist(x = topTenIncrease$variation,
     ylab = "Frequency",
     xlab = "% DPC increase",
     col = "gray70", 
     main = "")
dev.off()
# Stacked barplot with counts according to factor President
# we need a new data.frame with variables as presidents and intervals given
# in the previous histogram as rows
countNumberInBreaks <- function() {
  #create an empty dataframe with row names
  dataOut <- data.frame(row.names = c("0-10", "10-20", "20-30","30-40", "40-50"))
  #for each president in the dataframe
  for (president in levels(topTenIncrease$Presidencia)) {
    # create an empty named numeric vector
    president_out <- c("0-10" = 0, "10-20" = 0, "20-30" = 0,"30-40" = 0, "40-50" = 0)
    for (each_row in c(1:nrow(topTenIncrease))) {
      # If the president in the row is equal to the president evaluated then
      # enter in a test of each of the breaks
      if (topTenIncrease[each_row, "Presidencia"] == president) {
        if (topTenIncrease[each_row,"variation"] > 0 && topTenIncrease[each_row,"variation"] <= 10) {
          president_out["0-10"] <- president_out["0-10"] + 1
          } else if (topTenIncrease[each_row,"variation"] > 10 && topTenIncrease[each_row,"variation"] <= 20) {
          president_out["10-20"] <- president_out["10-20"] + 1
          } else if (topTenIncrease[each_row,"variation"] > 20 && topTenIncrease[each_row,"variation"] <= 30) {
          president_out["20-30"] <- president_out["20-30"] + 1
          } else if (topTenIncrease[each_row,"variation"] > 30 && topTenIncrease[each_row,"variation"] <= 40) {
          president_out["30-40"] <- president_out["30-40"] + 1
          } else if (topTenIncrease[each_row,"variation"] > 40 && topTenIncrease[each_row,"variation"] <= 50) {
          president_out["40-50"] <- president_out["40-50"] + 1
          }
      }
    }
    # Add a column to the dataOut object
    dataOut <- cbind(dataOut, president_out)
  }
  # naming the columns of the output dataframe
  colnames(dataOut) <- levels(topTenIncrease$Presidencia)
  return(dataOut)
}

# Run the function 
topTenIncreaseCountedByPresidents <- countNumberInBreaks()
# plotting  ---------------------------------------------------------------
# save the plot as a png file
png(filename = "StackedBarplotTopTenIncreaseByPresidents.png",
    width    = 17,
    height   = 13,
    units    = "cm",
    res      = 300 )
# Choose the colors
colores <- c("orange3",  "steelblue4", "gray20", "red","yellowgreen")
# Set the colors
palette(colores)
# Barplot
barplot(height = t(as.matrix(topTenIncreaseCountedByPresidents)), 
        col    = 1:5, 
        ylab   = "Frequency", 
        xlab   = "% DPC increase", 
        border = NA)
# Add legend
legend("topright",
       legend  = c("Macri", "Duhalde", "Fernandez 2"),
       # legend  = levels(dailyPercentageChange$Presidencia),
       col     = c(4,3,2),
       cex     = 1,
       pch = 15,
       box.lty = 0)
dev.off()

colores <- c(CFK1="orange3", NK="yellowgreen", CFK2="steelblue4", MM="red",  OTRO="gray20")
barplot(height = sort(table(topTenIncrease$Presidencia)), 
        col=colores,
        xlab = "President",
        ylab = "Frecuency",names.arg = c("Fernandez 1", "Kirchner", "Fernandez 2", "Macri", "Duhalde")
        )

png(filename = "Grafico_Dolares_Ardilla.png", width = 18, height = 15,units = "cm", res = 300 )
par(mfrow=c(2,1))
plot(dataDolar$fecha, cambio_3, 
     type = "l", 
     xlab="Fecha",
     ylab="Tasa de cambio en 10 dias")
abline(v=dataDolar$fecha[3552], lty=3)
plot(dataDolar, 
     type ="l", 
     xlab="Fecha",
     ylab="Pesos por dólar (divisa)")
abline(v=dataDolar$fecha[3552], lty=3)
dev.off()


plotPNGs <- function(data_input, steps){
  iteracionN <- 1
  for (i in seq(1,nrow(data_input),steps)){
    filenameOut <- paste("./pngs/file_",formatC(iteracionN,width=5, flag=0),".png", sep="")
    print(paste("Graficando:", filenameOut))
    png(filename = filenameOut, width = 8, height = 6,units = "cm", res = 300 )
    plot(data_input[1:i,],
         xlim=c(min(data_input[,1]), max(data_input[,1])),
         ylim=c(min(data_input[,2]), max(data_input[,2])),
         type = "l", 
         xlab="Fecha",
         ylab="Pesos por dólar",
         cex.axis=0.5,
         cex.lab=0.5, 
         las = 3)
    dev.off()
    iteracionN <- iteracionN + 1
  }
}
plotPNGs(dataDolar[1:20,], 5)
plotPNGs(dataDolar, 10)


save.image(file =  "currencyCrisis.RData")
