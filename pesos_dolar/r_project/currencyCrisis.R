load("currencyCrisis.RData")

pesoDollarDataSet <- read.csv("../data/pesoDollarDataSet.csv")
# Converting to an object of class date 
pesoDollarDataSet$fecha <- as.Date(pesoDollarDataSet$fecha, "%Y-%m-%d")
str(pesoDollarDataSet)
# Daily Percentage Change ----------------------------------------------------
# What's the daily percentage change of the exchange rate ARS/USD? 
DPC <- function(input_data, index){
  variation <- list(0)
  n_iteration <- 2
  for (i in seq(1,length(input_data[,index])-1,1)){
    n_interval <- input_data[i, index]
    nk_interval <- input_data[i+1, index]
    variation[[n_iteration]] <- ((nk_interval-n_interval)/n_interval)*100
    n_iteration <- n_iteration + 1
  }
  output_data <- input_data
  output_data$variation <- unlist(variation)
  return(output_data)
}
  
variation <- DPC(input_data = pesoDollarDataSet,index = 2)

plot(variation$fecha, variation$variation, type="p", 
     col=variation$Presidencia,
     cex=0.2)

which(variation$variation == max(variation$variation)) #3562
plot(variation$fecha[3512:3612], variation$variation[3512:3612], type="l")
plot(variation$fecha[1:300], variation$variation[1:300], type="l")

variation <- porcentualVariation(input_data = dataCurrency,index = 2, k_days = 5 )
plot(c(1:length(variation)), variation, type="l")

which(dataDolar$fecha == "2015-12-1")
cambio_3 <- cambio(dataDolar$divisa_venta, 10, dif=T)
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
