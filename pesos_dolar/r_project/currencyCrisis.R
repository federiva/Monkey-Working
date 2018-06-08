

dataCurrency <- read.csv("../data/data_peso_dolar.csv")
# Converting to an object of class date 
dataCurrency$fecha <- as.Date(dataCurrency$fecha, "%d/%m/%Y")
# As the days were mostly in weekends (due to Argentinian's elections
# are made in sunday), we changed the original date 
# to the nearest week day because the data only has weekdays .
asume_nestor <- as.Date("2003-05-26")
asume_cristina_1 <- as.Date("2007-12-10")
asume_cristina_2 <- as.Date("2011-12-12")
asume_miauri <- as.Date("2015-12-11")
# We wrote a function using the limit dates previously used in order to add a new column of factors by president in our data.frame 
factorPresidencias <- function(date_vectors){
  # Initialize an empty list
  lista_factor_presidencias <- list()
  # Initialize a counter
  i <- 1 
  for (datos in date_vectors){
    if (datos < asume_nestor){
      lista_factor_presidencias[[i]] <- "OTRO"
    }
    else if(datos >= asume_nestor & datos < asume_cristina_1){
      lista_factor_presidencias[[i]] <- "NK"
    }
    else if(datos >= asume_cristina_1 & datos < asume_cristina_2){
      lista_factor_presidencias[[i]] <- "CFK1"
    }
    else if(datos >= asume_cristina_2 & datos < asume_miauri){
      lista_factor_presidencias[[i]] <- "CFK2"
    }
    else if(datos >= asume_miauri){
      lista_factor_presidencias[[i]] <- "MM"
    }
    i <- i + 1
  }
  return(unlist(lista_factor_presidencias))
}
factor_presidente <- factorPresidencias(dataCurrency$fecha)
# We add the column as factor to the dataframe
dataCurrency$Presidencia <- factor_presidente
dataCurrency$Presidencia <- as.factor(dataCurrency$Presidencia)
# Explore the dataset
str(dataCurrency)

# Porcentual variation ----------------------------------------------------


# What's the daily porcentual variation of the exchange rate? 
# And in the n-th of k days period relative to the n-1 period of k days?
porcentualVariation <- function(input_data, index, k_days, dif = T){
  k <- k_days - 1
  result <- list()
  for (i in 1:c(length(datos))){
    if (i <= (length(datos)-ventana)){
      if (dif == F){
        res_parc <- sum(c(datos[i]:datos[i+ventana-1]))
      }
      if (dif == T){
        res_parc <- datos[i+ventana-1] - datos[i]}
    }
    resultado[[i]] <- res_parc/ventana
  }
  return(resultado)
}
#intervalo i 
(i) : (i + k) 
#intervalo i + k 
(i + k + 1) : (i + k + 1 + k)

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
