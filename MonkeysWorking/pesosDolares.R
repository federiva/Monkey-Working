load(file = "DataEnvironment.rdata")

dataDolar <- read.csv("../data_peso_dolar.csv")
plot(c(1:length(dataDolar$fecha)),dataDolar$divisa_venta, type ="l")
dataDolar$fecha <- as.Date(dataDolar$fecha, "%d/%m/%Y")
plot(dataDolar, type="l")
cambio <- function(datos, ventana, dif = T){
  resultado <- list()
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


save.image(file = "DataEnvironment.rdata")
