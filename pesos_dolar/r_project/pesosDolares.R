# Load the environment, only for local use
# load(file = "DataEnvironment.rdata")

# Reading data, formatting, and first plot  ------------------------------------------------------------
# Read data
dataDolar <- read.csv("../data/data_peso_dolar.csv")
# Converting to an object of class date 
dataDolar$fecha <- as.Date(dataDolar$fecha, "%d/%m/%Y")
# Plotting the first plot and saving it as a .png file
# save the plot as a png file
png(filename = "firstPlotDollarPriceVSDate.png", width = 17, height = 12,units = "cm", res = 300 )
plot(x = dataDolar$fecha,
     y = dataDolar$divisa_venta,
     type = "l", 
     xlab="Date",
     ylab="Argentine Peso / US Dollar")
dev.off()
# As the days were mostly in weekends (due to Argentinian's elections
# are made in sunday), we changed the original date 
# to the nearest week day because the data only has weekdays .
asume_nestor <- as.Date("2003-05-26")
asume_cristina_1 <- as.Date("2007-12-10")
asume_cristina_2 <- as.Date("2011-12-12")
asume_macri <- as.Date("2015-12-11")
# We wrote a function using the limit dates previously used in order to add a new column of factors by president in our data.frame 
factorPresidencias <- function(date_vectors){
  # Initialize an empty list
  lista_factor_presidencias <- list()
  # Initialize a counter
  i <- 1 
  for (datos in date_vectors) {
    if (datos < asume_nestor) {
      lista_factor_presidencias[[i]] <- "OTRO"
      } else if (datos >= asume_nestor & datos < asume_cristina_1) {
      lista_factor_presidencias[[i]] <- "NK"
      } else if (datos >= asume_cristina_1 & datos < asume_cristina_2) {
      lista_factor_presidencias[[i]] <- "CFK1"
      } else if (datos >= asume_cristina_2 & datos < asume_macri) {
      lista_factor_presidencias[[i]] <- "CFK2"
      } else if(datos >= asume_macri) {
      lista_factor_presidencias[[i]] <- "MM"
      }
    i <- i + 1
  }
  return(unlist(lista_factor_presidencias))
}
factor_presidente <- factorPresidencias(dataDolar$fecha)
# We add the column as factor to the dataframe
dataDolar$Presidencia <- factor_presidente
dataDolar$Presidencia <- as.factor(dataDolar$Presidencia)
# Explore the dataset
str(dataDolar)
# Plotting ----------------------------------------------------------------

# save the plot as a png file
png(filename = "secondPlotDollarPriceVSDate.png", width = 17, height = 12,units = "cm", res = 300 )
# set the background
par("bg"="white")
# Choose the colors
colores <- c("orange3",  "steelblue4", "red","yellowgreen","gray20")
# Set the colors
palette(colores)
# Plot
plot(x = dataDolar$fecha,
     y = dataDolar$divisa_venta,
     xlab="Date",
     ylab="Argentine Peso / US Dollar",
     type="p",
     col=dataDolar$Presidencia,
     cex=0.25
)
# Add legend
legend("topleft", legend = levels(dataDolar$Presidencia),
       col = 1:5,
       lwd = c(3,3,3,3),
       cex = 0.75,
       box.lty = 0)
dev.off()
# Others ------------------------------------------------------------------

# Save the environment, only for local use
# save.image(file = "DataEnvironment.rdata")
