# set your working directory
setwd("/home/josep/Documents/biopob/gen")
source("floracio.R")

regressioLineal <- function(xlab, ylab, x, y) {
  plot(x, y, xlab = xlab, ylab = ylab, cex.lab = 1.3)
  lm1 <- lm(y ~ x, na.action = na.omit)
  print(summary(lm1))
  lmLegend(summary(lm1), withIntercept = TRUE)
  abline(lm1)
}

regressioQuadratica <- function(xlab, ylab, x, y) {
  lm2 <- lm(y ~ x + I(x^2), na.action = na.omit)
  print(summary(lm2))
  newdat = data.frame(x = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE)))
  newdat$pred = predict(lm2, newdata = newdat)
  plot(x=x, y=y, pch = 16, xlab = xlab, ylab = ylab, cex.lab = 1.3)
  with(newdat, lines(x = x, y = pred))
  lmLegend(summary(lm2))
}

###################
### TEMPERATURA ###
###################

pdf("regressions_temperatura_vs_temps.pdf")

# cada mes per separat
for (colname in colnames(data[,15:20])) {
  regressioLineal("Any", colname, data$Any, data[[colname]])
  regressioQuadratica("Any", colname, data$Any, data[[colname]])
}

# temperatura mitjana anual
mitjanaTempAnual <- (data$temp_gen + data$temp_feb + data$temp_mar + data$temp_abr + data$temp_mai + data$temp_jun) / 6
regressioLineal("Any", "Temperatura mitjana anual", data$Any, mitjanaTempAnual)
regressioQuadratica("Any", "Temperatura mitjana anual", data$Any, mitjanaTempAnual)

dev.off()

####################
### PRECIPITACIO ###
####################

pdf("regressions_precipitacio_vs_temps.pdf")

# cada mes per separat
for (colname in colnames(data[,21:26])) {
  regressioLineal("Any", colname, data$Any, data[[colname]])
  regressioQuadratica("Any", colname, data$Any, data[[colname]])
}

# precipitacio acumulada anual
precAnual <- data$prec_gen + data$prec_feb + data$prec_mar + data$prec_abr + data$prec_mai + data$prec_jun
regressioLineal("Any", "Precipitació anual acumulada", data$Any, precAnual)

regressioQuadratica("Any", "Precipitació anual acumulada", data$Any, precAnual)

dev.off()

#########################
### FLORACIO ###
#########################

pdf("regressions_floracio_vs_temps.pdf")

# cada especie per separat
for (colname in colnames(data[,8:14])) {
  regressioLineal("Any", colname, data$Any, data[[colname]])
  regressioQuadratica("Any", colname, data$Any, data[[colname]])
}

# Regressions amb PC
source("pcas.R")

anys <- c(data$Any[1:55], data$Any[57:59]) # eliminem any 2000 (NA)
# regressio lineal floracio vs temps
regressioLineal("Any", "Floracio (PC1)", anys, floracio$pc1)
# p-value 0.48, Adj Rsq -0.008, pendent -0.0122
# pendent negatiu sense significacio

# regressio quadratica
regressioQuadratica("Any", "Floracio (PC1)", anys, floracio$pc1)
# p-value 0.0016, Adj Rsq 0.18
# regressio significativa

dev.off()
