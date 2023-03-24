

# set your working directory
setwd("D:/Users/Documentos/biopob/gen")

source("functions.R")
data <- read.table("BioPob_temporal_2023.csv",sep=";",dec=",",header=T)

# tests de normalitat

hist(data$FL_citrus.sinensis, breaks = 50)
shapiro.test(data$FL_citrus.sinensis) # p-value 0.04 uiuiui

hist(data$FL_prunus.dulcis, breaks = 50)
shapiro.test(data$FL_prunus.dulcis) # p-value 0.6 GOOD

hist(data$FL_prunus.armeniaca, breaks = 50)
shapiro.test(data$FL_prunus.armeniaca) # p-value 0.43 GOOD

hist(data$FL_cydonia.oblonga, breaks = 50)
shapiro.test(data$FL_cydonia.oblonga) # p-value 0.28

data$FL_malus.domestica[data$FL_malus.domestica > 150] <- NA
hist(data$FL_malus.domestica, breaks = 50)
shapiro.test(data$FL_malus.domestica) # p-value 0.33 GOOD

hist(data$FL_juglans.regia, breaks = 50)
shapiro.test(data$FL_juglans.regia) # p-value 0.21 WAY GOOD

hist(data$FL_vitis.vinifera, breaks = 50)
shapiro.test(data$FL_vitis.vinifera) # p-value 0.4 good

# eliminar outlier febrer
data$temp_feb[data$temp_feb > 15] <- NA



# Esta augmentant la temperatura?

plot(data$Any, data$temp_gen, xlab = "Any", ylab = "Temperatura al gener", cex.lab = 1.3)
lmGener <- lm(data$temp_gen ~ data$Any) # p-value 0.14, adj Rsq = 0.02, pendent 0.014
lmLegend(summary(lmGener))
abline(lmGener)
# pendent positiu sense significacio

plot(data$Any, data$temp_feb, xlab = "Any", ylab = "Temperatura al febrer", cex.lab = 1.3)
lmFebrer <- lm(data$temp_feb ~ data$Any) # p-value 0.4, adj Rsq = -0.000512, pendent 0.0097
lmLegend(summary(lmFebrer))
abline(lmFebrer)
# pendent positiu sense significacio

plot(data$Any, data$temp_mar, xlab = "Any", ylab = "Temperatura al març", cex.lab = 1.3)
lmMarc <- lm(data$temp_mar ~ data$Any) # p-value 0.29, adj Rsq = 0.00239, pendent 0.011
lmLegend(summary(lmMarc))
abline(lmMarc)
# pendent positiu sense significacio

plot(data$Any, data$temp_abr, xlab = "Any", ylab = "Temperatura a l'abril", cex.lab = 1.3)
lmAbril <- lm(data$temp_abr ~ data$Any) # p-value 0.025, adj Rsq = 0.0687, pendent -0.0235
lmLegend(summary(lmAbril))
abline(lmAbril)
# no se que dir xd

plot(data$Any, data$temp_mai, xlab = "Any", ylab = "Temperatura al maig", cex.lab = 1.3)
lmMaig <- lm(data$temp_mai ~ data$Any) # p-value 0.74, adj Rsq = -0.01, pendent -0.00362
lmLegend(summary(lmMaig))
abline(lmMaig)
# bueno...

plot(data$Any, data$temp_jun, xlab = "Any", ylab = "Temperatura al juny", cex.lab = 1.3)
lmJuny <- lm(data$temp_jun ~ data$Any) # p-value 0.67, adj Rsq = -0.0144, pendent 0.00397
lmLegend(summary(lmJuny))
abline(lmJuny)
# el canvi climatic no existeix xd

# Evolucio temperatures mitjanes "anuals"

mitjanaTempAnual <- (data$temp_gen + data$temp_feb + data$temp_mar + data$temp_abr + data$temp_mai + data$temp_jun) / 6
plot(data$Any, mitjanaTempAnual,
        ylab = "Temperatura mitjana anual", xlab = "Any")
lmMitjanaTemp <- lm(mitjanaTempAnual ~ data$Any) # p-value 0.73, adj Rsq = -0.01, pendent 0.00189
lmLegend(summary(lmMitjanaTemp))
abline(lmMitjanaTemp)
# pendent "positiu" sense significacio



# Esta plovent menys???

plot(data$Any, data$prec_gen, xlab = "Any", ylab = "Precipitacio acumulada al gener", cex.lab = 1.3)
lmGener <- lm(data$prec_gen ~ data$Any) # p-value 0.72, adj Rsq = -0.0152, pendent 0.11
lmLegend(summary(lmGener))
abline(lmGener)
# ...

plot(data$Any, data$prec_feb, xlab = "Any", ylab = "Precipitacio acumulada al febrer", cex.lab = 1.3)
lmFebrer <- lm(data$prec_feb ~ data$Any) # p-value 0.32, adj Rsq = -000000, pendent -0.273
lmLegend(summary(lmFebrer))
abline(lmFebrer)
# pendent negatiu sense significacio...

plot(data$Any, data$prec_mar, xlab = "Any", ylab = "Precipitacio acumulada al març", cex.lab = 1.3)
lmMarc <- lm(data$prec_mar ~ data$Any) # p-value 0.016, adj Rsq = 0.081, pendent -0.591
lmLegend(summary(lmMarc))
abline(lmMarc)
# pendent negatiu amb significacio!

plot(data$Any, data$prec_abr, xlab = "Any", ylab = "Precipitacio acumulada a l'abril", cex.lab = 1.3)
lmAbril <- lm(data$prec_abr ~ data$Any) # p-value 0.4, adj Rsq = -0.00481, pendent 0.169
lmLegend(summary(lmAbril))
abline(lmAbril)
# abril aguas mil

plot(data$Any, data$prec_mai, xlab = "Any", ylab = "Precipitacio acumulada al maig", cex.lab = 1.3)
lmMaig <- lm(data$prec_mai ~ data$Any) # p-value 0.94, adj Rsq = -0.00174, pendent -0.01333
lmLegend(summary(lmMaig))
abline(lmMaig)
# pel maig cada dia un raig

plot(data$Any, data$prec_jun, xlab = "Any", ylab = "Precipitacio acumulada al juny", cex.lab = 1.3)
lmJuny <- lm(data$prec_jun ~ data$Any) # p-value 0.26, adj Rsq = 0.00492, pendent -0.16
lmLegend(summary(lmJuny))
abline(lmJuny)
# pendent negatiu sense significacio


# Evolucio precipitacions mitjanes "anuals"

mitjanaPrecAnual <- (data$prec_gen + data$prec_feb + data$prec_mar + data$prec_abr + data$prec_mai + data$prec_jun) / 6
plot(data$Any, mitjanaPrecAnual,
     ylab = "Precipitacio acumulada mitjana anual", xlab = "Any")
lmMitjanaPrec <- lm(mitjanaPrecAnual ~ data$Any) # p-value 0.26, adj Rsq = 0.00492, pendent -0.16
lmLegend(summary(lmMitjanaPrec))
abline(lmMitjanaPrec)
# pendent negatiu sense significacio
