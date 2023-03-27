#############################################################################################
#############################################################################################
         #####							BIOLOGIA DE POBLACIONS 2021								#####
#############################################################################################
#############################################################################################

### Objectiu 1. Detectar tendencies temporals tant de les variables climatiques com de les fenologiques

# Explorar la base de dades
# Buscar valors extrems
# Detectar i corregir possibles errors
# Explorar la distribucio de les variables
# Testar si segueixen una distribucio normal
# Fer una analisi de regressio lineal i curvilinia per a totes les variables, tant climatiques com fenologiques


# Script de R

#############################################################################################

# Explorar la base de dades
# Buscar valors extrems
# Detectar i corregir possibles errors
# Explorar la distribucio de les variables
# Testar si segueixen una distribucio normal

setwd("C:/Users/far.aules/Downloads") # working directory
xdata <- read.table("BioPob_temporal_2023.csv",sep=";",dec=",",header=T) # llegir el fitxer / incorporar-lo a l'objecte "xdata2 (dataframe)
str(xdata) #mirar la estructura de "xdata"
head(xdata)

# Explorem la base de dades per detectar valors erronis, valors missing i normalitat
summary(xdata) #resum de les dades
summary(xdata$ARR_hirundo.rustica) # resum per a la segona variable (ARR_hirundo.rustica) 

hist(xdata$ARR_hirundo.rustica, breaks = 50) #histograma

hist(xdata$AP_apis.mellifera, breaks = 20) #histograma

boxplot(xdata$ARR_hirundo.rustica, main = "Boxplot of ARR_hirundo.rustica")
		# que veiem:
			# Q1 (25%), Q2 (mediana, 50%), Q3 (75%), RI =Rang Interquartilic, Valors Min/Max (<1.5 RI), outlayers
shapiro.test(xdata$ARR_hirundo.rustica) #test de normalitat
shapiro.test(xdata$temp_feb) #test de normalitat
		# si p < 0.05 hi ha diferencies significatives respecte d'una distribucio normal, i.e. no segueix normalitat

# QQ-plot		
qqnorm(xdata$ARR_hirundo.rustica, main = "Normal QQ Plot - ARR_hirundo.rustica")
qqline(xdata$ARR_hirundo.rustica, col = "red")

qqnorm(xdata$temp_feb, main = "Normal QQ Plot - temp_feb")
qqline(xdata$temp_feb, col = "red")

# Eliminacio dels valors extrems
xdata$ARR_hirundo.rustica[xdata$ARR_hirundo.rustica > 160] <- NA
xdata$FL_malus.domestica[xdata$FL_malus.domestica > 150] <- NA
xdata$temp_feb[xdata$temp_feb > 15] <- NA
# o
xdata$ARR_hirundo.rustica[xdata$ARR_hirundo.rustica == 186.39] <- 86.39

### Guardem dades en un nou arxiu
write.table(xdata, file = "C:/Users/far.aules/Downloads/BioPob_temporal_2023_sense_errors.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ",", row.names = TRUE,  col.names = TRUE, qmethod = c("escape", "double"))             

#### Obrim el nou arxiu sense "errors":
setwd("C:/Users/far.aules/Downloads/") #set working directory
xdata <- read.table("BioPob_temporal_2023_sense_errors.csv",sep=";",dec=",",header=T) # llegir el fitxer / incorporar-lo a l'objecte "xdata2 (dataframe)
str(xdata) #mirar la estructura de "xdata"

#############################################################################################
# Fer una analisi de regressio lineal i quadratica per a totes les variables, tant climatiques com fenologiques

lmLegend <- function(lm) {
  summary <- summary(lm)
  summary
  rvalue <- summary$adj.r.squared
  pvalue <- summary$coefficients[2,4]
  rp = vector('expression',2)
  rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                     list(MYVALUE = format(rvalue,dig=3)))[2]
  rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                     list(MYOTHERVALUE = format(pvalue, digits = 2)))[2]
  legend('topleft', legend = rp, bty = 'n')
}

### temp_gen vs. Any
# Regressio lineal
lm0 <- lm(temp_gen ~ 1 + Any, data=xdata) # regressio lineal
summary(lm0)

# grafiquem la regressio lineal
plot(x=xdata$Any, y=xdata$temp_gen, pch = 16, main="lm0")
abline(lm(xdata$temp_gen ~ 1 + xdata$Any))
lmLegend(lm0)

# Regressio quadratica
lm1 <- lm(temp_gen ~ 1 + Any + I(Any^2), data=xdata) # regressio quadratica
summary(lm1)

# grafiquem la regressio quadratica
newdat = data.frame(Any = seq(min(xdata$Any, na.rm = TRUE), max(xdata$Any, na.rm = TRUE), length.out = 100))
newdat$pred = predict(lm1, newdata = newdat)
plot(x=xdata$Any, y=xdata$temp_gen, pch = 16, main="lm1")
with(newdat, lines(x = Any, y = pred))
lmLegend(lm1)


### ARR_hirundo.rustica vs. Any
# Regressio lineal
lm2 <- lm(ARR_hirundo.rustica ~ 1 + Any, data=xdata) # regressio lineal
summary(lm2)

# grafiquem la regressio lineal
plot(x=xdata$Any, y=xdata$ARR_hirundo.rustica, pch = 16, main="lm2")
abline(lm(xdata$ARR_hirundo.rustica ~ 1 + xdata$Any))
lmLegend(lm2)

# Regressio quadratica
lm3 <- lm(ARR_hirundo.rustica ~ 1 + Any + I(Any^2), data=xdata) # regressio quadratica
summary(lm3)

# grafiquem la regressio quadratica
newdat = data.frame(Any = seq(min(xdata$Any, na.rm = TRUE), max(xdata$Any, na.rm = TRUE), length.out = 100))
newdat$pred = predict(lm3, newdata = newdat)
plot(x=xdata$Any, y=xdata$ARR_hirundo.rustica, pch = 16, main="lm3")
with(newdat, lines(x = Any, y = pred))
lmLegend(lm3)

#############################################################################################
#############################################################################################

### Fem una reduccio de les dades de floracio mitjanant una analisi de components principals (PCA)
### L'objectiu principal de l'analisi es explicar la maxima quantitat de variancia amb el minim nombre de variables (components principals, PC).

install.packages("stats")
library(stats)

# Seleccionem totes les variables de floracio (i l'Any)
xfloracio <- xdata[,c(1,8:14)]
xfloracio <- xfloracio[complete.cases(xfloracio), ] # eliminem registres amb NA

pca_floracio<-prcomp(~ FL_prunus.dulcis + FL_prunus.armeniaca + FL_cydonia.oblonga + FL_malus.domestica + FL_citrus.sinensis + FL_juglans.regia + FL_vitis.vinifera, data = na.omit(xfloracio), scale = TRUE)
print (pca_floracio) # proporciona els valors pels diferents PCs
plot(pca_floracio, type="l") # variança explicada pels diferents PCs
summary(pca_floracio)

pc1 <- predict(pca_floracio)[,1] #PC1
pc2 <- predict(pca_floracio)[,2] #PC2
biplot(pca_floracio)

xfloracio <- cbind(xfloracio, pc1, pc2)

#PC1
plot(x=xfloracio$pc1,type="n", y=xfloracio$FL_prunus.dulcis, ylim=c(min(xfloracio[,2:8]),max(xfloracio[,2:8])), main="Resposta floracio", ylab = "Floracio")
points(x=xfloracio$pc1, y=xfloracio$FL_prunus.dulcis, pch = 16, col=c("black"))
points(x=xfloracio$pc1, y=xfloracio$FL_prunus.armeniaca, pch = 16, col=c("blue"))
points(x=xfloracio$pc1, y=xfloracio$FL_cydonia.oblonga, pch = 16, col=c("orange"))
points(x=xfloracio$pc1, y=xfloracio$FL_malus.domestica, pch = 16, col=c("red"))
points(x=xfloracio$pc1, y=xfloracio$FL_citrus.sinensis, pch = 16, col=c("yellow"))
points(x=xfloracio$pc1, y=xfloracio$FL_juglans.regia, pch = 16, col=c("brown"))
points(x=xfloracio$pc1, y=xfloracio$FL_vitis.vinifera, pch = 16, col=c("purple"))

#PC2
plot(x=xfloracio$pc2,type="n", y=xfloracio$FL_prunus.dulcis, ylim=c(min(xfloracio[,2:8]),max(xfloracio[,2:8])), main="Resposta floracio", ylab = "Floracio")
points(x=xfloracio$pc2, y=xfloracio$FL_prunus.dulcis, pch = 16, col=c("black"))
points(x=xfloracio$pc2, y=xfloracio$FL_prunus.armeniaca, pch = 16, col=c("blue"))
points(x=xfloracio$pc2, y=xfloracio$FL_cydonia.oblonga, pch = 16, col=c("orange"))
points(x=xfloracio$pc2, y=xfloracio$FL_malus.domestica, pch = 16, col=c("red"))
points(x=xfloracio$pc2, y=xfloracio$FL_citrus.sinensis, pch = 16, col=c("yellow"))
points(x=xfloracio$pc2, y=xfloracio$FL_juglans.regia, pch = 16, col=c("brown"))
points(x=xfloracio$pc2, y=xfloracio$FL_vitis.vinifera, pch = 16, col=c("purple"))

