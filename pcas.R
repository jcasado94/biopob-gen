source("floracio.R")

floracio <- data[,c(1,8:14)]
floracio <- floracio[complete.cases(floracio), ]

pca_floracio<-prcomp(~ FL_prunus.dulcis + FL_prunus.armeniaca + FL_cydonia.oblonga + FL_malus.domestica + FL_citrus.sinensis + FL_juglans.regia + FL_vitis.vinifera, data = na.omit(floracio), scale = TRUE)
print (pca_floracio) # proporciona els valors pels diferents PCs
plot(pca_floracio, type="l") # variança explicada pels diferents PCs
summary(pca_floracio)

pc1 <- predict(pca_floracio)[,1] #PC1
pc2 <- predict(pca_floracio)[,2] #PC2
floracio <- cbind(floracio, pc1, pc2)

plot(x=floracio$pc1,type="n", y=floracio$FL_prunus.dulcis, ylim=c(min(floracio[,2:8]),max(floracio[,2:8])), ylab = "Floracio", xlab = "PC1")
points(x=floracio$pc1, y=floracio$FL_prunus.dulcis, pch = 16, col=c("black"))
points(x=floracio$pc1, y=floracio$FL_prunus.armeniaca, pch = 16, col=c("blue"))
points(x=floracio$pc1, y=floracio$FL_cydonia.oblonga, pch = 16, col=c("orange"))
points(x=floracio$pc1, y=floracio$FL_malus.domestica, pch = 16, col=c("red"))
points(x=floracio$pc1, y=floracio$FL_citrus.sinensis, pch = 16, col=c("yellow"))
points(x=floracio$pc1, y=floracio$FL_juglans.regia, pch = 16, col=c("brown"))
points(x=floracio$pc1, y=floracio$FL_vitis.vinifera, pch = 16, col=c("purple"))
