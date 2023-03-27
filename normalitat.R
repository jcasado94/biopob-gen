source(init.R)


# tests de normalitat

hist(data$FL_citrus.sinensis, breaks = 50)
shapiro.test(data$FL_citrus.sinensis) # p-value 0.04 uiuiui

hist(data$FL_prunus.dulcis, breaks = 50)
shapiro.test(data$FL_prunus.dulcis) # p-value 0.6 GOOD

hist(data$FL_prunus.armeniaca, breaks = 50)
shapiro.test(data$FL_prunus.armeniaca) # p-value 0.43 GOOD

hist(data$FL_cydonia.oblonga, breaks = 50)
shapiro.test(data$FL_cydonia.oblonga) # p-value 0.28

hist(data$FL_malus.domestica, breaks = 50) # outlier a extrem dret
shapiro.test(data$FL_malus.domestica) # p-value 0.33 GOOD

hist(data$FL_juglans.regia, breaks = 50)
shapiro.test(data$FL_juglans.regia) # p-value 0.21 WAY GOOD

hist(data$FL_vitis.vinifera, breaks = 50)
shapiro.test(data$FL_vitis.vinifera) # p-value 0.4 good