# set your working directory
setwd("/home/josep/Documents/biopob/gen")

data <- read.table("BioPob_temporal_2023.csv",sep=";",dec=",",header=T)
data$temp_feb[data$temp_feb > 15] <- NA
data$FL_malus.domestica[data$FL_malus.domestica > 150] <- NA


lmLegend <- function(summary, title, withIntercept = FALSE) {
  
  rvalue <- summary$adj.r.squared
  pvalue <- p <- pf(summary$fstatistic[1],summary$fstatistic[2],summary$fstatistic[3],lower.tail=F)
  intercept <- summary$coefficients[1,1]
  slope <- summary$coefficients[2,1]
  
  rp <-  vector('expression', 3)
  rp[1] <- substitute(expression(italic(R)^2 == RVALUE), 
                      list(RVALUE = format(rvalue, digits = 3)))[2]
  rp[2] <- substitute(expression(italic(p) == PVALUE), 
                      list(PVALUE = format(pvalue, digits = 2)))[2]
  if (withIntercept) {
    rp[3] <- if (intercept > 0) {
      substitute(expression(italic(y) == SLOPE*italic(x) + INTERCEPT),
                 list(SLOPE = format(slope, digits = 3),
                      INTERCEPT = format(abs(intercept), digits = 3)))[2]
    } else {
      substitute(expression(italic(y) == SLOPE*italic(x) - INTERCEPT),
                 list(SLOPE = format(slope, digits = 3),
                      INTERCEPT = format(abs(intercept), digits = 3)))[2]
    }
  }
  
  legend('topleft', legend = rp, bty = 'n', pt.cex = 1, cex = 1.3)
}
