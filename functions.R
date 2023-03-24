
lmLegend <- function(summary, title) {
  
  rvalue <- summary$adj.r.squared
  pvalue <- summary$coefficients[2,4]
  intercept <- summary$coefficients[1,1]
  slope <- summary$coefficients[2,1]
  
  rp <-  vector('expression', 3)
  rp[1] <- substitute(expression(italic(R)^2 == RVALUE), 
                      list(RVALUE = format(rvalue, digits = 3)))[2]
  rp[2] <- substitute(expression(italic(p) == PVALUE), 
                      list(PVALUE = format(pvalue, digits = 2)))[2]
  rp[3] <- if (intercept > 0) {
    substitute(expression(italic(y) == SLOPE*italic(x) + INTERCEPT),
               list(SLOPE = format(slope, digits = 3),
                    INTERCEPT = format(abs(intercept), digits = 3)))[2]
  } else {
    substitute(expression(italic(y) == SLOPE*italic(x) - INTERCEPT),
               list(SLOPE = format(slope, digits = 3),
                    INTERCEPT = format(abs(intercept), digits = 3)))[2]
  }
  
  legend('topleft', legend = rp, bty = 'n', pt.cex = 1, cex = 1.3)
}
