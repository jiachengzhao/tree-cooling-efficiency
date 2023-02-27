## ----------------------------------
# Title: Standard deviation of elevation
# Objective: To visualize the distribution of the standard deviation of elevation
# Created by: Jiacheng Zhao
# Created on: 2022-07-16
# Copyright (c) Jiacheng Zhao, 2022
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------



par(oma = c(2, 0.5, 5, 0.5))
expe = function(data, min, max) {
  af = approxfun(density(data))
  integrand = function(x) {
    f1 = x
    f2 = af(x)
    f2[is.na(f2)] = 0
    return(f1 * f2)
  }
  return(
    integrate(integrand, min, max)
  )
}
hist(esd$sd, prob = T, breaks = 100, main = '', xlab = 'Standard deviation of elevation (m)', xlim = c(0, 150))
lines(density(esd$sd, adjust = 2), col = 'black', lwd = 1.5)
abline(v = 100, col = 'red', lwd = 2)
e = expe(esd$sd, min(esd$sd), max(esd$sd))
title(substitute(paste(mu, ' = ', value), list(value = sprintf('%.2f', e$value))), adj = 0.16, line = -0.5, font.main = 1, cex.main = 1)
par(opar)