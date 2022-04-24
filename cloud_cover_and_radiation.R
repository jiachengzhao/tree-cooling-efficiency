## ----------------------------------
# Title: Cloud cover and radiation
# Objective: Analyzing the correlation between cloud cover and solar/thermal radiation
# Created by: Jiacheng Zhao
# Created on: 2022-04-24
# Copyright (c) Jiacheng Zhao, 2021
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------


# data ----
data.cloud = data.table(
  id = re$id, # city id
  lon = re$lon, # longitude
  lat = re$lat, # latitude
  region = re$region, # region
  country = re$country, # country
  cloud = apply(re[, grep('cloud_fraction', names(re), value = F), with = F], 1, mean, na.rm = T), # cloud fraction [-]
  solar = apply(re[, grep('^solar_radiation', names(re), value = F), with = F], 1, mean, na.rm = T), # solar radiation [W/m^2]
  thermal = apply(re[, grep('thermal', names(re), value = F), with = F], 1, mean, na.rm = T) # thermal radiation [W/m^2]
)
data.cloud = data.cloud[complete.cases(data.cloud)]
# cloud cover bins
data.cloud[, 'bins' := cut(cloud, breaks = c(0.1, seq(0.3, 1, 0.1)))]
# mean and sd
b = data.cloud[, .(mean.solar = mean(solar), mean.thermal = mean(thermal), sd.solar = sd(solar), sd.thermal = sd(thermal)), by = bins][order(bins, decreasing = F)]
b[, b := c(0.2, seq(0.35, 0.95, 0.1))]


# plotting ----
# colors
cols = c('yellow', 'orangered2')
# par
mypar(
  cex.axis = 1.1,
  oma = c(4, 3, 4, 5),
  # pty = 's',
  tck = 0.017
)
# base plot
baseplotframe(
  b[, .(b, mean.solar)], type = 'n',
  sec.y = T,
  at2 = seq(100, 400, 50), at4 = seq(100, 400, 50), at4.label = seq(180, 480, 50),
  xlim = c(0.1, 1.1), ylim = c(100, 400),
  mgp1 = c(3, 0.05, 0), mgp2 = c(3, 0.3, 0),
  ann = F
)
# sd bar
arrows(
  b$b,
  b$mean.solar + b$sd.solar,
  b$b,
  b$mean.solar - b$sd.solar,
  lwd = 0.1, angle = 90, code = 3, length = 0.03
)
arrows(
  b$b,
  b$mean.thermal - 80 + b$sd.thermal,
  b$b,
  b$mean.thermal - 80 - b$sd.thermal,
  lwd = 0.1, angle = 90, code = 3, length = 0.03
)
# mean point
points(mean.solar ~ b, data = b, pch = 21, col = 'black', bg = cols[1], cex = 1.5)
points(mean.thermal - 80 ~ b, data = b, pch = 21, col = 'black', bg = cols[2], cex = 1.5)
# smooth line
lines(smooth.spline(b$b, b$mean.solar, spar = 0.5), lty = 'longdash')
lines(smooth.spline(b$b, b$mean.thermal - 80, spar = 0.5), lty = 'longdash')
# text
mtext(1, text = 'Cloud cover', line = 1.6, cex = 1.1)
mtext(2, text = expression('Solar radiation (W/m'^2 * ')'), line = 2.4, cex = 1.1, las = 0)
mtext(4, text = expression('Thermal radiation (W/m'^2 * ')'), line = 2.5, cex = 1.1, las = 0)
# legend
legend(
  'topleft',
  legend = c(
    'Solar radiation',
    'Thermal radiation'
  ),
  pch = 21,
  pt.bg = cols,
  pt.cex = 1.5,
  cex = 1, bty = 'n',
  y.intersp = 1.3,
  inset = c(0.02, 0.02)
)