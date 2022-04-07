## ----------------------------------
# Title: Contribution
# Objective: Visualizing the partial contribution of each variable affecting TCE
# Created by: Jiacheng Zhao
# Created on: 2021-11-18
# Copyright (c) Jiacheng Zhao, 2021
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------



# BRT data ----
data.brt = data.table(
  id = re$id, # city id
  lon = re$lon, # longitude
  lat = re$lat, # latitude
  region = re$region, # region
  country = re$country, # country
  tce.10.25 = apply(re[, grep('tce.10.25', names(re), value = F), with = F], 1, mean, na.rm = T), # TCE.10.25 [°C/%]
  tce.20.25 = apply(re[, grep('tce.20.25', names(re), value = F), with = F], 1, mean, na.rm = T), # TCE.20.25 [°C/%]
  tce.30.25 = apply(re[, grep('tce.30.25', names(re), value = F), with = F], 1, mean, na.rm = T), # TCE.30.25 [°C/%]
  tce.10.mean = apply(re[, grep('tce.10.mean', names(re), value = F), with = F], 1, mean, na.rm = T), # TCE.10.mean [°C/%]
  tce.20.mean = apply(re[, grep('tce.20.mean', names(re), value = F), with = F], 1, mean, na.rm = T), # TCE.20.mean [°C/%]
  tce.30.mean = apply(re[, grep('tce.30.mean', names(re), value = F), with = F], 1, mean, na.rm = T), # TCE.30.mean [°C/%]
  cloud = apply(re[, grep('cloud_fraction', names(re), value = F), with = F], 1, mean, na.rm = T), # cloud fraction [-]
  albedo = apply(re[, grep('albedo', names(re), value = F), with = F], 1, mean, na.rm = T), # city albedo [-]
  lai = apply(re[, grep('leaf_area_index', names(re), value = F), with = F], 1, mean, na.rm = T), # MODIS LAI [-]
  lai2 = apply(re[, grep('leaf_area_index2', names(re), value = F), with = F], 1, mean, na.rm = T), # Landsat LAI [-]
  gdp = apply(re[, grep('gross_domestic_product', names(re), value = F), with = F], 1, mean, na.rm = T), # GDP [1e10 USD]
  prec = apply(re[, grep('^precipitation', names(re), value = F), with = F], 1, mean, na.rm = T), # precipitation [mm]
  solar = apply(re[, grep('^solar_radiation', names(re), value = F), with = F], 1, mean, na.rm = T), # solar radiation [W/m^2]
  vpd = apply(re[, grep('^vapor_pressure_deficit', names(re), value = F), with = F], 1, mean, na.rm = T), # VPD [kPa]
  ws = apply(re[, grep('^wind_speed', names(re), value = F), with = F], 1, mean, na.rm = T), # wind speed [m/s]
  mswx_prec = apply(re[, grep('mswx_precipitation', names(re), value = F), with = F], 1, mean, na.rm = T), # precipitation [mm]
  mswx_solar = apply(re[, grep('mswx_solar_radiation', names(re), value = F), with = F], 1, mean, na.rm = T), # solar radiation [W/m^2]
  mswx_vpd = apply(re[, grep('mswx_vapor_pressure_deficit', names(re), value = F), with = F], 1, mean, na.rm = T), # VPD [kPa]
  mswx_ws = apply(re[, grep('mswx_wind_speed', names(re), value = F), with = F], 1, mean, na.rm = T) # wind speed [m/s]
)
data.brt = data.brt[complete.cases(data.brt)]
data.brt = data.brt[cloud < 0.7] # cloud cover should be less than 70%



# BRT using ERA5 and MODIS LAI ----
set.seed(1)
gbms.era5 = dismo::gbm.step(
  data = data.brt, # mask out cities with high cloud cover
  gbm.x = colnames(data.brt)[c(13:14, 16:20)],
  gbm.y = 'tce.10.25',
  family = 'gaussian',
  tree.complexity = 10,
  tolerance = 0.01,
  tolerance.method = 'auto',
  learning.rate = 0.001,
  n.folds = 10,
  bag.fraction = 0.5
)
dismo::gbm.plot(
  gbms.era5,
  smooth = F, rug = F, common.scale = F,
  y.label = 'Partial effect on TCE',
  show.contrib = T,
  plot.layout = c(2, 4)
)
gbms.era5$var.names
bias(data.brt$tce.10.25, gbms.era5$fit)


# BRT using ERA5 and Landsat LAI ----
set.seed(1)
gbms.era5.landsat = dismo::gbm.step(
  data = data.brt, # mask out cities with high cloud cover
  gbm.x = colnames(data.brt)[c(13, 15:20)],
  gbm.y = 'tce.10.25',
  family = 'gaussian',
  tree.complexity = 10,
  tolerance = 0.01,
  tolerance.method = 'auto',
  learning.rate = 0.001,
  n.folds = 10,
  bag.fraction = 0.5
)
dismo::gbm.plot(
  gbms.era5.landsat,
  smooth = F, rug = F, common.scale = F,
  y.label = 'Partial effect on TCE',
  show.contrib = T,
  plot.layout = c(2, 4)
)
gbms.era5.landsat$var.names = c('albedo', 'lai', 'gdp', 'prec', 'solar', 'vpd', 'ws')



# BRT using MSWX and MODIS LAI ----
set.seed(1)
gbms.mswx = dismo::gbm.step(
  data = data.brt, # mask out cities with high cloud cover
  gbm.x = colnames(data.brt)[c(13:14, 16, 21:24)],
  gbm.y = 'tce.10.25',
  family = 'gaussian',
  tree.complexity = 10,
  tolerance = 0.01,
  tolerance.method = 'auto',
  learning.rate = 0.001,
  n.folds = 10,
  bag.fraction = 0.5
)
dismo::gbm.plot(
  gbms.mswx,
  smooth = F, rug = F, common.scale = F,
  y.label = 'Partial effect on TCE',
  show.contrib = T,
  plot.layout = c(2, 4)
)
gbms.mswx$var.names = c('albedo', 'lai', 'gdp', 'prec', 'solar', 'vpd', 'ws')



# Plotting partial effects ----
## marginal data ----
marginalData = function(gbms) {
  # marginal list
  ml = list()
  for (i in 1:length(gbms$var.names)) {
    # response
    response = gbm::plot.gbm(gbms, gbms$var.names[i], return.grid = T)
    ml[[i]] = data.table(x = response[, 1], y = response[, 2] - mean(response[, 2]))
  }
  return(ml)
}

## normalized marginal data ----
# gbms ------------- target gbms to be normalized
# gbms.margin ------ gbms marginal data based on which normalization will be done
marginalData.normalization = function(gbms, gbms.margin) {
  # marginal list
  ml = list()
  for (i in 1:length(gbms$var.names)) {
    # response
    response = gbm::plot.gbm(gbms, gbms$var.names[i], return.grid = T)
    # min
    a = min(gbms.margin[[gbms$var.names[i]]]$y)
    # max
    b = max(gbms.margin[[gbms$var.names[i]]]$y)
    # scale
    k = (b - a) / (max(response[, 2]) - min(response[, 2]))
    y.normal = a + k * (response[, 2] - min(response[, 2]))
    ml[[i]] = data.table(x = response[, 1], y = y.normal)
  }
  return(ml)
}

## marginal data using ETA5 and MODIS LAI ----
# data.margin.era5 = marginalData.normalization(gbms.era5, data.margin.era5.landsat)
data.margin.era5 = marginalData(gbms.era5)
names(data.margin.era5) = c('albedo', 'lai', 'gdp', 'prec', 'solar', 'vpd', 'ws')
data.margin.era5 = data.margin.era5[c('lai', 'albedo', 'vpd', 'solar', 'prec', 'gdp', 'ws')]

## marginal data using ETA5 and Landsat LAI ----
data.margin.era5.landsat = marginalData(gbms.era5.landsat)
gbms.era5.landsat$var.names
names(data.margin.era5.landsat) = c('albedo', 'lai', 'gdp', 'prec', 'solar', 'vpd', 'ws')
data.margin.era5.landsat = data.margin.era5.landsat[c('lai', 'albedo', 'vpd', 'solar', 'prec', 'gdp', 'ws')]

## marginal data using MSWX and MODIS LAI ----
data.margin.mswx = marginalData(gbms.mswx)
names(data.margin.mswx) = c('albedo', 'lai', 'gdp', 'prec', 'solar', 'vpd', 'ws')
data.margin.mswx = data.margin.mswx[c('lai', 'albedo', 'vpd', 'solar', 'prec', 'gdp', 'ws')]

## settings ----
par(
  cex.axis = 1.1,
  family = 'Calibri',
  las = 1,
  lwd = 0.1,
  mai = c(0.4, 0.4, 0, 0),
  mfrow = c(4, 2),
  oma = c(0, 32, 1, 2),
  # pty = 's',
  tck = 0.03
)
cex.font = 1.1; cex.legend = 1.1
label.line = 1.3
cols = c('dodgerblue', 'seagreen', 'tomato2', 'gold1', 'coral4', 'deeppink', 'gray50')
xlims = list(
  c(0, 1.94),
  c(0.085, 0.205),
  c(0, 2.1),
  c(0, 320),
  c(-100, 2350),
  c(-0.1, 2.35),
  c(0, 4.5)
)
at1 = list(
  c(-10, seq(0.2, 2, 0.5), 10),
  c(-10, seq(0.1, 0.2, 0.03), 10),
  c(-10, seq(0.3, 2.5, 0.5), 10),
  c(-10, seq(50, 350, 70), 1000),
  c(-10000, seq(200, 2500, 600), 10000),
  c(-10, seq(0.2, 2, 0.6), 10),
  c(-10, seq(0.8, 10, 1), 100) 
)
ylims.margin = list(
  c(-0.09, 0.039),
  c(-0.035, 0.044),
  c(-0.02, 0.05),
  c(-0.03, 0.05),
  c(-0.015, 0.022),
  c(-0.035, 0.015),
  c(-0.02, 0.03)
)
at2 = list(
  c(-1, seq(-0.1, 0.07, 0.03), 1),
  c(-1, seq(-0.026, 0.06, 0.02), 1),
  c(-1, seq(-0.03, 0.05, 0.018), 1),
  c(-1, seq(-0.04, 0.04, 0.02), 1),
  c(-1, seq(-0.01, 0.04, 0.01), 1),
  c(-1, seq(-0.04, 0.04, 0.012), 1),
  c(-1, seq(-0.014, 0.04, 0.012), 1)
)
ylims.density = list(
  c(0, 1.2),
  c(0, 33),
  c(0, 1.8),
  c(0, 0.016),
  c(0, 0.0019),
  c(0, 2.5),
  c(0, 1.05)
)
var.colnames = c('lai', 'albedo', 'vpd', 'solar', 'prec', 'gdp', 'ws')
var.names = c(
  'LAI',
  "City's albedo",
  'VPD (kPa)',
  expression('Solar radiation (W/m'^2 * ')'),
  'Precipitation (mm)',
  'GDP (1e+10 USD)',
  'Wind speed (m/s)'
)

## plotting ----
for (i in 1:length(data.margin.era5)) {
  # density plot
  dens = density(data.brt[, get(var.colnames[i])])
  plot(
    -1,
    type = 'n',
    axes = F, ann = F,
    xaxs = 'i', yaxs = 'i',
    xlim = xlims[[i]], ylim = ylims.density[[i]]
  )
  polygon(dens$x, dens$y, density = NULL, border = 'gray94', col = 'gray94', lwd = 0.1)
  par(new = T)
  # marginal effect plot
  baseplotframe(
    xlim = xlims[[i]],
    ylim = ylims.margin[[i]],
    at1 = at1[[i]],
    at2 = at2[[i]]
  )
  lines(smooth.spline(data.margin.era5[[i]], spar = 0.8), col = cols[i], lwd = 1.5)
  if (i == 1) {
    lines(smooth.spline(data.margin.era5.landsat[[i]], spar = 0.8), col = cols[i], lwd = 1.5, lty = 'dotdash')
    legend(
      'bottomright',
      legend = c(
        'MODIS',
        'Landsat'
      ),
      col = cols[i],
      lty = c('solid', 'dotdash'),
      lwd = 1.2,
      seg.len = 0.5,
      cex = 1,
      bty = 'n',
      inset = c(-0.78, 0), x.intersp = 0.2, y.intersp = 0.5
    )
  }
  if (i %in% c(3, 4, 5, 7)) {
    lines(smooth.spline(data.margin.mswx[[i]], spar = 0.8), col = cols[i], lwd = 1.5, lty = 'dotdash')
    legend(
      'topright',
      legend = c(
        'ERA5',
        'MSWX'
      ),
      col = cols[i],
      lty = c('solid', 'dotdash'),
      lwd = 1.2,
      seg.len = 0.5,
      cex = 1,
      bty = 'n',
      inset = c(-0.62, -0.05), x.intersp = 0.2, y.intersp = 0.5
    )
  }
  mtext(1, text = var.names[i], line = label.line, cex = cex.font - 0.4)
  if (i %in% seq(1, 7, 2)) mtext(2, text = expression('Partial effect on TCE (' * degree * 'C/%)'), line = label.line + 1, cex = cex.font - 0.4, las = 0)
  mtext(3, text = substitute(bold(letter), list(
    letter = toupper(letters)[i]
  )), line = -1.3, adj = 0.03, cex = cex.font - 0.2)
}



# Plotting variable contribution ----
contribution.era5 = gbms.era5$contributions
setDT(contribution.era5)
contribution.era5$var = factor(contribution.era5$var, levels = contribution.era5$var)
barplot(
  rel.inf ~ var,
  data = contribution.era5,
  axes = F, ann = F,
  xaxt = 'n', yaxs = 'i',
  ylim = c(0, 30),
  col = cols
)
axis(1, at = c(-10, 10), labels = F, lwd = 0.1, lwd.tick = 0.5)
axis(2, at = seq(-10, 30, 5), lwd = 0.1, lwd.tick = 0.5, mgp = c(3, 0.2, 0))
axis(3, at = c(-10, 10), labels = F, lwd = 0.1, lwd.tick = 0)
axis(4, at = seq(-10, 30, 5), labels = F, lwd = 0.1, lwd.tick = 0)
mtext(1, text = 'Variables', line = label.line - 0.1, cex = cex.font - 0.4, las = 0)
mtext(2, text = 'Relative contribution (%)', line = label.line + 0.1, cex = cex.font - 0.4, las = 0)
mtext(3, text = substitute(bold(letter), list(letter = toupper(letters)[8])), line = -1.3, adj = 0.03, cex = cex.font - 0.2)
par(opar)