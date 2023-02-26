## ----------------------------------
# Title: Contribution
# Objective: To visualize the partial effects of different variables affecting TCE
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
  tce.10.20 = apply(re[, grep('tce.10.20', names(re), value = F), with = F], 1, mean, na.rm = T), # TCE.10.20 [°C/%]
  tce.10.25 = apply(re[, grep('tce.10.25', names(re), value = F), with = F], 1, mean, na.rm = T), # TCE.10.25 [°C/%]
  tce.20.25 = apply(re[, grep('tce.20.25', names(re), value = F), with = F], 1, mean, na.rm = T), # TCE.20.25 [°C/%]
  tce.30.25 = apply(re[, grep('tce.30.25', names(re), value = F), with = F], 1, mean, na.rm = T), # TCE.30.25 [°C/%]
  tce.10.30 = apply(re[, grep('tce.10.30', names(re), value = F), with = F], 1, mean, na.rm = T), # TCE.10.30 [°C/%]
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
  mswx_ws = apply(re[, grep('mswx_wind_speed', names(re), value = F), with = F], 1, mean, na.rm = T), # wind speed [m/s]
  trend_lai = apply(re[, grep('lai_slope', names(re), value = F), with = F], 1, mean, na.rm = T) # trend in LAI [-/yr]
)
data.brt = data.brt[complete.cases(data.brt)]
data.brt = data.brt[cloud < 0.7] # cloud cover should be less than 70%
data.brt = merge(data.brt, esd, by = 'id')[sd < 100] # elevation sd should be less than 100 (m)
hist(data.brt$tce.10.25)
fwrite(data.brt, './data_brt.csv')



# BRT using ERA5 and MODIS LAI ----
set.seed(1)
gbms.era5 = dismo::gbm.step(
  data = data.brt,
  gbm.x = c('lai', 'trend_lai', 'vpd', 'solar', 'ws', 'albedo', 'cloud'),
  gbm.y = 'tce.10.25',
  family = 'gaussian',
  tree.complexity = 10,
  tolerance = 0.01,
  tolerance.method = 'auto',
  learning.rate = 0.005,
  n.folds = 10,
  bag.fraction = 0.5
)
dismo::gbm.plot(
  gbms.era5,
  smooth = F, rug = F, common.scale = T,
  y.label = 'Partial effect on TCE',
  show.contrib = T,
  plot.layout = c(2, 4)
)
gbms.era5$var.names
bias(data.brt$tce.10.25, gbms.era5$fit)
finalVariableOrder = c('lai', 'albedo', 'vpd', 'solar', 'cloud', 'ws', 'trend_lai')



# BRT using ERA5 and Landsat LAI ----
set.seed(1)
gbms.era5.landsat = dismo::gbm.step(
  data = data.brt, # mask out cities with high cloud cover
  gbm.x = c('lai2', 'trend_lai', 'vpd', 'solar', 'ws', 'albedo', 'cloud'),
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
gbms.era5.landsat$var.names = c('lai', 'trend_lai', 'vpd', 'solar', 'ws', 'albedo', 'cloud')
gbms.era5.landsat$contributions$var = c('lai', 'albedo', 'solar', 'vpd', 'cloud', 'ws', 'trend_lai')
bias(data.brt$tce.10.25, gbms.era5.landsat$fit)



# BRT using MSWX and MODIS LAI ----
set.seed(1)
gbms.mswx = dismo::gbm.step(
  data = data.brt, # mask out cities with high cloud cover
  gbm.x = c('lai', 'trend_lai', 'mswx_vpd', 'mswx_solar', 'mswx_ws', 'albedo', 'cloud'),
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
gbms.mswx$var.names = c('lai', 'trend_lai', 'vpd', 'solar', 'ws', 'albedo', 'cloud')
gbms.mswx$contributions$var = c('lai', 'albedo', 'solar', 'vpd', 'cloud', 'trend_lai', 'ws')
bias(data.brt$tce.10.25, gbms.mswx$fit)



# BRT using MSWX and Landsat LAI ----
set.seed(1)
gbms.mswx.landsat = dismo::gbm.step(
  data = data.brt, # mask out cities with high cloud cover
  gbm.x = c('lai2', 'trend_lai', 'vpd', 'solar', 'ws', 'albedo', 'cloud'),
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
  gbms.mswx.landsat,
  smooth = F, rug = F, common.scale = F,
  y.label = 'Partial effect on TCE',
  show.contrib = T,
  plot.layout = c(2, 4)
)
gbms.mswx.landsat$var.names = c('lai', 'trend_lai', 'vpd', 'solar', 'ws', 'albedo', 'cloud')
gbms.mswx.landsat$contributions$var = c('lai', 'albedo', 'solar', 'vpd', 'cloud', 'ws', 'trend_lai')
bias(data.brt$tce.10.25, gbms.mswx.landsat$fit)



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


## marginal data using ERA5 and MODIS LAI ----
# data.margin.era5 = marginalData.normalization(gbms.era5, data.margin.era5.landsat)
data.margin.era5 = marginalData(gbms.era5)
names(data.margin.era5) = c('lai', 'trend_lai', 'vpd', 'solar', 'ws', 'albedo', 'cloud')
data.margin.era5 = marginalData.normalization(gbms.era5, data.margin.era5)
names(data.margin.era5) = c('lai', 'trend_lai', 'vpd', 'solar', 'ws', 'albedo', 'cloud')
data.margin.era5 = data.margin.era5[finalVariableOrder]


## marginal data using ERA5 and Landsat LAI ----
data.margin.era5.landsat = marginalData(gbms.era5.landsat)
names(data.margin.era5.landsat) = c('lai', 'trend_lai', 'vpd', 'solar', 'ws', 'albedo', 'cloud')
data.margin.era5.landsat = marginalData.normalization(gbms.era5.landsat, data.margin.era5.landsat)
names(data.margin.era5.landsat) = c('lai', 'trend_lai', 'vpd', 'solar', 'ws', 'albedo', 'cloud')
data.margin.era5.landsat = data.margin.era5.landsat[finalVariableOrder]


## marginal data using MSWX and MODIS LAI ----
data.margin.mswx = marginalData(gbms.mswx)
names(data.margin.mswx) = c('lai', 'trend_lai', 'vpd', 'solar', 'ws', 'albedo', 'cloud')
data.margin.mswx = marginalData.normalization(gbms.mswx, data.margin.mswx)
names(data.margin.mswx) = c('lai', 'trend_lai', 'vpd', 'solar', 'ws', 'albedo', 'cloud')
data.margin.mswx = data.margin.mswx[finalVariableOrder]


## settings ----
### par ----
par(
  cex.axis = 1,
  las = 1,
  lwd = 0.1,
  mai = c(0.4, 0.4, 0, 0.1),
  mfrow = c(2, 4),
  oma = c(0.5, 2.5, 1.2, 1.5),
  pty = 's',
  tck = 0.03
)
cex.font = 1.1; cex.legend = 1.1
label.line = 1.4

### color palette ----
cols = c('dodgerblue', 'seagreen', 'tomato2', 'gold1', 'coral4', 'deeppink', 'gray50')

### xlim ----
xlims = list(
  c(0, 1.94),
  c(0.085, 0.205),
  c(0, 2.1),
  c(0, 320),
  c(0.1, 0.75),
  c(-0.25, 5.4),
  c(-0.02, 0.06)
)

### ylim ----
ylims.margin = list(
  c(-0.1, 0.05),
  c(-0.06, 0.06),
  c(-0.03, 0.07),
  c(-0.04, 0.08),
  c(-0.02, 0.02),
  c(-0.02, 0.04),
  c(-0.03, 0.03)
)

### ylim (density plot) ----
ylims.density = list(
  c(0, 1.3),
  c(0, 31),
  c(0, 1.7),
  c(0, 0.015),
  c(0, 5.3),
  c(0, 0.94),
  c(0, 52)
)

### at1 ----
at1 = list(
  c(-10, seq(0.2, 2, 0.5), 10),
  c(-10, seq(0.1, 0.2, 0.03), 10),
  c(-10, seq(0.3, 2.5, 0.5), 10),
  c(-10, seq(50, 350, 70), 1000),
  c(-1, seq(0.2, 1, 0.15), 2),
  c(-10, seq(0.8, 10, 1.2), 100),
  c(-10, seq(-0.02, 0.06, 0.02), 10)
)

### at2 ----
at2 = list(
  c(-1, seq(-0.1, 0.07, 0.03), 1),
  c(-1, seq(-0.06, 0.07, 0.03), 1),
  c(-1, seq(-0.03, 0.07, 0.02), 1),
  c(-1, seq(-0.04, 0.1, 0.03), 1),
  c(-1, seq(-0.03, 0.1, 0.01), 1),
  c(-1, seq(-0.02, 0.04, 0.02), 1),
  c(-1, seq(-0.04, 0.1, 0.02), 1)
)

### x-label ----
var.names = c(
  'LAI',
  "City albedo",
  'VPD (kPa)',
  expression('Solar radiation (W/m'^2 * ')'),
  'Cloud cover',
  'Wind speed (m/s)',
  'Trend in LAI (-/yr)'
)


## plotting ----
for (i in 1:length(data.margin.era5)) {
  
  # density plot
  dens = density(data.brt[, get(finalVariableOrder[i])], adjust = 2)
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
  plot(
    0, type = 'n',
    axes = F, ann = F,
    xaxs = 'i', yaxs = 'i',
    xlim = xlims[[i]], ylim = ylims.margin[[i]]
  )
  
  # axis
  axis(1, at = at1[[i]], lwd = 0.1, mgp = c(3, 0.2, 0))
  axis(2, at = at2[[i]], lwd = 0.1, mgp = c(3, 0.3, 0))
  axis(3, at = at1[[i]], labels = F, lwd = 0.1)
  axis(4, at = at2[[i]], labels = F, lwd = 0.1)
  
  # line
  if (i == 5) {
    lines(smooth.spline(data.margin.era5[[i]], spar = 1.1), col = cols[i], lwd = 2.5)
  } else if (i == 6) {
    lines(smooth.spline(data.margin.era5[[i]], spar = 0.9), col = cols[i], lwd = 2.5)
  } else if (i == 7) {
    lines(smooth.spline(data.margin.era5[[i]], spar = 1), col = cols[i], lwd = 2.5)
  } else {
    lines(smooth.spline(data.margin.era5[[i]], spar = 0.8), col = cols[i], lwd = 2.5)
  }
  
  if (i == 1) {
    lines(smooth.spline(data.margin.era5.landsat[[i]], spar = 0.8), col = cols[i], lwd = 1.5, lty = 'dashed')
    # legend
    legend(
      'bottomright',
      legend = c(
        'MODIS',
        'Landsat'
      ),
      col = cols[i],
      lty = c('solid', 'dashed'),
      lwd = 1.2,
      seg.len = 1,
      cex = 1,
      bty = 'n',
      inset = c(0.03, 0.02), x.intersp = 0.2, y.intersp = 0.9
    )
  }
  
  # line
  if (i %in% c(3, 4, 5, 6)) {
    
    if (i == 4) {
      lines(smooth.spline(data.margin.mswx[[i]], spar = 1), col = cols[i], lwd = 1.5, lty = 'dashed')
    } else if (i == 5) {
      lines(smooth.spline(data.margin.mswx[[i]], spar = 1), col = cols[i], lwd = 1.5, lty = 'dashed')
    } else if (i == 6) {
      lines(smooth.spline(data.margin.mswx[[i]], spar = 0.9), col = cols[i], lwd = 1.5, lty = 'dashed')
    } else {
      lines(smooth.spline(data.margin.mswx[[i]], spar = 0.8), col = cols[i], lwd = 1.5, lty = 'dashed')
    }
    
    # legend
    if (i == 4) {
      legend(
        'topright', legend = c(
          'ERA5',
          'MSWX'
        ), col = cols[i],
        lty = c('solid', 'dashed'),
        lwd = 1.2,
        seg.len = 1,
        cex = 1,
        bty = 'n',
        inset = c(0.4, 0.01), x.intersp = 0.2, y.intersp = 0.9
      )
    } else {
      legend(
        'topright', legend = c(
          'ERA5',
          'MSWX'
        ), col = cols[i],
        lty = c('solid', 'dashed'),
        lwd = 1.2,
        seg.len = 1,
        cex = 1,
        bty = 'n',
        inset = c(0.03, 0.01), x.intersp = 0.2, y.intersp = 0.9
      )
    }
    
  }
  
  # x-label
  mtext(1, text = var.names[i], line = label.line + 0.5, cex = cex.font - 0.35)
  
  # y-label
  if (i %in% c(1, 5)) mtext(2, text = expression('Partial effect on TCE.10.25 (' * degree * 'C/%)'), line = label.line + 1.8, cex = cex.font - 0.35, las = 0)
  
  # figure number
  mtext(3, text = substitute(bold(letter), list(
    letter = paste0('(', letters, ')')[i]
  )), line = -1.5, adj = 0.035, cex = cex.font - 0.3)
  
}



# Plotting variable contribution ----
## an ensemble of variable contribution from different combinations of the datasets ----
contribution = Reduce(
  function(x, y) merge(x, y, by = 'var'), list(
    gbms.era5$contributions, gbms.era5.landsat$contributions, gbms.mswx$contributions, gbms.mswx.landsat$contributions
  )
)
setDT(contribution)
colnames(contribution) = c('var', 'era5.m', 'era5.l', 'era5.m.m', 'era5.m.l')
contribution[, c('mean', 'sd') := .(rowMeans(.SD), apply(.SD, 1, sd)), .SDcols = c('era5.m', 'era5.l', 'era5.m.m', 'era5.m.l')][order(mean, decreasing = T)]


## contribution given by ERA5 + MODIS LAI ----
contribution.era5 = gbms.era5$contributions
setDT(contribution.era5)
contribution.era5$var = factor(contribution.era5$var, levels = contribution.era5$var)


## barplot ----
bp = barplot(
  rel.inf ~ var,
  data = contribution.era5,
  axes = F, ann = F,
  xaxt = 'n', yaxs = 'i',
  xlim = c(-0.2, 8.8),
  ylim = c(0, 30),
  col = cols
)


## error bar ----
arrows(
  bp,
  contribution[order(era5.m, decreasing = T)][, era5.m + sd],
  bp,
  contribution[order(era5.m, decreasing = T)][, era5.m - sd],
  lwd = 0.5, angle = 90, code = 3, length = 0.02
)


## axis ----
axis(1, at = c(-10, 10), labels = F, lwd = 0.1, lwd.tick = 0.5)
axis(2, at = seq(-10, 35, 5), lwd = 0.1, lwd.tick = 0.5, mgp = c(3, 0.2, 0))
axis(3, at = c(-10, 10), labels = F, lwd = 0.1, lwd.tick = 0)
axis(4, at = seq(-10, 35, 5), labels = F, lwd = 0.1, lwd.tick = 0)


## text ----
mtext(1, text = 'Variables', line = label.line + 0.25, cex = cex.font - 0.35, las = 0)
mtext(2, text = 'Relative contribution (%)', line = label.line + 0.4, cex = cex.font - 0.35, las = 0)
mtext(3, text = substitute(bold(letter), list(letter = '(h)')), line = -1.5, adj = 0.035, cex = cex.font - 0.2)
par(opar)