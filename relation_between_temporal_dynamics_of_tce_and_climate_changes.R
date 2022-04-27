## ----------------------------------
# Title: Links between tree cooling efficiency (TCE)'s temporal dynamics and climate changes
# Objective: Visualizing how the changes of climatic variables modify TCE temporally
# Created by: Jiacheng Zhao
# Created on: 2022-04-24
# Copyright (c) Jiacheng Zhao, 2022
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------


# data ----
data.box = re[, c(1, grep('tce.10.25', names(re)), value = F), with = F]
data.box = data.box[complete.cases(data.box)][id %in% data.brt$id]
colnames(data.box)[2:5] = c('tce_2000', 'tce_2005', 'tce_2010', 'tce_2015')
f = function(var1, var2, var3, var4) {
  d = data.table(x = 1:4, y = c(var1, var2, var3, var4))
  fit = lm(y ~ x, data = d)
  return(coef(fit)[2])
}
# trend in TCE
data.box[, k := mapply(f, tce_2000, tce_2005, tce_2010, tce_2015)]
data.box = merge(region, merge(slope, data.box[, .(id, k)], by = 'id'), by = 'id')[region %in% c(
  'AF', 'AN', 'NE', 'SE', 'WE', 'EE', 'NA', 'SAM', 'EA'
)]
# merge European cities
data.box[region %in% c('NE', 'SE', 'WE', 'EE'), region := 'EU']
# South America
data.box[region == 'SAM', region := 'SA']
# median TCE by continent
byregion = data.box[, median(k), by = region]
# grouping TCE into a fast increase group and a slow one
for (i in 1:6) {
  data.box[region == byregion[i, 1]$region & k < byregion[i, 2]$V1, f := 's'] # slow
  data.box[region == byregion[i, 1]$region & k > byregion[i, 2]$V1, f := 'f'] # fast
}
# continent levels
data.box$region = factor(data.box$region, levels = c('AF', 'AN', 'EU', 'NA', 'SA', 'EA'))
# group levels
data.box$f = factor(data.box$f, levels = c('f', 's'))


# plotting ----
cols = c('green', 'red')
# x-axis bar positions
myat = box.at(length(unique(data.box$region)), 2, 1.5, 0.4)
# par
mypar(
  cex.axis = 1.1,
  mai = c(0.2, 0.3, 0.1, 0.3),
  mfrow = c(1, 3),
  oma = c(4, 4, 20, 1),
  pty = 's',
  tck = 0.017
)
## LAI ----
basegroupedboxframe(
  lai_slope ~ f:region, data = data.box,
  col = cols,
  ylim = c(-0.01, 0.06),
  at1 = myat, at1.label = c('AF', 'AN', 'EU', 'NA', 'SA', 'EA'),
  at2 = seq(-0.01, 0.05, 0.02),
  medlwd = 0.5, boxwex = 0.3,
  ann = F
)
abline(h = 0, col = 'blue', lty = 'dashed')
mtext(2, text = 'Trend in LAI (-/yr)', line = 2.5, cex = 0.7, las = 0)
mtext(3, text = substitute(bold(letter), list(
  letter = toupper(letters)[2]
)), line = -1.4, adj = 0.03, cex = 1)
# legend
legend(
  'topright',
  legend = c(
    'Fast TCE increase',
    'Slow TCE increase'
  ),
  fill = cols,
  cex = 1, bty = 'n',
  y.intersp = 0.8,
  inset = c(-0.18, -0.02)
)

## AOD ----
basegroupedboxframe(
  aod_slope ~ f:region, data = data.box,
  col = cols,
  ylim = c(-0.005, 0.012),
  at1 = myat, at1.label = c('AF', 'AN', 'EU', 'NA', 'SA', 'EA'),
  medlwd = 0.5, boxwex = 0.3,
  ann = F
)
abline(h = 0, col = 'blue', lty = 'dashed')
mtext(2, text = 'Trend in AOD (-/yr)', line = 2.5, cex = 0.7, las = 0)
mtext(3, text = substitute(bold(letter), list(
  letter = toupper(letters)[3]
)), line = -1.4, adj = 0.03, cex = 1)
# legend
legend(
  'topright',
  legend = c(
    'Fast TCE increase',
    'Slow TCE increase'
  ),
  fill = cols,
  cex = 1, bty = 'n',
  y.intersp = 0.8,
  inset = c(-0.18, -0.02)
)

## RH ----
basegroupedboxframe(
  rh_slope ~ f:region, data = data.box,
  col = cols,
  ylim = c(-0.6, 0.55),
  at1 = myat, at1.label = c('AF', 'AN', 'EU', 'NA', 'SA', 'EA'),
  medlwd = 0.5, boxwex = 0.3,
  ann = F
)
abline(h = 0, col = 'blue', lty = 'dashed')
mtext(2, text = 'Trend in RH (-/yr)', line = 2.5, cex = 0.7, las = 0)
mtext(3, text = substitute(bold(letter), list(
  letter = toupper(letters)[4]
)), line = -1.4, adj = 0.03, cex = 1)
# legend
legend(
  'topright',
  legend = c(
    'Fast TCE increase',
    'Slow TCE increase'
  ),
  fill = cols,
  cex = 1, bty = 'n',
  y.intersp = 0.8,
  inset = c(-0.18, -0.02)
)
par(opar)