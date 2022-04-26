## ----------------------------------
# Title: Sensitivity of tree cooling efficiency (TCE) to background temperature
# Objective: Visualization of the sensitivity of TCE to background temperature against latitude
# Created by: Jiacheng Zhao
# Created on: 2021-12-26
# Copyright (c) Jiacheng Zhao, 2021
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------

# settings ----
# par
mypar(
  las = 1,
  mai = c(0.2, 0, 0.1, 0),
  mfrow = c(2, 3),
  oma = c(5, 5, 1, 1),
  pty = 's',
  tck = 0.017
)
cex.point = 0.8; cex.font = 1.1; cex.legend = 1.1
pch = 21
lwd = 0.1; medlwd = 1.4
boxwex = 0.4
x.intersp = 0.4; y.intersp = 0.7
inset = c(0, 0)
seg.len = 1.1
label.line = 1.7
col = 'forestgreen'; cols = c('darkorange', 'seagreen')
col.alphas = c(0.3, 0.6, 0.9)

# plotting ----
## TCE for different tree canopy cover ----
# column names
colnames = c('tce.10.25', 'tce.20.25', 'tce.30.25')
# data by region
data.byregion = copy(data.brt[region %in% c(
  'Australia/New Zealand',
  'Northern America', 'South America','Central America',
  'Northern Europe', 'Southern Europe', 'Western Europe', 'Eastern Europe',
  'European Russia', 'Asiatic Russia',
  'Eastern Asia', 'Southern Asia',
  'Northern Africa', 'Middle Africa', 'Eastern Africa', 'Western Africa', 'Southern Africa'
)])
data.byregion[, region2 := region]
data.byregion[region2 %in% c('Central America'), region2 := 'South America'][
  region2 %in% c('Northern Europe', 'Southern Europe', 'Western Europe', 'Eastern Europe'), region2 := 'Europe'
][
  region2 %in% c('European Russia', 'Asiatic Russia'), region2 := 'Russia'
][
  region2 %in% c('Northern Africa', 'Middle Africa', 'Eastern Africa', 'Western Africa', 'Southern Africa'), region2 := 'Africa'
]
data.byregion[
  region2 == 'Australia/New Zealand', region2 := 'AU'
][
  region2 == 'Northern America', region2 := 'NA'
][
  region2 == 'South America', region2 := 'LA'
][
  region2 == 'Europe', region2 := 'EU'
][
  region2 == 'Russia', region2 := 'RU'
][
  region2 == 'Eastern Asia', region2 := 'EA'
][
  region2 == 'Southern Asia', region2 := 'SA'
][
  region2 == 'Africa', region2 := 'AF'
]
# levels
levels = data.byregion[, median(tce.10.25), by = region2][order(V1)]$region2
data.byregion$region2 = factor(data.byregion$region2, levels = levels)
data.byregion[, .(mean(tce.10.25), mean(tce.30.25)), by = region2][order(V1, decreasing = T)]
# boxplot
for (i in 1:3) {
  f = as.formula(paste(colnames[i], '~ region2'))
  ifelse(
    i == 1,
    baseboxframe(f, data.byregion, col = col, alpha = col.alphas[i], ylim = c(0, 0.6), at2 = seq(0, 1, 0.1), tick.label1 = F, lwd = lwd, medlwd = medlwd),
    baseboxframe(f, data.byregion, col = col, alpha = col.alphas[i], ylim = c(0, 0.6), at2 = seq(0, 1, 0.1), tick.label1 = F, tick.label2 = F, lwd = lwd, medlwd = medlwd)
  )
  # labels
  text(
    cex = cex.font,
    x = c(-100, 1:length(levels), 100),
    y = -0.022,
    c('', levels, ''),
    xpd = NA,
    srt = -0
  )
  if (i == 1) mtext(2, text = expression('TCE at 25 ' * degree * 'C ' * T[a] * ' (' * degree * 'C/%)'), line = label.line + 0.5, cex = cex.font - 0.1, las = 0)
  mtext(3, text = substitute(bold(letter), list(
    letter = toupper(letters)[i + 1]
  )), line = -1.75, adj = 0.03, cex = cex.font + 0.2)
  mtext(3, text = substitute(x * ' % tree canopy cover', list(x = i * 10)), line = -1.9, adj = 0.9, cex = cex.font - 0.2)
}

## Sensitivity to temperature ----
# data
data.temp.sensitivity = copy(data.byregion)
# latitude bins
data.temp.sensitivity[, 'bins' := cut(latitude, breaks = 8, labels = 1:8)]
# box data
data.box = melt(
  data.temp.sensitivity, id.vars = 'bins', measure = c('tce.10.25', 'tce.10.mean', 'tce.20.25', 'tce.20.mean', 'tce.30.25', 'tce.30.mean')
)
# levels
data.level = copy(data.byregion)[, 'bins' := cut(latitude, breaks = 8)]
levels = levels(data.level$bins)
levels = c('(-38, -26]', '(-26, -14]', '(-14, -1]', '(-1, 11]', '(11, 23]', '(23, 35]', '(36, 48]', '(48, 60]')
# column list
collist = list(
  c('tce.10.25', 'tce.10.mean'),
  c('tce.20.25', 'tce.20.mean'),
  c('tce.30.25', 'tce.30.mean')
)
for (i in 1:3) {
  # data
  data.box = melt(
    data.temp.sensitivity, id.vars = 'bins', measure = collist[[i]]
  )
  # at
  myat = box.at(length(unique(data.box$bins)), 2, 1.2, 0.4)
  ifelse(
    i == 1,
    basegroupedboxframe(
      value ~ variable:bins, data = data.box,
      col = cols, alpha = col.alphas[i],
      ylim = c(0, 0.8),
      at1 = myat, at2 = seq(0, 0.8, 0.1),
      lwd = lwd, medlwd = medlwd, boxwex = boxwex
    ),
    basegroupedboxframe(
      value ~ variable:bins, data = data.box,
      col = cols, alpha = col.alphas[i],
      ylim = c(0, 0.8),
      at1 = myat, at2 = seq(0, 0.8, 0.1), tick.label2 = F,
      lwd = lwd, medlwd = medlwd, boxwex = boxwex
    )
  )
  # data.arrow = data.temp.sensitivity[,  lapply(.SD, median), by = bins, .SDcols = collist[[i]]][order(bins)]
  # med = as.numeric(as.matrix(data.arrow[, 2]))
  # max = as.numeric(as.matrix(data.arrow[, 3]))
  # min = as.numeric(as.matrix(data.arrow[, 4]))
  # xi <- 1:nrow(data.arrow)
  # points(xi, med, col = 'red', pch = 18)
  # arrows(
  #   xi, med - min,
  #   xi, med + max,
  #   code = 3, col = 'darkorange', angle = 75, length = 0.03
  # )
  
  
  # smooth line
  # data.smooth = data.temp.sensitivity[, mean(get(colnames[i])), by = bins][order(bins)]
  # lines(spline(1:13, data.smooth$V1, n = 1000), col = 'red', lwd = 1.4)
  # the equator
  # abline(v = 5.5, lty = 'dashed')
  # labels
  text(
    cex = cex.font,
    x = c(-100, myat$center, 100),
    y = -0.065,
    c('', levels, ''),
    xpd = NA,
    srt = 35
  )
  mtext(1, text = expression('Laititude' * ' (' * degree * ')'), line = 3.5, cex = cex.font - 0.1)
  if (i == 1) mtext(2, text = expression('TCE (' * degree * 'C/%)'), line = label.line + 0.5, cex = cex.font - 0.1, las = 0)
  mtext(3, text = substitute(bold(letter), list(
    letter = toupper(letters)[i + 4]
  )), line = -1.75, adj = 0.03, cex = cex.font + 0.2)
  mtext(3, text = substitute(x * ' % tree canopy cover', list(x = i * 10)), line = -1.9, adj = 0.9, cex = cex.font - 0.2)
  legend(
    'topright',
    legend = c(
      expression('TCE at 25 ' * degree * 'C ' * T[a]),
      expression('TCE at mean ' * T[a] * ' of growing seasons')
    ),
    fill = adjustcolor(cols, alpha.f = col.alphas[i]),
    # border = c('black', NA),
    cex = cex.legend, bty = 'n',
    x.intersp = x.intersp, y.intersp = y.intersp,
    inset = c(-0.35, 0.1)
  )
}

f = function(i) {
  data.box = melt(
    data.temp.sensitivity, id.vars = 'bins', measure = collist[[3]]
  )
  round(
    (data.box[bins == i & variable == 'tce.30.25', mean(value)] - data.box[bins == i & variable == 'tce.30.mean', mean(value)]) / data.box[bins == i & variable == 'tce.30.mean', mean(value)] * 100,
    0
  )
}
f(8)
par(opar)