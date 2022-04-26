## ----------------------------------
# Title: Saturation of tree cooling efficiency (TCE)
# Objective: Visualization of the saturation of TCE as tree canopy cover (TCC) increases
# Created by: Jiacheng Zhao
# Created on: 2021-12-26
# Copyright (c) Jiacheng Zhao, 2021
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------

# settings ----
# par
mypar(
  cex.axis = 1.2,
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
x.intersp = 0.4; y.intersp = 0.9
inset = c(0, 0)
seg.len = 1.1
label.line = 1.7
col = 'forestgreen'; cols = c('darkorange', 'seagreen')
col.alphas = c(0.3, 0.6, 0.9)

# plotting ----
## TCE for different TCC levels ----
# column names
colnames = c('tce.10.25', 'tce.20.25', 'tce.30.25')
# data by region
data.byregion = copy(data.brt[region %in% c(
  'AN', 'NA', 'SAM','CAM', 'NE', 'SE', 'WE', 'EE', 'EA', 'SA', 'AF'
)])
data.byregion[, region2 := region]
data.byregion[region2 %in% c('NE', 'SE', 'WE', 'EE'), region2 := 'EU']
# levels
levels = data.byregion[, median(tce.10.25), by = region2][order(V1)]$region2
data.byregion$region2 = factor(data.byregion$region2, levels = levels)
data.byregion[, .(mean(tce.10.25), mean(tce.30.25)), by = region2][order(V1, decreasing = T)]
# boxplot
for (i in 1:3) {
  f = as.formula(paste(colnames[i], '~ region2'))
  if (i == 1) {
    jboxplot(
      f, data.byregion,
      col = col, alpha = col.alphas[i],
      ylim = c(0, 0.6),
      at2 = seq(0, 1, 0.1),
      medlwd = medlwd
    )
  } else {
    jboxplot(
      f, data.byregion,
      col = col, alpha = col.alphas[i],
      ylim = c(0, 0.6),
      at2 = seq(0, 1, 0.1), at2.label = rep('', length(seq(0, 1, 0.1))),
      medlwd = medlwd
    )
  }
  # labels
  if (i == 1) mtext(2, text = expression('TCE at 25 ' * degree * 'C ' * T[a] * ' (' * degree * 'C/%)'), line = label.line + 0.5, cex = cex.font - 0.1, las = 0)
  mtext(3, text = substitute(bold(letter), list(
    letter = toupper(letters)[i + 1]
  )), line = -1.75, adj = 0.03, cex = cex.font + 0.2)
  mtext(3, text = substitute(x * '% TCC', list(x = i * 10)), line = -1.9, adj = 0.92, cex = cex.font - 0.2)
}
# mean difference of TCE for different TCC levels at 25° Ta between European and South American cities
cat(
  'The mean difference of TCE for 10% TCC at 25° Ta between European and South American cities is ',
  round(
    data.byregion[region2 == 'EU', mean(tce.10.25)] - data.byregion[region2 == 'SAM', mean(tce.10.25)],
    2
  ),
  '.\n',
  sep = ''
)
cat(
  'The mean difference of TCE for 10% TCC at 25° Ta between European and South American cities is ',
  round(
    data.byregion[region2 == 'EU', mean(tce.30.25)] - data.byregion[region2 == 'SAM', mean(tce.30.25)],
    2
  ),
  '.\n',
  sep = ''
)


## TCE for different TCC and Ta levels ----
# data by latitude
data.bylat = copy(data.byregion)
# latitude bins
data.bylat[, 'bins' := cut(lat, breaks = 8, labels = 1:8)]
# box data
data.box = melt(
  data.bylat, id.vars = 'bins', measure = c('tce.10.25', 'tce.10.mean', 'tce.20.25', 'tce.20.mean', 'tce.30.25', 'tce.30.mean')
)
# levels
data.level = copy(data.byregion)[, 'bins' := cut(lat, breaks = 8)]
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
    data.bylat, id.vars = 'bins', measure = collist[[i]]
  )
  # at
  myat = box.at(length(unique(data.box$bins)), 2, 1.2, 0.5)
  # boxplot
  if (i == 1) {
    basegroupedboxframe(
      value ~ variable:bins, data = data.box,
      col = cols, alpha = col.alphas[i],
      ylim = c(0, 0.8),
      at1 = myat, at1.label = rep('', length(unique(data.box$bins))),
      at2 = seq(0, 0.8, 0.1),
      medlwd = medlwd, boxwex = boxwex
    )
  } else {
    basegroupedboxframe(
      value ~ variable:bins, data = data.box,
      col = cols, alpha = col.alphas[i],
      ylim = c(0, 0.8),
      at1 = myat, at1.label = rep('', length(unique(data.box$bins))),
      at2 = seq(0, 0.8, 0.1), at2.label = rep('', 9),
      medlwd = medlwd, boxwex = boxwex
    )
  }
  # labels
  text(
    cex = cex.font,
    x = c(myat$center),
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
  mtext(3, text = substitute(x * '% TCC', list(x = i * 10)), line = -1.9, adj = 0.92, cex = cex.font - 0.2)
  # legend
  legend(
    'topright',
    legend = c(
      expression('TCE at 25 ' * degree * 'C ' * T[a]),
      expression('TCE at mean ' * T[a] * ' of the growing season')
    ),
    fill = adjustcolor(cols, alpha.f = col.alphas[i]),
    cex = cex.legend, bty = 'n',
    x.intersp = x.intersp, y.intersp = y.intersp,
    inset = c(-0.28, 0.1)
  )
}
par(opar)
# function to compute the relative difference of TCE for 10% TCC at 25° Ta and mean Ta during the growing season
f = function(i) {
  data.box = melt(
    data.bylat, id.vars = 'bins', measure = collist[[3]]
  )
  round(
    (data.box[bins == i & variable == 'tce.30.25', mean(value)] - data.box[bins == i & variable == 'tce.30.mean', mean(value)]) / data.box[bins == i & variable == 'tce.30.mean', mean(value)] * 100,
    0
  )
}
# tropical cities
cat(
  'The relative difference of TCE for 10% TCC at 25° Ta and mean Ta during the growing season for tropical cities is between ',
  f(5),
  ' to ',
  f(4),
  '%.\n',
  sep = ''
)

# temperate cities
cat(
  'The relative difference of TCE for 10% TCC at 25° Ta and mean Ta during the growing season for temperate cities is ',
  f(7),
  '%.\n',
  sep = ''
)
# boreal cities
cat(
  'The relative difference of TCE for 10% TCC at 25° Ta and mean Ta during the growing season for boreal cities is ',
  f(8),
  '%.\n',
  sep = ''
)