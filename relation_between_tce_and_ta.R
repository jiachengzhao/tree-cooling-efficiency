## ----------------------------------
# Title: Relationship between tree cooling efficiency (TCE) and air temperature (Ta)
# Objective: Visualization the linear relationship between TCE and Ta
# Created by: Jiacheng Zhao
# Created on: 2022-02-21
# Copyright (c) Jiacheng Zhao, 2021
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------



# Data (2010) ----
relation = tce.filtered[[3]]
# city id and name
name = data.table(
  id = c(10533, 41448, 15418, 11166, 26262, 9505, 21855, 30077, 37520, 32068, 12490, 26704, 1578, 34896, 23620, 12398),
  name = c(
    'Brussels', 'Dallas', 'Paris', 'Rio de Janeiro', 'Lisbon', 'Perth', 'Rome',
    'Valencia', 'Detroit', 'Kyiv', 'Montreal', 'Moscow', 'Beijing', 'Denver', 'Seoul', 'Quebec City'
  )
)



# par ----
par(
  cex.axis = 0.9,
  las = 1,
  lwd = 0.1,
  mai = c(0.2, 0.3, 0.25, 0.15),
  mfcol = c(4, 4),
  oma = c(3, 4, 2, 2),
  tck = -0.03
)



# Plotting ----
for (i in 1:nrow(name)) {
  d = relation[id == name$id[i], .(air_temperature, tce.10)]
  d[, tce.10 := abs(tce.10)]
  plot(
    tce.10 ~ air_temperature, data = d,
    xaxs = 'i', yaxs = 'i', axes = F, ann = F,
    xlim = c(0, 40), ylim = c(0, 0.6)
    # at2 = seq(0, 0.6, 0.1),
  )
  axis(1, lwd = 0.5, mgp = c(3, 0.25, 0))
  axis(2, at = seq(0, 0.6, 0.15), lwd = 0.5, mgp = c(3, 0.4, 0))
  axis(3, lwd = 0.5, labels = F, mgp = c(3, 0.25, 0))
  axis(4, at = seq(0, 0.6, 0.15), labels = F, lwd = 0.5, mgp = c(3, 0.4, 0))
  points(d$air_temperature, abs(d$tce.10), pch = 21, bg = 'gray80', cex = 1.3)
  fit = MASS::rlm(abs(d$tce.10) ~ air_temperature, data = d)
  abline(fit, lwd = 0.1)
  ci95(d)
  if (i %in% seq(4, 16, 4)) mtext(1, text = expression('T'[a] * ' (' * degree * 'C)'), line = 1.9, cex = 0.7, las = 0)
  if (i %in% 1:4) mtext(2, text = expression('TCE.10 (' * degree * 'C/%)'), line = 2.8, cex = 0.7, las = 0)
  title(name[, name][i], adj = 0.955, line = -6, font.main = 1, col.main = 'black', cex.main = 1)
  title(addlmeq(fit, 3), adj = 0.18, line = -0.8, col.main = 'black', cex.main = 0.9)
  title(addr2(d$tce.10, predict(fit)), adj = 0.085, line = -1.8, col.main = 'black', cex.main = 0.9)
}
par(opar)