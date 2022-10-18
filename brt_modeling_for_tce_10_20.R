## ----------------------------------
# Title: BRT modeling (for Ta = 20 and 30 degree)
# Objective: To visualize the partial effects of different variables affecting TCE.10.20 and TCE.10.30
# Created by: Jiacheng Zhao
# Created on: 2022-11-18
# Copyright (c) Jiacheng Zhao, 2022
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------

# BRT for TCE.10.20 ----
set.seed(1)
gbms.10.20 = dismo::gbm.step(
  data = data.brt,
  gbm.x = c('lai', 'gdp', 'vpd', 'solar', 'ws', 'albedo', 'cloud'),
  gbm.y = 'tce.10.20',
  family = 'gaussian',
  tree.complexity = 10,
  tolerance = 0.01,
  tolerance.method = 'auto',
  learning.rate = 0.005,
  n.folds = 10,
  bag.fraction = 0.5
)
dismo::gbm.plot(
  gbms.10.20,
  smooth = F, rug = F, common.scale = F,
  x.label = '1',
  y.label = 'Partial effect on TCE.10.20',
  show.contrib = T,
  plot.layout = c(2, 4),
  cex.axis = 1.2, cex.lab = 1.2,
  las = 1,
  mgp = c(3, 0.3, 0),
  tck = 0.05
)

# BRT for TCE.10.30 ----
set.seed(1)
gbms.10.30 = dismo::gbm.step(
  data = data.brt,
  gbm.x = c('lai', 'gdp', 'vpd', 'solar', 'ws', 'albedo', 'cloud'),
  gbm.y = 'tce.10.30',
  family = 'gaussian',
  tree.complexity = 10,
  tolerance = 0.01,
  tolerance.method = 'auto',
  learning.rate = 0.005,
  n.folds = 10,
  bag.fraction = 0.5
)
dismo::gbm.plot(
  gbms.10.30,
  smooth = F, rug = F, common.scale = F,
  x.label = '1',
  y.label = 'Partial effect on TCE.10.30',
  show.contrib = T,
  plot.layout = c(2, 4),
  cex.axis = 1.2, cex.lab = 1.2,
  las = 1,
  mgp = c(3, 0.3, 0),
  tck = 0.05
)
