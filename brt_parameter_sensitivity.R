## ----------------------------------
# Title: Sensitivity test
# Objective: To test parameter sensitivity of BRT model
# Created by: Jiacheng Zhao
# Created on: 2022-05-10
# Copyright (c) Jiacheng Zhao, 2022
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------

sens = data.table(
  tc = c(rep(5, 12), rep(10, 12)),
  lr = c(rep(0.001, 4), rep(0.005, 4), rep(0.01, 4), rep(0.001, 4), rep(0.005, 4), rep(0.01, 4)),
  bf = rep(seq(0.5, 0.8, 0.1), 6)
)
tr.corr = list()
cv.corr = list()
for (i in 1:nrow(sens)) {
  set.seed(1)
  mygbms = dismo::gbm.step(
    data = data.brt,
    gbm.x = c('lai', 'gdp', 'vpd', 'solar', 'ws', 'albedo', 'cloud'),
    gbm.y = 'tce.10.25',
    family = 'gaussian',
    tree.complexity = sens$tc[i],
    tolerance = 0.01,
    tolerance.method = 'auto',
    learning.rate = sens$lr[i],
    n.folds = 10,
    bag.fraction = sens$bf[i]
  )
  tr.corr[[i]] = mygbms$self.statistics$correlation
  cv.corr[[i]] = mygbms$cv.statistics$correlation.mean
}
sens[, c('tr.corr', 'cv.corr') := .(Reduce(c, tr.corr), Reduce(c, cv.corr))]
print(sens[order(cv.corr, decreasing = T)])
print(sens)