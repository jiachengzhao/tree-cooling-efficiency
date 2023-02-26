## ----------------------------------
# Title: Changing tree cooling efficiency (TCE) with tree cover and air temperature (Ta)
# Objective: To visualize how TCE change with tree cover and Ta
# Created by: Jiacheng Zhao
# Created on: 2022-08-10
# Copyright (c) Jiacheng Zhao, 2022
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------



require(lattice)



# Data ----
n = 20
# tree cover
x = seq(5, 40, length.out = n)
# Ta
y = seq(10, 30, length.out = n)
# data generator
data3d.generator = function(i, x, y) {
  slope.a = tce2[[3]][id == i]$slope.a
  intcp.a = tce2[[3]][id == i]$intcp.a
  slope.b = tce2[[3]][id == i]$slope.b
  intcp.b = tce2[[3]][id == i]$intcp.b
  d = expand.grid(x = x, y = y)
  setDT(d)
  d[, c('fa', 'fb') := .(slope.a * y + intcp.a, slope.b * y + intcp.b)][
    , z := -fa * fb * x ^ (fb - 1)
  ]
  return(copy(d))
}
id1 = 39175
id2 = 21855
tce2[[3]][id == id1, slope.a]
tce2[[3]][id == id2, slope.a]

data3d1 = data3d.generator(id1, x, y) # New York
data3d2 = data3d.generator(id2, x, y) # Madrid

# Plotting ----
print(
  wireframe(
    z ~ x * y,
    data = data3d1,
    scales = list(arrows = F, col = 'black', z = list(arrows = T)),
    xlab = list('Tree cover (%)', rot = -15, cex = 0.75),
    ylab = list(expression(T[a] * ' (' * degree * 'C)'), rot = 46, cex = 0.75),
    zlab = list(expression('TCE ' * ' (' * degree * 'C/%)'), rot = 97, cex = 0.75),
    zlim = c(0, 1),
    main = 'New York',
    drape = T,
    at = seq(0, 0.8, length = 10000),
    col.regions = rainbow(10000, alpha = 1),
    colorkey = list(height = 0.5, width = 1, tck = 0.5, axis.line = list(col = 'black', lwd = 0.6)),
    screen = list(z = 330, x = -60),
    par.settings = list(axis.line = list(col = 'transparent'))),
  split = c(1,1,2,1), more = T
)

print(
  wireframe(
    z ~ x * y,
    data = data3d2,
    scales = list(arrows = F, col = 'black', z = list(arrows = T)),
    xlab = list('Tree cover (%)', rot = -15, cex = 0.75),
    ylab = list(expression(T[a] * ' (' * degree * 'C)'), rot = 46, cex = 0.75),
    zlab = list(expression('TCE ' * ' (' * degree * 'C/%)'), rot = 97, cex = 0.75),
    zlim = c(0, 1),
    main = 'Rome',
    drape = T,
    at = seq(0, 0.8, length = 10000),
    col.regions = rainbow(10000, alpha = 1),
    colorkey = list(height = 0.5, width = 1, tck = 0.5, axis.line = list(col = 'black', lwd = 0.6)),
    screen = list(z = 330, x = -60),
    par.settings = list(axis.line = list(col = 'transparent'))),
  split = c(2,1,2,1)
)