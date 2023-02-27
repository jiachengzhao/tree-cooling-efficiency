# Title: Original scatter
# Objective: To plot the original scatter between land surface temperature (LST) and tree canopy cover (TCC)
# Created by: Jiacheng Zhao
# Created on: 2021-07-18
# Copyright (c) Jiacheng Zhao, 2021
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn


# Functions ------------------------------------------------------------------------------------------------------------
# ## function to mosaic images ----
# tif.process <- function(n) {
#   tif.list <- list()
#   for (i in 1:length(tif.files)) {
#     tif.list[[i]] <- stack(tif.files[i])[[n]]
#   }
#   tif.list$fun <- mean # use mean function to mosaic
#   m <- do.call(mosaic, tif.list)
#   names(m) <- names(tif.list[[1]])
#   return(m)
# }
opar = par(no.readonly = T)
library(smot)
## function to plot scatters ----
addeq = function(fit, decimal = 2) {
    eq = substitute(
      italic('y') ~ '=' ~ b * italic('x') ^ a,
      list(
        a = sprintf(paste0('%.', decimal, 'f'), as.numeric(stats::coef(fit))[2]),
        b = sprintf(paste0('%.', decimal, 'f'), as.numeric(stats::coef(fit))[1])
      )
    )
  return(eq)
}



scatters <- function(
  tree, lst,
  xlabel = expression(bold(paste('Tree cover (%)'))),
  ylabel = expression(bold(paste('LST (', degree, 'C)', sep = ''))),
  main, city.name
) {
  # data
  value.tree <- raster::getValues(tree)
  value.lst <- raster::getValues(lst)
  d <- data.table::data.table(x = value.tree, y = value.lst)
  d <- d[complete.cases(d)]
  d <- d[x > 0]
  # make bins
  # for (k in 0:100) d[(x >= min(d$x) + k) & (x < min(d$x) + k + 1), bin := (k + 1)]
  d[, bin := ceiling(x)]
  db <- d[, .(bin_x = median(x), bin_y = median(y)), by = bin]
  colnames(db) <- c('bin', 'x', 'y')
  # plotting original scatters
  scattermore::scattermoreplot(
    d$x, d$y,
    axes = F, ann = F,
    xaxs = 'i', yaxs = 'i',
    # xlim = c(0, 80), ylim = c(20, 60),
    xlab = '', ylab = '',
    col = rgb(0.25, 0.5, 0.75, 1)
  )
  # non-linear regression
  fit <- nls(
    y ~ a * bin ^ b,
    data = db,
    start = list(a = 50, b = -0.01)
  )
  lft = lm(
    y ~ bin,
    data = db
  )
  # regression line
  lines(
    seq(1, max(db$bin), 0.01),
    predict(fit, newdata = data.table::data.table(bin = seq(1, max(db$bin), 0.01))),
    col = 'red', lwd = 1.7
  )
  # abline(lft, col = 'black')
  # axis
  axis(1, at = pretty(d$x), lwd = 0.1, mgp = c(3, 0.1, 0))
  axis(2, at = pretty(d$y), lwd = 0.1, mgp = c(3, 0.15, 0))
  axis(3, at = seq(0, 100, 10), labels = F, lwd = 0.1, lwd.tick = 0)
  axis(4, at = seq(-100, 100, 10), labels = F, lwd = 0.1, lwd.tick = 0)
  # label
  mtext(1, text = xlabel, line = 1.5, cex = 0.7, las = 0)
  mtext(2, text = ylabel, line = 1.5, cex = 0.7, las = 0)
  # add title and r2
  mtext(main, side = 3, line = 0.1, font = 2, cex = 0.75)
  title(city.name, adj = 0.955, line = -1, font.main = 1, col.main = 'black', cex.main = 1)
  title(addeq(fit), adj = 0.955, line = -2, col.main = 'black', cex.main = 1)
  title(addr2(db$y, predict(fit)), adj = 0.955, line = -3, col.main = 'black', cex.main = 1)
}

# Plot parameters ------------------------------------------------------------------------------------------------------
par(
  cex.axis = 0.9,
  family = 'sans',
  las = 1,
  mai = c(0.15, 0.2, 0.15, 0.2),
  mfcol = c(4, 5),
  oma = c(2.5, 2.5, 2, 0.2),
  # pty = 's',
  tck = 0.025
)

# Scatter plots --------------------------------------------------------------------------------------------------------
sp <- function(direction) {
  dir = list.dirs(direction)[-1]
  for (j in 1:length(dir)) {
    tif.files <- list.files(dir[j], pattern = '.tif$', all.files = T, full.names = T)
    tif.names = tools::file_path_sans_ext(
      list.files(dir[j], pattern = '.tif$', all.files = T, full.names = F)
    )
    for (i in 1:length(tif.files)) {
      s = raster::stack(tif.files[i])
      if (j == 1) {
        if (i == 1) {
          scatters(
            s[[2]], s[[1]],
            xlabel = '', 
            main = list.dirs(direction, full.names = F)[-1][j],
            city.name = tif.names[i]
          )
        } else if (i == 4) {
          scatters(
            s[[2]], s[[1]], 
            main = '',
            city.name = tif.names[i]
          )
        } else {
          scatters(
            s[[2]], s[[1]],
            xlabel = '', main = '',
            city.name = tif.names[i]
          )
        }
      } else {
        if (i == 1) {
          scatters(
            s[[2]], s[[1]],
            xlabel = '', ylabel = '', 
            main = list.dirs(direction, full.names = F)[-1][j],
            city.name = tif.names[i]
          )
        } else if (i == 4) {
          scatters(
            s[[2]], s[[1]],
            ylabel = '', 
            main = '',
            city.name = tif.names[i]
          )
        } else {
          scatters(
            s[[2]], s[[1]],
            xlabel = '', ylabel = '', 
            main = '',
            city.name = tif.names[i]
          )
        }
      }
    }
  }
}
sp('./tce scatter images')

par(opar)