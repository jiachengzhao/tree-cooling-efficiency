## ----------------------------------
# Title: Tree cooling
# Objective: Computing the surface cooling from global urban afforestation during the period 2000 to 2015
# Created by: Jiacheng Zhao
# Created on: 2021-11-19
# Copyright (c) Jiacheng Zhao, 2021
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------


# data processing ----
## tree cooling data ----
tc = list()
years = seq(2000, 2015, 5)
for (i in 1:4) {
  tc[[i]] = Reduce(function(...) merge(..., by = 'id', all = T), list(region, tce2[[i]], climate[[i]][, .(id, air_temperature)], tcc[[i]]))
  colnames(tc[[i]]) = paste(colnames(tc[[i]]), years[i], sep = '_')
  colnames(tc[[i]])[1:5] = c('id', 'lon', 'lat', 'region', 'country')
}
# merge
data.tc = Reduce(function(...) merge(..., by = c('id', 'lon', 'lat', 'region', 'country'), all = T), tc)
# use the same cities as those in BRT modeling
data.tc = data.tc[id %in% data.brt$id]

## fill up no-data parameter a and b ----
# mean parameter a during the growing season
data.tc[, c('a_2000_mean', 'a_2005_mean', 'a_2010_mean', 'a_2015_mean') := .(
  slope.a_2000 * air_temperature_2000 + intcp.a_2000,
  slope.a_2005 * air_temperature_2005 + intcp.a_2005,
  slope.a_2010 * air_temperature_2010 + intcp.a_2010,
  slope.a_2015 * air_temperature_2015 + intcp.a_2015
)]
# mean parameter b during the growing season
data.tc[, c('b_2000_mean', 'b_2005_mean', 'b_2010_mean', 'b_2015_mean') := .(
  slope.b_2000 * air_temperature_2000 + intcp.b_2000,
  slope.b_2005 * air_temperature_2005 + intcp.b_2005,
  slope.b_2010 * air_temperature_2010 + intcp.b_2010,
  slope.b_2015 * air_temperature_2015 + intcp.b_2015
)]
# city ids with no-data a and b
na.ids.2000 = sort(data.tc[which(is.na(data.tc$a_2000_mean))]$id)
na.ids.2005 = sort(data.tc[which(is.na(data.tc$a_2005_mean))]$id)
na.ids.2010 = sort(data.tc[which(is.na(data.tc$a_2010_mean))]$id)
na.ids.2015 = sort(data.tc[which(is.na(data.tc$a_2015_mean))]$id)
# fill up using the closest data
for (i in na.ids.2000) {
  dt.a = data.tc[id == i, .(a_2005_mean, a_2010_mean, a_2015_mean)]
  dt.b = data.tc[id == i, .(b_2005_mean, b_2010_mean, b_2015_mean)]
  va = as.numeric(dt.a)
  vb = as.numeric(dt.b)
  data.tc[id == i, c('a_2000_mean', 'b_2000_mean') := .(va[!is.na(va)][1], vb[!is.na(vb)][1])]
}
data.tc[id %in% na.ids.2005, c('a_2005_mean', 'b_2005_mean') := data.tc[id %in% na.ids.2005, .(a_2000_mean, b_2000_mean)]]
data.tc[id %in% na.ids.2010, c('a_2010_mean', 'b_2010_mean') := data.tc[id %in% na.ids.2010, .(a_2005_mean, b_2005_mean)]]
data.tc[id %in% na.ids.2015, c('a_2015_mean', 'b_2015_mean') := data.tc[id %in% na.ids.2015, .(a_2010_mean, b_2010_mean)]]

## tree canopy cover (TCC) change from 2000 to 2015 ----
data.tc[, tree_canopy_cover_change := tree_canopy_cover_2015 - tree_canopy_cover_2000]


# tree cooling ----
## cooling using changing tree cooling efficiency (TCE) ----
data.tc[, tree_cooling_mean :=
         a_2000_mean * (tree_canopy_cover_2000 + (tree_canopy_cover_2005 - tree_canopy_cover_2000)/2) ^ b_2000_mean - a_2000_mean * tree_canopy_cover_2000 ^ b_2000_mean + # the cooling from 2000 to 2002.5
         a_2005_mean * tree_canopy_cover_2005 ^ b_2005_mean - a_2005_mean * (tree_canopy_cover_2000 + (tree_canopy_cover_2005 - tree_canopy_cover_2000)/2) ^ b_2005_mean + # the cooling from 2002.5 to 2005
         a_2005_mean * (tree_canopy_cover_2005 + (tree_canopy_cover_2010 - tree_canopy_cover_2005)/2) ^ b_2005_mean - a_2005_mean * tree_canopy_cover_2005 ^ b_2005_mean + # the cooling from 2005 to 2007.5
         a_2010_mean * tree_canopy_cover_2010 ^ b_2010_mean - a_2010_mean * (tree_canopy_cover_2005 + (tree_canopy_cover_2010 - tree_canopy_cover_2005)/2) ^ b_2010_mean + # the cooling from 2007.5 to 2010
         a_2010_mean * (tree_canopy_cover_2010 + (tree_canopy_cover_2015 - tree_canopy_cover_2010)/2) ^ b_2010_mean - a_2010_mean * tree_canopy_cover_2010 ^ b_2010_mean + # the cooling from 2010 to 2012.5
         a_2015_mean * tree_canopy_cover_2015 ^ b_2015_mean - a_2015_mean * (tree_canopy_cover_2010 + (tree_canopy_cover_2015 - tree_canopy_cover_2010)/2) ^ b_2015_mean   # the cooling from 2012.5 to 2015
]

## cooling using non-linear and invariant TCE (2000) ----
data.tc[, tree_cooling_invariant_2000 := 
         a_2000_mean * tree_canopy_cover_2005 ^ b_2000_mean - a_2000_mean * tree_canopy_cover_2000 ^ b_2000_mean + # cooling from 2000 to 2005
         a_2000_mean * tree_canopy_cover_2010 ^ b_2000_mean - a_2000_mean * tree_canopy_cover_2005 ^ b_2000_mean + # cooling from 2005 to 2010
         a_2000_mean * tree_canopy_cover_2015 ^ b_2000_mean - a_2000_mean * tree_canopy_cover_2010 ^ b_2000_mean]  # cooling from 2010 to 2015

## cooling using non-linear and invariant TCE (2015) ----
data.tc[, tree_cooling_invariant_2015 := 
         a_2015_mean * tree_canopy_cover_2005 ^ b_2015_mean - a_2015_mean * tree_canopy_cover_2000 ^ b_2015_mean + # cooling from 2000 to 2005
         a_2015_mean * tree_canopy_cover_2010 ^ b_2015_mean - a_2015_mean * tree_canopy_cover_2005 ^ b_2015_mean + # cooling from 2005 to 2010
         a_2015_mean * tree_canopy_cover_2015 ^ b_2015_mean - a_2015_mean * tree_canopy_cover_2010 ^ b_2015_mean]  # cooling from 2010 to 2015
# absolute value of the cooling
data.tc[, tree_cooling_mean := -tree_cooling_mean][, tree_cooling_invariant_2000 := -tree_cooling_invariant_2000][, tree_cooling_invariant_2015 := -tree_cooling_invariant_2015]
# only select cities with TCC larger than 0.5 % to avoid too large TCE
data.tc = data.tc[tree_canopy_cover_2000 > 0.5][tree_canopy_cover_2005 > 0.5][tree_canopy_cover_2010 > 0.5][tree_canopy_cover_2015 > 0.5]
# description
cat(
  'TCC in more than ',
  round(nrow(data.tc[tree_canopy_cover_change > 0]) / nrow(data.tc) * 100),
  '% cities is increasing with a global mean increase of ',
  round(mean(data.tc[, tree_canopy_cover_change])),
  ' ± ',
  round(sd(data.tc[, tree_canopy_cover_change])),
  '%.\n',
  sep = ''
)
cat(
  'Urban afforestation on average yielded an additional surface cooling of ',
  round(mean(data.tc[, tree_cooling_mean]), 1),
  ' ± ',
  round(sd(data.tc[, tree_cooling_mean]), 1),
  ' \u00B0C.\n',
  sep = ''
)
# a fast look at the distribution of the cooling
hist(data.tc[, tree_cooling_mean], xlab = expression('Urban tree cooling (' * degree * 'C)'), main = '')
# fwrite(data.tc, 'tree_cooling.csv')

# density plot of tree cooling ----
# pdf of TCC
mypar(cex.axis = 2.5, mai = c(1, 1, 0, 0))
jplot(xlim = c(-10, 20), ylim = c(0, 0.1), at2 = seq(0, 0.12, 0.03), mgp1 = c(3, 1.2, 0), mgp2 = c(3, 0.5, 0), bg = adjustcolor('white', 0.5))
plotdensity(data.tc$tree_canopy_cover_change, adj = 2, alpha = 0.5)
par(opar)
# pdf of tree cooling
mypar(cex.axis = 2.5, mai = c(1, 1, 0, 0))
jplot(xlim = c(-2, 8), ylim = c(0, 0.35), at2 = seq(0, 0.35, 0.08), mgp1 = c(3, 1.2, 0), mgp2 = c(3, 0.5, 0), bg = adjustcolor('white', 0.5))
plotdensity(data.tc$tree_cooling_mean, adj = 2, alpha = 0.5)
expe(data.tc$tree_canopy_cover_change) # expectation
expe(data.tc$tree_cooling_mean) # expectation
par(opar)


# cooling comparison using the TCE in different periods ----
data.tc[, .N, by = region][order(N, decreasing = T)]
data.tc[, mean(tree_canopy_cover_change), by = region][order(V1, decreasing = T)]
# barplot data
data.bar = data.tc[region %in% data.tc[, .N, by = region][N > 10]$region, .(
  tree_cooling_invariant_2000 = median(tree_cooling_invariant_2000),
  tree_cooling = median(tree_cooling_mean),
  tree_cooling_invariant_2015 = median(tree_cooling_invariant_2015)
), by = region][order(tree_cooling, decreasing = T)]
region.name = data.bar$region
data.bar = t(as.matrix(data.bar[, -1]))
colnames(data.bar) = region.name
# par
mypar(
  cex.axis = 1.3,
  lwd = 0.8,
  mai = c(0.7, 0.7, 0.2, 0.2), mgp = c(3, 0.4, 0),
  oma = c(0, 0, 0.5, 0), tck = 0.01
)
# barplot
barplot(
  data.bar,
  beside = T,
  ylim = c(0, 4),
  col = c('yellow', 'green', 'firebrick2'),
  legend.text = c(
    'From TCE in 2000',
    'From dynamic TCE',
    'From TCE in 2015'
  ),
  args.legend = list(x = 35, y = 3, cex = 1.2)
)
# y-label
mtext(side = 2, line = 1.8, text = expression('Cooling benefits (' * degree * 'C)'), cex = 1.2, las = 0)
par(opar)
t(data.bar)[, 2] - t(data.bar)[, 1]
t(data.bar)[, 3] - t(data.bar)[, 2]