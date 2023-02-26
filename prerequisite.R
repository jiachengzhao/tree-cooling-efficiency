## ----------------------------------
# Title: Prerequisite
# Objective: Prerequisite and data processing
# Created by: Jiacheng Zhao
# Created on: 2021-08-16
# Copyright (c) Jiacheng Zhao, 2021
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------



# Prerequisite ----
Sys.setenv(LANG = 'en')
opar = par(no.readonly = T)
require(smot)
require(data.table)
source('./functions.R')
setwd('./data')



# Data processing ----
## LST-tree cover scatters ----
scatter = lapply(list.files(pattern = 'scatter.+gfcc+.csv$'), fread)
# a basic filter
scatter = lapply(scatter, scatter.filter)


## Tree cover ----
tcc = lapply(list.files(pattern = 'tree_canopy_cover.+csv$'), fread)


## Trend in tree cover ----
trend.tcc = fread('./temporal_trends_in_tree_cover.csv')


## Climatic variables ----
### ERA 5 ----
climate = grep(list.files(pattern = 'era5'), pattern = 'annually', invert = T, value = T)
climate = lapply(climate, fread)
lapply(climate, function(x) x[, c('air_temperature', 'gross_domestic_product') := .(air_temperature - 273.15, gross_domestic_product / 1e10)])


### MSWX ----
climate2 = list.files(pattern = 'climate_variables_mswx.+csv$')
climate2 = lapply(climate2, fread)
lapply(climate2, function(x) {
  cols = names(x)[2:6]
  setnames(x, 1:6, c('id', paste0('mswx_', cols)))
})


## Albedo ----
albedo = lapply(list.files(pattern = 'albedo.+csv$'), fread)


## LAI ----
### MODIS LAI ----
lai = lapply(list.files(pattern = '^lai.*modis'), fread)
### Landsat LAI ----
lai2 = lapply(list.files(pattern = '^lai.*landsat'), fread)


## Trends in LAI, AOD and RH ----
slope = fread('./slope_of_lai_aod_and_rh_landsat.csv')


## Elevation standard deviation (sd) ----
esd = fread('./elevation_sd.csv', na.strings = NULL)


## Region and country ----
region = fread('./region.csv', na.strings = NULL)



# Generation of TCE ----
tce = lapply(scatter, tce.generator)
tce.filtered = lapply(tce, function(x) x[a > 0][b < 0])
# proportion of city filtered by r2
years = seq(2000, 2015, 5)
for (i in 1:4) {
  cat(
    'The proportion of city filtered by r2 for ',
    years[i],
    ' is ',
    round(
      (length(unique(tce[[i]]$id)) - length(unique(tce.filtered[[i]]$id))) / length(unique(tce[[i]]$id)) * 100,
      1
    ),
    '%.\n',
    sep = ''
  )
}
tce2 = lapply(tce.filtered, tce.generator2)
tce2 = lapply(tce2, function(x) x[tce.10.25 > 0])
# dataset list
dl = list()
# merge TCE and other data
mymerge = function(x, y) data.table::merge.data.table(x, y, by = 'id')
for (i in 1:4) {
  dl[[i]] = Reduce(mymerge, list(tce2[[i]], tcc[[i]], climate[[i]], climate2[[i]], albedo[[i]], lai[[i]], lai2[[i]], region, slope))
  # add year label
  dl[[i]][, year := 2000 + 5 * (i - 1)][, year := as.factor(year)]
  # mean TCE during the growing season
  dl[[i]][, c('tce.10.mean', 'tce.20.mean', 'tce.30.mean') := .(intcp.10 + slope.10 * air_temperature, intcp.20 + slope.20 * air_temperature, intcp.30 + slope.30 * air_temperature)]
  # set column order
  setcolorder(
    dl[[i]],
    c(
      'year', 'id', 'lon', 'lat', 'region', 'country', 'tree_canopy_cover',
      colnames(
        dl[[i]][, .SD, .SDcols = !c('year', 'id', 'lon', 'lat', 'region', 'country', 'tree_canopy_cover')]
      )
    )
  )
}



# Data integration ----
re = Reduce(function(x, y) data.table::merge.data.table(x, y, by = c('id', 'lon', 'lat', 'region', 'country'), all = T), dl)