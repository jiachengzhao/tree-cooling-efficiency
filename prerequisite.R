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


# Data processing ----
## LST-tree cover scatters ----
scatter = lapply(list.files(pattern = 'scatter.+gfcc+.csv$'), fread)
# a basic filter
scatter = lapply(scatter, scatter.filter)

## tree cover ----
tcc = lapply(list.files(pattern = 'tree_canopy_cover.+csv$'), fread)

## climatic variables ----
### ERA 5 ----
climate = grep(list.files(pattern = 'era5'), pattern = 'annually', invert = T, value = T)
climate = lapply(climate, fread)
lapply(climate, function(x) x[, c('air_temperature', 'gross_domestic_product') := .(air_temperature - 273.15, gross_domestic_product / 1e10)])


# climate.x = list.files(pattern = 'climate_variables_era5_annually.+csv$')
# climate.x = lapply(climate.x, fread)
# lapply(climate.x, function(x) x[, c('air_temperature') := .(air_temperature - 273.15)])
# 
# pr = lapply(climate.x, function(x) x[, .(id, date, precipitation)][, lapply(.SD, sum), by = .(id), .SDcols = !c('date')])
# others = lapply(climate.x, function(x) x[, .(id, date, air_temperature, wind_speed, vapor_pressure_deficit, solar_radiation, thermal_radiation)][, lapply(.SD, mean), by = .(id), .SDcols = !c('date')])
# 
# 
# 
# for (i in 1:4) {
#   climate.x[[i]] = merge(
#     pr[[i]],
#     others[[i]],
#     by = 'id'
#   )
# }




### MSWX ----
climate2 = list.files(pattern = 'climate_variables_mswx.+csv$')
climate2 = lapply(climate2, fread)
lapply(climate2, function(x) {
  cols = names(x)[2:6]
  setnames(x, 1:6, c('id', paste0('mswx_', cols)))
})


### TerraClimate ----
climate3 = list.files(pattern = 'climate_variables_terra_climate.+csv$')
climate3 = lapply(climate3, fread)
lapply(climate3, function(x) {
  cols = names(x)[2:5]
  setnames(x, 1:5, c('id', paste0('terra_', cols)))
})

# ### GLDAS ----
# climate4 = list.files(pattern = 'climate_variables_gldas.+csv$')
# climate4 = lapply(climate4, fread)
# lapply(climate4, function(x) {
#   cols = names(x)[2:4]
#   setnames(x, 1:4, c('id', paste0('gldas_', cols)))
# })




## albedo ----
albedo = lapply(list.files(pattern = 'albedo.+csv$'), fread)


## LAI ----
### MODIS LAI ----
lai = lapply(list.files(pattern = '^lai.*modis'), fread)

### Landsat LAI ----
lai2 = lapply(list.files(pattern = '^lai.*landsat'), fread)


## trends in LAI, AOD and RH ----
slope = fread('./slope_of_lai_aod_and_rh_landsat.csv')


## elevation standard deviation (sd) ----
esd = fread('./elevation_sd.csv', na.strings = NULL)


## region and country ----
region = fread('./region.csv', na.strings = NULL)


# generation of TCE ----
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
mymerge = function(x, y) merge.data.table(x, y, by = 'id', all = T)
for (i in 1:4) {
  dl[[i]] = Reduce(mymerge, list(tce2[[i]], tcc[[i]], climate[[i]], climate2[[i]], climate3[[i]], albedo[[i]], lai[[i]], lai2[[i]], region))
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


# integrated data ----
re = Reduce(function(x, y) merge.data.table(x, y, by = c('id', 'lon', 'lat', 'region', 'country'), all = T), dl)
