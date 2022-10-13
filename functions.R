## ----------------------------------
# Title: Functions 
# Objective: To generating an ensemble of functions used in this project
# Created by: Jiacheng Zhao
# Created on: 2022-04-26
# Copyright (c) Jiacheng Zhao, 2021
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------

# Functions ----
## scatter filter ----
scatter.filter = function(data) {
  DT = copy(data)
  DT[, id_cat := paste(id, image_id, sep = ':')]
  ids = DT[, .N, by = id_cat][N > 30, id_cat] # tree cover classes should be not less than 30
  ids2 = DT[id_cat %in% ids][, max(cover_class), by = id_cat][V1 > 30, id_cat] # maximum tree cover class should be not less than 30%
  return(
    DT[id_cat %in% ids2][
      cover_class > 0
    ][
      , !c('id_cat'), with = F
    ]
  )
}

## power fitting ----
pf = function(data) {
  tryCatch({
    fit = nls(
      median ~ a * cover_class ^ b,
      data = data,
      start = list(a = 30, b = -0.05),
    )
    list(
      id = data$id[1],
      image_id = data$image_id[1],
      air_temperature = data$air_temperature[1],
      a = as.numeric(coef(fit)[1]),
      b = as.numeric(coef(fit)[2]),
      r2 = as.numeric(bias(data$median, predict(fit))[1])
    )
  },
  warning = function(w) {},
  error = function(e) {})
}

## tce generator for a given tree canopy cover ----
tce.generator = function(data) {
  re = as.data.table(
    Reduce(
      rbind,
      sapply(split(data, by = c('id', 'image_id')), FUN = pf)
    )
  )
  cols = names(re)[3:ncol(re)]
  re[, (cols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = cols]
  # TCE at 10 % tree canopy cover
  re[, tce.10 := a * b * 10 ^ (b - 1)]
  # TCE at 20 % tree canopy cover
  re[, tce.20 := a * b * 20 ^ (b - 1)]
  # TCE at 30 % tree canopy cover
  re[, tce.30 := a * b * 30 ^ (b - 1)]
  return(re)
}

## tce generator for a given temperature ----
tce.generator2 = function(data) {
  
  city = list()
  
  slope.10 = list() # slope between the TCE at 10% tree canopy cover and air temperature
  slope.20 = list() # slope between the TCE at 20% tree canopy cover and air temperature
  slope.30 = list() # slope between the TCE at 30% tree canopy cover and air temperature
  
  intcp.10 = list() # intercept between the TCE at 10% tree canopy cover and air temperature
  intcp.20 = list() # intercept between the TCE at 10% tree canopy cover and air temperature
  intcp.30 = list() # intercept between the TCE at 10% tree canopy cover and air temperature
  
  slope.a = list() # slop between parameter a and air temperature
  slope.b = list() # slop between parameter b and air temperature
  
  intcp.a = list() # intercept between parameter a and air temperature
  intcp.b = list() # intercept between parameter b and air temperature
  
  r2.slope.10 = list() # r-square of slope.10
  
  tce.10.25 = list() # TCE at 10% tree canopy cover and 25 °C air temperature
  tce.20.25 = list() # TCE at 20% tree canopy cover and 25 °C air temperature
  tce.30.25 = list() # TCE at 30% tree canopy cover and 25 °C air temperature
  
  # loop in cities
  i = 1
  
  for (cityid in unique(data$id)) {
    
    d = data[id == cityid]
    
    tryCatch(
      
      {
        
        fit.10 = MASS::rlm(abs(tce.10) ~ air_temperature, data = d)
        fit.20 = MASS::rlm(abs(tce.20) ~ air_temperature, data = d)
        fit.30 = MASS::rlm(abs(tce.30) ~ air_temperature, data = d)
        
        fit.a = MASS::rlm(a ~ air_temperature, data = d)
        fit.b = MASS::rlm(b ~ air_temperature, data = d)
        
        city[[i]] = cityid
        
        slope.10[[i]] = as.numeric(coef(fit.10)[2])
        slope.20[[i]] = as.numeric(coef(fit.20)[2])
        slope.30[[i]] = as.numeric(coef(fit.30)[2])
        
        intcp.10[[i]] = as.numeric(coef(fit.10)[1])
        intcp.20[[i]] = as.numeric(coef(fit.20)[1])
        intcp.30[[i]] = as.numeric(coef(fit.30)[1])
        
        slope.a[[i]] = as.numeric(coef(fit.a)[2])
        slope.b[[i]] = as.numeric(coef(fit.b)[2])
        
        intcp.a[[i]] = as.numeric(coef(fit.a)[1])
        intcp.b[[i]] = as.numeric(coef(fit.b)[1])
        
        r2.slope.10[[i]] = as.numeric(bias(abs(d$tce.10), predict(fit.10))[1])
        
        tce.10.25[[i]] = predict(fit.10, newdata = data.frame(air_temperature = 25))
        tce.20.25[[i]] = predict(fit.20, newdata = data.frame(air_temperature = 25))
        tce.30.25[[i]] = predict(fit.30, newdata = data.frame(air_temperature = 25))
        
        i = i + 1
        
      },
      
      warning = function(w) {},
      error = function(e) {}
      
    )
    
  }
  
  DT = as.data.table(
    cbind(
      Reduce(rbind, city),
      Reduce(rbind, slope.10),
      Reduce(rbind, slope.20),
      Reduce(rbind, slope.30),
      Reduce(rbind, intcp.10),
      Reduce(rbind, intcp.20),
      Reduce(rbind, intcp.30),
      Reduce(rbind, slope.a),
      Reduce(rbind, slope.b),
      Reduce(rbind, intcp.a),
      Reduce(rbind, intcp.b),
      Reduce(rbind, r2.slope.10),
      Reduce(rbind, tce.10.25),
      Reduce(rbind, tce.20.25),
      Reduce(rbind, tce.30.25)
    )
  )
  
  names(DT) = c(
    'id',
    'slope.10', 'slope.20', 'slope.30',
    'intcp.10', 'intcp.20', 'intcp.30',
    'slope.a', 'slope.b', 'intcp.a', 'intcp.b',
    'r2.slope.10',
    'tce.10.25', 'tce.20.25', 'tce.30.25'
  )
  
  return(DT)
  
}

## get density ----
get_density = function(x, y, ...) {
  dens = MASS::kde2d(x, y, ...)
  ix = findInterval(x, dens$x)
  iy = findInterval(y, dens$y)
  ii = cbind(ix, iy)
  return(dens$z[ii])
}