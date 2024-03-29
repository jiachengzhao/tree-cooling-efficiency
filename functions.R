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
scatter.filter = function(data) {
  DT = copy(data)
  DT[, id_cat := paste(id, image_id, sep = ':')]
  ids = DT[, .N, by = id_cat][N > 30, id_cat] # total tree cover classes should be not less than 30
  return(
    DT[id_cat %in% ids][
      cover_class > 0
    ][
      , !c('id_cat'), with = F
    ]
  )
}

gs.process = function(data) {
  DT = copy(data)
  DT[, c('start', 'end') := .(
    as.Date(start, origin = '1970-01-01'),
    as.Date(end, origin = '1970-01-01')
  )]
  return(DT[complete.cases(DT)])
}

gs.filter = function(data, gs) {
  m = merge(data, gs, by = 'id')
  m[, 'month' := as.numeric(format(lubridate::fast_strptime(date, '%Y/%m/%d'), '%m'))]
  l = list()
  for (j in 1:nrow(gs)) {
    l[[j]] = unique(
      as.numeric(
        format(seq(gs[j]$start, gs[j]$end, by = 'day'), '%m') # growing seasons
      )
    )
  }
  names(l) = gs$id # name each element by city id
  m.filtered = list()
  z = 1
  for (k in unique(m$id)) {
    m.filtered[[z]] = m[id == k][m[id == k, month] %in% unlist(l[as.character(k)], use.names = F)]
    z = z + 1
  }
  return(
    rbindlist(m.filtered)[, !c('start', 'end', 'month')] # very fast rbind for list
  )
}

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

lf = function(data) {
  tryCatch({
    fit = lm(
      median ~ cover_class,
      data = data
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

tce.generator = function(data, linear.tce = F) {
  if (linear.tce == T) {
    rl = rbindlist(
      parallel::mclapply(split(data, by = c('id', 'image_id')), FUN = lf)
    )
    # TCE at 10 % tree canopy cover
    rl[, tce.10 := b]
    # TCE at 20 % tree canopy cover
    rl[, tce.20 := b]
    # TCE at 30 % tree canopy cover
    rl[, tce.30 := b]
  } else {
    rl = rbindlist(
      parallel::mclapply(split(data, by = c('id', 'image_id')), FUN = pf)
    )
    # TCE at 10 % tree canopy cover
    rl[, tce.10 := a * b * 10 ^ (b - 1)]
    # TCE at 20 % tree canopy cover
    rl[, tce.20 := a * b * 20 ^ (b - 1)]
    # TCE at 30 % tree canopy cover
    rl[, tce.30 := a * b * 30 ^ (b - 1)]
  }
  return(rl)
}

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
  
  tce.10.20 = list() # TCE at 10% tree canopy cover and 30 °C air temperature
  
  tce.10.25 = list() # TCE at 10% tree canopy cover and 25 °C air temperature
  tce.20.25 = list() # TCE at 20% tree canopy cover and 25 °C air temperature
  tce.30.25 = list() # TCE at 30% tree canopy cover and 25 °C air temperature
  
  tce.10.30 = list() # TCE at 10% tree canopy cover and 30 °C air temperature
  
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
        
        tce.10.20[[i]] = predict(fit.10, newdata = data.frame(air_temperature = 20))
        
        tce.10.25[[i]] = predict(fit.10, newdata = data.frame(air_temperature = 25))
        tce.20.25[[i]] = predict(fit.20, newdata = data.frame(air_temperature = 25))
        tce.30.25[[i]] = predict(fit.30, newdata = data.frame(air_temperature = 25))
        
        tce.10.30[[i]] = predict(fit.10, newdata = data.frame(air_temperature = 30))
        
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
      Reduce(rbind, tce.10.20),
      Reduce(rbind, tce.10.25),
      Reduce(rbind, tce.20.25),
      Reduce(rbind, tce.30.25),
      Reduce(rbind, tce.10.30)
    )
  )
  
  names(DT) = c(
    'id',
    'slope.10', 'slope.20', 'slope.30',
    'intcp.10', 'intcp.20', 'intcp.30',
    'slope.a', 'slope.b', 'intcp.a', 'intcp.b',
    'r2.slope.10',
    'tce.10.20',
    'tce.10.25', 'tce.20.25', 'tce.30.25',
    'tce.10.30'
  )
  
  return(DT)
  
}

bio.tce = function(
    Rsw,
    alpha_i, alpha_v,
    Ts, Tl, Ta, rh, Pa,
    fc, # tree fraction
    rs, ra,
    Qah = 0, # anthropogenic heat
    return.vpd = F
) {
  
  vp = function(temperature) {
    611 * exp(17.27 * temperature / (temperature + 237.3))
  }
  delta = function(temperature) {
    e = vp(temperature)
    17.27 * 237.3 * e / (temperature + 237.3) ^ 2 
  }
  dew = function(rh, temperature) {
    a = 17.625
    b = 243.04
    alpha = log(rh / 100) + a * temperature / (b + temperature)
    b * alpha / (a - alpha)
  }
  
  epsilon = 0.98 # emissivity [-]
  sigma = 5.67e-08 # Stefan-Boltzmann constant [W/(m^2·K^4)]
  Ts_celsius = Ts - 273.15 # surface temperature in Celsius [°C]
  Tl_celsius = Tl - 273.15 
  Ta_celsius = Ta - 273.15 # air temperature in Celsius [°C]
  Ta_dew_celsius = dew(rh, Ta_celsius) # dew point temperature in Celsius [°C]
  esat = vp(Tl_celsius) # saturation vapor pressure [Pa]
  ea = vp(Ta_dew_celsius) # actual vapor pressure [Pa]
  vpd = esat - ea # vapor pressure deficit [Pa]
  rho_a = Pa / (287.04 * Ta) * (1 - ea / Pa * (1 - 0.622)) # air density [kg/m^3]
  Cp = 1005 + (Ta + 23.15) ^ 2 / 3364 # specific heat capacity of air at constant pressure [J/(kg·K)]
  gamma = 67  # psychrometric constant [Pa/K]
  delta_s = delta(Tl_celsius) # slope of saturation vapor pressure curve [Pa/K]
  delta_a = delta(Ta_dew_celsius) # slope of actual vapor pressure curve [Pa/K]
  delta_Ta = as.numeric(coef(lm(Ta ~ seq(0.2, 0.8, 0.2)))[2]) # sensitivity of Ta to fc
  delta_ra = as.numeric(coef(lm(ra ~ seq(0.2, 0.8, 0.2)))[2]) # sensitivity of aerodynamic resistance to fc
  
  fs = 4 * epsilon * sigma * Ts ^ 3 + (rho_a * Cp) / ra + (fc * rho_a * Cp * delta_s) / (gamma * (rs + ra))
  fa = rho_a * Cp / ra + (fc * rho_a * Cp * delta_a) / (gamma * (rs + ra))
  fra = rho_a * Cp * (Ts - Ta) / ra ^ 2 + (fc * rho_a * Cp * vpd) / (gamma * (rs + ra) ^ 2)
  
  tce = (Rsw * (alpha_i - alpha_v) - Qah - (rho_a * Cp * vpd) / (gamma * (rs + ra)) + fa * delta_Ta + fra * delta_ra) / fs
  
  if (return.vpd) {
    return(vpd / 1000)
  } else {
    return(-tce / 100)
  }
  
}

get_density = function(x, y, ...) {
  dens = MASS::kde2d(x, y, ...)
  ix = findInterval(x, dens$x)
  iy = findInterval(y, dens$y)
  ii = cbind(ix, iy)
  return(dens$z[ii])
}