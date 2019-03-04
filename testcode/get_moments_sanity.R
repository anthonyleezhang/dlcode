# Get sensitivity of moment functions


setwd("C:/Users/Anthony Lee Zhang/Dropbox/projects/ownership/dynamic_calibration")
##############

rm(list = ls())
source("calibration_functions.R")

get_moments = function(paramvec) {
  my_sdlog = paramvec[1]
  decay_rate = paramvec[2]
  
  if(sum(is.na(paramvec)) > 0) stop("Paramvec missing")
  if(decay_rate > 1) stop("Decay rate > 1")
  
  tau_res = 100
  delta = 0.95
  q_res = 500
  beta_shape = 10
  
  params = list(decay_rate = decay_rate, delta = delta, beta_shape = beta_shape,
                max_runs = 500, tol_cutoff = 10^-3, quiet = 1)
  
  qvec = (1:(q_res-1)) / q_res
  data = data.table(gam = qlnorm(qvec, meanlog = 0, sdlog = my_sdlog), Fgam = qvec)
  
  ## Manually set highest Fgam to 1
  data[.N, Fgam := 1]
  data[, fgam := diff(c(0, Fgam))]
  setkey(data, "gam")
  
  tau_try = 0
  
  # Overwrite V with some start values
  data[, V := gam / (1 - delta * (1-tau_try))]
  
  data = solve_value_function(data, tau_try = tau_try, params = params)
  out = get_social_welfare(data, decay_rate = decay_rate, quiet = 1)
  
  seller_dist = out$seller_dist
  saleprob_by_seller = out$saleprob_by_seller
  prices = data$best_p
  
  saleprice_dist = (seller_dist * saleprob_by_seller) / sum (seller_dist * saleprob_by_seller)
  
  mean_price = sum(saleprice_dist * prices)
  var_price = sum((prices - mean_price)^2 * saleprice_dist)
  sd_price = sqrt(var_price)
  sdmean_moment = sd_price / mean_price
  
  saleprob_moment = out$saleprob
  
  return(c(sdmean_moment, saleprob_moment))
}

data = data.table(logsd = seq(0.9, 1, by = 0.01), decay_rate = 0.97)

for(i in 1:dim(data)[1]) {
  print(i)
  logsd = data[i, logsd]
  decay_rate = data[i, decay_rate]
  
  out = get_moments(c(logsd, decay_rate))
  
  data[i, sdmean := out[1]]
  data[i, saleprob := out[2]]
}

save(data, file = "results/getmoments_sanity_1.RData")

qplot(data$logsd, data$sdmean)
qplot(data$logsd, data$saleprob)

### This works

data = data.table(logsd = seq(0.9, 0.91, by = 0.001), decay_rate = 0.97)

for(i in 1:dim(data)[1]) {
  print(i)
  logsd = data[i, logsd]
  decay_rate = data[i, decay_rate]
  
  out = get_moments(c(logsd, decay_rate))
  
  data[i, sdmean := out[1]]
  data[i, saleprob := out[2]]
}

save(data, file = "results/getmoments_sanity_2.RData")

qplot(data$logsd, data$sdmean)
qplot(data$logsd, data$saleprob)

## At qres 500 I get ~logsd 0.001 resolution

data = data.table(logsd = 1, decay_rate = seq(0.97, 0.973, length.out = 6))

for(i in 1:dim(data)[1]) {
  print(i)
  logsd = data[i, logsd]
  decay_rate = data[i, decay_rate]
  
  out = get_moments(c(logsd, decay_rate))
  
  data[i, sdmean := out[1]]
  data[i, saleprob := out[2]]
}

save(data, file = "results/getmoments_sanity_3.RData")

qplot(data$decay_rate, data$sdmean)
qplot(data$decay_rate, data$saleprob)

## This works


data = data.table(logsd = 1, decay_rate = seq(0.97, 0.971, length.out = 10))

for(i in 1:dim(data)[1]) {
  print(i)
  logsd = data[i, logsd]
  decay_rate = data[i, decay_rate]
  
  out = get_moments(c(logsd, decay_rate))
  
  data[i, sdmean := out[1]]
  data[i, saleprob := out[2]]
}

save(data, file = "results/getmoments_sanity_4.RData")

qplot(data$decay_rate, data$sdmean)
qplot(data$decay_rate, data$saleprob)

## OK, this resolution is * really * high

data = data.table(logsd = 1, decay_rate = seq(0.97, 0.9701, length.out = 10))

for(i in 1:dim(data)[1]) {
  print(i)
  logsd = data[i, logsd]
  decay_rate = data[i, decay_rate]
  
  out = get_moments(c(logsd, decay_rate))
  
  data[i, sdmean := out[1]]
  data[i, saleprob := out[2]]
}

save(data, file = "results/getmoments_sanity_5.RData")

qplot(data$decay_rate, data$sdmean)
qplot(data$decay_rate, data$saleprob)

# * really really * high


data = data.table(logsd = 1, decay_rate = seq(0.97, 0.97001, length.out = 10))

for(i in 1:dim(data)[1]) {
  print(i)
  logsd = data[i, logsd]
  decay_rate = data[i, decay_rate]
  
  out = get_moments(c(logsd, decay_rate))
  
  data[i, sdmean := out[1]]
  data[i, saleprob := out[2]]
}

save(data, file = "results/getmoments_sanity_6.RData")

qplot(data$decay_rate, data$sdmean)
qplot(data$decay_rate, data$saleprob)

# OK, finally this starts breaking down for SDmean. But for saleprob we're still good and smooth.
# So the resolution we have on here is really high.