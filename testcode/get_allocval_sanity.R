
setwd("C:/Users/Anthony Lee Zhang/Dropbox/projects/ownership/dynamic_calibration")
##############

rm(list = ls())
source("calibration_functions.R")

get_alloc_welfare = function(tau_try, paramvec) {
  
  print(paste("TAU TRY:", tau_try))
  my_sdlog = paramvec[1]
  decay_rate = paramvec[2]
  
  if(sum(is.na(paramvec)) > 0) stop("Paramvec missing")
  if(decay_rate > 1) stop("Decay rate > 1")
  
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
  
  # Overwrite V with some start values
  data[, V := gam / (1 - delta * (1-tau_try))]
  
  data = solve_value_function(data, tau_try = tau_try, params = params)
  out = get_social_welfare(data, decay_rate = decay_rate, quiet = 1)
  
  val_ss = out$val_ss
  avg_gam = val_ss %*% data$gam
  
  print(paste("AVG GAM:", avg_gam))
  
  return(avg_gam)
}

paramvec = c(1, 0.97)

data = data.table(tau_try = seq(0.05, 0.1, by = 0.001))

for(i in 1:dim(data)[1]) {
  print(i)
  data[i, alloc_val := get_alloc_welfare(tau_try, paramvec)]
}

save(data, file = "results/allocval_sanity.RData")

qplot(data$tau_try, data$alloc_val)

## It seems then that this isn't the problem... almost fully monotone... 
# I can basically get things up to around tau 0.001 resolution but not really further. 
# Is it worth writing my own binsearch? The nonmonotonicities for small tau are serious enough
# to mess with the standard optimizer...



data = data.table(tau_try = seq(0.1, 0.2, by = 0.01/3))

for(i in 1:dim(data)[1]) {
  print(i)
  data[i, alloc_val := get_alloc_welfare(tau_try, paramvec)]
}

save(data, file = "results/allocval_sanity_2.RData")

qplot(data$tau_try, data$alloc_val)

## Ah, OK here's the issue. Instability is pretty high in this range, it seems. 
# Let's zoom in a bit more to verify...

data = data.table(tau_try = seq(0.15, 0.16, by = 0.001))

for(i in 1:dim(data)[1]) {
  print(i)
  data[i, alloc_val := get_alloc_welfare(tau_try, paramvec)]
}

save(data, file = "results/allocval_sanity_3.RData")

data2 = data
qplot(data2$tau_try, data2$alloc_val)

load("results/allocval_sanity_3.RData")

asdf = data[tau_try %% 0.001 == 0]
qplot(asdf$tau_try, asdf$alloc_val)


## OK with 0.01 res?

data = data.table(tau_try = seq(0.05, 0.2, by = 0.01))

for(i in 1:dim(data)[1]) {
  print(i)
  data[i, alloc_val := get_alloc_welfare(tau_try, paramvec)]
}

save(data, file = "results/allocval_sanity_4.RData")

qplot(data$tau_try, data$alloc_val)

### From some playing around I did... the value function tol literally doesn't matter.
# Changing tol from 10^-3 to 10^-4 literally to double error doesn't change avg gam...
# So it looks like the issue is more the qres, maybe...

## Increasing qres helps a lot... the qres 400 curve looks pretty smooth at 0.01 resolution. 
# Trying a higher res...

data = data.table(tau_try = seq(0.05, 0.2, by = 0.01/4))

for(i in 1:dim(data)[1]) {
  print(i)
  data[i, alloc_val := get_alloc_welfare(tau_try, paramvec)]
}

save(data, file = "results/allocval_sanity_5.RData")

qplot(data$tau_try, data$alloc_val)

### Look at this! Isn't it beautiful?
