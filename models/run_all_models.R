setwd("E:/Dropbox/projects/ownership/calibration")
rm(list = ls())
library(Rcpp)

for (f in list.files(path = "functions", pattern="*.R")) {source(paste("functions/", f, sep = ""))}
for (f in list.files(path = "functions/gamdist_functions", pattern="*.R")) {source(paste("functions/gamdist_functions/", f, sep = ""))}
for (f in list.files(path = "functions/transmat_functions", pattern="*.R")) {source(paste("functions/transmat_functions/", f, sep = ""))}

source("models/run_model_function.R")

params = list(delta = 0.95, max_runs = 500, Vtol = 10^-3, quiet = 1)
target_moments = c(0.2, 0.05)
qres = 2000
gamres = 2000

###########

model_name = "lognormal_beta"

datafun = function(param) {
  return(build_lognormal_stitchgrid_data(qres = qres, gamres = gamres, meanlog = 0, sdlog = param))
}

transmatfun = function(param, data) {
  return(build_gambeta_transmat(data = data, beta_shape = 20, decay_rate = param))
}

run_model(datafun, transmatfun, startparam = c(1, 0.9), 
          model_name = model_name, params = params, target_moments = target_moments)

###########

model_name = "lognormal_mixbeta_75"

datafun = function(param) {
  return(build_lognormal_stitchgrid_data(qres = qres, gamres = gamres, meanlog = 0, sdlog = param))
}

transmatfun = function(param, data) {
  return(build_gambeta_mixture_transmat(data = data, mixparam = param, mean1 = 0.98, mean2 = 0.75, 
                                        betashape1 = 30, betashape2 = 10))
}

run_model(datafun, transmatfun, startparam = c(1, 0.9), 
          model_name = model_name, params = params, target_moments = target_moments)

###########

model_name = "lognormal_mixbeta_50"

datafun = function(param) {
  return(build_lognormal_stitchgrid_data(qres = qres, gamres = gamres, meanlog = 0, sdlog = param))
}

transmatfun = function(param, data) {
  return(build_gambeta_mixture_transmat(data = data, mixparam = param, mean1 = 0.98, mean2 = 0.5, 
                                        betashape1 = 30, betashape2 = 10))
}

run_model(datafun, transmatfun, startparam = c(1, 0.9), 
          model_name = model_name, params = params, target_moments = target_moments)

###########

model_name = "lognormal_mixbeta_25"

datafun = function(param) {
  return(build_lognormal_stitchgrid_data(qres = qres, gamres = gamres, meanlog = 0, sdlog = param))
}

transmatfun = function(param, data) {
  return(build_gambeta_mixture_transmat(data = data, mixparam = param, mean1 = 0.98, mean2 = 0.25, 
                                        betashape1 = 30, betashape2 = 10))
}

run_model(datafun, transmatfun, startparam = c(1, 0.9), 
          model_name = model_name, params = params, target_moments = target_moments)

###########

model_name = "lognormal_consjump"

datafun = function(param) {
  return(build_lognormal_stitchgrid_data(qres = qres, gamres = gamres, meanlog = 0, sdlog = param))
}

transmatfun = function(param, data) {
  return(build_consjump_transmat(data = data, p = param))
}

run_model(datafun, transmatfun, startparam = c(0.5, 0.9), 
          model_name = model_name, params = params, target_moments = target_moments)

