setwd("E:/Dropbox/projects/ownership/calibration")
rm(list = ls())
library(Rcpp)
source("functions/solve_value_function.R")
source("functions/solve_steadystate.R")

params = list(tau_res = 50, tau_max = 0.25, theta = 0.85, inv_frac = 0.4,
              delta = 0.95, max_runs = 500, Vtol = 10^-3, quiet = 1)

tau_res = params$tau_res
tau_max = params$tau_max
theta = params$theta
inv_frac = params$inv_frac
delta = params$delta
quiet = params$quiet

dir_string = "lognormal_gambeta"
load(paste("models/", dir_string, "/matched_data.RData", sep = ""))

valdata = data.table(tau = numeric(0), gam = numeric(0), Fgam = numeric(0), V = numeric(0), EV = numeric(0),
                     wtp = numeric(0), best_saleprob = numeric(0), best_p = numeric(0), ss = numeric(0), val_ss = numeric(0), 
                     buyer_dist = numeric(0), seller_dist = numeric(0))

for(tau_try in (0:tau_res) / tau_res * tau_max) {
  print(paste("TAU_TRY:", tau_try))
  
  data = rawdata
  
  data = solve_value_function(tau = tau_try, data = data, transmat = transmat, params = params)
  data = solve_steadystate(data = data, transmat = transmat, efficient = 0)
  data[, tau := tau_try]
  
  setcolorder(data, names(valdata))
  valdata = rbindlist(list(valdata, data))
}

taxdata = valdata[, .(
  avg_gam = sum(val_ss * gam),
  sale_freq = sum(ss * best_saleprob),
  avg_transaction_price = sum(seller_dist * best_p),
  avg_offered_price = sum(ss * best_p),
  avg_pctile_markup = sum(ss * ((1-best_saleprob) - Fgam))
), by = tau]

slp(taxdata, tau, avg_gam)
slp(taxdata, tau, sale_freq)





