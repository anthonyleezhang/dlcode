setwd("E:/Dropbox/projects/ownership/calibration_revisions")
rm(list = ls())
library(Rcpp)
source("functions/solve_value_function.R")
source("functions/solve_steadystate_old.R")

## Testing solve steadystate code

params = list(tau_res = 250, tau_max = 0.25, theta = 0.85, inv_frac = 0.4,
              delta = 0.95, max_runs = 500, Vtol = 10^-3, quiet = 1)

dir_string = "smalldata"
load(paste("models/", dir_string, "/matched_data.RData", sep = ""))

tau_try = 0

data = rawdata

data = solve_value_function(tau = tau_try, data = data, transmat = transmat, params = params)

## Testing

data = solve_steadystate_old(data, transmat, efficient = 0)

sourceCpp("functions/solvesteadystate.cpp")
out = solvesteadystate(data$Fgam, data$wtp, data$best_p, transmat, 0)

datac = copy(rawdata)

datac[, ss := out[,1]]
datac[, val_ss := out[,2]]
datac[, buyer_dist := out[,3]]
datac[, seller_dist := out[,4]]

sum(data$ss - datac$ss)
sum(data$val_ss - datac$val_ss)
sum(data$buyer_dist - datac$buyer_dist)
sum(data$seller_dist - datac$seller_dist)

# Efficient

data = solve_steadystate_old(data, transmat, efficient = 1)
out = solvesteadystate(data$Fgam, data$wtp, data$best_p, transmat, 1)

datac = copy(rawdata)

datac[, ss := out[,1]]
datac[, val_ss := out[,2]]
datac[, buyer_dist := out[,3]]
datac[, seller_dist := out[,4]]

sum(data$ss - datac$ss)
sum(data$val_ss - datac$val_ss)
sum(data$buyer_dist - datac$buyer_dist)
sum(data$seller_dist - datac$seller_dist)


