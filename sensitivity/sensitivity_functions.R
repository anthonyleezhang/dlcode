
rm(list = ls())
library(Rcpp)
##############

for (f in list.files(path = "functions", pattern="*.R")) {
  source(paste("functions/", f, sep = ""))
}

for (f in list.files(path = "functions/gamdist_functions", pattern="*.R")) {
  source(paste("functions/gamdist_functions/", f, sep = ""))
}

for (f in list.files(path = "functions/transmat_functions", pattern="*.R")) {
  source(paste("functions/transmat_functions/", f, sep = ""))
}

delta = 0.95
theta = 0.85

### Given logsd, decay rate values, produce investment function

get_alloc_welfare = function(tau_try, rawdata, transmat) {
  
  print(paste("TAU TRY:", tau_try))
  
  data = copy(rawdata)
  
  params = list(delta = delta, max_runs = 500, Vtol = 10^-2, quiet = 1)
  
  data = solve_value_function(tau = tau_try, data = data, transmat = transmat, params = params)
  data = solve_steadystate(data = data, transmat = transmat, efficient = 0)
  
  avg_gam = data[, val_ss %*% gam]
  
  print(paste("AVG GAM:", avg_gam))
  
  return(avg_gam)
}

make_investment_function = function(rawdata, transmat, target_frac) {
  # Get asset price at 0
  
  data = copy(rawdata)
  
  params = list(delta = delta, max_runs = 500, Vtol = 10^-2, quiet = 1)
  
  data = solve_value_function(tau = 0, data = data, transmat = transmat, params = params)
  data = solve_steadystate(data = data, transmat = transmat, efficient = 0)
  
  avg_price = data[, seller_dist %*% best_p]
  
  inv_factor = target_frac / (1-target_frac)
  inv_total_value = inv_factor * avg_price * (1-delta)
  
  invval_func = function(tau) {return(persistent_investment_value_function(tau, investment_total_value = inv_total_value,
                                                                           delta = delta, theta = theta))}
  return(invval_func)
}

get_total_welfare = function(tau_try, invval_func, rawdata, transmat) {
  
  avggam = get_alloc_welfare(tau_try, rawdata, transmat)
  invval = invval_func(tau_try)
  
  total_welfare = avggam + invval
  print(paste("TOTAL WELFARE:", total_welfare))
  
  return(total_welfare)
}

get_base_asset_price = function(rawdata, transmat, inv_frac) {
  params = list(delta = delta, theta = theta, inv_frac = inv_frac, 
                max_runs = 500, Vtol = 10^-2, quiet = 1)
  
  data = copy(rawdata)
  
  data = solve_value_function(tau = 0, data = data, transmat = transmat, params = params)
  data = solve_steadystate(data = data, transmat = transmat, efficient = 0)
  data[, tau := 0]
  
  # For safety, this is copypasted from calibration
  taxdata = data[, .(
    avg_gam = sum(val_ss * gam),
    sale_freq = sum(ss * best_saleprob),
    avg_transaction_price = sum(seller_dist * best_p),
    avg_offered_price = sum(ss * best_p),
    avg_pctile_markup = sum(ss * ((1-best_saleprob) - Fgam))
  ), by = tau]
  
  taxdata = solve_investment_value(taxdata, inv_frac = inv_frac, delta = delta, theta = theta)
  
  return(taxdata$transaction_asset_price_inc_inv)
}

