
run_calibration = function(dir_string, params) {
  tau_res = params$tau_res
  tau_max = params$tau_max
  theta = params$theta
  inv_frac = params$inv_frac
  delta = params$delta
  quiet = params$quiet
  
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
  
  taxdata = solve_investment_value(taxdata, inv_frac = inv_frac, delta = delta, theta = theta)
  
  data = rawdata
  # Hack
  data[, best_p := 0]
  data[, wtp := 0]
  effdata = solve_steadystate(data = data, transmat = transmat, efficient = 1)
  
  load(paste("models/", dir_string, "/moment_matching_out.RData", sep = ""))
  
  outdata = make_summarystats(taxdata, effdata, params, moment_matching_out, dir_string)
  write.csv(outdata, file = paste("results/", dir_string, "_sumstats.csv", sep = ""))
  
  save(valdata, taxdata, effdata, params, file = paste("results/", dir_string, "_calout.RData", sep = ""))
}















