# Get saleprob, sdlog moments given data and transmat

get_moments = function(data, transmat, params) {
  data = solve_value_function(tau = 0, data = data, transmat = transmat, params = params)
  data = solve_steadystate(data = data, transmat = transmat, efficient = 0)
  
  seller_dist = data$seller_dist
  prices = data$best_p
  
  mean_price = sum(seller_dist * prices)
  var_price = sum((prices - mean_price)^2 * seller_dist)
  sd_price = sqrt(var_price)
  sdmean_moment = sd_price / mean_price
  
  saleprob_moment = data[, ss %*% best_saleprob]
  
  return(c(sdmean_moment, saleprob_moment))
}


