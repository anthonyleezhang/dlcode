
solve_investment_value = function(taxdata, inv_frac, theta, delta) {
  # Might generalize later, but mostly just a code dump for now
  
  base_price = taxdata[tau == 0, avg_transaction_price]
  
  ## Make investment to be inv_frac times transaction price, inclusive of investment value
  # This calculation is slightly wrong because it doesn't account for the 1-period cost accounting in asset price.
  # But it's pretty close and things are more intuitive this way, so leaving as-is.
  inv_factor = inv_frac / (1-inv_frac)
  inv_total_value = inv_factor * base_price * (1-delta)
  
  taxdata[, investment_value := 
            persistent_investment_value_function(tau, investment_total_value = inv_total_value,
                                                 delta = delta, theta = theta)]
  taxdata[, total_value := investment_value + avg_gam]
  
  taxdata[, inv_value_stock := investment_value / (1 - delta * (1-tau))]
  
  # Asset prices should add in flow investment cost to first period 
  # net util = total util - cost
  # Conditional on cost being sunk, we want to count total util into price
  # Hence have net util + cost = total_util
  
  taxdata[, investment_cost :=
            persistent_investment_cost_function(tau, investment_total_value = inv_total_value,
                                                delta = delta, theta = theta)]
  
  taxdata[, transaction_asset_price_inc_inv := avg_transaction_price + inv_value_stock + investment_cost]
  taxdata[, offered_asset_price_inc_inv := avg_offered_price + inv_value_stock + investment_cost]
  taxdata[, tax_rev_inc_inv := tau * offered_asset_price_inc_inv]
  
  # taxdata[, transaction_asset_price_inc_inv_old := avg_transaction_price + inv_value_stock]

  # Sanity, this should be equal invfrac
  # taxdata[1, investment_value / ((avg_transaction_price + inv_value_stock) * (1-delta))]
}

persistent_investment_value_function = function(tau, investment_total_value, theta, delta) {
  g_param = 2 * (1 - delta * theta) * investment_total_value
  
  investment = g_param * 
    (1 - tau) * (1 - delta * theta) / 
    (1 - delta * theta * (1 - tau))
  value = (investment - investment^2 / (2 * g_param) ) / (1 - delta * theta)
  
  if(g_param == 0) {value = 0 * tau}
  return(value)
}

# persistent_investment_value_function(0, 1, 0.85, 0.95)

persistent_investment_cost_function = function(tau, investment_total_value, theta, delta) {
  g_param = 2 * (1 - delta * theta) * investment_total_value
  
  investment = g_param * 
    (1 - tau) * (1 - delta * theta) / 
    (1 - delta * theta * (1 - tau))
  cost = (investment^2 / (2 * g_param) ) / (1 - delta * theta)
  
  if(g_param == 0) {cost = 0 * tau}
  return(cost)
}

# persistent_investment_cost_function(0, 1, 0.85, 0.95)
