# Get optimal tax, welfare numbers for varying saleprob moment

setwd("E:/Dropbox/projects/ownership/calibration")
##############

rm(list = ls())
source("sensitivity/sensitivity_functions.R")

load("results/sensitivity/moments_sensitivity_grid_saleprob.RData")

invfrac = 0.4
qres = 2000
gamres = 2000

for(i in 1:dim(saleprob_data)[1]) {
  print(i)
  
  data = build_lognormal_stitchgrid_data(qres = qres, gamres = gamres, meanlog = 0, sdlog = saleprob_data[i, logsd])
  transmat = build_gambeta_transmat(data = data, beta_shape = 20, decay_rate = saleprob_data[i, decayrate])
  
  invval_func = make_investment_function(data, transmat, invfrac)
  
  saleprob_data[i, base_awelfare := get_alloc_welfare(0, data, transmat)]
  saleprob_data[i, base_twelfare := base_awelfare + invval_func(0)]
  saleprob_data[i, base_assetprice := get_base_asset_price(data, transmat, invfrac)]
  
  saleprob_data[i, rot_awelfare := get_alloc_welfare(saleprob/2, data, transmat)]
  saleprob_data[i, rot_twelfare := rot_awelfare + invval_func(saleprob/2)]
  
  alloc_out = optimize(function(tau_try) {return(get_alloc_welfare(tau_try, data, transmat))}, 
                       interval = c(0, 0.3), tol = 0.0002, maximum = TRUE)
  
  saleprob_data[i, alloc_tau := alloc_out$maximum]
  saleprob_data[i, alloc_tau_awelfare := alloc_out$objective]
  
  total_out = optimize(function(tau_try) {return(get_total_welfare(tau_try, invval_func = invval_func, data, transmat))}, 
                       interval = c(0, 0.3), tol = 0.0002, maximum = TRUE)
  
  saleprob_data[i, total_tau := total_out$maximum]
  saleprob_data[i, total_tau_twelfare := total_out$objective]
  saleprob_data[i, total_tau_awelfare := get_alloc_welfare(total_out$maximum, data, transmat)]
}

save(saleprob_data, file = "results/sensitivity/saleprob_sensitivity.RData")
write.csv(saleprob_data, file = "results/sensitivity/saleprob_sensitivity.csv")

qplot(saleprob_data$saleprob, saleprob_data$alloc_tau)
qplot(saleprob_data$saleprob, saleprob_data$total_tau)

saleprob_data[, alloc_pct_gain := alloc_tau_awelfare / base_awelfare - 1]
qplot(saleprob_data$saleprob, saleprob_data$alloc_pct_gain)

delta = 0.95

saleprob_data[, total_pct_gain := (total_tau_twelfare - base_twelfare) / (1-delta) / base_assetprice]
qplot(saleprob_data$saleprob, saleprob_data$total_pct_gain)

saleprob_data[, total_pct_gain_rot := (rot_twelfare - base_twelfare) / (1-delta) / base_assetprice]
qplot(saleprob_data$saleprob, saleprob_data$total_pct_gain)

ggplot(saleprob_data) + 
  geom_line(aes(x = saleprob, y = total_pct_gain), color = "red") + 
  geom_line(aes(x = saleprob, y = total_pct_gain_rot), color = "blue") + 
  scale_y_continuous(limits = c(0,saleprob_data[, max(total_pct_gain)]))

