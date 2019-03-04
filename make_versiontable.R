setwd("E:/Dropbox/projects/ownership/calibration")
rm(list = ls())

library(stringr)
library(stargazer)

for (f in list.files(path = "functions", pattern="*.R")) {
  source(paste("functions/", f, sep = ""))
}

pctmaker = function(numvec, digits = 2) {
  temp = format(round(numvec * 100, digits = digits), nsmall = 2)
  out = p(temp, "%")
  return(out)
}

load("results/lognormal_beta_calout.RData")

data = data.table(invfrac = numeric(0), totOpt_tau = numeric(0), totOpt_total_gain = numeric(0),
                          totOpt_alloc_gain = numeric(0), totOpt_inv_loss = numeric(0), tau25_total_gain = numeric(0), tau5_total_gain = numeric(0))

for(my_invfrac in c(0, 0.1, 0.4, 0.7)) {
  taxdata = solve_investment_value(taxdata, inv_frac = my_invfrac, theta = 0.85, delta = 0.95)
  temp = make_summarystats(taxdata, effdata, params = list(delta = 0.95), moment_matching_out = list(moment_vec = c(0,0)), dir_string = "test")
  temp2 = temp[, .(invfrac = my_invfrac, totOpt_tau, totOpt_total_gain, totOpt_alloc_gain, totOpt_inv_loss, tau25_total_gain, tau5_total_gain)]
  
  data = rbindlist(list(data, temp2))
}

data[, invfrac := percent(invfrac)]
data[, totOpt_tau := pctmaker(totOpt_tau, digits = 1)]
data[, totOpt_total_gain := pctmaker(totOpt_total_gain)]
data[, totOpt_alloc_gain := pctmaker(totOpt_alloc_gain)]
data[, totOpt_inv_loss := pctmaker(totOpt_inv_loss)]
data[, tau25_total_gain := pctmaker(tau25_total_gain)]
data[, tau5_total_gain := pctmaker(tau5_total_gain)]

data[, tau5_total_gain := NULL]

write.csv(data, file = "results/versiontable.csv")
stargazer(data, type = "latex", summary = FALSE, out = "results/versiontable.tex", 
          out.header = FALSE, rownames = FALSE)

# Appendix table

rm(list = ls())

for (f in list.files(path = "functions", pattern="*.R")) {
  source(paste("functions/", f, sep = ""))
}

pctmaker = function(numvec, digits = 2) {
  temp = format(round(numvec * 100, digits = digits), nsmall = 2)
  out = p(temp, "%")
  return(out)
}

my_invfrac = 0.4

load("results/lognormal_beta_calout.RData")

taxdata = solve_investment_value(taxdata, inv_frac = my_invfrac, theta = 0.85, delta = 0.95)
temp = make_summarystats(taxdata, effdata, params = list(delta = 0.95), moment_matching_out = list(moment_vec = c(0,0)), dir_string = "test")
baseline_data = temp[, .(type = "Baseline", totOpt_tau, totOpt_total_gain, totOpt_alloc_gain, totOpt_inv_loss, tau25_total_gain, tau5_total_gain, allOpt_alloc_gain_pctmax)]

load("results/lognormal_mixbeta_25_calout.RData")

taxdata = solve_investment_value(taxdata, inv_frac = my_invfrac, theta = 0.85, delta = 0.95)
temp = make_summarystats(taxdata, effdata, params = list(delta = 0.95), moment_matching_out = list(moment_vec = c(0,0)), dir_string = "test")
mixbeta_data = temp[, .(type = "Mixbeta", totOpt_tau, totOpt_total_gain, totOpt_alloc_gain, totOpt_inv_loss, tau25_total_gain, tau5_total_gain, allOpt_alloc_gain_pctmax)]

load("results/lognormal_consjump_calout.RData")

taxdata = solve_investment_value(taxdata, inv_frac = my_invfrac, theta = 0.85, delta = 0.95)
temp = make_summarystats(taxdata, effdata, params = list(delta = 0.95), moment_matching_out = list(moment_vec = c(0,0)), dir_string = "test")
consjump_data = temp[, .(type = "Jump", totOpt_tau, totOpt_total_gain, totOpt_alloc_gain, totOpt_inv_loss, tau25_total_gain, tau5_total_gain, allOpt_alloc_gain_pctmax)]

data = rbindlist(list(baseline_data, mixbeta_data, consjump_data))

data[, invfrac := percent(invfrac)]
data[, totOpt_tau := pctmaker(totOpt_tau, digits = 1)]
data[, totOpt_total_gain := pctmaker(totOpt_total_gain)]
data[, totOpt_alloc_gain := pctmaker(totOpt_alloc_gain)]
data[, totOpt_inv_loss := pctmaker(totOpt_inv_loss)]
data[, tau25_total_gain := pctmaker(tau25_total_gain)]
data[, tau5_total_gain := pctmaker(tau5_total_gain)]
data[, allOpt_alloc_gain_pctmax := pctmaker(allOpt_alloc_gain_pctmax)]

data[, tau5_total_gain := NULL]

write.csv(data, file = "results/spectable.csv")
stargazer(data, type = "latex", summary = FALSE, out = "results/spectable.tex", 
          out.header = FALSE, rownames = FALSE)



