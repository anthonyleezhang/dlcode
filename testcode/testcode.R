
setwd("C:/Users/Anthony Lee Zhang/Dropbox/projects/ownership/calibration_robustness")

source("models/lognormal_oldbeta/build_lognormal_data.R")
source("models/lognormal_oldbeta/build_betadown_transmat.R")
source("solve_value_function.R")
source("solve_steadystate.R")

build_data = function() {build_lognormal_data(qres = 100, meanlog = 1, sdlog = 1)}
  
data = build_data()

build_transmat = function() {build_betadown_transmat(qres = 100, beta_shape = 10, decay_rate = 0.97)}

transmat = build_transmat()

params = list(delta = 0.95, max_runs = 500, Vtol = 10^-4, quiet = 0)

outdata = solve_value_function(tau_try = 0.5, data = data, transmat = transmat, params = params)
ssdata = solve_steadystate(data = outdata, transmat = transmat, quiet = 0, efficient = 0)

qplot(ssdata$Fgam, ssdata$ss)
qplot(ssdata$Fgam, ssdata$val_ss)
qplot(ssdata$Fgam, ssdata$buyer_dist)
qplot(ssdata$Fgam, ssdata$seller_dist)
