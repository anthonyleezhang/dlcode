####################################
# Code written by Anthony Lee Zhang
# Email: anthonyzhang0@gmail.com
####################################

setwd("E:/Dropbox/projects/ownership/calibration")
library(Rcpp)

rm(list = ls())

for (f in list.files(path = "functions", pattern="*.R")) {
  source(paste("functions/", f, sep = ""))
}

params = list(tau_res = 250, tau_max = 0.25, theta = 0.85, inv_frac = 0.4,
              delta = 0.95, max_runs = 500, Vtol = 10^-3, quiet = 1)

# params = list(tau_res = 5, tau_max = 0.25, theta = 0.85, inv_frac = 0.4,
#               delta = 0.95, max_runs = 500, Vtol = 10^-3, quiet = 1)

run_calibration(dir_string = "lognormal_beta", params = params)
make_plots_tables(dir_string = "lognormal_beta")
run_calibration(dir_string = "lognormal_mixbeta_25", params = params)
run_calibration(dir_string = "lognormal_consjump", params = params)

clean_up(c("lognormal_beta", "lognormal_mixbeta_25", "lognormal_consjump"))


