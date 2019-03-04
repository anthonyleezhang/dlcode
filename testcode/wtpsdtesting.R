

for (f in list.files(path = "functions", pattern="*.R")) {
  source(paste("functions/", f, sep = ""))
}

for (f in list.files(path = "functions/gamdist_functions", pattern="*.R")) {
  source(paste("functions/gamdist_functions/", f, sep = ""))
}

for (f in list.files(path = "functions/transmat_functions", pattern="*.R")) {
  source(paste("functions/transmat_functions/", f, sep = ""))
}


params = list(delta = 0.95, max_runs = 500, Vtol = 10^-3, quiet = 1)

paramvec = c(1.5, 0.95)

data = build_lognormal_stitchgrid_data(qres = 200, gamres = 200, meanlog = 1, sdlog = paramvec[1])
transmat = build_gambeta_transmat(data = data, beta_shape = 10, decay_rate = paramvec[2])

data = solve_value_function(tau_try = 0, data = data, transmat = transmat, params = params)
data = solve_steadystate(data = data, transmat = transmat, quiet = 1, efficient = 0)

## calculating various SD's

seller_dist = data$seller_dist
prices = data$best_p

mean_price = sum(seller_dist * prices)
var_price = sum((prices - mean_price)^2 * seller_dist)
sd_price = sqrt(var_price)
sd_price / mean_price


seller_dist = data$val_ss
prices = data$gam

mean_price = sum(seller_dist * prices)
var_price = sum((prices - mean_price)^2 * seller_dist)
sd_price = sqrt(var_price)
sd_price / mean_price


seller_dist = data$fgam
prices = data$gam

mean_price = sum(seller_dist * prices)
var_price = sum((prices - mean_price)^2 * seller_dist)
sd_price = sqrt(var_price)
sd_price / mean_price


data[, fgam := diff(c(0, Fgam))]

seller_dist = data$fgam
prices = data$wtp

mean_price = sum(seller_dist * prices)
var_price = sum((prices - mean_price)^2 * seller_dist)
sd_price = sqrt(var_price)
sd_price / mean_price


data[, logwtp := log(wtp)]

seller_dist = data$fgam
prices = data$logwtp

mean_price = sum(seller_dist * prices)
var_price = sum((prices - mean_price)^2 * seller_dist)
sd_price = sqrt(var_price)
sd_price / mean_price

qplot(data$logwtp, data$Fgam)

## Approxfun density
Fapprox = approxfun(data$logwtp, data$Fgam)

wtpgrid = seq(data[, min(logwtp)], data[, max(logwtp)], length.out = 1000)
qwer = data.table(logwtp = wtpgrid, Fgam = Fapprox(wtpgrid))
qwer[, fgam := diff(c(0, Fgam))]

qplot(qwer$logwtp, qwer$fgam)




