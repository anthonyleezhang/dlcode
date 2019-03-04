asdf = valdata[tau == 0]

ggplot(asdf, aes(x = Fgam, y = ss)) +
  geom_line()

mygam = data[(.N-1), gam]
asdf = big_gamdata[seller_gam == mygam]

asdf[, cval := buyer_value_piece + Fgam * seller_wtp]
asdf[, max_cval := max(cval), by = seller_gam]

asdf[cval == max_cval]


big_gamdata[, cval := buyer_value_piece + Fgam * seller_wtp]
big_gamdata[, max_cval := max(cval), by = seller_gam]
asdf[465, cval] - asdf[464, cval]

for (f in list.files(path = "functions", pattern="*.R")) {
  source(paste("functions/", f, sep = ""))
}

dir_string = "lognormal_consjump"
load(paste("models/", dir_string, "/matched_data.RData", sep = ""))

params = list(tau_res = 50, tau_max = 0.5, theta = 0.85, inv_frac = 0.4, 
              delta = 0.95, max_runs = 500, Vtol = 10^-2, quiet = 0)

data = solve_value_function(tau_try = 0, data = data, transmat = transmat, params = params)
data = solve_steadystate(data = data, transmat = transmat, quiet = quiet, efficient = 0)

ggplot(data, aes(x = Fgam, y = ss)) +
  geom_line()

# Debugging...

gam_grid = data.table(expand.grid(list(seller_gam = data$gam, buyer_gam = data$gam)) )
setkey(gam_grid, "seller_gam", "buyer_gam")

data[, wtp := gam + delta * EV]
data[, buyer_value_piece := -Fgam * tau_try * wtp + (1 - Fgam) * (1-tau_try) * wtp]

temp = merge(gam_grid, data[, list(gam, buyer_value_piece, Fgam, wtp)], by.x = "buyer_gam", by.y = "gam")
big_gamdata = merge(temp, data[, list(gam, seller_wtp = wtp)], by.x = "seller_gam", by.y = "gam")

mygam = data[420, gam]

asdf = big_gamdata[seller_gam == mygam]
asdf[, cval := buyer_value_piece + Fgam * seller_wtp]

qwer = asdf[420:465]

ggplot(qwer, aes(x = buyer_gam, y = cval)) + 
  geom_line()

data[, Fss := cumsum(ss)]

ggplot(data, aes(x = Fss, y = best_p)) + 
  geom_line()



