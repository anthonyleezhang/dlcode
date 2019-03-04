
setwd("E:/Dropbox/projects/ownership/calibration")
rm(list = ls())

source("sensitivity/moments_grids_functions.R")

target_sdmean = 0.2

### Vary saleprob

find_sdlog = function(decay_rate, target_sdmean) {
  sdmean_offset = function(sdlog) {
    print(paste("SDLOG:", sdlog))
    out = param_to_moments(c(sdlog, decay_rate), qres = 2000, gamres = 2000)[1] - target_sdmean
    print(paste("TARGET LOSS:", out))
    return(out)
  }
  
  asdf = uniroot(sdmean_offset, interval = c(0.5, 2), tol = 0.0002)
  return(asdf$root)
}

saleprob_data = data.table(decayrate = c(seq(0.88, 0.98, by = 0.002)))

for(i in 1:dim(saleprob_data)[1]) {
  print(i)
  my_decayrate = saleprob_data[i, decayrate]
  print(paste("DECAY RATE:", my_decayrate))
  
  my_sdlog = find_sdlog(my_decayrate, target_sdmean = target_sdmean)
  out_moments = param_to_moments(c(my_sdlog, my_decayrate))
  
  saleprob_data[i, logsd := my_sdlog]
  saleprob_data[i, sdmean := out_moments[1]]
  saleprob_data[i, saleprob := out_moments[2]]
}

save(saleprob_data, file = "results/sensitivity/moments_sensitivity_grid_saleprob.RData")
write.csv(saleprob_data, file = "results/sensitivity/moments_sensitivity_grid_saleprob.csv")
