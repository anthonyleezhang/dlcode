
setwd("E:/Dropbox/projects/ownership/calibration")
rm(list = ls())

source("sensitivity/moments_grids_functions.R")

target_saleprob = 0.05

### Vary sdmean

find_decayrate = function(logsd, target_saleprob) {
  saleprob_offset = function(decayrate) {
    print(paste("DECAY RATE:", decayrate))
    out = param_to_moments(c(logsd, decayrate), qres = 2000, gamres = 2000)[2] - target_saleprob
    print(paste("TARGET MISS:", out))
    return(out)
  }
  
  asdf = uniroot(saleprob_offset, interval = c(0.8, 0.995), tol = 0.00002)
  return(asdf$root)
}

sdmean_data = data.table(logsd = seq(0.5, 1.5, by = 0.02))

for(i in 1:dim(sdmean_data)[1]) {
  print(i)
  my_logsd = sdmean_data[i, logsd]
  print(paste("LOGSD:", my_logsd))
  
  my_decayrate = find_decayrate(my_logsd, target_saleprob = target_saleprob)
  out_moments = param_to_moments(c(my_logsd, my_decayrate))
  
  sdmean_data[i, decayrate := my_decayrate]
  sdmean_data[i, sdmean := out_moments[1]]
  sdmean_data[i, saleprob := out_moments[2]]
}

save(sdmean_data, file = "results/sensitivity/moments_sensitivity_grid_sdmean.RData")
write.csv(sdmean_data, file = "results/sensitivity/moments_sensitivity_grid_sdmean.csv")

