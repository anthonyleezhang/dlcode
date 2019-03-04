# Get grids of moments that vary sdmean holding decayrate constant, 
# and vice versa

##############

rm(list = ls())
##############

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

### Derivative moment matching code

param_to_moments = function(paramvec, qres = 1000, gamres = 1000) {
  data = build_lognormal_stitchgrid_data(qres = qres, gamres = gamres, meanlog = 0, sdlog = paramvec[1])
  transmat = build_gambeta_transmat(data = data, beta_shape = 20, decay_rate = paramvec[2])
  
  moments = get_moments(data, transmat, params)
  return(moments)
}

