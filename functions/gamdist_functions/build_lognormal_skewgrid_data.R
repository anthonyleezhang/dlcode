# Skewed grid for better numerical accuracy

build_lognormal_stitchgrid_data = function(qres, gamres, 
                                           stitch_start = 0.8, stitch_end = 0.95, pctile_max = 0.99, meanlog, sdlog) {
  ### Make a grid that's quantile resolution below a cutoff, and then gam res above the cutoff
  
  ## Quantile grid piece
  
  temp = (0:qres) / qres * stitch_start
  qdata = data.table(gam = qlnorm(temp, meanlog = meanlog, sdlog = sdlog), temp = temp)
  
  stitchstart_gam = qlnorm(stitch_start, meanlog = meanlog, sdlog = sdlog)
  
  ## Gamma grid piece
  
  stitchend_gam = qlnorm(stitch_end, meanlog = meanlog, sdlog = sdlog)
  maxval = qlnorm(pctile_max, meanlog = meanlog, sdlog = sdlog) 
  
  gamdata_gamvec = seq(stitchend_gam, maxval, length.out = gamres)
  gamdata_pvec = plnorm(gamdata_gamvec, meanlog = meanlog, sdlog = sdlog)
  
  gamdata = data.table(gam = gamdata_gamvec, temp = gamdata_pvec)
  
  ### Stitching piece
  qstep = (qdata[.N, gam] - qdata[.N-1, gam])
  gamstep = (maxval - stitchend_gam) / gamres
  
  # Distance to travel
  dist = stitchend_gam - stitchstart_gam
  numsteps = floor(dist / ((qstep + gamstep)/2))
  
  temp = seq(qstep, gamstep, length.out = numsteps)
  stepsizes = temp / sum(temp) * dist
  
  # gamma values
  
  stitch_gamvec = stitchstart_gam + cumsum(stepsizes)
  stitch_pvec = plnorm(stitch_gamvec, meanlog = meanlog, sdlog = sdlog)
  stitchdata = data.table(gam = stitch_gamvec, temp = stitch_pvec)
  stitchdata = stitchdata[1:(.N-1)]
  
  data = rbindlist(list(qdata, stitchdata, gamdata))
  
  # Sanity
  # data[, plnorm(gam, meanlog = meanlog, sdlog = sdlog)] - data[, temp]
  
  data[ , fgam := diff(c(temp,1))]
  data[ , Fgam := cumsum(fgam)]
  data[ , temp := NULL]
  data[ , fgam := NULL]
  
  # qplot(data$gam, data$Fgam)
  
  return(data)
}

# asdf = build_lognormal_stitchgrid_data(qres = 200, gamres = 200, meanlog = 1, sdlog = 1)