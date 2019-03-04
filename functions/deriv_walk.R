
deriv_walk = function(param_to_moments, startparam, target_moments, pcttol = c(0.001, 0.001), stepsize = 0.6) {
  myparam = startparam
  
  print(paste("START PARAM1: ", startparam[1], " START PARAM2: ", startparam[2]))
  
  # How far off
  base_moments = param_to_moments(startparam)
  moments_miss = matrix(target_moments - base_moments)
  pct_error = moments_miss / target_moments
  
  print(paste("PRICE SD: ", base_moments[1], " SALEPROB: ", base_moments[2]))
  print(paste("PRICE SD REL ERROR: ", pct_error[1], " SALEPROB REL ERROR: ", pct_error[2]))
  
  old_moments = base_moments
  
  while(any(abs(moments_miss) / target_moments > pcttol)) {
    # Derivative estimation
    
    eps_sdlog = 0.01
    
    sdlog_out = param_to_moments(myparam + c(eps_sdlog, 0))
    sdlog_deriv = (sdlog_out - old_moments) / eps_sdlog
    
    eps_decayrate = 0.0001
    
    decayrate_out = param_to_moments(myparam + c(0, eps_decayrate))
    decayrate_deriv = (decayrate_out - old_moments) / eps_decayrate
    
    deriv_matrix = cbind(sdlog_deriv, decayrate_deriv)
    # Walk
    myparam = myparam + solve(deriv_matrix) %*% moments_miss * stepsize
    
    print(paste("PARAM1: ", myparam[1], " PARAM2: ", myparam[2]))
    
    # Test
    moment_vec = param_to_moments(myparam)
    moments_miss = matrix(target_moments - moment_vec)
    pct_error = moments_miss / target_moments
    
    print(paste("PRICE SD: ", moment_vec[1], " SALEPROB: ", moment_vec[2]))
    print(paste("PRICE SD REL ERROR: ", pct_error[1], " SALEPROB REL ERROR: ", pct_error[2]))
    
    old_moments = moment_vec
  }
  
  rownames(myparam) = NULL
  return(list(paramvec = myparam, moment_vec = moment_vec))
}