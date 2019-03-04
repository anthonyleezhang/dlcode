
build_gambeta_transmat = function(data, beta_shape, decay_rate) {
  # If gamma = ith quantile, then gamma_t+1 distributed according to row i of decay_matrix  
  
  n = data[, length(gam)]
  decay_matrix = matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    my_gam = data[i, gam]
    betavec = data[gam <= my_gam, gam / my_gam]
    
    # Get beta CDF for temp, and then take difference to get pdf
    beta_pdf = diff(c(0, pbeta(betavec, shape1 = beta_shape * decay_rate, shape2 = beta_shape * (1-decay_rate))))
    
    decay_matrix[1:length(beta_pdf), i] = beta_pdf
  }
  
  # Hack..
  decay_matrix[1,1] = 1
  
  return(decay_matrix)
}

# Test
# data = data.table(gam = 50:100)
# transmat = build_gambeta_transmat(data, beta_shape = 10, decay_rate = 0.9)
# (t(transmat) %*% data$gam)
# (t(transmat) %*% data$gam) / data$gam
