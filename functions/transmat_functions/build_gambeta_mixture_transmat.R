
build_gambeta_mixture_transmat = function(data, mixparam, mean1, mean2, betashape1, betashape2) {
  # If gamma = ith quantile, then gamma_t+1 distributed according to row i of decay_matrix  
  
  n = data[, length(gam)]
  decay_matrix = matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    my_gam = data[i, gam]
    betavec = data[gam <= my_gam, gam / my_gam]
    
    # Get beta CDF for temp, and then take difference to get pdf
    beta_pdf_1 = diff(c(0, pbeta(betavec, shape1 = betashape1 * mean1, shape2 = betashape1 * (1-mean1))))
    beta_pdf_2 = diff(c(0, pbeta(betavec, shape1 = betashape2 * mean2, shape2 = betashape2 * (1-mean2))))
    
    beta_pdf = mixparam * beta_pdf_1 + (1-mixparam) * beta_pdf_2
    
    decay_matrix[1:length(beta_pdf), i] = beta_pdf
  }
  
  # Hack..
  decay_matrix[1,1] = 1
  
  return(decay_matrix)
}

# Test
# data = data.table(gam = 1:10)
# transmat = build_gambeta_transmat(data, beta_shape = 10, decay_rate = 0.9)
# (t(transmat) %*% data$gam)
# (t(transmat) %*% data$gam) / data$gam
# 
# data = data.table(gam = 1:10)
# transmat = build_gambeta_mixture_transmat(data, mixparam = 0, mean1 = 0.2, mean2 = 0.8, beta_shape = 10)
# asdf = transmat[,200]
# qplot(1:200, asdf)
# 
# qwer = build_gambeta_mixture_transmat(data, mixparam = 0, mean1 = 0.2, mean2 = 0.9, beta_shape = 10)
# asdf = transmat[,200]
# qplot(1:200, asdf)

