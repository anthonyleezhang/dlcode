library(Rcpp)
sourceCpp("functions/solvevalfun.cpp")

solve_value_function = function(tau, data, transmat, params) {
  out = solvevalfun(data$gam, data$Fgam, transmat, tau, params$delta, params$Vtol, params$quiet)
  
  data[, V := out[,1]]
  data[, EV := out[,2]]
  data[, wtp := out[,3]]
  data[, best_saleprob := out[,4]]
  data[, best_p := out[,5]]
  
  return(data)
}
