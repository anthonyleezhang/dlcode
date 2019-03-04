library(Rcpp)
sourceCpp("functions/solvesteadystate.cpp")

solve_steadystate = function(data, transmat, efficient = 0) {
  
  out = solvesteadystate(data$Fgam, data$wtp, data$best_p, transmat, efficient)
  
  data[, ss := out[,1]]
  data[, val_ss := out[,2]]
  data[, buyer_dist := out[,3]]
  data[, seller_dist := out[,4]]
  
  return(data)
}