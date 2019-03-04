
build_consjump_transmat = function(data, p) {
  
  n = data[, length(gam)]
  decay_matrix <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    if (i == 1) {
      decay_matrix[1,i] = 1
    } else {
      decay_matrix[i,i] = p
      decay_matrix[1,i] = 1-p
    }
  }
  
  return(decay_matrix)
}



