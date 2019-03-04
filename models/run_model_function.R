run_model = function(datafun, transmatfun, startparam, model_name, params, target_moments) {
  
  param_to_moments = function(paramvec) {
    data = datafun(paramvec[1])
    transmat = transmatfun(paramvec[2], data)
    
    moments = get_moments(data, transmat, params)
    return(moments)
  }
  
  moment_matching_out = deriv_walk(param_to_moments = param_to_moments, 
                                   startparam = startparam, target_moments = target_moments)
  
  if(!file.exists(p("models/", model_name))) {dir.create(p("models/", model_name))}
  
  save(moment_matching_out, file = 
         paste("models/", model_name, "/moment_matching_out.RData", sep = ""))
  
  paramvec = moment_matching_out$paramvec
  write.csv(paramvec, file = paste("models/", model_name, "/param_values.csv", sep = ""))
  
  rawdata = datafun(paramvec[1])
  transmat = transmatfun(paramvec[2], rawdata)
  
  save(rawdata, transmat, file = 
         paste("models/", model_name, "/matched_data.RData", sep = ""))
}