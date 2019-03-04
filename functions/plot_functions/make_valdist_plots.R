
make_valdist_plots = function(valdata) {
  
  ### Make log gamma pdfs by interpfun tricks
  
  plotdata = data.table(
    loggam = numeric(0),
    Fgam = numeric(0),
    fgam = numeric(0),
    tau = numeric(0)
  )
  
  for(my_tau in c(0, 0.05, 0.1, 0.15, 0.2)) {
  # for(my_tau in c(0)) {
    data = valdata[tau == my_tau]
    data[, loggam := log(gam)]
    
    # Hack -- code lowest to a finite number
    min_loggam = data[, min(loggam)]
    data[is.na(loggam), loggam := min_loggam - 0.01]
    
    # End truncation
    subdata = data[Fgam < 0.9985]
    subdata[, val_cdf := cumsum(val_ss)]
    
    Fgam_approxfun = approxfun(x = subdata$loggam, y = subdata$val_cdf, rule = 2)
    
    loggrid = seq(subdata[loggam > -Inf, min(loggam)], subdata[, max(loggam)], length.out = 1000)
    
    temp = data.table(loggam = loggrid, Fgam = Fgam_approxfun(loggrid))
    temp[, fgam := c(0, diff(Fgam) / diff(loggam))]
    temp[, tau := my_tau]
    
    plotdata = rbindlist(list(plotdata, temp))
  }
  
  # Entering buyer distribution
  subdata = data[Fgam < 0.9985]
  subdata[, fgam := c(0, diff(Fgam) / diff(loggam))]
  
  valpdf = ggplot(plotdata) + 
    geom_line(size = 1.3, aes(x = loggam, y = fgam, group = tau, color = tau)) + 
    scale_x_continuous(name = "Log use value") + 
    scale_y_continuous(name = "Density") + 
    scale_color_gradient(name = "Tau", low = "green3", high = "red", limits = c(0, 0.26), label = percent) + 
    geom_line(size = 1.3, data = subdata, aes(x = loggam, y = fgam), color = "gray50") +
    theme(text = element_text(size = 60),
          legend.key.size = unit(1.5, "cm"),
          legend.title = element_text(size = 50),
          legend.text = element_text(size = 40))
  
  data = valdata[tau %in% c(0, 0.05, 0.1, 0.15, 0.2)]
  
  valcdf = ggplot(plotdata, aes(x = loggam, y = Fgam, group = tau, color = tau)) +
    geom_line(size = 1.3) +
    scale_x_continuous(name = "Log use value") +
    scale_y_continuous(name = "CDF") +
    scale_color_gradient(name = "Tau", low = "green3", high = "red", limits = c(0, 0.26)) +
    theme(text = element_text(size = 60),
          legend.key.size = unit(1.5, "cm"),
          legend.title = element_text(size = 50),
          legend.text = element_text(size = 40))
  
  out = list()
  
  out$valpdf = valpdf
  out$valcdf = valcdf
  
  return(out)
}