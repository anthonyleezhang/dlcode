
make_saleprob_plot = function(taxdata, effdata) {
  eff_tradeprob = effdata[, (1-Fgam) %*% ss]
  eff_gam = effdata[, val_ss %*% gam]
  
  tau_eff = taxdata[which.max(total_value), tau]
  tau_alloc = taxdata[which.max(avg_gam), tau]
  tau_max = taxdata[, max(tau)]
  
  #####
  
  plotdata = rbindlist(list(
    taxdata[, .(tau, val = sale_freq, Series = "Sale freq")],
    taxdata[, .(tau, val = tau, Series = "Tau")],
    taxdata[, .(tau, val = avg_pctile_markup, Series = "Avg quantile markup")]
  ))
  
  text_ypos = plotdata[, min(val)] + plotdata[, max(val) - min(val)] * 0.01
  text_xpos = tau_alloc + tau_max * 0.01
  
  plotdata[, Series := factor(Series, levels = c("Tau", "Sale freq", "Avg quantile markup"))]
  
  saleprob_plot = ggplot(plotdata, aes(x = tau, y = val, group = Series, color = Series)) + 
    geom_line(size = 1.3) + 
    geom_vline(aes(xintercept = tau_alloc), size = 1.3, color = "brown", linetype = "dashed", alpha = 0.7) + 
    annotate("text", label = "tau[alloc]",
             x = tau_alloc + tau_max * 0.01, 
             y = text_ypos,
             size = 15, color = "brown", alpha = 0.7, hjust = 0, parse = TRUE) + 
    geom_vline(aes(xintercept = tau_eff), size = 1.3, color = "brown", linetype = "dashed", alpha = 0.7) + 
    annotate("text", label = "tau[eff]",
             x = tau_eff + tau_max * 0.01, 
             y = text_ypos,
             size = 15, color = "brown", alpha = 0.7, hjust = 0, parse = TRUE) + 
    geom_vline(aes(xintercept = eff_tradeprob), size = 1.3, color = "brown", linetype = "dashed", alpha = 0.7) + 
    annotate("text", label = "eff_tradeprob",
             x = eff_tradeprob + tau_max * 0.01, 
             y = text_ypos,
             size = 12, color = "brown", alpha = 0.7, hjust = 0, parse = TRUE) + 
    scale_x_continuous(name = "Tau", limits = c(0, tau_max), label = percent) + 
    scale_y_continuous(name = "Percentage", 
                       limits = c(plotdata[, min(val)], plotdata[, max(val)]),
                       label = percent) + 
    scale_color_manual(name = "Type", values = c("darkorange", "blue", "forestgreen")) + 
    labs(title = "Sale frequency, quantile markups") + 
    theme(text = element_text(size = 60), legend.position = "bottom", legend.key.size = unit(2, "cm"),
          plot.margin = unit(c(1,1,1,1), "cm"))
  
  return(saleprob_plot)
}
