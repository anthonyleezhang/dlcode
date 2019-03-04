
make_stocks_plot = function(taxdata, effdata, params) {
  eff_tradeprob = effdata[, (1-Fgam) %*% ss]
  eff_gam = effdata[, val_ss %*% gam]
  
  tau_eff = taxdata[which.max(total_value), tau]
  tau_alloc = taxdata[which.max(avg_gam), tau]
  tau_max = taxdata[, max(tau)]
  
  #####
  
  delta = params$delta
  
  base_total_welfare = taxdata[1, total_value]
  taxdata[, tax_rev_inv_stock := tax_rev_inc_inv / (1-delta)]
  
  plotdata = rbindlist(list(
    taxdata[, .(x = tau, y = transaction_asset_price_inc_inv / base_total_welfare, Series = "License price")],
    taxdata[, .(x = tau, y = tax_rev_inv_stock / base_total_welfare, Series = "License fee NPV")],
    taxdata[, .(x = tau, y = total_value / (1-delta) / base_total_welfare, Series = "Use value NPV")]
  ))
  
  text_ypos = plotdata[, max(y)] * 0.01
  text_xpos = tau_eff + tau_max * 0.01
  
  plotdata[, Series := factor(Series, levels = c("Use value NPV", "License fee NPV", "License price"))]
  
  stocks_plot = ggplot(plotdata, aes(x = x, y = y, group = Series, color = Series)) + 
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
    scale_y_continuous(name = "Multiples base flow welfare", limits = c(0, plotdata[, max(y)])) + 
    scale_color_manual(values = c("red", "darkorange", "forestgreen")) + 
    labs(title = "License prices, revenues") + 
    theme(text = element_text(size = 60), legend.position = "bottom", legend.key.size = unit(2, "cm"),
          plot.margin = unit(c(1,1,1,1), "cm"))
  
  return(stocks_plot)
}