
make_welfare_plot = function(taxdata, effdata, params) {
  eff_tradeprob = effdata[, (1-Fgam) %*% ss]
  eff_gam = effdata[, val_ss %*% gam]
  
  tau_eff = taxdata[which.max(total_value), tau]
  tau_alloc = taxdata[which.max(avg_gam), tau]
  tau_max = taxdata[, max(tau)]
  
  delta = params$delta
  
  ##### Units in baseline asset price
  
  base_transaction_price_flowequiv = taxdata[1, transaction_asset_price_inc_inv] * (1-delta)
  # base_offered_price_flowequiv = taxdata[1, offered_asset_price_inc_inv] * (1-delta)
  # base_total_welfare = taxdata[1, total_value]
  
  taxdata[, alloc_dev := (avg_gam - .SD[1, avg_gam]) / base_transaction_price_flowequiv]
  taxdata[, inv_dev := (investment_value - .SD[1, investment_value]) / base_transaction_price_flowequiv]
  taxdata[, total_dev := (total_value - .SD[1, total_value])  / base_transaction_price_flowequiv]
  
  # eff_tradeprob = efficient_summary$sale_freq[1]
  tau_eff = taxdata[which.max(total_value), tau]
  tau_alloc = taxdata[which.max(avg_gam), tau]
  
  plotdata = rbindlist(list(
    taxdata[, .(x = tau, y = alloc_dev, Series = "Allocative")],
    taxdata[, .(x = tau, y = inv_dev, Series = "Investment")],
    taxdata[, .(x = tau, y = total_dev, Series = "Total")]
  ))
  
  plotdata[, Series := factor(Series, levels = c("Allocative", "Investment", "Total"))]
  
  alloc_max = taxdata[, (eff_gam - .SD[1, avg_gam]) / base_transaction_price_flowequiv]
  alloc_eff = taxdata[tau == tau_eff, alloc_dev]
  alloc_min = taxdata[tau == 0, alloc_dev]
  
  inv_min = taxdata[which.max(alloc_dev), inv_dev]
  inv_eff = taxdata[tau == tau_eff, inv_dev]
  inv_max = taxdata[tau == 0, inv_dev]
  
  ylower = taxdata[, min(pmin(alloc_dev, inv_dev, total_dev))*1.2]
  yupper = alloc_max * 1.2
  text_ypos = ylower + (yupper - ylower) * 0.01
  
  welfare_plot = ggplot(plotdata, aes(x = x, y = y, group = Series, color = Series)) + 
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
    geom_hline(aes(yintercept = alloc_max), size = 1.3, color = "red", linetype = "dashed", alpha = 0.7) + 
    annotate("text", label = "eff_alloc_welfare",
             x = tau_max, 
             y = alloc_max - alloc_max * 0.01,
             size = 15, color = "red", alpha = 0.7, vjust = 0, hjust = 1, parse = TRUE)
  
  # Add scales, etc.
  welfare_plot = welfare_plot + 
    scale_x_continuous(name = "Tau", limits = c(0, tau_max), label = percent) + 
    scale_y_continuous(name = "Welfare Change", label = percent,
                       limits = c(ylower, yupper)) + 
    scale_color_manual(name = "Type", values = c("red", "blue", "purple")) + 
    labs(title = "Welfare") + 
    theme(text = element_text(size = 60), legend.position = "bottom", legend.key.size = unit(2, "cm"),
          plot.margin = unit(c(1,1,1,1), "cm"))
  
  return(welfare_plot)
  
}