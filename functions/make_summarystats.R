
make_summarystats = function(taxdata, effdata, params, moment_matching_out, dir_string) {
  
  delta = params$delta
  # Make output table
  outdata = data.table(type = dir_string)
  
  # Zero tax rate statistics
  notax_data = taxdata[tau == 0]
  
  temp_notax_awelfare = notax_data[, avg_gam]
  temp_notax_iwelfare = notax_data[, investment_value]
  temp_notax_twelfare = notax_data[, total_value]
  temp_notax_salefreq = notax_data[, sale_freq]
  temp_notax_assetprice = notax_data[, transaction_asset_price_inc_inv]
  temp_notax_taxrev = notax_data[, tax_rev_inc_inv]
  
  # Allocatively optimal tax rate statistics
  allOpt_data = taxdata[which.max(avg_gam)]
  
  temp_allOpt_tau = allOpt_data[, tau]
  temp_allOpt_awelfare = allOpt_data[, avg_gam]
  temp_allOpt_iwelfare = allOpt_data[, investment_value]
  temp_allOpt_twelfare = allOpt_data[, total_value]
  temp_allOpt_salefreq = allOpt_data[, sale_freq]
  temp_allOpt_assetprice = allOpt_data[, transaction_asset_price_inc_inv]
  temp_allOpt_taxrev = allOpt_data[, tax_rev_inc_inv]
  
  # Total optimal tax statistics
  totOpt_data = taxdata[which.max(total_value)]
  
  temp_totOpt_tau = totOpt_data[, tau]
  temp_totOpt_awelfare = totOpt_data[, avg_gam]
  temp_totOpt_iwelfare = totOpt_data[, investment_value]
  temp_totOpt_twelfare = totOpt_data[, total_value]
  temp_totOpt_salefreq = totOpt_data[, sale_freq]
  temp_totOpt_assetprice = totOpt_data[, transaction_asset_price_inc_inv]
  temp_totOpt_taxrev = totOpt_data[, tax_rev_inc_inv]
  
  # 2.5 tax statistics
  tau25_data = taxdata[tau == 0.025]
  
  temp_tau25_tau = tau25_data[, tau]
  temp_tau25_awelfare = tau25_data[, avg_gam]
  temp_tau25_iwelfare = tau25_data[, investment_value]
  temp_tau25_twelfare = tau25_data[, total_value]
  temp_tau25_salefreq = tau25_data[, sale_freq]
  temp_tau25_assetprice = tau25_data[, transaction_asset_price_inc_inv]
  temp_tau25_taxrev = tau25_data[, tax_rev_inc_inv]
  
  # 5 tax statistics
  tau5_data = taxdata[tau == 0.05]
  
  temp_tau5_tau = tau5_data[, tau]
  temp_tau5_awelfare = tau5_data[, avg_gam]
  temp_tau5_iwelfare = tau5_data[, investment_value]
  temp_tau5_twelfare = tau5_data[, total_value]
  temp_tau5_salefreq = tau5_data[, sale_freq]
  temp_tau5_assetprice = tau5_data[, transaction_asset_price_inc_inv]
  temp_tau5_taxrev = tau5_data[, tax_rev_inc_inv]
  
  ## Flow equiv of baseline asset price
  notax_assetprice_flowequiv = temp_notax_assetprice * (1-delta)
  
  # Eff tradeprob
  eff_tradeprob = effdata[, (1-Fgam) %*% ss]
  outdata[, efficient_tradeprob := eff_tradeprob]
  
  # No tax tradeprob
  outdata[, notax_tradeprob := temp_notax_salefreq]
  
  # Allocatively optimal tax
  outdata[, allOpt_tau := temp_allOpt_tau]
  
  # Max alloc gain, as pct of initial average transaction price
  outdata[, allOpt_alloc_gain := (temp_allOpt_awelfare - temp_notax_awelfare) / notax_assetprice_flowequiv]
  
  # Alloc gain as pct of max alloc gain
  eff_gam = effdata[, val_ss %*% gam]
  outdata[, allOpt_alloc_gain_pctmax := (temp_allOpt_awelfare - temp_notax_awelfare) / (eff_gam - temp_notax_awelfare)]
  
  # Alloc tradeprob
  outdata[, allOpt_tradeprob := temp_allOpt_salefreq]
  
  
  ### Total optimal tax
  
  # Total optimal tax
  outdata[, totOpt_tau := temp_totOpt_tau]
  
  # Total welfare gain, pct transaction price
  outdata[, totOpt_total_gain := (temp_totOpt_twelfare - temp_notax_twelfare) / notax_assetprice_flowequiv]
  
  # Alloc gain
  outdata[, totOpt_alloc_gain := (temp_totOpt_awelfare - temp_notax_awelfare) / notax_assetprice_flowequiv]
  
  # Inv loss
  outdata[, totOpt_inv_loss := (temp_totOpt_iwelfare - temp_notax_iwelfare) / notax_assetprice_flowequiv]
  
  # Tradeprob
  outdata[, totOpt_tradeprob := temp_totOpt_salefreq]
  
  # Asset prices
  outdata[, totOpt_assetprice := temp_totOpt_assetprice]
  outdata[, notax_assetprice := temp_notax_assetprice]
  
  # Tax rev
  outdata[, totOpt_taxrev_npv := (temp_totOpt_taxrev / (1-delta) )]
  
  # Asset price change
  outdata[, totOpt_assetprice_ratio := temp_totOpt_assetprice / temp_notax_assetprice]
  
  ### Tau 2.5 total gain
  outdata[, tau25_total_gain := (temp_tau25_twelfare - temp_notax_twelfare) / notax_assetprice_flowequiv]
  
  ### Tau 5 total gain
  outdata[, tau5_total_gain := (temp_tau5_twelfare - temp_notax_twelfare) / notax_assetprice_flowequiv]
  
  outdata[, sdmean_moment := moment_matching_out$moment_vec[1]]
  outdata[, saleprob_moment := moment_matching_out$moment_vec[2]]
  
  return(outdata)
  
}
