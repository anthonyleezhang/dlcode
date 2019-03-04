
library(grid)
library(gridExtra)
library(ggplot2)
library(data.table)
library(scales)
library(Cairo)

for (f in list.files(path = "functions/plot_functions", pattern="*.R")) {
  source(paste("functions/plot_functions/", f, sep = ""))
}

make_plots_tables = function(dir_string) {
  
  load(paste("results/", dir_string, "_calout.RData", sep = ""))
  
  if(!file.exists(p("graphs/", dir_string))) {dir.create(p("graphs/", dir_string))}
  
  ## Welfare
  welfare_plot = make_welfare_plot(taxdata, effdata, params)
  
  CairoPNG(filename = paste("graphs/", dir_string, "/welfare.png", sep = ""), width = 1600, height = 1200)
  print(
    welfare_plot
  )
  dev.off()
  
  ### Sale probability 
  
  saleprob_plot = make_saleprob_plot(taxdata, effdata)
  
  CairoPNG(filename = paste("graphs/", dir_string, "/saleprob.png", sep = ""), width = 1600, height = 1200)
  print(
    saleprob_plot
  )
  dev.off()
  
  ### Stock values, asset prices, etc.
  
  stocks_plot = make_stocks_plot(taxdata, effdata, params)
  
  CairoPNG(filename = paste("graphs/", dir_string, "/stocks.png", sep = ""), width = 1600, height = 1200)
  print(
    stocks_plot
  )
  dev.off()
  
  g_welfare_plot = ggplotGrob(welfare_plot)
  g_saleprob_plot = ggplotGrob(saleprob_plot)
  g_stocks_plot = ggplotGrob(stocks_plot)
  
  maxwidths = unit.pmax(g_welfare_plot$widths[1:3], g_saleprob_plot$widths[1:3], g_stocks_plot$widths[1:3])
  
  g_welfare_plot$widths[1:3] = maxwidths
  g_saleprob_plot$widths[1:3] = maxwidths
  g_stocks_plot$widths[1:3] = maxwidths
  
  # minwidth = unit.pmin(g_welfare_plot$widths[4], g_saleprob_plot$widths[4], g_stocks_plot$widths[4])
  # 
  # g_welfare_plot$widths[4] = minwidth
  # g_saleprob_plot$widths[4] = minwidth
  # g_stocks_plot$widths[4] = minwidth
  
  # grob grid
  g = arrangeGrob(
    g_welfare_plot, g_saleprob_plot, g_stocks_plot,
    ncol = 1)
  
  CairoPNG(filename = paste("graphs/", dir_string, "/threeplotgrid.png", sep = ""), width = 1600, height = 1200 * 3)
  grid.draw(g)
  dev.off()
  
  # Valdists
  
  out = make_valdist_plots(valdata)
  
  CairoPNG(filename = paste("graphs/", dir_string, "/valpdf.png", sep = ""), width = 1600, height = 1200)
  print(out$valpdf)
  dev.off()
  
  CairoPNG(filename = paste("graphs/", dir_string, "/valcdf.png", sep = ""), width = 1600, height = 1200)
  print(out$valcdf)
  dev.off()
  
}


