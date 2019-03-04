setwd("E:/Dropbox/projects/ownership/calibration")
rm(list = ls())

library(grid)
library(gridExtra)

delta = 0.95
theta = 0.85

load("results/sensitivity/saleprob_sensitivity.RData")
load("results/sensitivity/sdmean_sensitivity.RData")

plotdata = rbindlist(list(
  sdmean_data[, .(x = sdmean, y = alloc_tau, Series = "Allocative")],
  sdmean_data[, .(x = sdmean, y = total_tau, Series = "Total")]
))

sdmean_besttax = ggplot(plotdata, aes(x = x, y = y, group = Series, color = Series)) + 
  geom_line(size = 1.3, alpha = 0.8) + 
  scale_x_continuous(name = "SDmean", label = percent) + 
  scale_y_continuous(limits = c(0, 0.15), name = "Optimal depreciation rate", label = percent) + 
  theme(text = element_text(size = 70), legend.position = "bottom") +
  scale_color_manual(name = "Type", values = c("darkorange", "blue"))
  
sdmean_data[, total_pct_gain_price := (total_tau_twelfare - base_twelfare) / (1-delta) / base_assetprice]
sdmean_data[, total_pct_gain_rot := (rot_twelfare - base_twelfare) / (1-delta) / base_assetprice]

plotdata = rbindlist(list(
  sdmean_data[, .(x = sdmean, y = total_pct_gain_price, Series = "Optimal")],
  sdmean_data[, .(x = sdmean, y = total_pct_gain_rot, Series = "Rule-of-thumb")]
))

sdmean_total_gain = ggplot(plotdata, aes(x = x, y = y, group = Series, color = Series)) + 
  geom_line(size = 1.3, alpha = 0.8) + 
  scale_x_continuous(name = "SDmean", label = percent) + 
  scale_y_continuous(limits = c(0, 0.15), name = "Welfare gain (% asset price)", label = percent) + 
  theme(text = element_text(size = 70), legend.position = "bottom") +
  scale_color_manual(name = "Type", values = c("red", "forestgreen"))

### Saleprob

plotdata = rbindlist(list(
  saleprob_data[, .(x = saleprob, y = alloc_tau, Series = "Allocative")],
  saleprob_data[, .(x = saleprob, y = total_tau, Series = "Total")]
))

saleprob_besttax = ggplot(plotdata, aes(x = x, y = y, group = Series, color = Series)) + 
  geom_line(size = 1.3, alpha = 0.8) + 
  scale_x_continuous(name = "Saleprob", label = percent) + 
  scale_y_continuous(limits = c(0, 0.15), name = "Optimal depreciation rate", label = percent) + 
  theme(text = element_text(size = 70), legend.position = "bottom") +
  scale_color_manual(name = "Type", values = c("darkorange", "blue"))


saleprob_data[, total_pct_gain_price := (total_tau_twelfare - base_twelfare) / (1-delta) / base_assetprice]
saleprob_data[, total_pct_gain_rot := (rot_twelfare - base_twelfare) / (1-delta) / base_assetprice]

plotdata = rbindlist(list(
  saleprob_data[, .(x = saleprob, y = total_pct_gain_price, Series = "Optimal")],
  saleprob_data[, .(x = saleprob, y = total_pct_gain_rot, Series = "Rule-of-thumb")]
))

saleprob_data[, total_pct_gain_price := (total_tau_twelfare - base_twelfare) / (1-delta) / base_assetprice]

saleprob_total_gain = ggplot(plotdata, aes(x = x, y = y, group = Series, color = Series)) + 
  geom_line(size = 1.3, alpha = 0.8) + 
  scale_x_continuous(name = "Saleprob", label = percent) + 
  scale_y_continuous(limits = c(0, 0.15), name = "Welfare gain (% asset price)", label = percent) + 
  theme(text = element_text(size = 70), legend.position = "bottom") +
  scale_color_manual(name = "Type", values = c("red", "forestgreen"))

g = arrangeGrob(
  sdmean_besttax, saleprob_besttax,
  sdmean_total_gain, saleprob_total_gain,
  ncol = 2)

png(filename = "graphs/moments_sensitivity.png", width = 1600*2, height = 1200*2)
grid.draw(g)
dev.off()





