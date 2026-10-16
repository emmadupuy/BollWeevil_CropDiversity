rm(list = ls())
source("clean/paths.R")

library(data.table)
library(ggplot2)
library(usmap)

`%!in%` <- Negate(`%in%`)

avg_prices <- fread(build_file(
  "optional_covariates", "nass_prices", "clean_price_11_crops.csv"
))


## price variance ----
sd <- avg_prices[flag_crop_that_appears_after_1880 == 0, 
                 .(sd_avg_prices = sd(avg_prices)), 
                 by = "year"]
summary(avg_prices$sd_avg_prices)

sd_no_tob <- avg_prices[flag_crop_that_appears_after_1880 == 0 & commodity != "TOBACCO", 
                        .(sd_avg_prices_no_tob = sd(avg_prices)), 
                 by = "year"]


instruments <- merge(sd, sd_no_tob, by ="year")


write.csv(instruments, 
          build_file("optional_covariates", "price_instrument.csv"),
          row.names = F)



## plots
ggplot(instruments[year < 2008 & year > 1885]) + 
  geom_line(aes(x = year, y = sd_avg_prices)) +
  theme_bw() + 
  ylab("Price variance between crops")

ggsave(build_file("optional_covariates", "sd_crop_avg_prices.png"))



ggplot(instruments[ year < 2008 & year > 1885]) + 
  geom_line(aes(x = year, y = sd_avg_prices_no_tob)) +
  theme_bw() + 
  ylab("Price variance between crops no tobacco")

ggsave(build_file("optional_covariates", "sd_crop_avg_prices_no_tobacco.png"))
