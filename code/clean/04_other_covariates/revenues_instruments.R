## Script to look at the relations: 
# pest-diversity, IV-ed by highway

rm(list=ls())
source("clean/paths.R")
packages<-c("data.table","ggplot2","parallel","lubridate","tidyr",
            "stringr","dplyr","foreign","readxl","labelled","grid","ggExtra",
            "fixest","grid","ggpubr","wesanderson","binsreg","gridExtra", "boot",
            "modelr","usmap", "stringr")
lapply(packages, require, character.only = TRUE)
numCores <- detectCores() - 2
# Command
`%!in%` <- Negate(`%in%`)



# Price data 
prices <- fread(build_file(
  "optional_covariates", "nass_prices", "clean_price_11_crops.csv"
))

# FAO potential yields
output_density_hist <- fread(build_file(
  "optional_covariates", "fao", "output_density_6190_clean.csv"
))
output_density_hist[, fips := str_pad(fips, 5, "left", "0")]
output_density_hist[, state_fips_code := substr(fips, 1, 2)]
output_density_hist[, commodity := toupper(Crop)]
output_density_hist[commodity == "MAIZE", commodity := "CORN"]
output_density_hist[commodity == "WHITE POTATO", commodity := "POTATOES"]
output_density_hist[commodity == "SWEET POTATO", commodity := "SWEET POTATOES"]
output_density_hist[commodity == "DRYLAND RICE", commodity := "RICE"]
output_density_hist[commodity == "OAT", commodity := "OATS"]
output_density_hist <- output_density_hist[commodity != "SWEET POTATOES"]

## Join
revenues <- merge(
  prices[, c("commodity","year","avg_prices", "flag_crop_that_appears_after_1880")], 
  output_density_hist[, c("fips","output_density","Data Units","state_fips_code","commodity")], 
  by = c("commodity"), 
  allow.cartesian = T)

# Cleaning
revenues[, output_density := output_density / 1000]
revenues[, revenues_per_ha := output_density * avg_prices]

## revenue variance ----
instruments <- revenues[flag_crop_that_appears_after_1880 == 0,
                        .(sd_revenues_per_ha = sd(revenues_per_ha, na.rm = T),
                          mean_revenues_per_ha = mean(revenues_per_ha, na.rm = T)), 
                        by = c("year", "fips")]
instruments_no_tob <- revenues[flag_crop_that_appears_after_1880 == 0 & commodity != "TOBACCO", 
                               .(sd_revenues_per_ha_no_tob = sd(revenues_per_ha, na.rm = T),
                                 mean_revenues_per_ha_no_tob = mean(revenues_per_ha, na.rm = T)), 
                               by = c("year", "fips")]

instruments <- merge(instruments, instruments_no_tob, by = c("fips", "year"))

write.csv(instruments, 
          build_file("optional_covariates", "revenue_instruments.csv"),
          row.names = F)



## Other
for (yr in seq(1890, 1950, 10)){
  temp <- instruments[year == yr]
  # temp <-  unique(temp[, c("state", "sd_revenues_per_ha")])
  temp <- temp[!is.na(sd_revenues_per_ha)]
  print(plot_usmap(regions = "counties", exclude = c("HI", "AK"),
             data = temp, values = "sd_revenues_per_ha_no_tob", color = NA) + 
    ggtitle(yr) + 
    scale_fill_continuous(low = "white", high = "blue")) + 
    theme(legend.position = "bottom")
  
  ggsave(build_file(
    "optional_covariates",
    paste0("revenuessd_crop_avg_prices_by_state_", yr, ".png")
  ))

}
