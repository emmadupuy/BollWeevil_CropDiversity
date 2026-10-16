rm(list = ls())
source("clean/paths.R")

library(data.table)
library(ggplot2)

`%!in%` <- Negate(`%in%`)


## load prices 
field <- fread(build_file("optional_covariates", "nass_prices", "price_field_crop_1865_2022.csv"))
fruits <- fread(build_file("optional_covariates", "nass_prices", "price_fruits_1865_2022_national.csv"))
veg <- fread(build_file("optional_covariates", "nass_prices", "price_vegetables_1865_2022.csv"))

prices <- rbindlist(list(field, fruits, veg), fill = TRUE)
prices[, county_code := NULL]

## keep only the crops that appear in the crop diversity measure
crops <- c("RICE", "SUGARCANE","WHEAT", "CORN", "COTTON",
          "POTATOES","SWEET POTATOES",
           "BARLEY","RYE","TOBACCO", "OATS")

prices <- prices[commodity %in% crops]
prices[commodity == "OATS" & year == 1890 & state_name== "MARYLAND", 
       Value := Value/1000]

prices <- prices[Value >0]

## deflate
cpi <- setDT(read_excel(raw_file(
  "optional_covariates", "historical_cpi", "historical_cpi.xls"
)))
prices <- merge(prices, cpi, by = "year")
prices[, Value_adj := Value / cpi]



## average prices 
avg_prices <- prices[, .(avg_prices = mean(Value, na.rm = T)), by = c("commodity", "year")]
# ggplot(avg_prices[year >=1880]) + 
#   geom_bar(aes(x = factor(year)))

ggplot(avg_prices[commodity== "COTTON"& year <1920]) + 
  geom_line(aes(x = year, y = avg_prices))


## add a flag for the crops that are there always between
avg_prices[, flag_crop_that_appears_after_1880 := ifelse(
  commodity %in% c("RICE", "RYE", "SUGARCANE"), 1,0 )]

## Save
write.csv(avg_prices, 
          build_file("optional_covariates", "nass_prices", "clean_price_11_crops.csv"),
          row.names = F)


## plot prices 
ggplot(avg_prices[year >=1880]) + 
  geom_line(aes(x = year, y = avg_prices))+ 
  facet_wrap(~commodity, scales = "free_y") + 
  theme_bw()


temp <- avg_prices[, .(min_year_price = min(year)), by = "commodity"]
temp[ ,rank := 1:.N]
ggplot(temp) + geom_text(aes(x = factor(min_year_price), y = rank ,label = commodity)) + 
  theme_bw()
