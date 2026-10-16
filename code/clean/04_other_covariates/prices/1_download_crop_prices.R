rm(list = ls())
source("clean/paths.R")

if (!nzchar(Sys.getenv("NASSQS_TOKEN")))
  stop("Set your own NASSQS_TOKEN environment variable before downloading NASS data.")



library(rnassqs)
library(data.table)
library(ggplot2)

`%!in%` <- Negate(`%in%`)

# Field crops ----
## state ---- 
params <- list(
  agg_level_desc = "NATIONAL",
  statisticcat_desc = "PRICE RECEIVED",
  group_desc = "FIELD CROPS",
  freq_desc = "ANNUAL",
  year__GE = 1865
)
price_field_crop <- nassqs(params)
price_field_crop <- data.table(price_field_crop)

## clean commodity name
price_field_crop[class_desc %!in% c("WINTER", "SPRING, DURUM", "SPRING, (EXCL DURUM)"), class_desc :=""]
price_field_crop[class_desc == "SPRING, DURUM", class_desc :="DURUM "]
price_field_crop[class_desc == "SPRING, (EXCL DURUM)", class_desc :="SPRING "]
price_field_crop[class_desc == "WINTER", class_desc :="WINTER "]
price_field_crop[, commodity := paste0(class_desc, commodity_desc)]
price_field_crop[, c("class_desc", "commodity_desc") := NULL]
price_field_crop <- price_field_crop[commodity != "MAPLE SYRUP"]


## keep only most common production and utilization
price_field_crop[, mode_prod_util_practice := .N, by = c("commodity", "prodn_practice_desc",  "util_practice_desc")]
price_field_crop[, max_prod_util_practice := max(mode_prod_util_practice), by = c("commodity")]
price_field_crop <- price_field_crop[mode_prod_util_practice == max_prod_util_practice]
price_field_crop[, c("mode_prod_util_practice", "max_prod_util_practice") := NULL]



## convert units
price_field_crop[grepl("LB", unit_desc), 
                 Value := Value * 2000]
price_field_crop[grepl("LB", unit_desc), 
                 unit_desc := gsub("LB", "TONS", unit_desc)]
price_field_crop[grepl("CWT", unit_desc), 
                 Value := Value * 17.9]
price_field_crop[grepl("CWT", unit_desc), 
                 unit_desc := gsub("CWT", "TONS", unit_desc)]
price_field_crop[commodity=="CORN" & grepl("BU", unit_desc), 
                 Value := Value /  0.0254]
price_field_crop[commodity=="CORN" & grepl("BU", unit_desc), 
                 unit_desc := gsub("BU", "TONS", unit_desc)]
price_field_crop[commodity=="SORGHUM" & grepl("BU", unit_desc), 
                 Value := Value /  0.0254]
price_field_crop[commodity=="SORGHUM" & grepl("BU", unit_desc),
                 unit_desc := gsub("BU", "TONS", unit_desc)]
price_field_crop[grepl("WHEAT", commodity) & grepl("BU", unit_desc), 
                 Value := Value / 0.027216]
price_field_crop[grepl("WHEAT", commodity) & grepl("BU", unit_desc), 
                 unit_desc := gsub("BU", "TONS", unit_desc)]
price_field_crop[grepl("SOYBEAN", commodity) & grepl("BU", unit_desc), 
                 Value := Value / 0.027216]
price_field_crop[grepl("SOYBEAN", commodity) & grepl("BU", unit_desc),
                 unit_desc := gsub("BU", "TONS", unit_desc)]
price_field_crop[grepl("BARLEY", commodity) & grepl("BU", unit_desc), 
                 Value := Value / 0.022772]
price_field_crop[grepl("BARLEY", commodity) & grepl("BU", unit_desc), 
                 unit_desc := gsub("BU", "TONS", unit_desc)]
price_field_crop[grepl("OAT", commodity) & grepl("BU", unit_desc), 
                 Value := Value / 0.014515]
price_field_crop[grepl("OAT", commodity) & grepl("BU", unit_desc), 
                 unit_desc := gsub("BU", "TONS", unit_desc)]
price_field_crop[grepl("FLAX", commodity) & grepl("BU", unit_desc), 
                 Value := Value / 0.02540134]
price_field_crop[grepl("FLAX", commodity) & grepl("BU", unit_desc), 
                 unit_desc := gsub("BU", "TONS", unit_desc)]
price_field_crop[grepl("RYE", commodity) & grepl("BU", unit_desc), 
                 Value := Value / 0.02540134]
price_field_crop[grepl("RYE", commodity) & grepl("BU", unit_desc), 
                 unit_desc := gsub("BU", "TONS", unit_desc)]
price_field_crop[grepl("BALE", unit_desc), 
                 Value := Value /  0.2142857]
price_field_crop[grepl("BALE", unit_desc), 
                 unit_desc := gsub("BALES", "TONS", unit_desc)]
price_field_crop[grepl("MILLET", commodity) & grepl("BU", unit_desc), 
                 Value := Value /  0.02540134]
price_field_crop[grepl("MILLET", commodity) & grepl("BU", unit_desc), 
                 unit_desc := gsub("BU", "TONS", unit_desc)]
price_field_crop[, unit_desc := gsub("TONS", "TON", unit_desc)]



## keep relevant columns
price_field_crop <- price_field_crop[
  , .(Value= mean(Value, na.rm=T)),
  by  = c("commodity", "year","state_fips_code", "state_name", 
          "county_code", "unit_desc")]#, "prodn_practice_desc", "util_practice_desc")]


write.csv(price_field_crop, 
          build_file("optional_covariates", "nass_prices", "price_field_crop_1865_2022.csv"),
          row.names = F)


# Fruits -----
## national -----
params <- list(
  agg_level_desc = "NATIONAL",
  statisticcat_desc = "PRICE RECEIVED",
  group_desc = "FRUIT & TREE NUTS",
  freq_desc = "ANNUAL",
  year__GE = 1865
)
price_fruits_tree_nuts <- nassqs(params)
price_fruits_tree_nuts <- data.table(price_fruits_tree_nuts)



## keep only most common production and utilization
price_fruits_tree_nuts[, mode_prod_util_practice := .N, by = c("commodity_desc", "prodn_practice_desc",  "util_practice_desc")]
price_fruits_tree_nuts[, max_prod_util_practice := max(mode_prod_util_practice), by = c("commodity_desc")]
price_fruits_tree_nuts <- price_fruits_tree_nuts[mode_prod_util_practice == max_prod_util_practice]
price_fruits_tree_nuts[, c("mode_prod_util_practice", "max_prod_util_practice") := NULL]



price_fruits_tree_nuts <- price_fruits_tree_nuts[unit_desc != "PCT OF PARITY"]
price_fruits_tree_nuts <- price_fruits_tree_nuts[unit_desc != "$ / BOX, ON TREE EQUIV"]
price_fruits_tree_nuts <- price_fruits_tree_nuts[unit_desc != "$ / BOX, PHD EQUIV"]
price_fruits_tree_nuts <- price_fruits_tree_nuts[unit_desc != "$ / BOX, FOB"]


## clean some records
price_fruits_tree_nuts <- price_fruits_tree_nuts[commodity_desc != "APRICOTS" | unit_desc != "$ / TON, DRY BASIS"]
price_fruits_tree_nuts <- price_fruits_tree_nuts[commodity_desc != "FIGS" | unit_desc != "$ / TON, DRY BASIS"]
price_fruits_tree_nuts <- price_fruits_tree_nuts[commodity_desc != "PRUNES" | unit_desc != "$ / TON, DRY BASIS"]
price_fruits_tree_nuts <- price_fruits_tree_nuts[ unit_desc != "$ / TON, FRESH BASIS"]
price_fruits_tree_nuts <- price_fruits_tree_nuts[ unit_desc != "$ / LB, GREEN BASIS"]


## drop some observations that dont have a simple unit
price_fruits_tree_nuts <- price_fruits_tree_nuts[ commodity_desc != "CRANBERRIES"]
price_fruits_tree_nuts <- price_fruits_tree_nuts[ commodity_desc != "COFFEE"]

## correct units
price_fruits_tree_nuts[grepl("LB", unit_desc), Value := Value / .0005]
price_fruits_tree_nuts[grepl("LB", unit_desc), unit_desc := gsub("LB", "TON", unit_desc)]
price_fruits_tree_nuts[grepl("BOX", unit_desc), Value := Value * 900]
price_fruits_tree_nuts[grepl("BOX", unit_desc), unit_desc := gsub("BOX", "TON", unit_desc)]
price_fruits_tree_nuts[grepl("CENTS", unit_desc), Value := Value * 100]
price_fruits_tree_nuts[grepl("CENTS", unit_desc), unit_desc := gsub("CENTS", "$", unit_desc)]
price_fruits_tree_nuts[grepl("CWT", unit_desc), Value := Value * 17.9 ]
price_fruits_tree_nuts[grepl("CWT", unit_desc), unit_desc := gsub("CWT", "TON", unit_desc)]


## aggregate by unit 
price_fruits_tree_nuts <- price_fruits_tree_nuts[
  , .(Value = mean(Value, na.rm=T), unit_desc=first(unit_desc)), 
  by = c("commodity_desc", "year", "state_fips_code", "state_name")]
price_fruits_tree_nuts[, commodity := commodity_desc]
price_fruits_tree_nuts[, commodity_desc := NULL]

write.csv(price_fruits_tree_nuts, 
          build_file("optional_covariates", "nass_prices", "price_fruits_1865_2022_national.csv"),
          row.names = F)




# Vegetables -----
## state ----
params <- list(
  agg_level_desc = "NATIONAL",
  statisticcat_desc = "PRICE RECEIVED",
  group_desc = "VEGETABLES",
  freq_desc = "ANNUAL",
  year__GE = 1865
)
price_veg <- data.table(nassqs(params))



## keep only most common production and utilization
price_veg[, mode_prod_util_practice := .N, by = c("commodity_desc", "prodn_practice_desc",  "util_practice_desc")]
price_veg[, max_prod_util_practice := max(mode_prod_util_practice), by = c("commodity_desc")]
price_veg <- price_veg[mode_prod_util_practice == max_prod_util_practice]
price_veg[, c("mode_prod_util_practice", "max_prod_util_practice") := NULL]



## delete some units
price_veg <- price_veg[unit_desc != "PCT OF PARITY"]

price_veg[grepl("CWT", unit_desc), Value := Value * 17.8]
price_veg[grepl("CWT", unit_desc), unit_desc := "$ / TON"]

price_veg[grepl("LB", unit_desc), Value := Value / .0005]
price_veg[grepl("LB", unit_desc), unit_desc := "$ / TON"]

## aggregate 
price_veg <- price_veg[, .(Value = mean(Value, na.rm =T)), 
                       by = c("commodity_desc", "year", "state_fips_code",
                              "state_name", 'unit_desc')]

price_veg[, commodity := commodity_desc]
price_veg[, commodity_desc := NULL]

write.csv(price_veg, 
          build_file("optional_covariates", "nass_prices", "price_vegetables_1865_2022.csv"),
          row.names = F)



