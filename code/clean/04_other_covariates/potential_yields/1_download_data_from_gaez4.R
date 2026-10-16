source("clean/paths.R")
library(GAEZr)


allscenarios <- function(){
    tidyr::expand_grid(climate_model = "CRUTS32",
                       rcp = "Hist",
                       time_period = "6190")
}

## yl: Output Density (potential production divided by total grid cell area)
gaez_download_yield_allcrops(dir = raw_file("optional_covariates", "fao", "raw"),
                             scenarios = allscenarios(),
                             var = "yl")
