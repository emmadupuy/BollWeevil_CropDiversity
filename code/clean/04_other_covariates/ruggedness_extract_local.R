## Ruggedness local ##
## Add TPI to TRI

rm(list=ls())
source("clean/paths.R")
list_packages <- list("parallel","dplyr","sf","CropScapeR","sp","xgboost","caret","mlr",
                      "parallelMap","mice","raster","stars","prism","stringr","purrr",
                      "rgdal","httr","SpaDES","tigris","exactextractr")
lapply(list_packages, require, character.only = TRUE)

set.seed(123)

## FUNCTION
make_shapefiles <- function(year, atlas){
  ## Used to make shapefiles for relevant years from Atlas of Historical County Boundaries
  my_year <- as.Date(year,"%Y-%m-%d")
  year_layer <- historical_counties[which(historical_counties$START_DATE<=my_year&historical_counties$END_DATE>my_year),]
  # move to USA Contiguous Albers Equal Area Conic
  year_layer <- st_transform(year_layer,proj_albers)
  year <- as.numeric(format(my_year,"%Y"))
  res <- list(year_layer,as.numeric(year))
  names(res) <- c("layer","year")
  return(res)
}

## 0. Prep ----

proj_albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 
                +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
historical_counties <- st_read(raw_file(
  "geography", "US_AtlasHCB_Counties", "US_HistCounties_Shapefile",
  "US_HistCounties.shp"
))
years <- c("1997-01-01")
# Get shapefiles
map1997 <- make_shapefiles(year=years, atlas=historical_counties)[[1]] %>% st_transform(proj_albers)
#testmap <- map1997 %>% filter(FIPS=="20133") 
#testmap <- st_make_valid(testmap)

## 1. Elevation Data ----
elevation <- raster(raw_file(
  "landfire", "LF2020_Elev_220_CONUS", "Tif", "LC20_Elev_220.tif"
))
crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
map1997 <- map1997 %>% st_transform(crs=crs)

tri <- terrain(x=elevation, opt="TRI", neighbors=8)
print("here")
ruggedness <- exactextractr::exact_extract(x=tri, y=map1997, fun="mean", weights="area",
                                           force_df=TRUE, append_cols=names(map1997))
colnames(ruggedness)[which(names(ruggedness) == "mean")] <- "ruggedness"
path_to_file <- build_file("covariates", "landfire_ruggedness_county.rds")
saveRDS(ruggedness,path_to_file)


# tpi 
tpi <- terrain(x=elevation, opt="roughness", neighbors=8)
ruggedness <- exactextractr::exact_extract(x=tpi, y=map1997, fun="mean", weights="area", force_df=TRUE, append_cols=names(map1997))
colnames(ruggedness)[which(names(ruggedness) == "mean")] <- "ruggedness_roughness"
path_to_file <- build_file("covariates", "ruggedness_county_roughness.rds")
saveRDS(ruggedness,path_to_file)

