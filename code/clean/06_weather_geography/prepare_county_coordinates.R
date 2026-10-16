# Build the county coordinates and areas used by the regression scripts.
# Open code/replication.Rproj, then Source this file.

library(sf)
library(dplyr)
library(data.table)

input <- "../data/raw/geography/cb_2016_us_county_500k/cb_2016_us_county_500k.shp"
output <- "../data/intermediate/panels/county_coordinates.rds"
if (!file.exists(input))
  stop("Missing ../data/raw/geography/cb_2016_us_county_500k/cb_2016_us_county_500k.shp")

# Preserve the original area/centroid calculation, projection and row order.
counties <- read_sf(input)
area <- st_area(counties) * 1e-6
counties <- st_centroid(counties)
counties <- counties %>% dplyr::select(STATEFP, COUNTYFP)
counties <- cbind(counties, st_coordinates(counties), area)
counties <- as.data.table(st_drop_geometry(counties))
counties[, base_fips := as.numeric(paste0(STATEFP, COUNTYFP))]
counties[, lat := Y]
counties[, lon := X]
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
saveRDS(counties, output)
