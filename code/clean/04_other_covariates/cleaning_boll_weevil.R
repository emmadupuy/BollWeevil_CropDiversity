# Build the boll-weevil county-year variables used by the paper.
# Open replication.Rproj, then run this file from top to bottom.

rm(list = ls())
source("clean/paths.R")

library(data.table)
library(dplyr)
library(readxl)
library(stringr)
library(sf)
library(purrr)
library(tidyr)

make_shapefile <- function(date, atlas, crs) {
  date <- as.Date(date)
  atlas[atlas$START_DATE <= date & atlas$END_DATE > date, ] |>
    st_transform(crs)
}

weighted_sum <- function(data, variable) {
  data |>
    group_by(base_FIPS) |>
    summarize(
      "{variable}" := sum(.data[[variable]] * weight_agarea1, na.rm = TRUE),
      .groups = "drop"
    )
}

proj_albers <- paste(
  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96",
  "+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
)

boll_weevil <- as.data.table(read_excel(raw_file(
  "boll_weevil", "Analysis_Master_1900.xlsx"
)))
setnames(boll_weevil, c("STATE", "COUNTY"), c("NHGISST", "NHGISCTY"))
gis_columns <- grep(
  "GISJ|FID|DECADE|SHAPE|Orig|Max|Min", names(boll_weevil), value = TRUE
)
boll_weevil[, (gis_columns) := NULL]
boll_weevil[, c("STATENAM", "NHGISNAM") := NULL]

historical_counties <- st_read(raw_file(
  "geography", "US_AtlasHCB_Counties", "US_HistCounties_Shapefile",
  "US_HistCounties.shp"
), quiet = TRUE)
map_1900 <- make_shapefile("1900-01-01", historical_counties, proj_albers) |>
  st_make_valid()

nhgis_1900 <- st_read(raw_file(
  "geography", "nhgis_historical_shapefiles",
  "nhgis0001_shapefile_tl2000_us_county_1900", "US_county_1900.shp"
), quiet = TRUE) |>
  select(NHGISST, NHGISCTY) |>
  st_transform(proj_albers) |>
  st_make_valid()

intersections <- st_intersection(nhgis_1900, map_1900) |>
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) |>
  st_drop_geometry() |>
  select(NHGISST, NHGISCTY, FIPS)

boll_weevil <- boll_weevil |>
  inner_join(intersections, by = c("NHGISST", "NHGISCTY")) |>
  select(-NHGISST, -NHGISCTY)

crosswalk <- as.data.table(readRDS(build_file("crosswalk", "mycrosswalk_1997.rds")))
crosswalk <- crosswalk[
  year == 1900,
  .(base_FIPS, beginning_FIPS, weight_agarea1)
]
boll_weevil <- boll_weevil |>
  left_join(crosswalk, by = c("FIPS" = "beginning_FIPS")) |>
  as.data.table()
boll_weevil[is.na(weight_agarea1), weight_agarea1 := 0]
boll_weevil[, FIPS := NULL]

area_columns <- grep("Area", names(boll_weevil), value = TRUE)
boll_weevil <- lapply(area_columns, function(variable) {
  weighted_sum(boll_weevil, variable)
}) |>
  reduce(left_join, by = "base_FIPS") |>
  pivot_longer(cols = all_of(area_columns), names_to = "year",
               values_to = "BWpresence") |>
  as.data.table()

boll_weevil[, year := as.numeric(str_extract(year, "[[:digit:]]+"))]
boll_weevil[year > 80, year := as.numeric(paste0("18", year))]
boll_weevil[year < 30, year := as.numeric(paste0("19", year))]
boll_weevil[year < 200, year := as.numeric(paste0("190", substr(year, 3, 3)))]
boll_weevil[, BWpresence := as.integer(BWpresence > 0)]
setnames(boll_weevil, "base_FIPS", "fips")
boll_weevil[, fips := str_pad(as.character(fips), 5, "left", "0")]

map_1997 <- make_shapefile("1997-01-01", historical_counties, proj_albers) |>
  select(fips = FIPS)
touches <- st_touches(map_1997)
neighbors <- lapply(seq_along(touches), function(index) {
  if (!length(touches[[index]])) return(NULL)
  data.table(
    fips = str_pad(as.character(map_1997$fips[index]), 5, "left", "0"),
    neighbor = str_pad(as.character(map_1997$fips[touches[[index]]]), 5, "left", "0")
  )
}) |>
  rbindlist()

neighbor_presence <- merge(
  neighbors,
  boll_weevil[, .(neighbor = fips, year, neighbor_presence = BWpresence)],
  by = "neighbor",
  allow.cartesian = TRUE
)[, .(BWneighboringpresence = as.integer(any(neighbor_presence == 1, na.rm = TRUE))),
  by = .(fips, year)]

boll_weevil <- merge(
  boll_weevil, neighbor_presence, by = c("fips", "year"), all.x = TRUE
)
boll_weevil[is.na(BWneighboringpresence), BWneighboringpresence := 0L]
boll_weevil[, state := substr(fips, 1, 2)]
boll_weevil[state %in% c("35", "38", "46"),
            c("BWpresence", "BWneighboringpresence") := 0L]
boll_weevil[, state := NULL]

saveRDS(
  boll_weevil,
  build_file("covariates", "boll_weevil_data.rds")
)
