# Main Figure 1.
# Open replication.Rproj, then run this file from top to bottom (or click Source).
# Paths are relative to code/; this script does not change the working directory.

if (!dir.exists("../data/intermediate"))
  stop("Open replication.Rproj or set the working directory to code/ first.")

# 1. Packages and output folders ----
library(data.table)
library(dplyr)
library(sf)
library(ggplot2)
library(usmap)
library(cowplot)
library(R.utils)

dir.create("../output/figures", recursive = TRUE, showWarnings = FALSE)
set.seed(123)

# 2. Data and analysis ----
## 0. Get Data ----
datacrop <- fread("../data/intermediate/panels/datacrop.csv.gz")
datacrop[BWpresence%in%c(1910)&year>=1910,BWpresence:=1]
datacrop[BWpresence%in%c(1910)&year<1910,BWpresence:=0]
datacrop[BWpresence%in%c(1920)&year>=1920,BWpresence:=1]
datacrop[BWpresence%in%c(1920)&year<1920,BWpresence:=0]
# test <- datacrop[(BWpresence==0&year==1920)&(BWpresence==1&year==1930),]
# georgia, south carolina all in 1920
datacrop[,state:=substr(base_fips,1,2)]
datacrop[state=="13"&year>=1920,BWpresence:=1]
datacrop[state=="45"&year>=1920,BWpresence:=1]
datacrop[base_fips=="12075"&year>=1920,BWpresence:=1]
datacrop <- datacrop[, c("year", "base_fips", "BWpresence")]
datacrop <- datacrop[year>1880]
datacrop <- melt(datacrop, id.vars = c("year", "base_fips"))
datacrop <- datacrop[value==1]
datacrop[, fips := base_fips]

datacrop <- datacrop[, min(year), by = c("variable", "fips")]
# datacrop[, V1 := as.character(V1)]

# Cleaning for BW
datatest <- datacrop[variable=="BWpresence",]
datatest[fips=="48473",V1:=1910]
datatest[fips=="48471",V1:=1910]
datatest[fips=="48477",V1:=1910]
datatest[fips=="48481",V1:=1910]
datatest[fips=="48491",V1:=1910]
datatest[fips=="48493",V1:=1910]
datatest[fips=="47065",V1:=1920]
datatest[fips=="13217",V1:=1920]
datatest[fips=="13998",V1:=1920]
datatest[,state:=substr(fips,1,2)]
test <- datatest[state=="40",]
datatest[state=="40"&V1==1900,V1:=1910]
add_arkansas <- data.frame(t(c("BWpresence",5079,1910,"05")))
names(add_arkansas) <- names(datatest)
datatest <- setDT(data.frame(rbind(datatest,add_arkansas)))
datatest[,V1:=as.numeric(V1)]

## 1. Map with geographic context ----
# Preserve the existing arrival-period coding and county corrections above.
region_fips <- c("10", "11", "51", "12", "21", "54", "13", "20", "22",
                 "28", "29", "37", "40", "45", "47", "48", "01", "05")
county_map <- usmap::us_map(regions = "counties", include = region_fips)
# Only mapped counties determine the legend (aggregate census codes have no geometry).
arrival_years <- sort(unique(datatest$V1[sprintf("%05d", as.integer(datatest$fips)) %in% county_map$fips]))
stopifnot(identical(as.numeric(arrival_years), c(1890, 1900, 1910, 1920)))
datatest[, fips := sprintf("%05d", as.integer(fips))]
datatest[, arrival := factor(as.character(V1), levels = as.character(arrival_years))]

# usmap releases return either sf geometries or polygon coordinates.
state_map <- usmap::us_map(regions = "states", include = region_fips)
us_states <- usmap::us_map(regions = "states", exclude = c("AK", "HI"))
county_map$arrival <- datatest$arrival[match(county_map$fips, datatest$fips)]
county_map$arrival <- factor(ifelse(is.na(county_map$arrival), "No recorded arrival",
                                  as.character(county_map$arrival)),
                            levels = c(as.character(arrival_years), "No recorded arrival"))

polygon_layer <- function(dat, mapping = NULL, ...) {
  if (inherits(dat, "sf")) {
    geom_sf(data = dat, mapping = mapping, ...)
  } else {
    if (is.null(mapping)) mapping <- aes(x = x, y = y, group = group)
    geom_polygon(data = dat, mapping = mapping, ...)
  }
}
map_coords <- if (inherits(county_map, "sf")) coord_sf(datum = NA) else coord_equal()
map_aes <- if (inherits(county_map, "sf")) aes(fill = arrival) else
  aes(x = x, y = y, group = group, fill = arrival)

# Place geographic labels using longitude/latitude, transformed to the map CRS.
locations <- data.frame(
  label = c("Texas", "Oklahoma", "Kansas", "Missouri", "Arkansas", "Louisiana",
            "Mississippi", "Alabama", "Georgia", "Florida", "Tennessee", "Kentucky",
            "North\nCarolina", "South\nCarolina", "Virginia", "West\nVirginia"),
  lon = c(-100, -97.3, -98, -92.5, -92.3, -92.4, -89.7, -86.8, -83.5, -82.1,
          -86, -85.5, -79.5, -80.9, -78.2, -81.0),
  lat = c(31.4, 35.4, 38.4, 38.3, 34.8, 30.9, 32.0, 33.0, 32.5, 28.2,
          35.8, 37.6, 35.7, 33.6, 37.5, 39.1))
project_points <- function(dat) {
  if (inherits(county_map, "sf")) {
    pts <- sf::st_as_sf(dat, coords = c("lon", "lat"), crs = 4326)
    xy <- sf::st_coordinates(sf::st_transform(pts, sf::st_crs(county_map)))
    dat$x <- xy[, 1]; dat$y <- xy[, 2]
  } else {
    pts <- usmap::usmap_transform(dat, input_names = c("lon", "lat"))
    dat$x <- pts$x; dat$y <- pts$y
  }
  dat
}
locations <- project_points(locations)
entry <- project_points(data.frame(lon = -97.4975, lat = 25.9017))
entry_label <- project_points(data.frame(lon = -100.5, lat = 26.7))

main_map <- ggplot() +
  polygon_layer(county_map, map_aes, colour = NA) +
  polygon_layer(state_map, fill = NA, colour = "#666666", linewidth = 0.28) +
  geom_text(data = locations, aes(x, y, label = label), size = 4.4, lineheight = 0.85, colour = "#252525") +
  geom_segment(data = entry_label, aes(x = x, y = y),
               xend = entry$x, yend = entry$y, linewidth = 0.4, colour = "#222222") +
  geom_point(data = entry, aes(x, y), shape = 21, size = 2.8,
             fill = "#e39136", colour = "#222222", stroke = 0.5) +
  geom_label(data = entry_label, aes(x, y), label = "Brownsville, Texas\nEntry point",
             size = 3, hjust = 1, label.size = 0, fill = "white") +
  scale_fill_manual(name = "First recorded arrival (census period)",
                    values = c("1890" = "#2171b5", "1900" = "#6baed6",
                               "1910" = "#bdd7e7", "1920" = "#eff3ff",
                               "No recorded arrival" = "#d1d1d1"),
                    breaks = as.character(arrival_years), drop = FALSE) +
  map_coords + theme_void(base_size = 12) +
  theme(legend.position = "bottom", legend.title.position = "top",
        legend.title = element_text(size = 18), legend.text = element_text(size = 16),
        legend.key.width = grid::unit(1.2, "cm"),
        legend.key.height = grid::unit(0.7, "cm"),
        plot.margin = margin(4, 10, 0, 10), plot.background = element_rect(fill = "white", colour = NA)) +
  guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5))

us_states$focus <- ifelse(us_states$fips %in% region_fips, "Shown in main map", "Other states")
inset_aes <- if (inherits(us_states, "sf")) aes(fill = focus) else
  aes(x = x, y = y, group = group, fill = focus)
inset <- ggplot() + polygon_layer(us_states, inset_aes, colour = "white", linewidth = 0.15) +
  scale_fill_manual(values = c("Shown in main map" = "#5386a5", "Other states" = "#e5e5e5"), guide = "none") +
  (if (inherits(us_states, "sf")) coord_sf(datum = NA) else coord_equal()) +
  labs(title = "Contiguous United States") + theme_void(base_size = 9) +
  theme(plot.title = element_text(size = 9, hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "#dddddd", linewidth = 0.3),
        plot.margin = margin(5, 5, 5, 5))

graph1 <- cowplot::ggdraw() +
  cowplot::draw_plot(main_map, x = 0, y = 0.015, width = 1, height = 0.985) +
  cowplot::draw_plot(inset, x = 0.015, y = 0.73, width = 0.21, height = 0.20) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Export the filename used by the manuscript.
output_dir <- "../output/figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(output_dir, "Figure1.pdf"), plot = graph1,
       width = 11, height = 7.4, units = "in", bg = "white")
