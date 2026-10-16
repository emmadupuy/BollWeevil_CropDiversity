# Main Figure 2.
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
project_dir <- "."
d <- fread(file.path(project_dir, "../data/intermediate/panels/datacrop.csv.gz"),
           select = c("base_fips", "year", "ginisimpson", "ruggedness"))
d[, fips := sprintf("%05d", as.integer(base_fips))]
# Ruggedness is a fixed county characteristic; retain counties observed in
# other census waves instead of imposing the 1890 agricultural coverage.
rugged <- d[!is.na(ruggedness), .(ruggedness = first(ruggedness)), by = fips]
d <- d[year == 1890]
stopifnot(!anyDuplicated(d$fips))
region_fips <- c("10", "11", "51", "12", "21", "54", "13", "20", "22",
                 "28", "29", "37", "40", "45", "47", "48", "01", "05")
county_map <- usmap::us_map(regions = "counties", include = region_fips)
state_map <- usmap::us_map(regions = "states", include = region_fips)
polygon_layer <- function(dat, mapping = NULL, ...) {
  if (inherits(dat, "sf")) {
    geom_sf(data = dat, mapping = mapping, ...)
  } else {
    if (is.null(mapping)) mapping <- aes(x = x, y = y, group = group)
    geom_polygon(data = dat, mapping = mapping, ...)
  }
}
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
# The manuscript uses smaller, separately positioned labels in panel b.
# Coordinates and line breaks retain that panel's saved layout.
rugged_locations <- locations
rugged_locations$lat[match(c("Alabama", "Mississippi", "Virginia", "West\nVirginia"),
                           rugged_locations$label)] <- c(32.7, 32.7, 37.7, 38.8)
rugged_locations$lon[match(c("Virginia", "West\nVirginia"), rugged_locations$label)] <- c(-79, -80.7)
rugged_locations$label <- sub("North\nCarolina", "North Carolina", rugged_locations$label, fixed = TRUE)
rugged_locations$label <- sub("South\nCarolina", "South Carolina", rugged_locations$label, fixed = TRUE)
rugged_locations <- project_points(rugged_locations)
locations <- project_points(locations)

# Use Figure 1's four blue colours as a continuous scale, with darker blue
# denoting larger values. Both panels share geometry and export dimensions.
make_map <- function(variable, legend_title) {
  dat <- county_map
  is_rugged <- variable == "ruggedness"
  map_labels <- if (is_rugged) rugged_locations else locations
  source_data <- if (variable == "ruggedness") rugged else d
  dat$value <- source_data[[variable]][match(dat$fips, source_data$fips)]
  mapping <- if (inherits(dat, "sf")) aes(fill = value) else
    aes(x = x, y = y, group = group, fill = value)
  ggplot() + polygon_layer(dat, mapping, colour = NA) +
    polygon_layer(state_map, fill = NA, colour = "#666666", linewidth = 0.28) +
    geom_text(data = map_labels, aes(x, y, label = label),
              size = if (is_rugged) 3.1 else 4.4, lineheight = if (is_rugged) 1.2 else 0.85,
              colour = "#252525") +
    scale_fill_gradientn(name = legend_title,
                         colours = c("#eff3ff", "#bdd7e7", "#6baed6", "#2171b5"),
                         na.value = "#d1d1d1") +
    (if (inherits(dat, "sf")) coord_sf(datum = NA) else coord_equal()) +
    theme_void(base_size = 12) +
    theme(legend.position = "bottom", legend.title = element_text(size = 18),
          legend.text = element_text(size = 16),
          plot.margin = margin(8, 8, 5, 8),
          plot.background = element_rect(fill = "white", colour = NA)) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5,
                                  barwidth = grid::unit(10, "cm"),
                                  barheight = grid::unit(0.6, "cm")))
}
diversity_map <- make_map("ginisimpson", "Gini-Simpson index (1890)")
ruggedness_map <- make_map("ruggedness", "Terrain ruggedness index")
output_dir <- "../output/figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(output_dir, "Figure2a.pdf"), plot = diversity_map,
       width = 10, height = 7, units = "in", bg = "white")
ggsave(file.path(output_dir, "Figure2b.pdf"), plot = ruggedness_map,
       width = 10, height = 7, units = "in", bg = "white")
