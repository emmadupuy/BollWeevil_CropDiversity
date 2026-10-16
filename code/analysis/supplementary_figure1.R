# Supplementary Figure 1: agricultural land share in Kansas, 2021.
# This script uses raw data that are not distributed with the replication
# package but can be downloaded free of charge. See the README files in
# data/raw/cropland_data_layer/ and data/raw/geography/ for sources, filenames,
# and download instructions.
# Open replication.Rproj, then run this file from top to bottom (or click Source).
# Paths are relative to code/; this script does not change the working directory.

if (!dir.exists("../data/raw"))
  stop("Open replication.Rproj or set the working directory to code/ first.")

# 1. Packages, source files, and output folder ----

library(sf)
library(raster)
library(ggplot2)

cdl_file <- paste0(
  "../data/raw/cropland_data_layer/2021_30m_cdls/",
  "2021_30m_cdls.tif"
)
county_file <- paste0(
  "../data/raw/geography/tl_2021_us_county/",
  "tl_2021_us_county.shp"
)

missing_files <- c(cdl_file, county_file)[
  !file.exists(c(cdl_file, county_file))
]
if (length(missing_files)) {
  stop(
    "Figure S1 requires raw files that are not distributed with the package. ",
    "See the README files in data/raw/cropland_data_layer/ and data/raw/geography/. ",
    "Missing: ", paste(missing_files, collapse = ", ")
  )
}

dir.create("../output/figures", recursive = TRUE, showWarnings = FALSE)

# 2. Read and align the Cropland Data Layer and county boundaries ----

cdl <- raster(cdl_file)

counties <- read_sf(county_file)
if (!"STATEFP" %in% names(counties))
  stop("The county shapefile must contain the Census STATEFP field.")

counties <- counties[as.character(counties$STATEFP) == "20", ]
if (!nrow(counties))
  stop("The county shapefile contains no Kansas counties (STATEFP 20).")

counties <- st_transform(counties, crs(cdl))
kansas <- st_sf(geometry = st_union(counties))

# 3. Calculate agricultural share on the plotting grid ----

# Reproduce the classification used for the manuscript map: CDL codes
# 1--67 and codes above 200 are agricultural; background code 0 and
# codes 68--200 are non-agricultural.
cdl_kansas <- crop(cdl, as(kansas, "Spatial"))
cdl_agriculture <- reclassify(
  cdl_kansas,
  matrix(
    c(
      0, 0, 0,
      1, 67, 1,
      68, 200, 0,
      201, Inf, 1
    ),
    ncol = 3,
    byrow = TRUE
  )
)

# The manuscript map averages 133 x 133 cells of the 30 m CDL. Each
# plotted cell is therefore approximately 4 km square.
agricultural_share <- aggregate(
  cdl_agriculture,
  fact = 133,
  fun = mean,
  na.rm = TRUE
)
agricultural_share <- mask(agricultural_share, as(kansas, "Spatial"))

plot_data <- as.data.frame(
  as(agricultural_share, "SpatialPixelsDataFrame")
)
colnames(plot_data) <- c("value", "x", "y")

# 4. Draw and save Figure S1 ----

figure_s1 <- ggplot() +
  geom_tile(data = plot_data, aes(x = x, y = y, fill = value)) +
  geom_sf(
    data = counties,
    fill = NA,
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_gradient(high = "lightgreen", name = "Share ag. area") +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  "../output/figures/FigureS1.png",
  plot = figure_s1,
  width = 2170 / 300,
  height = 4,
  units = "in",
  dpi = 300,
  bg = "transparent"
)
