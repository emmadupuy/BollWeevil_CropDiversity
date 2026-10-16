# Script to get a map of the US with ag coverage per intersection of the 1870 map with the 1997 baseline #
# Ag from 2021 CDL #
# Started March 24th 2023 ----

rm(list=ls())
source("clean/paths.R")

library(sf)
library(raster)
library(ggplot2)
library(rasterVis)



## load tif CDL 
cdl <- raster(raw_file(
  "cropland_data_layer", "2021_30m_cdls", "2021_30m_cdls.tif"
))

## load 2021 shapefile
counties <- read_sf(raw_file(
  "geography", "tl_2021_us_county", "tl_2021_us_county.shp"
)) %>%
  filter(STATEFP == 20)
counties <- st_transform(counties, crs(cdl))
state <- st_sf(st_union(counties))
ggplot(counties) + geom_sf()
ggplot(state) + geom_sf()

## clip cdl to Kansas extent
cdl <- crop(cdl, state)

## keep only ag 
cdl_ag = raster(cdl)
cdl_ag[cdl[] < 68] <- 1
cdl_ag[cdl[] > 200] <- 1
cdl_ag[cdl[] >= 68 & cdl[] < 200] <- 0
cdl_ag[cdl[] == 0] <- 0
plot(cdl_ag)


## prepare for plotting
test <- aggregate(cdl_ag, 133, mean) ## downsample
test <- mask(test, state)
test_spdf <- as(test, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

ggplot() + 
  geom_tile(data =test_df, aes(x=x, y=y, fill=value)) + 
  geom_sf(data=counties, 
          fill=NA, color="white", size=3) +
  scale_fill_gradient(high = "lightgreen", name = "Share ag. area") + 
  theme_void()+
  theme(panel.grid = element_blank(),
        panel.border = element_blank())


ggsave(clean_output_file("figures", "FigureS1.png"),
       height = 4)




# 
# 
# ## FUNCTION
# loadRdata <- function(fileName){
#   # Function to load Rdata without old name
#   load(fileName)
#   get(ls()[ls() != "fileName"])
# }
# 
# ## FUNCTION
# make_shapefiles <- function(year, atlas){
#   ## Used to make shapefiles for relevant years from Atlas of Historical County Boundaries
#   my_year <- as.Date(year,"%Y-%m-%d")
#   year_layer <- historical_counties[which(historical_counties$START_DATE<=my_year&historical_counties$END_DATE>my_year),]
#   # move to USA Contiguous Albers Equal Area Conic
#   year_layer <- st_transform(year_layer,proj_albers)
#   year <- as.numeric(format(my_year,"%Y"))
#   res <- list(year_layer,as.numeric(year))
#   names(res) <- c("layer","year")
#   return(res)
# }
# 
# ## FUNCTION
# read_my_files <- function(file_elem){
#   ## Used to read the NHGIS Shapefiles and stack them in a list of the right format (layer,corresponding_year)
#   layer <- st_read(file_elem)
#   year <- as.numeric(str_extract(file_elem,"(?<=US_county_)\\d+"))
#   res <- list(layer,year)
#   names(res) <- c("layer","year")
#   return(res)
# }
# 
# 
# ## 0. Get intersections ----
# proj_albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 
#                 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# 
# # We first make a shapefile per year of the census which we do not have
# # For 1840, 1850, 1860, 1870, 1880, 1890, 1900, 1910, 1920, 1930, 1940, 1950, 2002, 2007 and 2012
# # For 1925, 1935, 1945, 1954, 1959, 1964, 1969, 1974, 1978, 1982, 1987, 1992, 1997 we use the historical atlas
# # Atlas data is in WGS84 geographic coordinate system - while NHGIS is USA Contiguous Albers Equal Area Conic Projected Coordinate System
# historical_counties <- st_read("1_data/US_AtlasHCB_Counties/US_HistCounties_Shapefile/US_HistCounties.shp")
# years <- c("1840-01-01","1850-01-01","1860-01-01","1870-01-01","1880-01-01","1890-01-01","1900-01-01",
#            "1910-01-01","1920-01-01","1930-01-01","1940-01-01","1950-01-01",
#            "1925-01-01","1935-01-01","1945-01-01","1954-01-01","1959-01-01","1964-01-01",
#            "1969-01-01",'1974-01-01',"1978-01-01","1982-01-01","1987-01-01","1992-01-01","1997-01-01")
# # Get shapefiles
# atlas_list <- lapply(years,FUN = function(x) make_shapefiles(year=x, atlas=historical_counties))
# rm(historical_counties)
# NGHIS data
# filenames <- list.files(path="1_data/nhgis_historical_shapefiles/",
#                         pattern = "2000.shp$|2009.shp$|2012.shp$", recursive = TRUE, full.names=TRUE)
# nhgis_list <- lapply(filenames, read_my_files)
# # Change years in NHGIS
# nhgis_list[[1]][[2]] <- "2002"
# nhgis_list[[2]][[2]] <- "2007"
#
# # Concatenate the lists
# atlas_list[[length(atlas_list)+1]] <- nhgis_list[[1]]
# atlas_list[[length(atlas_list)+1]] <- nhgis_list[[2]]
# atlas_list[[length(atlas_list)+1]] <- nhgis_list[[3]]
#
# layer_beginning <- rlist::list.filter(atlas_list,year=="1870")[[1]][[1]] # list of 1 of a list of 2 (shapefile,year)
# layer_beginning <- st_make_valid(layer_beginning)
# layer_beginning <- layer_beginning[!grepl(x = layer_beginning$STATE_TERR, pattern = "alaska|hawaii", ignore.case = T),]
# layer_base <- rlist::list.filter(atlas_list,year=="1997")[[1]][[1]] # list of 1 of a list of 2 (shapefile,year)
# layer_base <- st_make_valid(layer_base)
# layer_base <- layer_base[!grepl(x = layer_base$STATE_TERR, pattern = "alaska", ignore.case = T),]
# layer_beginning <- split(layer_beginning, seq(nrow(layer_beginning)))
#
# intersect_county <- function(x,list_counties){
#   # get all the intersections between each beginning county and the set of base counties
#   layer_intersection <- st_intersection(x,list_counties)
#   return(layer_intersection)
# }
# intersections <- mclapply(layer_beginning,
#                           FUN= function(x) intersect_county(x=x,list_counties=layer_base),
#                           mc.cores=numCores)
# intersections <- do.call("bind_rows", intersections)
# # Filter out those which are not polygons or multi-polygons
# intersections <- sf::st_as_sf(intersections)
# intersections <- intersections %>% filter(
#   st_geometry_type(.)
#   %in% c("POLYGON", "MULTIPOLYGON"))
# saveRDS(intersections,"~/Dropbox/agricultural_unions/1_data/nhgis_historical_shapefiles/intersect18701997.rds")
# rm(layer_base,layer_beginning,atlas_list)
# #intersections <- readRDS("~/Dropbox/agricultural_unions/1_data/nhgis_historical_shapefiles/intersect18701997.rds")
#
# 
# ## 1. Extract CDL ag. coverage ----
# load("1_data/croplanddatalayer/prism_cdl2021.Rdata")
# prism_year <- setDT(prism_year)
# prism_year <- prism_year[,c("mean"),with=T]
# prism_year$ID <- seq.int(nrow(prism_year))
# 
# prism2021 <- loadRdata("~/Dropbox/agricultural_unions/1_data/prism/prism_2021.Rdata")
# prism2021 <- st_as_sf(prism2021)
# prism2021 <- prism2021 %>% subset(select=c(geometry))
# prism2021$ID <- seq.int(nrow(prism2021))
# 
# # match
# prism2021 <- left_join(prism2021,prism_year,by=c("ID"))
# rm(prism_year)
# 
# ## 2. Graph ----
# 
# # Keep only Dakota
# intersections2 <- intersections %>% filter(STATE_TERR=="Kansas") # Dakota Territory
# intersections2 <- intersections2 %>% select(subset=geometry)
# 
# prism2021 <- st_transform(prism2021,crs=crs(intersections2))
# st_crs(prism2021) <- NA
# st_crs(intersections2) <- NA
# prism2021_2 <- st_crop(st_as_sf(prism2021),st_as_sf(intersections2))
# 
# ggplot() + 
#   geom_sf(data=prism2021_2,aes(fill=mean),lwd=0,col=NA) +
#   geom_sf(data=intersections2,color="white",alpha=0) +
#   scale_fill_viridis_c() + 
#   theme_bw() +
#   ggtitle("")
# ggsave("4_output/2_graphs/new_crosswalk/intersectionsandag_kansas.png",
#        dpi=500)
# 
# 
