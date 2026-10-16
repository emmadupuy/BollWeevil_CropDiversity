
## WE NOW USE ECKERT IN THE PROOF OF CONCEPTS

#######################
#                     #
# *  MY CROSSWALK  *  #
#                     #
#######################

## This script builds the classical crosswalk, using intersecting, beginning and base areas to 
## build weights

rm(list=ls())
source("clean/paths.R")
packages<-c("kableExtra","dplyr","readxl","withr","stringr","haven","ggplot2",
            "stats","lubridate","gridExtra","readxl","expss","stringr","sf","rlist",
            "parallel","raster","stars","purrr","SpaDES","caret","mlr")
lapply(packages, require, character.only = TRUE)
numCores <- detectCores() 



###### 1. FUNCTIONS ----

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

## FUNCTION
loadRdata <- function(fileName){
  # Function to load Rdata without old name
  load(fileName)
  get(ls()[ls() != "fileName"])
}

## FUNCTION
crosswalk_builder <- function(starting_year,base_year,list_shapefile){
  # Function to Build Classical Crosswalk 
  
  ## STEP 1 : select the relevant shapefiles
  layer_beginning <- rlist::list.filter(list_shapefile,year==starting_year)[[1]][[1]] # list of 1 of a list of 2 (shapefile,year)
  layer_beginning <- st_make_valid(layer_beginning) # to avoid evaluation error - topology exception in st_intersec
  names <- sapply(names(layer_beginning)[1:17], FUN=function(x) paste0("beginning_",x))
  names(layer_beginning)[1:17] <- names
  layer_base <- rlist::list.filter(list_shapefile,year==base_year)[[1]][[1]] # list of 1 of a list of 2 (shapefile,year)
  layer_base <- st_make_valid(layer_base)
  names <- sapply(names(layer_base)[1:17], FUN=function(x) paste0("base_",x))
  names(layer_base)[1:17] <- names
  # Remove Alaska
  layer_beginning <- layer_beginning[!grepl(x = layer_beginning$beginning_STATE_TERR, pattern = "alaska", ignore.case = T),]
  layer_base <- layer_base[!grepl(x = layer_base$base_STATE_TERR, pattern = "alaska", ignore.case = T),]
  # Move to SQM
  layer_beginning <- layer_beginning %>% mutate(beginning_AREA_SQM = (1/0.00000038610)*beginning_AREA_SQMI)
  layer_base <- layer_base %>% mutate(base_AREA_SQM = (1/0.00000038610)*base_AREA_SQMI)
  
  ## STEP 2: Get dataset of all intersections ##
  ## Historical is from Atlas ##
  intersect_county <- function(x,list_counties){
    # get all the intersections between each beginning county and the set of base counties
    layer_intersection <- st_intersection(x,list_counties)
    layer_intersection <- layer_intersection %>% mutate(intersected_area = st_area(.) %>% as.numeric())
    return(layer_intersection)
  }
  layer_beginning <- split(layer_beginning, seq(nrow(layer_beginning))) 
  intersections <- lapply(layer_beginning, FUN= function(x) intersect_county(x=x,list_counties=layer_base))
  intersections <- do.call("bind_rows", intersections)
  ## add an ID to each intersection 
  intersections$ID <- seq.int(nrow(intersections))
  intersections <- intersections %>% st_drop_geometry()
  rm(layer_base,layer_beginning) ; gc()
  
  ## STEP 3: Compute the Area  Weights ##
  intersections <- intersections %>% mutate(weight_area1 = (intersected_area)/(beginning_AREA_SQM), # for stocks - over origin area
                                            weight_area2 = (intersected_area)/(base_AREA_SQM)) # for averages - over base area
  
  intersections <- subset(intersections, select=c(ID,beginning_FIPS,base_FIPS,
                                                   beginning_NAME,base_NAME,
                                                   intersected_area,
                                                   beginning_AREA_SQM,
                                                   base_AREA_SQM,
                                                   weight_area1,
                                                   weight_area2))
  intersections$year <- as.numeric(starting_year)
  
  # normalization so weights sum to 1
  intersections <- intersections %>%
    group_by(beginning_FIPS) %>%
    mutate(normalization1 = sum(weight_area1,na.rm=T))
  intersections <- intersections %>%
    group_by(base_FIPS) %>%
    mutate(normalization2 = sum(weight_area2,na.rm=T))
  intersections <- intersections %>%
    mutate(weight_area1 = weight_area1*(1/normalization1),
           weight_area2 = weight_area2*(1/normalization2))

  return(intersections)
}

###### 2. IMPLEMENTATION ----

proj_albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 
                +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# We first make a shapefile per year of the census which we do not have
# For 1840, 1850, 1860, 1870, 1880, 1890, 1900, 1910, 1920, 1930, 1940, 1950, 2002, 2007 and 2012
# For 1925, 1935, 1945, 1954, 1959, 1964, 1969, 1974, 1978, 1982, 1987, 1992, 1997 we use the historical atlas
# Atlas data is in WGS84 geographic coordinate system - while NHGIS is USA Contiguous Albers Equal Area Conic Projected Coordinate System
historical_counties <- st_read(raw_file(
  "geography", "US_AtlasHCB_Counties", "US_HistCounties_Shapefile",
  "US_HistCounties.shp"
))
years <- c("1840-01-01","1850-01-01","1860-01-01","1870-01-01","1880-01-01","1890-01-01","1900-01-01",
           "1910-01-01","1920-01-01","1930-01-01","1940-01-01","1950-01-01",
           "1925-01-01","1935-01-01","1945-01-01","1954-01-01","1959-01-01","1964-01-01",
           "1969-01-01",'1974-01-01',"1978-01-01","1982-01-01","1987-01-01","1992-01-01","1997-01-01")
# Get shapefiles
atlas_list <- lapply(years,FUN = function(x) make_shapefiles(year=x, atlas=historical_counties))

years <- list("1840","1850","1860","1870","1880","1890","1900","1910","1920","1930","1940","1950",
              "1925","1935","1945","1954","1959","1964","1969",'1974',"1978","1982","1987","1992")

## Build Crosswalk ##
crosswalk <- mclapply(years,
                      FUN = function(x) 
                        crosswalk_builder(starting_year=x,base_year="1900",
                                          list_shapefile=atlas_list),
                      mc.cores=2)
crosswalk <- do.call("bind_rows", crosswalk)

saveRDS(crosswalk, build_file("crosswalk", "classical_crosswalk.rds"))
