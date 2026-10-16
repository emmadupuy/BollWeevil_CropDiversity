#######################
#                     #
# *  MY CROSSWALK  *  #
#                     #
#######################

# In this script, we build our own historical crosswalk fit for the study of the US Census of Agriculture
# Done with continuous prediction of agricultural coverage
# Use the prism_topography_year files generated in step0_my_crosswalk_v4_part1.R
# We use the Atlas of Historical County Boundaries to get the right shapefiles
# We use historical climate data to predict the share of land planted per county (PRISM)
# This PRISM data is obtained with the script: step0_prism_for_crosswalk.R
# The prediction model is obtained with the scripts: step_0_cropland (1 to 5)
# This is used to update pure area-based weights that accompany our final crosswalk

## ON CLUSTER ##

rm(list=ls())
source("clean/paths.R")

packages<-c("dplyr","withr","haven",
            "lubridate","stringr","sf","parallel",
            "raster","stars","purrr","stats","rlist",
            "caret","mlr") # "SpaDES"
lapply(packages, require, character.only = TRUE)
numCores <- detectCores() - 9 

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
read_my_files <- function(file_elem){
  ## Used to read the NHGIS Shapefiles and stack them in a list of the right format (layer,corresponding_year)
  layer <- st_read(file_elem)
  year <- as.numeric(str_extract(file_elem,"(?<=US_county_)\\d+"))
  res <- list(layer,year)
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
  # Function to Build My Crosswalk 
  
  ## STEP 1 : select the relevant shapefiles
  layer_beginning <- rlist::list.filter(list_shapefile,year==starting_year)[[1]][[1]] # list of 1 of a list of 2 (shapefile,year)
  layer_beginning <- st_make_valid(layer_beginning) # to avoid evaluation error - topology exception in st_intersec
  if(starting_year<=1997){
    names <- sapply(names(layer_beginning)[1:17], FUN=function(x) paste0("beginning_",x))
    names(layer_beginning)[1:17] <- names
    layer_beginning <- layer_beginning[!grepl(x = layer_beginning$beginning_STATE_TERR, pattern = "alaska|hawaii", ignore.case = T),]
    layer_beginning <- layer_beginning %>% mutate(beginning_AREA_SQM = (1/0.00000038610)*beginning_AREA_SQMI)
  } else if (starting_year==2002) {
    layer_beginning <- layer_beginning[!grepl(x = layer_beginning$STATENAM, pattern = "alaska", ignore.case = T),]
    layer_beginning$ID_NUM <- seq.int(nrow(layer_beginning))
    layer_beginning$NAME <- layer_beginning$ICPSRNAM
    layer_beginning$START_DATE <- NA
    layer_beginning$END_DATE <- NA
    # FOR 2000-2012 NHGIS and FIPS correspond if you just remove last 0
    # https://www.nhgis.org/geographic-crosswalks
    layer_beginning <- layer_beginning %>%
      mutate(FIPS = paste0(substr(as.character(NHGISST),1,2),substr(as.character(NHGISCTY),1,3)))
    layer_beginning$AREA_SQM <- layer_beginning$SHAPE_AREA # already in sq meters
    layer_beginning <- subset(layer_beginning, select=c(ID_NUM,NAME,START_DATE,END_DATE,
                                                        FIPS,AREA_SQM))
    names <- sapply(names(layer_beginning)[1:6], FUN=function(x) paste0("beginning_",x))
    names(layer_beginning)[1:6] <- names
  } else {
    layer_beginning <- layer_beginning[which(layer_beginning$STATEFP!="02"),]
    layer_beginning$ID_NUM <- seq.int(nrow(layer_beginning))
    layer_beginning$START_DATE <- NA
    layer_beginning$END_DATE <- NA
    layer_beginning <- layer_beginning %>%
      mutate(FIPS = paste0(STATEFP,COUNTYFP))
    if(starting_year<=2007){
      layer_beginning$AREA_SQM <- layer_beginning$SHAPE_AREA # already in sq meters
    } else {
      layer_beginning$AREA_SQM <- layer_beginning$Shape_Area # already in sq meters
    }
    layer_beginning <- subset(layer_beginning, select=c(ID_NUM,NAME,START_DATE,END_DATE,
                                                        FIPS,AREA_SQM))
    names <- sapply(names(layer_beginning)[1:6], FUN=function(x) paste0("beginning_",x))
    names(layer_beginning)[1:6] <- names
    layer_beginning <- st_transform(layer_beginning,crs=proj_albers)
  }
  
  layer_base <- rlist::list.filter(list_shapefile,year==base_year)[[1]][[1]] # list of 1 of a list of 2 (shapefile,year)
  layer_base <- st_make_valid(layer_base)
  names <- sapply(names(layer_base)[1:17], FUN=function(x) paste0("base_",x))
  names(layer_base)[1:17] <- names
  # Remove Alaska
  layer_base <- layer_base[!grepl(x = layer_base$base_STATE_TERR, pattern = "alaska", ignore.case = T),]
  # Move to SQM
  layer_base <- layer_base %>% mutate(base_AREA_SQM = (1/0.00000038610)*base_AREA_SQMI)
  
  
  ## STEP 2: get climate data for the year 
  # PRISM files - 73 variables + geometry per year
  # Prism file loaded + name of vars. used to train the model 
  #prism_path <- "~/Dropbox/agricultural_unions/1_data/prism/"
  fileName <- build_file(
    "crosswalk", "prism_topography",
    paste0("prism_topography", starting_year, ".Rdata")
  )
  prism_year <- loadRdata(fileName) 
  
  if(as.numeric(starting_year)>1997&as.numeric(starting_year)<2012){
    names <- list(list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_ppt_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),   
                  list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tmin_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
                  list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tmax_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
                  list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tdmean_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
                  list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_vpdmin_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
                  list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_vpdmax_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
                  "elevation","slope")
    names <- unlist(names)
    names(prism_year) <- names
    prism_year$year <- starting_year
  } else {
    names <- list(list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_ppt_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),   
                  list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tmin_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
                  list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tmax_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
                  list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tdmean_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
                  list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_vpdmin_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
                  list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_vpdmax_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
                  "year","elevation","slope")
    names <- unlist(names)
    names(prism_year) <- names
  }

  prism_year$ID <- seq.int(nrow(prism_year))
  prism_prediction <- subset(prism_year, select=-c(year,ID))
  
  # match it to prism shapefile
  fileName <- build_file(
    "crosswalk", "prism", paste0("prism_", starting_year, ".Rdata")
  )
  prism_shape <- loadRdata(fileName)
  prism_shape <- st_as_sf(prism_shape)
  prism_shape$ID <- seq.int(nrow(prism_shape))
  prism_shape <- subset(prism_shape, select=c(ID))
  prism_year <- left_join(prism_shape, prism_year, by=c("ID"))
  prism_year <- prism_year %>% st_transform(proj_albers)
  prism_year <- subset(prism_year, select=-c(ID,year))

  
  ## STEP 3: predict binary presence of agriculture for each grid cell
  print("here 1")
  result <- predict(xgb_model, newdata = prism_prediction)
  print("here 2")
  prism_year <- bind_cols(prism_year, result$data)
  rm(result, prism_prediction)
  gc()
  prism_year <- subset(prism_year, select=c(response)) %>%
    mutate(response = pmax(response, 0))
  saveRDS(prism_year, build_file(
    "crosswalk", "predicted_grid", paste0("prismgrid_withag", starting_year, ".rds")
  ))
  
  ## STEP 4: Before Intersection, get predicted ag. area per county on historical map ##
  layer_beginning <- st_join(prism_year,layer_beginning,st_within)
  layer_beginning <- layer_beginning %>%
    group_by(beginning_ID_NUM,beginning_NAME,beginning_FIPS,beginning_START_DATE,
             beginning_END_DATE,beginning_AREA_SQM) %>%
    summarize(beginning_ag_coverage = mean(response, na.rm=T))
  
  layer_base <- st_join(prism_year,layer_base,st_within)
  layer_base <- layer_base %>%
    group_by(base_ID_NUM,base_NAME,base_FIPS,base_AREA_SQM) %>%
    summarize(base_ag_coverage = mean(response, na.rm=T))
  
  layer_beginning <- split(layer_beginning, seq(nrow(layer_beginning))) 
  
  ## STEP 5: Get dataset of all intersections ##
  ## Historical is from Atlas ##
  intersect_county <- function(x,list_counties){
    # get all the intersections between each beginning county and the set of base counties
    layer_intersection <- st_intersection(x,list_counties)
    layer_intersection <- layer_intersection %>% filter(
      st_geometry_type(.)
      %in% c("POLYGON", "MULTIPOLYGON"))
    layer_intersection <- layer_intersection %>% mutate(intersected_area = st_area(.) %>% as.numeric())
    return(layer_intersection)
  }
  intersections <- lapply(layer_beginning, FUN= function(x) intersect_county(x=x,list_counties=layer_base))
  intersections <- do.call("bind_rows", intersections)
  # Filter out those which are not polygons or multi-polygons
  intersections <- sf::st_as_sf(intersections)
  #intersections <- intersections %>% filter(
  #  st_geometry_type(.)
  #  %in% c("POLYGON", "MULTIPOLYGON"))
  intersections <- intersections[which(intersections$intersected_area>.5),]
  
  ## add an ID to each intersection 
  intersections$ID <- seq.int(nrow(intersections))
  rm(layer_base) ; gc()
  
  
  ## STEP 6: Get each intesections' average crop coverage binary ##
  intersections <- st_join(x=prism_year,y=intersections,st_within)
  intersections <- intersections %>%
    group_by(ID,beginning_NAME,beginning_FIPS,
             beginning_AREA_SQM,beginning_ag_coverage,
             base_ID_NUM,base_NAME,base_FIPS,base_AREA_SQM,
             base_ag_coverage,intersected_area) %>%
    summarize(intersected_ag_coverage = mean(response, na.rm=TRUE))
  
  rm(prism_year)
  intersections <- intersections %>% st_drop_geometry()
  
  
  ## STEP 7: Compute the Agricultural-Weighted  Area  Weight ##
  intersections <- intersections %>% mutate(weight_agarea1 = (intersected_ag_coverage*intersected_area)/(beginning_ag_coverage*beginning_AREA_SQM), # for stocks - over origin area
                                            weight_agarea2 = (intersected_ag_coverage*intersected_area)/(base_ag_coverage*base_AREA_SQM)) # for averages - over base area
  
  intersections <- subset(intersections, select=c(ID,beginning_FIPS,base_FIPS,
                                                  beginning_NAME,base_NAME,
                                                  intersected_area,
                                                  beginning_AREA_SQM,
                                                  base_AREA_SQM,
                                                  beginning_ag_coverage,
                                                  base_ag_coverage,
                                                  intersected_ag_coverage,
                                                  weight_agarea1,
                                                  weight_agarea2))
  intersections$year <- as.numeric(starting_year)
  
  # normalization so weights sum to 1
  intersections <- intersections %>%
    group_by(beginning_FIPS) %>%
    mutate(normalization1 = sum(weight_agarea1,na.rm=T))
  intersections <- intersections %>%
    group_by(base_FIPS) %>%
    mutate(normalization2 = sum(weight_agarea2,na.rm=T))
  intersections <- intersections %>%
    mutate(weight_agarea1 = weight_agarea1*(1/normalization1),
           weight_agarea2 = weight_agarea2*(1/normalization2))
  gc()
  print(starting_year)
  #filename <- paste0("/moto/sscc/users/td2631/data_agwages/crosswalk/crosswalk_bit",starting_year,".rds")
  filename <- build_file(
    "crosswalk", "parts", paste0("crosswalk_bit", starting_year, ".rds")
  )
  saveRDS(intersections,filename)
  rm(intersections)
  
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
#historical_counties <- st_read("~/Dropbox/agricultural_unions/1_data/US_AtlasHCB_Counties/US_HistCounties_Shapefile/US_HistCounties.shp")
years <- c("1840-01-01","1850-01-01","1860-01-01","1870-01-01","1880-01-01","1890-01-01","1900-01-01",
           "1910-01-01","1920-01-01","1930-01-01","1940-01-01","1950-01-01",
           "1925-01-01","1935-01-01","1945-01-01","1954-01-01","1959-01-01","1964-01-01",
           "1969-01-01",'1974-01-01',"1978-01-01","1982-01-01","1987-01-01","1992-01-01","1997-01-01")
# Get shapefiles
atlas_list <- lapply(years,FUN = function(x) make_shapefiles(year=x, atlas=historical_counties))
rm(historical_counties)
# NGHIS data
#filenames <- list.files(path="~/Dropbox/agricultural_unions/1_data/nhgis_historical_shapefiles/", 
#                        pattern = "2000.shp$|2009.shp$|2012.shp$", recursive = TRUE, full.names=TRUE)
filenames <- list.files(path=raw_file("geography", "nhgis_historical_shapefiles"),
                        pattern = "2000.shp$|2009.shp$|2012.shp$", recursive = TRUE, full.names=TRUE)
nhgis_list <- lapply(filenames, read_my_files)
# Change years in NHGIS
nhgis_list[[1]][[2]] <- "2002"
nhgis_list[[2]][[2]] <- "2007"

# Concatenate the lists
atlas_list[[length(atlas_list)+1]] <- nhgis_list[[1]]
atlas_list[[length(atlas_list)+1]] <- nhgis_list[[2]]
atlas_list[[length(atlas_list)+1]] <- nhgis_list[[3]]

# Years for which we need a crosswalk 
years <- list("1840","1850","1860","1870","1880","1890","1900","1910","1920","1930","1940","1950",
              "1925","1935","1945","1954","1959","1964","1969",'1974',"1978","1982","1987","1992",
              "2002","2007","2012") # no need to have crosswalk for 1997


## Trained XGBoost Model ##
load(build_file("xgboost", "model", "xgboost_v2.Rdata"))
xgb_model <- xgb_save[[1]]
rm(xgb_save)

## Watershed Data ##
#layers <- ogrListLayers("~/Dropbox/agricultural_unions/1_data/watersheds/WBD_National_GDB/WBD_National_GDB.gdb")
#watersheds = readOGR("~/Dropbox/agricultural_unions/1_data/watersheds/WBD_National_GDB/WBD_National_GDB.gdb","WBDHU16")
#watersheds <- st_read("~/Dropbox/agricultural_unions/1_data/watersheds/WBD_National_GDB/WBD_National_GDB.gdb",layer="WBDHU16")


## Prism path
prism_path <- build_dir("crosswalk")
#prism_path <- "~/Dropbox/agricultural_unions/1_data/prism/"
## Build Crosswalk Elements ##
task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
ranks <- if (nzchar(task_id)) as.numeric(task_id) else seq_along(years)
for (rank in ranks) {
  crosswalk_builder(starting_year=years[[rank]], base_year="1997",
                    list_shapefile=atlas_list)
}


#------------------------------------------------------- HOME ------------------------------->
### This part on home computer to finish the crosswalk --
# No crosswalk for 1992

rm(list=ls())
source("clean/paths.R")
packages<-c("kableExtra","dplyr","readxl","withr","stringr","haven","ggplot2",
            "stats","lubridate","gridExtra","readxl","expss","stringr","sf","rlist",
            "parallel","raster","purrr","SpaDES","caret","mlr")
lapply(packages, require, character.only = TRUE)
numCores <- detectCores() 

files <- list.files(build_dir("crosswalk", "parts"),
                    pattern="crosswalk_bit", full.names=T, recursive=T)
files <- lapply(files, FUN = function(x) readRDS(x))
files <- do.call("bind_rows", files)

#binary1992 <- readRDS("~/Dropbox/agricultural_unions/1_data/mycrosswalk/binary_version/crosswalk_bit1992.rds")
#files <- bind_rows(files, binary1992)

saveRDS(files, build_file("crosswalk", "mycrosswalk_1997.rds"))
