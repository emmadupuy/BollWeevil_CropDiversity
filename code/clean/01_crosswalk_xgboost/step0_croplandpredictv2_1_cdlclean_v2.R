## Code for Cluster Training ##
rm(list=ls())
source("clean/paths.R")

list_packages <- list("parallel","dplyr","sf","CropScapeR","sp","xgboost","caret","mlr",
                      "parallelMap","mice","raster","stars","prism","stringr","purrr",
                      "rgdal","httr","SpaDES","tigris","cdlTools","stringr")
lapply(list_packages, require, character.only = TRUE)
## module requirements: gcc/6.1.0 for xgboost, for raster: geos/3.6.5, gdal/2.2.2, boost/1.72
numCores <- detectCores()
httr::set_config(httr::config(ssl_verifypeer = 0L)) 
rasterOptions(tmpdir=build_dir("scratch", "raster"),
              tmptime = 2)
RNGkind("L'Ecuyer-CMRG")
set.seed(1)

#cdl_year <- lapply(as.list(seq(2013,2020,by=1)),as.character)
cdl_year <- as.list(as.character(unlist(seq(2008,2021,by=1))))


raster_reclassify <- function(elem){
  # 1 for agriculture, 0 otherwise
  tmp_name1 <- scratch_file(paste0("tmp", rnorm(1), ".grd"))
  crop <- union(union(union(seq(1,14,by=1),
                            seq(21,60,by=1)),seq(66,77,by=1)),seq(204,254,by=1))
  elem <- calc(elem, fun=function(x){ x[x %in% crop] <- 1; return(x)}, filename=tmp_name1)
  tmp_name2 <- scratch_file(paste0("tmp", rnorm(1), ".grd"))
  elem<- calc(elem, fun=function(x){ x[x != 1] <- 0; return(x)}, filename=tmp_name2)
  
  file.remove(c(tmp_name1,tmp_name2))
  return(elem)
  
}

topographics_reclassify <- function(elem){
  # NA values to NA
  
  tmp_name1 <- scratch_file(paste0("tmp", rnorm(1), ".grd"))
  crop <- c(-9999,-32768)
  elem <- calc(elem, fun=function(x){ x[x %in% crop] <- 1; return(x)}, filename=tmp_name1)
  file.remove(tmp_name1)

  return(elem)
}

# Actual Code - 

#for (year in cdl_year){
  
#  for(rank in seq_along(us_states)){
    
#    state_char <- us_states[[rank]]$STATEFP
#    state_num <- as.numeric(us_states[[rank]]$STATEFP)
#    year_num <- as.numeric(year)
#    Sys.sleep(60)
#    tmp <- tryCatch(
#      {
#        cdlTools::getCDL(x = state_num, year = year_num, location = path_base)[[1]]
#      },
#      error=function(cond) {
#        GetCDLData(aoi = state_num, year = year_num, 
#                   type = 'f', format = "raster", 
#                   save_path = paste0(path_base,"CDL_",year,"_",us_states[[rank]]$STATEFP,".tif"), tol_time=400000)
#      } )
    
    # split and recode
#    tmp <- splitRaster(tmp, nx=8, ny=8) #SpaDES.tools::
#    tmp <- mclapply(tmp, raster_reclassify, mc.cores = numCores)
    
#    tmp$fun <- mean # if overlapping areas _ but there should not be any
#    tmp$na.rm <- TRUE
#    tmp <- do.call(what=raster::mosaic, args=tmp)
#    path_save <- paste0(path_base,"cdl_",state_char,year,".tif")
#    writeRaster(tmp, filename=path_save, format="GTiff", overwrite=TRUE)
    
#  }
  
#}


### Final Step - Load and Reproject Elevation and Slope

## Elevation ##

# Elevation is one band - -9999 for NA
# Split to accomodate memory limitations
elevation <- raster(raw_file(
  "landfire", "LF2020_Elev_220_CONUS", "Tif", "LC20_Elev_220.tif"
))
# Already in Albers Conical
elevation <- splitRaster(elevation, nx=8, ny=8) #SpaDES.tools::
elevation <- mclapply(elevation, topographics_reclassify, mc.cores = numCores)
elevation$fun <- mean # if overlapping areas _ but there should not be any
elevation$na.rm <- TRUE
elevation <- do.call(what=raster::mosaic, args=elevation)
path <- build_file("crosswalk", "topography", "elevation_projected.tif")
writeRaster(elevation, file=path, format="GTiff", overwrite=TRUE) 

## Slope ## 

# One band - no negative or NA
slope <- raster(raw_file(
  "landfire", "LF2020_SlpD_220_CONUS", "Tif", "LC20_SlpD_220.tif"
))
# Already in Albers Conical
# split and recode
slope <- splitRaster(slope, nx=8, ny=8) #SpaDES.tools::
slope <- mclapply(slope, topographics_reclassify, mc.cores = numCores)
slope$fun <- mean # if overlapping areas _ but there should not be any
slope$na.rm <- TRUE
slope <- do.call(what=raster::mosaic, args=slope)
path <- build_file("crosswalk", "topography", "slope_projected.tif")
writeRaster(slope, file=path, format="GTiff", overwrite=TRUE) 
