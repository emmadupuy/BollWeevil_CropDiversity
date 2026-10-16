## Code for Cluster Weight Creation ##
rm(list=ls())
source("clean/paths.R")

list_packages <- list("parallel","dplyr","sf","CropScapeR","sp","xgboost","caret","mlr",
                      "parallelMap","mice","raster","stars","prism","stringr","purrr",
                      "rgdal","httr","SpaDES","tigris","exactextractr")
lapply(list_packages, require, character.only = TRUE)
## module requirements: gcc/6.1.0 for xgboost, for raster: geos/3.6.5, gdal/2.2.2, boost/1.72

#*#*#*#*#*#*#*#*#*#
#*#* FUNCTIONS #*#*
#*#*#*#*#*#*#*#*#*#

numCores <- detectCores()

proj_albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 
                +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


## 1. Functions ----

loadRdata <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

## Download the PRISM data and make a large shapefile
get_layer <- function(var_item,month,type,year){
  
  local_files <- prism::prism_archive_ls() # lists all the prism data file locally available at the prism path
  # take 30y average or just average over one month/year (so keep the same)
  if(type=="30"){
    list_years <- list("1895","1896","1897","1898","1899","1900","1901","1902","1903","1904","1905",
                       "1906","1907","1908","1909","1910","1911","1912","1913","1914","1915","1916",
                       "1917","1918","1919","1920","1921","1922","1923","1924")
    name_checks <- lapply(list_years, FUN=function(x) paste0("(?<=PRISM_",var_item,"_stable_4kmM)\\d{1}\\_",x,month))
    name_checks <- unlist(name_checks, recursive = TRUE, use.names = TRUE)
    monthly_files <- purrr::map(name_checks, str_subset, string = local_files) %>% 
      reduce(union)
    
    raster <- prism::pd_stack(monthly_files)
    raster_res <- mean(raster)
  }else{
    name_check <- paste0("(?<=PRISM_",var_item,"_stable_4kmM)\\d{1}\\_",year,month)
    monthly_files <- str_subset(string=local_files,pattern=name_check) 
    raster <- prism::pd_stack(monthly_files)
    raster_res <- raster
  }
  proj4string(raster_res) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  return(raster_res)  
}

## FUNCTION
get_raster <- function(var_item,path,year){
  
  type <- if (as.numeric(year) < 1895) "30" else "one_year"
  
  if(type=="30"){
    prism::get_prism_monthlys(type = var_item, years=1895:1924, mon=1:12, keepZip = FALSE)
    list_months <- list("01","02","03","04","05","06","07","08","09","10","11","12")
    list_tmp <- lapply(list_months, FUN=function(x) get_layer(var_item=var_item,month=x,type=type,year=year))
    raster_list_tmp <- do.call(raster::addLayer,list_tmp)
  }
  
  if(type=="one_year"){
    year_get <- as.numeric(year)
    prism::get_prism_monthlys(type = var_item, years=year_get, mon=1:12, keepZip = FALSE)
    list_months <- list("01","02","03","04","05","06","07","08","09","10","11","12")
    list_tmp <- lapply(list_months, FUN=function(x) get_layer(var_item=var_item,month=x,type=type,year=year))
    raster_list_tmp <- do.call(raster::addLayer,list_tmp)
  }
  
  return(raster_list_tmp)
  
}


# Function to bind the 12 raster layers for one variable in a year into a unique sf object
get_sfobj <- function(var_item,rasters,path_to_prism){
  sf::sf_use_s2(FALSE)
  var_rank <- match(var_item,var_list)
  data_tmp <- subset(rasters[[var_rank]],1) # for each list of yearly rasters, pick the relevant yearly one
  data_tmp <- splitRaster(data_tmp, nx=4, ny=4) 
  # raster to sf
  data_tmp <- lapply(data_tmp, FUN = function(raster_elem)
    sf::st_as_sf(stars::st_as_stars(raster_elem), as_points = FALSE, merge = TRUE))
  # bind back to sf
  tmp <- data_tmp[[1]]
  tmp <- st_transform(tmp, crs=st_crs(5070))
  tmp <- st_sf(tmp, crs=st_crs(5070))
  name <- str_extract(names(tmp)[1], ".+(?=\\_bil)")
  names(tmp)[1] <- c(name)
  for(i in seq_along(data_tmp)){
    data_tmp[[i]] <- st_transform(data_tmp[[i]], crs=st_crs(5070))
    data_tmp[[i]] <- st_sf(data_tmp[[i]], crs=st_crs(5070))
    names(data_tmp[[i]])[1] <- c(name)
    tmp <- rbind(tmp, data_tmp[[i]]) 
  }
  data_tmp <- tmp ; rm(tmp)
  data_tmp <- st_make_valid(data_tmp)
  for(j in 2:12){
    tmp <- subset(rasters[[var_rank]],j)
    tmp <- splitRaster(tmp, nx=4, ny=4) 
    tmp <- lapply(tmp, FUN = function(raster_elem)
      sf::st_as_sf(stars::st_as_stars(raster_elem), as_points = FALSE, merge = TRUE))
    # bind back to sf
    tmp2 <- tmp[[1]]
    tmp2 <- st_transform(tmp2, crs=st_crs(5070))
    tmp2 <- st_sf(tmp2, crs=st_crs(5070))
    name <- str_extract(names(tmp2)[1], ".+(?=\\_tile)")
    names(tmp2)[1] <- c(name)
    for(i in seq_along(tmp)){
      tmp[[i]] <- st_transform(tmp[[i]], crs=st_crs(5070))
      tmp[[i]] <- st_sf(tmp[[i]], crs=st_crs(5070))
      names(tmp[[i]])[1] <- c(name)
      tmp2 <- rbind(tmp2, tmp[[i]]) #, sf_column_name = "geometry"
    }
    tmp <- tmp2 ; rm(tmp2)
    tmp <- st_make_valid(tmp)
    data_tmp <-  st_join(data_tmp,tmp,join=st_equals)
    rm(tmp)
    gc()
  }
  return(data_tmp)
}

# Function to call get_sfobj for one year and all the variables - returns a list of size the # of vars
get_list_sf <- function(rasters,var_list,path_to_prism){
  tmp <- lapply(var_list, FUN = function(var_item) get_sfobj(var_item=var_item,rasters=rasters,
                                                             path_to_prism=path_to_prism))
  return(tmp)
}

# Function to bind the 6 sf objects for a given year
get_sfobj2 <- function(list_datas){
  len <- length(list_datas)
  prism_data_year <- list_datas[[1]] # 1st variable of the 6
  for(i in 2:len){
    prism_data_year <- st_join(prism_data_year,list_datas[[i]],join=st_equals)
  }
  return(prism_data_year)
}

# reproject and transform for each year
modif_sf <- function(sf_item,proj_type){
  #sf_item <- st_transform(sf_item,proj_type)
  sf_item <- sf::st_as_sf(sf_item)
  sf_item <- sf:::as_Spatial(sf_item)
  return(sf_item)
}

raster_to_final <- function(year, var_list, path_to_prism, path_save){
  # function to load prism and transform
  # load elevation and slope
  # match them, clean sf object and move it to dataframe
  
  #*#*#*#*#*#*#*#*#*#*#*#*#*#*
  #*# First Get Prism Data #*#
  #*#*#*#*#*#*#*#*#*#*#*#*#*#* 
  # This is already done
  #prism_year <- mclapply(var_list,
  #                       FUN=function(x) get_raster(var_item=x,path=path_to_prism,year=year),
  #                       mc.cores = numCores)

  # move to a unique sf per variableXyear
  #prism_year <- mclapply(var_list, FUN = function(var_item) get_sfobj(var_item=var_item,rasters=prism_year,
  #                                                                    path_to_prism=path_to_prism),
  #                       mc.cores = numCores)
  
  # move to a unique sf & transform
  #prism_year <- get_sfobj2(prism_year)
  #prism_year <- modif_sf(prism_year) 
  #prism_year$year <- year
  year_char <- toString(year)
  prism_year <- loadRdata(build_file(
    "crosswalk", "prism", paste0("prism_", year_char, ".Rdata")
  ))
  prism_year$ID <- seq.int(nrow(prism_year)) # ID used to match agri/elevation/slope
  prism_base <- subset(prism_year,select=c(ID))
  
  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
  #*#  Second Get Others #*#
  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#* 
  
  # Load Elevation and Slope and match too
  elevation <- raster(build_file("crosswalk", "topography", "elevation_projected.tif"))
  elevation <- exactextractr::exact_extract(x=elevation, y=prism_year, fun="mean", weights="area",
                                            force_df=TRUE, append_cols=names(prism_year))
  colnames(elevation)[which(names(elevation) == "mean")] <- "elevation"
  
  slope <- raster(build_file("crosswalk", "topography", "slope_projected.tif"))
  slope <- exactextractr::exact_extract(x=slope, y=prism_base, fun="mean", weights="area",
                                        force_df=TRUE, append_cols=names(prism_base))
  names(slope)[length(slope[1,])] <- "slope"
  
  # Match all datasets
  prism_year <- left_join(elevation,slope,by="ID")
  prism_year <- subset(prism_year,select=-c(ID))
  
  path <- paste0(path_save, "prism_topography", year_char, ".Rdata")
  save(prism_year, file=path)
  print("point reached 3")
  print(year)
  rm(prism_year, path,slope,elevation) ; gc()
  
}

#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#*# Third Implementation #*#
#*#*#*#*#*#*#*#*#*#*#*#*#*#* 

path_to_prism <- raw_file("prism")
path_save <- paste0(build_dir("crosswalk", "prism_topography"), .Platform$file.sep)
options(prism.path = path_to_prism)

years <- list("1840","1850","1860","1870","1880","1890","1900","1910","1920","1930","1940","1950",
              "1925","1935","1945","1954","1959","1964","1969",'1974',"1978","1982","1987","1992",
              "1997","2002","2007","2012")
var_list <- list('ppt','tmin','tmax','tdmean','vpdmin','vpdmax')

## Prepare these sf objects for a unique panel of PRISM 2008-2021 for the US for our 72 variables (6*12) + elevation + slope
# This is sent in array mode on cluster
task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
ranks <- if (nzchar(task_id)) as.numeric(task_id) else seq_along(years)
for (rank in ranks) {
  raster_to_final(year=years[[rank]], var_list=var_list,
                  path_to_prism=path_to_prism, path_save=path_save)
}
