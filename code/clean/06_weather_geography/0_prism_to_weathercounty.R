## Get historical prism data ---
## Started May 30th 2025

rm(list=ls())
source("clean/paths.R")
list_packages <- list("parallel","dplyr","sf","CropScapeR","sp","xgboost","caret","mlr",
                      "parallelMap","mice","raster","stars","prism","stringr","purrr",
                      "rgdal","httr","SpaDES","tigris","exactextractr")
lapply(list_packages, require, character.only = TRUE)

set.seed(123)


## 0. Prep. ----

numCores <- detectCores()

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

proj_albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 
                +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
historical_counties <- st_read(raw_file(
  "geography", "US_AtlasHCB_Counties", "US_HistCounties_Shapefile",
  "US_HistCounties.shp"
))
years <- c("1997-01-01")
# Get shapefiles
map1997 <- make_shapefiles(year=years, atlas=historical_counties)[[1]] %>% st_transform(proj_albers)

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
  
  if(as.numeric(year)<1885){
    type <- "30"
  } else {
    type <- "one_year"
  }
  
  if(type=="30"){
    list_months <- list("01","02","03","04","05","06","07","08","09","10","11","12")
    list_tmp <- lapply(list_months, FUN=function(x) get_layer(var_item=var_item,month=x,type=type,year=year))
    raster_list_tmp <- do.call(raster::addLayer,list_tmp)
  }
  
  if(type=="one_year"){
    
    if(year==1890){year <- 1895}
      
    year_get <- as.numeric(year)
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
  data_tmp <- splitRaster(data_tmp, nx=4, ny=4) # path = file.path(path_to_prism, "y0"
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
    #name <- str_extract(names(data_tmp[[i]])[1], ".+(?=\\_bil)")
    names(data_tmp[[i]])[1] <- c(name)
    tmp <- rbind(tmp, data_tmp[[i]]) #, sf_column_name = "geometry"
  }
  data_tmp <- tmp ; rm(tmp)
  data_tmp <- st_make_valid(data_tmp)
  for(j in 2:12){
    tmp <- subset(rasters[[var_rank]],j)
    tmp <- splitRaster(tmp, nx=4, ny=4) #, path = file.path(path_to_prism, "y0"
    tmp <- lapply(tmp, FUN = function(raster_elem)
      sf::st_as_sf(stars::st_as_stars(raster_elem), as_points = FALSE, merge = TRUE))
    # bind back to sf
    tmp2 <- tmp[[1]]
    tmp2 <- st_transform(tmp2, crs=st_crs(5070))
    tmp2 <- st_sf(tmp2, crs=st_crs(5070))
    name <- str_extract(names(tmp2)[1], ".+(?=\\_bil)")
    names(tmp2)[1] <- c(name)
    for(i in seq_along(tmp)){
      tmp[[i]] <- st_transform(tmp[[i]], crs=st_crs(5070))
      tmp[[i]] <- st_sf(tmp[[i]], crs=st_crs(5070))
      #name <- str_extract(names(tmp[[i]])[1], ".+(?=\\_bil)")
      names(tmp[[i]])[1] <- c(name)
      tmp2 <- rbind(tmp2, tmp[[i]]) #, sf_column_name = "geometry"
    }
    tmp <- tmp2 ; rm(tmp2)
    tmp <- st_make_valid(tmp)
    data_tmp <- st_join(data_tmp,tmp,join=st_equals)
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
  #for(i in 2:len){
  #  prism_data_year <- st_join(prism_data_year,list_datas[[i]],join=st_equals)
  #}
  return(prism_data_year)
}

# reproject and transform for each year
modif_sf <- function(sf_item,proj_type){
  #sf_item <- st_transform(sf_item,proj_type)
  sf_item <- sf::st_as_sf(sf_item)
  sf_item <- sf:::as_Spatial(sf_item)
  return(sf_item)
}

loadRdata <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}


raster_to_final <- function(year, var_list, path_to_prism, path_save){
  # function to load prism and transform and save
  prism_year <- mclapply(var_list,
                         FUN=function(x) get_raster(var_item=x,path=path_to_prism,year=year),
                         mc.cores = numCores)
  
  # move to a unique sf per variableXyear
  #prism_year <- mclapply(var_list, FUN = function(var_item) get_sfobj(var_item=var_item,rasters=prism_year,
  #                                                                    path_to_prism=path_to_prism),
  #                       mc.cores = numCores)
  # move to a unique sf & transform
  #prism_year <- get_sfobj2(prism_year)
  #prism_year <- modif_sf(prism_year) # otherwise add proj_albers if want to tranform here
  #prism_year$year <- year
  year_char <- toString(year)
  saveRDS(prism_year, file=build_file(
    "weather", paste0("prism_", year_char, "_", var_list[[1]], ".rds")
  ))
  print(year_char)  
}  

path_to_prism <- raw_file("prism")
path_save <- build_dir("weather")
options(prism.path = path_to_prism)
years_prism <- list("1890","1900","1910","1920","1930","1940","1950","1925","1935",
                    "1945","1954","1959","1964","1969",'1974',"1978",
                    "1982","1987","1992","1997")

## 1. Get ----

# counties
map1997 <- split(map1997, seq(nrow(map1997)))

# ppt
var_list <- list('ppt') #,'tmin','tmax','tdmean','vpdmin','vpdmax'
list <- list()
for(rank in seq_along(years_prism)){
  raster_to_final(year=years_prism[[rank]], var_list=var_list,
                  path_to_prism=path_to_prism, path_save=path_save)  
  path <- build_file(
    "weather", paste0("prism_", years_prism[[rank]], "_", var_list[[1]], ".rds")
  )
  test <- readRDS(path)
  test <- test[[1]]
  communes_data <- mclapply(map1997, 
                          FUN = function(elem) exactextractr::exact_extract(x=test, y=elem, fun="mean", 
                                                                            weights="area", force_df=TRUE, append_cols=names(elem)),
                          mc.cores=numCores)
  communes_data <- data.table::setDT(dplyr::bind_rows(communes_data))
  tp_cols <- names(communes_data)[18:29] # monthly precipitation, which we sum below
  communes_data[,tp:=rowSums(.SD,na.rm=T),.SDcols=c(tp_cols)]
  communes_data <- communes_data[,c("FIPS","tp"),with=F]
  communes_data$year <- years_prism[[rank]]
  list[[rank]] <- communes_data
}
communes_data <- dplyr::bind_rows(list)
saveRDS(communes_data, intermediate_file("weather", "prism_tp_county.rds"))


# tmin
var_list <- list('tmin') #,'tmin','tmax','tdmean','vpdmin','vpdmax'
for(rank in seq_along(years_prism)){
  raster_to_final(year=years_prism[[rank]], var_list=var_list,
                  path_to_prism=path_to_prism, path_save=path_save)  
  
  path <- build_file(
    "weather", paste0("prism_", years_prism[[rank]], "_", var_list[[1]], ".rds")
  )
  test <- readRDS(path)
  test <- test[[1]]
  communes_data <- mclapply(map1997, 
                            FUN = function(elem) exactextractr::exact_extract(x=test, y=elem, fun="mean", 
                                                                              weights="area", force_df=TRUE, append_cols=names(elem)),
                            mc.cores=numCores)
  communes_data <- data.table::setDT(dplyr::bind_rows(communes_data))
  tp_cols <- names(communes_data)[18:29] # monthly precipitation, which we sum below
  communes_data[,tmin:=rowMeans(.SD,na.rm=T),.SDcols=c(tp_cols)]
  communes_data <- communes_data[,c("FIPS","tmin"),with=F]
  communes_data$year <- years_prism[[rank]]
  list[[rank]] <- communes_data
}
communes_data <- dplyr::bind_rows(list)
saveRDS(communes_data, intermediate_file("weather", "prism_tmin_county.rds"))


# tmax
var_list <- list('tmax') #,'tmin','tmax','tdmean','vpdmin','vpdmax'
for(rank in seq_along(years_prism)){
  raster_to_final(year=years_prism[[rank]], var_list=var_list,
                  path_to_prism=path_to_prism, path_save=path_save)  
  
  path <- build_file(
    "weather", paste0("prism_", years_prism[[rank]], "_", var_list[[1]], ".rds")
  )
  test <- readRDS(path)
  test <- test[[1]]
  communes_data <- mclapply(map1997, 
                            FUN = function(elem) exactextractr::exact_extract(x=test, y=elem, fun="mean", 
                                                                              weights="area", force_df=TRUE, append_cols=names(elem)),
                            mc.cores=numCores)
  communes_data <- data.table::setDT(dplyr::bind_rows(communes_data))
  tp_cols <- names(communes_data)[18:29] # monthly precipitation, which we sum below
  communes_data[,tmax:=rowMeans(.SD,na.rm=T),.SDcols=c(tp_cols)]
  communes_data <- communes_data[,c("FIPS","tmax"),with=F]
  communes_data$year <- years_prism[[rank]]
  list[[rank]] <- communes_data
}
communes_data <- dplyr::bind_rows(list)
saveRDS(communes_data, intermediate_file("weather", "prism_tmax_county.rds"))

