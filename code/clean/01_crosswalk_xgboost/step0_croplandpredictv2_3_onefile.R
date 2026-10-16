## Code for Cluster Training ##
## In this file we finish building our dataset, on which we will train our model ##

rm(list=ls())
source("clean/paths.R")

list_packages <- list("parallel","dplyr","sf","CropScapeR","sp","xgboost","caret","mlr",
                      "parallelMap","mice","raster","stars","prism","stringr","purrr",
                      "rgdal","httr","SpaDES","tigris","exactextractr","stringr")
lapply(list_packages, require, character.only = TRUE)
numCores <- detectCores()
## module requirements: gcc/6.1.0 for xgboost, for raster: geos/3.6.5, gdal/2.2.2, boost/1.72

path_to_prism <- raw_file("prism")
path_save <- paste0(build_dir("xgboost", "training"), .Platform$file.sep)
options(prism.path = path_to_prism)
years_cdl <- as.list(as.character(unlist(seq(2008,2020,by=1))))
var_list <- list('ppt','tmin','tmax','tdmean','vpdmin','vpdmax')

us_states <- tigris::states(cb = FALSE, resolution = "500k") %>% st_as_sf() %>%
  filter(!NAME %in% c("Hawaii","United States Virgin Islands",
                      "Commonwealth of the Northern Mariana Islands",
                      "Guam","American Samoa","Puerto Rico","Alaska"))
us_states <- split(us_states, seq(nrow(us_states)))

# Function to load Rdata without having them named with their past object's name
loadRdata <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

## 1. Moving to One Prism & CDL File

fileName <- paste0(path_save, "prism_cdl", year=years_cdl[[1]], ".Rdata")
prism_and_cdl <- loadRdata(fileName) # first file
years_cdl_left <- as.list(as.character(unlist(seq(2009,2020,by=1))))

names <- list(list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_ppt_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),   
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tmin_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tmax_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tdmean_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_vpdmin_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_vpdmax_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              "year","mean","elevation","slope")
names <- unlist(names)
names(prism_and_cdl) <- names

for(i in seq_along(years_cdl_left)){
  
  fileName <- paste0(path_save, "prism_cdl", year=years_cdl_left[[i]], ".Rdata")
  tmp <- loadRdata(fileName)
  names(tmp) <- names
  prism_and_cdl <- rbind(prism_and_cdl,tmp) 
  rm(tmp) ; gc()
}

path <- build_file("xgboost", "model", "mat_trans.Rdata")
save(prism_and_cdl, file=path)
