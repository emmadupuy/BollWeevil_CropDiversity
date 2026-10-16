## In this code we get the 2021 CDL and test the trained model on it ##

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

path_to_prism <- raw_file("prism")
path_save <- paste0(build_dir("xgboost", "training"), .Platform$file.sep)
options(prism.path = path_to_prism)

loadRdata <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#*#*#*#*#*#*#*#*#
#* 1. Step 1   *#
#*#*#*#*#*#*#*#*#

proj_albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 
                +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
cdl_year <- list("2021")

#*#*#*#*#*#*#*#
#* 3. Step 3 *#
#*#*#*#*#*#*#*#

fileName <- paste0(path_save, "prism_cdl", year="2021", ".Rdata")
prism_and_cdl <- loadRdata(fileName) 

names <- list(list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_ppt_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),   
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tmin_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tmax_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tdmean_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_vpdmin_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_vpdmax_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              "year","mean","elevation","slope")
names <- unlist(names)
names(prism_and_cdl) <- names

prism_and_cdl <- subset(prism_and_cdl, select=-year)
prism_and_cdl <- prism_and_cdl[which(!is.na(prism_and_cdl$mean)),] 

#*#*#*#*#*#*#*#
#* 4. Step 4 *#
#*#*#*#*#*#*#*#

# Run the trained model and check accuracy 
load(build_file("xgboost", "model", "xgboost_v2.Rdata")) # xgb_save - list(xgb_model)
xgb_model <- xgb_save[[1]]
testTask <- makeRegrTask(data = prism_and_cdl, target = "mean")

# Make a new prediction
result <- predict(xgb_model, testTask)
xgb_save <- list(result)
save(xgb_save, file=intermediate_file("supplementary", "model2_results2021.Rdata"))


#### Locally ----

## Our Model
results <- setDT(xgb_save[[1]]$data)

# RMSE
rmse <- sqrt(mean(results[,square_error:=(truth-response)^2]$square_error,na.rm=T))
#0.1724401

# R2
mean <- mean(results$truth)
results[,`:=`(distance_predict=(truth-response)^2,
              distance_mean=(truth-mean)^2)]
r2 <- 1 - sum(results$distance_predict)/sum(results$distance_mean)

# Confusion Matrix 
results[truth>=0&truth<=.25,category:=1]
results[truth>=.25&truth<=.55,category:=2]
results[truth>=.5&truth<=.75,category:=3]
results[truth>=.75&truth<=1,category:=4]
results[response>=0&response<=.25,category_predict:=1]
results[response>=.25&response<=.55,category_predict:=2]
results[response>=.5&response<=.75,category_predict:=3]
results[response>=.75&response<=1,category_predict:=4]

library(caret,xtable)
matrix <- confusionMatrix(factor(results$category),factor(results$category_predict))
print(xtable(matrix$table))

## Regular Model set value of 1 for everyone
results <- results[,response2:=1]
rmse2 <- sqrt(mean(results[,square_error2:=(truth-response2)^2]$square_error2,na.rm=T))
