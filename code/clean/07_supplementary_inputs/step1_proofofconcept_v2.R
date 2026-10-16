#---# Proof of Concept #---#
## Version with continuous prediction
## We mimick the reallocation of 1870 values to a 1997 baseline
## Because we don't have real 1870 values, we use 2021 ag data for crop coverage and pretend it's the 1870 one
## We compute their ag. fraction (over past prior and later county total area) 
## We compare the true weights to the two methods

rm(list=ls())
source("clean/paths.R")

packages<-c("kableExtra","dplyr","readxl","withr","stringr","haven","ggplot2",
            "stats","lubridate","gridExtra","readxl","expss","stringr","rlist",
            "parallel","raster","purrr","sf","mlr")
lapply(packages, require, character.only = TRUE)
numCores <- detectCores() - 2

# bug with sf - reinstalled pkg-config then run this
#install.packages("sf", configure.args = "--with-proj-lib=/usr/local/lib/")


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

#*# Step 1 -- Get 1870 list of county subsets ----

proj_albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 
                +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
historical_counties <- st_read(raw_file(
  "geography", "US_AtlasHCB_Counties", "US_HistCounties_Shapefile",
  "US_HistCounties.shp"
))
years <- c("1870-01-01","1997-01-01")
atlas_list <- lapply(years,FUN = function(x) make_shapefiles(year=x, atlas=historical_counties))

layer_beginning <- rlist::list.filter(atlas_list,year=="1870")[[1]][[1]] # list of 1 of a list of 2 (shapefile,year)
layer_beginning <- sf::st_make_valid(layer_beginning) # to avoid evaluation error - topology exception in st_intersec
names <- sapply(names(layer_beginning)[1:17], FUN=function(x) paste0("beginning_",x))
names(layer_beginning)[1:17] <- names
layer_base <- rlist::list.filter(atlas_list,year=="1997")[[1]][[1]] # list of 1 of a list of 2 (shapefile,year)
layer_base <- st_make_valid(layer_base)
names <- sapply(names(layer_base)[1:17], FUN=function(x) paste0("base_",x))
names(layer_base)[1:17] <- names

# Remove Alaska
layer_beginning <- layer_beginning[!grepl(x = layer_beginning$beginning_STATE_TERR, pattern = "alaska", ignore.case = T),]
layer_base <- layer_base[!grepl(x = layer_base$base_STATE_TERR, pattern = "alaska", ignore.case = T),]
# Move to SQKM
layer_beginning <- layer_beginning %>% mutate(beginning_AREA_SQM = (1/0.00000038610)*beginning_AREA_SQMI)
layer_base <- layer_base %>% mutate(base_AREA_SQM = (1/0.00000038610)*base_AREA_SQMI)

# Need to get beginning and base predicted ag. coverage before we get the intersections
# PRISM files - 73 variables + geometry per year + elevation + slope
# Remove CDL data
# Prism file loaded + name of vars. used to train the model 
fileName <- build_file("xgboost", "training", "prism_cdl2021.Rdata")
prism_year <- loadRdata(fileName) 
names <- list(list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_ppt_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),   
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tmin_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tmax_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_tdmean_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_vpdmin_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              list(lapply(seq(1,12), FUN = function(x) paste0("PRISM_vpdmax_stable_4kmM3_2008",str_pad(x,width=2,side="left","0")))),
              "year","mean","elevation","slope")
names <- unlist(names)
names(prism_year) <- names
prism_year <- subset(prism_year, select=-year)
prism_year$ID <- seq.int(nrow(prism_year))

# match it to prism shapefile
fileName <- build_file("crosswalk", "prism", "prism_2021.Rdata")
prism_shape <- loadRdata(fileName)
prism_shape <- st_as_sf(prism_shape)
prism_shape$ID <- seq.int(nrow(prism_shape))
prism_shape <- subset(prism_shape, select=c(ID))
prism_year <- left_join(prism_shape, prism_year, by=c("ID"))
prism_year <- prism_year %>% st_transform(proj_albers)
prism_year <- subset(prism_year, select=-c(ID))

# Remove rows with NAs
prism_year <- na.omit(prism_year)
prism_prediction <- st_drop_geometry(prism_year)

# Predict
load(build_file("xgboost", "model", "xgboost_v2.Rdata"))
xgb_model <- xgb_save[[1]]
testTask <- makeRegrTask(data = prism_prediction, target = "mean")
# Make a new prediction
result <- predict(xgb_model, testTask)
xgb_save <- list(result)
results <- setDT(xgb_save[[1]]$data)
#xgb_model <- xgb_save[[1]]
#result <- predict(xgb_model, newdata = prism_prediction)
prism_year <- bind_cols(prism_year, results)

# Match to beginning and baseline layer
layer_beginning = st_join(x=layer_beginning, y=prism_year, st_intersects) %>%
  group_by(beginning_ID_NUM,beginning_NAME,beginning_FIPS,beginning_START_DATE,
           beginning_END_DATE,beginning_AREA_SQM) %>%
  summarize(beginning_ag_coverage = mean(response, na.rm=TRUE))

layer_base = st_join(x=layer_base, y=prism_year, st_intersects) %>%
  group_by(base_ID_NUM,base_NAME,base_FIPS,base_START_DATE,base_END_DATE,base_AREA_SQM) %>%
  summarize(base_ag_coverage = mean(response, na.rm=TRUE))

# Get Intersections
list_beginning <- split(layer_beginning, seq(nrow(layer_beginning))) 
intersect_county <- function(x,list_counties){
  # get all the intersections between each beginning county and the set of base counties
  layer_intersection <- st_intersection(x,list_counties)
  layer_intersection <- layer_intersection %>% mutate(intersected_area = st_area(.) %>% as.numeric())
  return(layer_intersection)
}
intersections <- lapply(list_beginning, FUN= function(x) intersect_county(x=x,list_counties=layer_base))
intersections <- do.call("bind_rows", intersections)
## add an ID to each intersection 
intersections$ID <- seq.int(nrow(intersections))

#*# Step 2 -- Compute the actual ag fractions for these subsets ----

# Load the already reclassified CDL 2021
cdl2021 <- raster(build_file(
  "crosswalk", "cdl", "2021_210m_cdls_agg_for_prediction.tif"
))
intersections <- sf::st_as_sf(intersections)
intersections_td <- st_transform(intersections, crs(cdl2021))
# Filter the ones which are not polygons
intersections_keep <- intersections_td %>% filter(
  st_geometry_type(.)
  %in% c("POLYGON", "MULTIPOLYGON"))
# Extract for them
beginCluster(numCores)
ground_truth <- exactextractr::exact_extract(x=cdl2021, y=intersections_keep, fun="mean", weights="area",
                                             force_df=TRUE, append_cols=names(intersections_keep))
endCluster()

# Compute by Ag. Area by Beginning and Base Shapes too
ground_truth <- ground_truth %>%
  group_by(beginning_ID_NUM,beginning_NAME,beginning_FIPS) %>%
  mutate(beginning_true_agcover = mean(mean,na.rm=T)) %>%
  ungroup()
ground_truth <- ground_truth %>%
  group_by(base_ID_NUM,base_FIPS,base_NAME) %>%
  mutate(base_true_agcover = mean(mean,na.rm=T)) %>%
  ungroup()

# Compute the true weights
ground_truth <- ground_truth %>% 
  mutate(weight_agarea1 = (mean*intersected_area)/(beginning_true_agcover*beginning_AREA_SQM), # for stocks - over origin area
         weight_agarea2 = (mean*intersected_area)/(base_true_agcover*base_AREA_SQM)) # for averages - over base area

# Normalize
ground_truth <- ground_truth %>%
  group_by(beginning_FIPS) %>%
  mutate(normalization1 = sum(weight_agarea1,na.rm=T))
ground_truth <- ground_truth %>%
  group_by(base_FIPS) %>%
  mutate(normalization2 = sum(weight_agarea2,na.rm=T))
ground_truth <- ground_truth %>%
  mutate(weight_agarea1 = weight_agarea1*(1/normalization1),
         weight_agarea2 = weight_agarea2*(1/normalization2))

#*# Step 3 -- Giving the 1910 counties 2021 weather values, we compute our predicted weigths ----

# Match predictions to intersection layer and compute weights
intersections_keep <- st_as_sf(intersections_keep)
intersections_keep <- st_transform(intersections_keep, st_crs(prism_year))

ag_predictions <- st_join(x=intersections_keep, y=prism_year) %>%
  group_by(ID,beginning_NAME,beginning_FIPS,beginning_START_DATE,beginning_END_DATE,
           beginning_AREA_SQM,beginning_ag_coverage,
           base_ID_NUM,base_NAME,base_FIPS,base_AREA_SQM,
           base_ag_coverage,intersected_area) %>%
  summarize(intersected_ag_coverage = mean(response, na.rm=TRUE))

# Compute Weights
ag_predictions <- ag_predictions %>% 
  mutate(weight_agarea1 = (intersected_ag_coverage*intersected_area)/(beginning_ag_coverage*beginning_AREA_SQM), # for stocks - over origin area
         weight_agarea2 = (intersected_ag_coverage*intersected_area)/(base_ag_coverage*base_AREA_SQM)) # for averages - over base area

# Normalize
ag_predictions <- ag_predictions %>%
  group_by(beginning_FIPS) %>%
  mutate(normalization1 = sum(weight_agarea1,na.rm=T))
ag_predictions <- ag_predictions %>%
  group_by(base_FIPS) %>%
  mutate(normalization2 = sum(weight_agarea2,na.rm=T))
ag_predictions <- ag_predictions %>%
  mutate(weight_agarea1 = weight_agarea1*(1/normalization1),
         weight_agarea2 = weight_agarea2*(1/normalization2))

ag_predictions <- ag_predictions %>% st_drop_geometry()


#*# Step 4 -- We compute the classical weights ----

# Get weights
classical_method <- intersections_keep %>% mutate(weight_area1 = (intersected_area)/(beginning_AREA_SQM), # for stocks - over origin area
                                                  weight_area2 = (intersected_area)/(base_AREA_SQM)) # for averages - over base area

# Normalize
classical_method <- classical_method %>%
  group_by(beginning_FIPS) %>%
  mutate(normalization1 = sum(weight_area1,na.rm=T))
classical_method <- classical_method %>%
  group_by(base_FIPS) %>%
  mutate(normalization2 = sum(weight_area2,na.rm=T))
classical_method <- classical_method %>%
  mutate(weight_area1 = weight_area1*(1/normalization1),
         weight_area2 = weight_area2*(1/normalization2))

classical_method <- classical_method %>% st_drop_geometry()

#*# Step 5 -- Histogram of Weights ----
data_graph1 <- subset(ground_truth, select=c(weight_agarea1, weight_agarea2))
data_graph1$type <- "ground truth"
data_graph2 <- subset(ag_predictions, select=c(weight_agarea1, weight_agarea2))
data_graph2$type <- "prediction method"
data_graph3 <- subset(classical_method, select=c(weight_area1, weight_area2))
data_graph3$type <- "area method"
names(data_graph3) <- c("weight_agarea1", "weight_agarea2","type")

data <- bind_rows(data_graph1,data_graph2,data_graph3)

g1 <- ggplot(data[which(data$weight_agarea1<1&data$weight_agarea1<1&data$weight_agarea1>0),], aes(weight_agarea1, colour = type)) +
  geom_density() + theme_bw() +
  xlab("Weight Value - Stocks") + ylab("Density")

g2 <- ggplot(data[which(data$weight_agarea2<1&data$weight_agarea2>0),], aes(weight_agarea2, colour = type)) +
  geom_density() + theme_bw() +
  xlab("Weight Value - Averages") + ylab("Density")

graph <- grid.arrange(g1,g2,
                      nrow=2,
                      top="Allocation Weights for the 1870-1997 Match under 2021 Agricultural Conditions")

density_curve <- function(values, method) {
  values <- values[is.finite(values) & values > 0 & values < 1]
  curve <- stats::density(values, from = 0, to = 1, n = 512)
  data.frame(weight = curve$x, density = curve$y, method = method)
}

curves <- bind_rows(
  density_curve(ground_truth$weight_agarea1, "Ground truth"),
  density_curve(ag_predictions$weight_agarea1, "Machine-learning crosswalk"),
  density_curve(classical_method$weight_area1, "Area-weighted crosswalk")
)
data.table::fwrite(
  curves,
  intermediate_file("supplementary", "proof_density_curves.csv")
)
