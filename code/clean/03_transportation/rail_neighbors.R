## Add neighboring county data to the rail data ##

rm(list=ls())
source("clean/paths.R")
packages<-c("data.table","sf","parallel","lubridate","tidyr",
            "stringr","dplyr","foreign","readxl","labelled",
            "crunch")
lapply(packages, require, character.only = TRUE)
numCores <- detectCores() - 2

## FUNCTION
make_shapefiles <- function(year, atlas){
  ## Used to make shapefiles for relevant years from Atlas of Historical County Boundaries
  my_year <- as.Date(year,"%Y-%m-%d")
  year_layer <- historical_counties[which(
    historical_counties$START_DATE<=my_year&historical_counties$END_DATE>my_year),]
  # move to USA Contiguous Albers Equal Area Conic
  year_layer <- st_transform(year_layer,proj_albers)
  year <- as.numeric(format(my_year,"%Y"))
  res <- list(year_layer,as.numeric(year))
  names(res) <- c("layer","year")
  return(res)
}

## 0. Rail Data ----

rail <- fread(raw_file("transportation", "gee_county_centroid_rail_distance.csv"))
rail[, `.geo` := NULL]
rail[, `system:index` := NULL]
rail <- melt(rail, id.vars = "GEOID")
colnames(rail) <- c("fips", "year", "distance_to_rail")
rail[, fips := str_pad(fips, 5, "left", "0")]
rail[, year := as.numeric(gsub(".*_(.*)", "\\1", year))]
rail[, less_than_10km := distance_to_rail < 10000]


## 1. Neighbor Data ----

## add neighboring county information
# Add 1997 county area
historical_counties <- st_read(
  raw_file("geography", "US_AtlasHCB_Counties", "US_HistCounties_Shapefile",
           "US_HistCounties.shp")
)
years <- c("1997-01-01")
proj_albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 
                +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# Get shapefiles
map1997 <- make_shapefiles(year=years, atlas=historical_counties)
map1997 <- map1997[[1]] %>% st_as_sf()
map1997 <- map1997 %>% select(subset=c(FIPS))
neighboringcounties <- st_touches(map1997, sparse = FALSE)
rownames(neighboringcounties) <- map1997$subset
colnames(neighboringcounties) <- map1997$subset

map1997 <- map1997 %>% st_drop_geometry()
names(map1997) <- c("fips")
map1997 <- setDT(map1997)

for(rank in seq_along(neighboringcounties[,1])){
  # for each county - get its neighbors
  # note max number is 14
  row <- neighboringcounties[rank,]
  list <- as.list(map1997$fips[row])
  for(rank2 in seq_along(list)){
    col <- paste0("neighbor",rank2)
    map1997[rank,(col):=list[[rank2]]]
  }
}

# Remove if un-necessary neighbor var
map1997 <- map1997[,(names(map1997)):=lapply(.SD,
                                             FUN=function(elem) str_pad(as.character(elem),width=5,side="left",pad="0")),
                   .SDcols=names(map1997)]

data <- left_join(rail,map1997,by=c("fips"))
rail[,distance_to_rail:=NULL]

# match each neighbor's info
for(rank in seq(1,14)){
  
  col <- paste0("neighbor",rank)
  setnames(rail,"less_than_10km",paste0("less_than_10km_",col))
  setnames(data,col,"neighbor")
  data <- setDT(left_join(data,rail,by=c("neighbor"="fips","year"="year")))
  setnames(rail,paste0("less_than_10km_",col),"less_than_10km")
  setnames(data,"neighbor",col)
  
}


# Now create dummy for being the neighbor of a quarantined county
data[,nb_less_than_10km:=rowSums(.SD,na.rm=T),.SDcols=c("less_than_10km_neighbor1","less_than_10km_neighbor2",
                                                            "less_than_10km_neighbor3","less_than_10km_neighbor4",
                                                            "less_than_10km_neighbor5","less_than_10km_neighbor6",
                                                            "less_than_10km_neighbor7","less_than_10km_neighbor8",
                                                            "less_than_10km_neighbor9","less_than_10km_neighbor10",
                                                            "less_than_10km_neighbor11","less_than_10km_neighbor12",
                                                            "less_than_10km_neighbor13","less_than_10km_neighbor14")]
data[is.na(nb_less_than_10km)|nb_less_than_10km==0,nb_less_than_10km:=0]
data[nb_less_than_10km!=0,nb_less_than_10km:=1]
colstorem <- grep(names(data),pattern="neighbor",value=T)
data[,(colstorem):=NULL]

saveRDS(data, build_file("transportation", "raildata.rds"))

