## Script to clean the quarantine data ----
## Also clean Chris Muller's boll weevil data --
# Started November 16th #

rm(list=ls())
source("clean/paths.R")
packages<-c("dplyr","haven","parallel","data.table","lubridate","ggExtra",
            "stringr","sf","ggplot2","ipumsr","readxl","gridExtra","grid",
            "binsreg","stringr","DescTools","foreign","rgdal","rgeos")
lapply(packages, require, character.only = TRUE)
numCores <- detectCores() - 2
 
## 1. Quarantine Data ----

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

# files - remove asian longhorned beetle, very recent with little data
listfiles <- list.files(raw_file("optional_covariates", "other_pests", "quarantine"),
                        full.names=TRUE, pattern="xl")[-4][-3]
listfiles <- lapply(listfiles,FUN=function(elem)setDT(read_excel(elem)))

# Emerald Ash Borer
listfiles[[1]]$pest <- "Emerald Ash Borer"
listfiles[[1]] <- listfiles[[1]][,c("fips","pest","year"),with=F]

# Fire Ants - add FIPS
listfiles[[2]] <- listfiles[[2]][,c("pest","status","name","state"),with=F]
listfiles[[2]][,`:=`(name=str_replace_all(tolower(name)," ",""),
                     state=str_replace_all(tolower(state)," ",""))]
listfiles[[2]][,name:=str_replace(name,"\\.","")]

# change some names
listfiles[[2]] <- listfiles[[2]][name=="chesapeake",name:="chesapeakecity"]
listfiles[[2]] <- listfiles[[2]][name=="hampton"&state=="virginia",name:="hamptoncity"]
listfiles[[2]] <- listfiles[[2]][name=="newportnews",name:="newportnewscity"]
listfiles[[2]] <- listfiles[[2]][name=="norfolk",name:="norfolkcity"]
listfiles[[2]] <- listfiles[[2]][name=="poquoson",name:="poquosoncity"]
listfiles[[2]] <- listfiles[[2]][name=="portsmouth",name:="portsmouthcity"]
listfiles[[2]] <- listfiles[[2]][name=="suffolk",name:="suffolkcity"]
listfiles[[2]] <- listfiles[[2]][name=="virginiabeach",name:="virginiabeachcity"]
listfiles[[2]] <- listfiles[[2]][name=="williamsburg"&state=="virginia",name:="williamsburgcity"]
listfiles[[2]] <- listfiles[[2]][name=="emporia",name:="emporiacity"]

usnames <- setDT(read_excel(raw_file(
  "optional_covariates", "other_pests", "county_names", "uscounties.xlsx"
)))
usnames[,`:=`(Name=str_replace_all(tolower(Name)," ",""),
              State=str_replace_all(tolower(State)," ",""))]
usfips <- fread(raw_file(
  "optional_covariates", "other_pests", "county_names", "us_fips.csv"
))
usfips[,c("FIPS","V4","V5","V6","V7"):=NULL]
names(usfips) <- c("state","stateabv")
usfips[,`:=`(state=str_replace_all(tolower(state)," ",""),
             stateabv=str_replace_all(tolower(stateabv)," ",""))]
usnames <- left_join(usnames,usfips,by=c("State"="stateabv"))
usnames <- setDT(usnames)
usnames[,State:=NULL]

listfiles[[2]] <- left_join(listfiles[[2]],usnames,by=c("state"="state",
                                              "name"="Name"))

listfiles[[2]][,c("name","state"):=NULL]
names(listfiles[[2]]) <- c("pest","year","fips") 
listfiles[[2]][,year:=as.numeric(str_extract(year,"[[:digit:]]{4}"))]

# Move to dataset with 1/0 for whether the county has already been quarantined
listfiles[[1]][,presence:=0]
data1 <- listfiles[[1]] %>% tidyr::complete(fips,year = 1931:2020, 
                                     fill = list(presence = 0)) %>% as.data.frame()
listfiles[[1]][,c("presence","pest"):=NULL]
names(listfiles[[1]]) <- c("fips","arrival")
data1 <- setDT(data1)
data1[,pest:="Emerald Ash Borer"]
data1 <- setDT(left_join(data1,listfiles[[1]],by=c("fips")))
data1 <- data1[year>=arrival,presence:=1]
data1 <- data1[!is.na(fips),]

listfiles[[2]][,presence:=0]
data2 <- listfiles[[2]] %>% tidyr::complete(fips,year = 1892:2020, 
                                     fill = list(presence = 0)) %>% as.data.frame()
listfiles[[2]][,c("presence","pest"):=NULL]
names(listfiles[[2]]) <- c("arrival","fips")
data2 <- setDT(data2)
data2[,pest:="Fire Ant"]
data2 <- setDT(left_join(data2,listfiles[[2]],by=c("fips")))
data2 <- data2[year>=arrival,presence:=1]
data2 <- data2[!is.na(fips),]

data <- setDT(bind_rows(data1,data2))
data[,fips:=str_pad(as.character(fips),width=5,side="left",pad="0")]
data[,arrival:=NULL]

## 2. Boll Weevil Data ----

# Note 1907 is the creation of Oklahoma, 1900 file has Indian territory still
bollweevil <- setDT(read_excel(raw_file(
  "boll_weevil", "Analysis_Master_1900.xlsx"
)))
gisvar <- grep(names(bollweevil),pattern="GISJ|FID|DECADE|SHAPE|Orig|Max|Min",value=T)
bollweevil[,c(gisvar):=NULL]
bollweevil[,c("STATENAM","NHGISNAM"):=NULL]

# Add 1900 county area
historical_counties <- st_read(
  raw_file("geography", "US_AtlasHCB_Counties", "US_HistCounties_Shapefile",
           "US_HistCounties.shp")
)
years <- c("1900-01-01")
proj_albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 
                +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# Get shapefiles - map with 1910 fips
map1900 <- make_shapefiles(year=years, atlas=historical_counties)
map1900 <- map1900[[1]] %>% st_as_sf()
map1900 <- map1900 %>% st_transform(proj_albers)
map1900 <- st_make_valid(map1900) # to avoid evaluation error - topology exception in st_intersect

# map with 1900 NHGiS
nhgis1900 <- read_sf(raw_file(
  "geography", "nhgis_historical_shapefiles",
  "nhgis0001_shapefile_tl2000_us_county_1900", "US_county_1900.shp"
))
nhgis1900 <- nhgis1900 %>% subset(select=c("NHGISST","NHGISCTY"))
nhgis1900 <- nhgis1900 %>% st_transform(proj_albers)
nhgis1900 <- st_make_valid(nhgis1900) # to avoid evaluation error - topology exception in st_intersec
nhgis1900 <- split(nhgis1900, seq(nrow(nhgis1900))) 

# Overlay the 2
intersect_county <- function(x,list_counties){
  # get all the intersections between each beginning county and the set of base counties
  layer_intersection <- st_intersection(x,list_counties)
  layer_intersection <- layer_intersection %>% mutate(intersected_area = st_area(.) %>% as.numeric())
  return(layer_intersection)
}
intersections <- mclapply(nhgis1900, FUN= function(x) intersect_county(x=x,list_counties=map1900), mc.cores=numCores)
intersections <- do.call("bind_rows", intersections)
# Filter out those which are not polygons or multi-polygons
intersections <- sf::st_as_sf(intersections)
intersections <- intersections %>% filter(
  st_geometry_type(.)
  %in% c("POLYGON", "MULTIPOLYGON"))

#plot(intersections)
intersections <- intersections %>% st_drop_geometry()
intersections <- intersections %>% subset(select=c(NHGISST,NHGISCTY,FIPS))

# Get 1920 FIPS on boll weevil data
bollweevil <- semi_join(bollweevil,intersections,by=c("NHGISST"="NHGISST","NHGISCTY"="NHGISCTY")) # all matched
bollweevil <- left_join(bollweevil,intersections,by=c("NHGISST"="NHGISST","NHGISCTY"="NHGISCTY")) 
bollweevil <- bollweevil %>% subset(select=-c(NHGISST,NHGISCTY))

# Not match 1902 counties to the 1997 ones
mycrosswalk_1997 <- setDT(readRDS(build_file("crosswalk", "mycrosswalk_1997.rds")))
mycrosswalk_1997 <- mycrosswalk_1997[year==1900,]
mycrosswalk_1997 <- mycrosswalk_1997[,c("base_FIPS","beginning_FIPS","weight_agarea1"),with=F]
bollweevil <- left_join(bollweevil,mycrosswalk_1997,by=c("FIPS"="beginning_FIPS")) # 80 obs
bollweevil <- setDT(bollweevil)
bollweevil <- bollweevil[is.na(weight_agarea1),weight_agarea1:=0]
bollweevil <- bollweevil[,c("FIPS"):=NULL]

# Sum the values by base_fips
## Aggregating Function 
myweighted_sum <- function(colname,dataframe,weightname){
  
  # set up
  dataframe <- get(dataframe)
  dataframe <- data.frame(dataframe)
  colname2 <- enquo(colname) ; weightname2 <- enquo(weightname)
  final_colname <- quo_name(colname)
  
  # operation
  sub_data <- dataframe %>% 
    dplyr::select(!!colname2,base_FIPS,!!weightname2) %>%
    group_by(base_FIPS)  %>% 
    summarize(!!final_colname:= sum(!!sym(colname)*!!sym(weightname),na.rm =TRUE))
  
  return(sub_data)
}

colstoavg <- grep(names(bollweevil),pattern="Area",value=T)
bollweevilag <- lapply(colstoavg,FUN=function(x) myweighted_sum(colname=x,dataframe="bollweevil",weightname="weight_agarea1"))
bollweevilag <- bollweevilag %>% purrr::reduce(left_join,by = c("base_FIPS"))

# Now need to pivot
bollweevilag <- tidyr::pivot_longer(data=bollweevilag,
                                    cols=all_of(colstoavg),
                                    names_to=c("area_infested_bw")) 
bollweevilag <- setDT(bollweevilag)
names(bollweevilag) <- c("base_FIPS","year","areainfested_bw")
bollweevilag[,year:=as.numeric(str_extract(year,pattern="[[:digit:]]+"))]
bollweevilag <- bollweevilag[year>80,year:=as.numeric(paste0("18",as.character(year)))]
bollweevilag <- bollweevilag[year<30,year:=as.numeric(paste0("19",as.character(year)))]
bollweevilag <- bollweevilag[year<200,year:=as.numeric(paste0("190",substr(as.character(year),3,3)))]
bollweevilag <- bollweevilag[areainfested_bw>0,areainfested_bw:=1]
bollweevilag[,pest:="Boll Weevil"]
names(bollweevilag) <- c("fips","year","presence","pest")


## 3. Bind ----
data <- setDT(bind_rows(data,bollweevilag))
# keep split it two for matching
data1 <- data[pest=="Emerald Ash Borer",]
data2 <- data[pest=="Fire Ant",]
data3 <- data[pest=="Boll Weevil",]


## 4. Neighbors ----

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

# Add neighbors to pest dataset
# not matched is Puerto Rico + Montgomery NY and Gloucester NJ

# datasets from which to get neighbor's info
data1_info <- data[pest=="Emerald Ash Borer",]
data1_info <- data1_info[,pest:=NULL]
data2_info <- data[pest=="Fire Ant",]
data2_info <- data2_info[,pest:=NULL]
data3_info <- data[pest=="Boll Weevil",]
data3_info <- data3_info[,pest:=NULL]

# final datasets
data1 <- data[pest=="Emerald Ash Borer",]
data2 <- data[pest=="Fire Ant",]
data3 <- data[pest=="Boll Weevil",]
data1 <- left_join(data1,map1997,by=c("fips"))
data2 <- left_join(data2,map1997,by=c("fips"))
data3 <- left_join(data3,map1997,by=c("fips"))

# match each neighbor's info
for(rank in seq(1,14)){
  
  col <- paste0("neighbor",rank)
  
  # Data 1
  setnames(data1_info,"presence",paste0("presence_",col))
  setnames(data1,col,"neighbor")
  data1 <- setDT(left_join(data1,data1_info,by=c("neighbor"="fips","year"="year")))
  setnames(data1_info,paste0("presence_",col),"presence")
  setnames(data1,"neighbor",col)
  
  # Data 2
  setnames(data2_info,"presence",paste0("presence_",col))
  setnames(data2,col,"neighbor")
  data2 <- left_join(data2,data2_info,by=c("neighbor"="fips","year"="year"))
  setnames(data2_info,paste0("presence_",col),"presence")
  setnames(data2,"neighbor",col)
  
  # Data 3
  setnames(data3_info,"presence",paste0("presence_",col))
  setnames(data3,col,"neighbor")
  data3 <- left_join(data3,data3_info,by=c("neighbor"="fips","year"="year"))
  setnames(data3_info,paste0("presence_",col),"presence")
  setnames(data3,"neighbor",col)
  
}

# If NA for the neighbor presence info - means that fips was not in the quarantine data, or that the county 
# was never quarantined - so can leave it NA

# Now create dummy for being the neighbor of a quarantined county
data1[,neighboring_presence:=rowSums(.SD,na.rm=T),.SDcols=c("presence_neighbor1","presence_neighbor2",
                                                            "presence_neighbor3","presence_neighbor4",
                                                            "presence_neighbor5","presence_neighbor6",
                                                            "presence_neighbor7","presence_neighbor8",
                                                            "presence_neighbor9","presence_neighbor10",
                                                            "presence_neighbor11","presence_neighbor12",
                                                            "presence_neighbor13","presence_neighbor14")]
data2[,neighboring_presence:=rowSums(.SD,na.rm=T),.SDcols=c("presence_neighbor1","presence_neighbor2",
                                                            "presence_neighbor3","presence_neighbor4",
                                                            "presence_neighbor5","presence_neighbor6",
                                                            "presence_neighbor7","presence_neighbor8",
                                                            "presence_neighbor9","presence_neighbor10",
                                                            "presence_neighbor11","presence_neighbor12",
                                                            "presence_neighbor13","presence_neighbor14")]
data3[,neighboring_presence:=rowSums(.SD,na.rm=T),.SDcols=c("presence_neighbor1","presence_neighbor2",
                                                            "presence_neighbor3","presence_neighbor4",
                                                            "presence_neighbor5","presence_neighbor6",
                                                            "presence_neighbor7","presence_neighbor8",
                                                            "presence_neighbor9","presence_neighbor10",
                                                            "presence_neighbor11","presence_neighbor12",
                                                            "presence_neighbor13","presence_neighbor14")]
data1[is.na(neighboring_presence)|neighboring_presence==0,neighboring_presence:=0]
data1[neighboring_presence!=0,neighboring_presence:=1]
data2[is.na(neighboring_presence)|neighboring_presence==0,neighboring_presence:=0]
data2[neighboring_presence!=0,neighboring_presence:=1]
data3[is.na(neighboring_presence)|neighboring_presence==0,neighboring_presence:=0]
data3[neighboring_presence!=0,neighboring_presence:=1]

# remove columns
colstorem <- grep(names(data1),pattern="neighbor",value=T)
colstorem <- grep(colstorem,pattern="neighboring_presence",invert=T,value=T)
data1[,(colstorem):=NULL]
data2[,(colstorem):=NULL]
data3[,(colstorem):=NULL]
data1[,pest:=NULL]
data2[,pest:=NULL]
data3[,pest:=NULL]

names(data1) <- c("fips","year","EABpresence","EABneighboringpresence")
names(data2) <- c("fips","year","IFApresence","IFAneighboringpresence")
names(data3) <- c("fips","year","BWpresence","BWneighboringpresence")
data <- setDT(full_join(data1,data2,by=c("fips","year")))
data <- setDT(full_join(data,data3,by=c("fips","year")))

data <- data[is.na(EABpresence),EABpresence:=0]
data <- data[is.na(EABneighboringpresence),EABneighboringpresence:=0]
data <- data[is.na(IFApresence),IFApresence:=0]
data <- data[is.na(IFAneighboringpresence),IFAneighboringpresence:=0]
data <- data[is.na(BWpresence),BWpresence:=0]
data <- data[is.na(BWneighboringpresence),BWneighboringpresence:=0]

# Final cleaning for BW to match maps
# Remove New Mexico and Nebraska/Dakota (North/South) contaminations
data[,state:=substr(fips,1,2)]
data[state%in%c(35,38,46),BWpresence:=0]
data[state%in%c(35,38,46),BWneighboringpresence:=0]
data[,state:=NULL]

saveRDS(data, build_file("covariates", "allpest_data.rds"))
