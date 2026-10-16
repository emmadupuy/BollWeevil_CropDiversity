## Clean the Highway data ##
## Take the Form 500 data from Baum-Snow & a Freeway Shapefile ##
## Get the map of the network every year from 1950 onwards for: 
# - interstates that started/finished opening in a given state
# - interstates that started construction/finished in a given state
## The compute distance to county centroids in a given year - for these 4 networks
# Started December 21st 2022 ----

rm(list=ls())
source("clean/paths.R")
packages<-c("data.table","sf","parallel","lubridate","tidyr",
            "stringr","dplyr","foreign","readxl","labelled",
            "crunch","ggplot2","binsreg","ggpubr","lwgeom",
            "ggExtra","gridExtra","grid")
lapply(packages, require, character.only = TRUE)
numCores <- detectCores() - 5
proj_albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 
                +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

## 0. Form 500 ----

formdata <- setDT(read.dta(raw_file(
  "transportation", "highway", "baum_snow", "data", "form500.dta"
)))
formdata <- formdata[,c("statefips","intst","d_open","d_first"),with=F]
formdata <- formdata %>%
  mutate(d_open=ifelse(d_open<100,as.numeric(paste0("19",as.character(d_open))),2000),
         d_first=ifelse(d_first<100,as.numeric(paste0("19",as.character(d_first))),2000))
formdata[,intst:=paste0("I",intst)]
## keep first and last d_open, d_first for each statefipsXintst
# d_open first
formdata <- formdata[order(d_open)]
formdata_openfirst <- formdata[!is.na(d_open), .SD[1], by = .(statefips, intst)]
formdata_openfirst[,d_first:=NULL]
# d_open last
formdata <- formdata[order(-d_open)]
formdata_openlast <- formdata[!is.na(d_open), .SD[1], by = .(statefips, intst)]
formdata_openlast[,d_first:=NULL]
# d_first first
formdata <- formdata[order(d_first)]
formdata_firstfirst <- formdata[!is.na(d_first), .SD[1], by = .(statefips, intst)]
formdata_firstfirst[,d_open:=NULL]
# d_first last
formdata <- formdata[order(-d_first)]
formdata_firstlast <- formdata[!is.na(d_first), .SD[1], by = .(statefips, intst)]
formdata_firstlast[,d_open:=NULL]
rm(formdata)


## 1. Highway Data ----

highway <- read_sf(raw_file(
  "transportation", "highway", "interstatehighway_shapefile",
  "USA_Freeway_System.shp"
))
highway <- highway %>% dplyr::filter(str_detect(ROUTE_NUM,"I"))
highway <- highway %>% dplyr::filter(!is.na(NUMBER))
highway <- highway %>% subset(select=c(ROUTE_NUM))
highway <- highway %>% st_transform(crs=proj_albers)

# add states
us <- read_sf(raw_file(
  "geography", "nhgis_historical_shapefiles",
  "nhgis0001_shapefile_tl2000_us_county_2000", "US_county_2000.shp"
))
us <- st_make_valid(us)
us <- us %>%
  group_by(ICPSRST) %>%
  summarise(SHAPE_AREA=sum(SHAPE_AREA))
us <- us %>% subset(select=c(ICPSRST))
us <- us %>% st_transform(crs=proj_albers)

## Get shapefile with interstate segments within states

# Function to get segment of highway within state
returnseg <- function(state,road){
  # for a state-interstate pair
  # returns the section of the road within the state
  # if any
  
  ## Get info 
  stateno <- state$ICPSRST
  roadno <- road$ROUTE_NUM
  roadno <- roadno[!is.na(roadno)]
  
  ## Road & state into unique shapes
  state <- st_combine(state)
  # make sure actual road in our sf dataframe
  road <- road[!is.na(road$ROUTE_NUM),]
  road <- st_combine(road)
  
  ## Filter for segments within the state
  A_new = lwgeom::st_split(road, state)
  A_new = st_collection_extract(A_new, "LINESTRING")
  A_new <- st_as_sf(A_new)
  state <- st_as_sf(state)
  A_new <- st_join(A_new,state,left=F)
  
  if(nrow(A_new)>0){
    ## Merge the segments into a multiline
    A_new <- split(A_new, seq(nrow(A_new)))
    final <- A_new[[1]]
    A_new <- A_new[-1]
    for(rank in seq_along(A_new)){
      final <- sf::st_union(final,A_new[[rank]],by_feature=F)
    }
    
    ## Add road and state number to object
    final$ICPSRST <- stateno
    final$ROADNUM <- roadno
    
    return(final)
  } else {
    return(NA)
  }
  
}

us <- split(us, seq(nrow(us)))
highway <- split(highway, seq(nrow(highway)))

res <- mclapply(us,FUN=function(elem1)
                 lapply(highway, FUN=function(elem2) returnseg(elem1,elem2)),
                mc.cores=numCores)
for(rank in seq_along(res)){
  res[[rank]] <- res[[rank]][!is.na(res[[rank]])]
  res[[rank]] <- bind_rows(res[[rank]])
}
res <- bind_rows(res)
res <- res %>% mutate(ICPSRST=as.numeric(ICPSRST))

# add state fips code
xwalk <- setDT(read_excel(raw_file(
  "transportation", "highway", "fips_to_icpsr.xls"
)))
xwalk <- xwalk[,c("FIPS","STICPSR"),with=F]
res <- semi_join(res,xwalk,by=c("ICPSRST"="STICPSR")) # remove the NA states
res <- left_join(res,xwalk,by=c("ICPSRST"="STICPSR")) # remove the NA states
# remove the " N" and " S" in the road names
res <- res %>% mutate(ROADNUM=sub(" .*","",ROADNUM))
saveRDS(res, build_file("transportation", "highway", "intermediary_hw_data.rds"))

# Test for Connecticut
test <- us[[1]]
final <- res[1:9,]

graph <- ggplot() + 
  geom_sf(data=test) +
  geom_sf(data=final,color="red",lwd=1) + theme_bw() +
  ggtitle("Interstates in Connecticut (2020)")
ggsave(graph, filename = build_file("transportation", "highway", "connecticut_ex.png"),
       width = 5, height = 6)


## 1. Clean ---- 

# 36 not matched - we remove them from the analysis?
highway_openfirst <- inner_join(res,formdata_openfirst,by=c("ROADNUM"="intst",
                                                           "FIPS"="statefips"))
highway_openlast <- inner_join(res,formdata_openlast,by=c("ROADNUM"="intst",
                                                            "FIPS"="statefips"))
highway_startfirst <- inner_join(res,formdata_firstfirst,by=c("ROADNUM"="intst",
                                                            "FIPS"="statefips"))
highway_startlast <- inner_join(res,formdata_firstlast,by=c("ROADNUM"="intst",
                                                            "FIPS"="statefips"))

saveRDS(highway_openfirst,
        build_file("transportation", "highway", "highway_data1.rds"))
saveRDS(highway_openlast,
        build_file("transportation", "highway", "highway_data2.rds"))
saveRDS(highway_startfirst,
        build_file("transportation", "highway", "highway_data3.rds"))
saveRDS(highway_startlast,
        build_file("transportation", "highway", "highway_data4.rds"))

# Load 
highway_openfirst <- readRDS(build_file(
  "transportation", "highway", "highway_data1.rds"
))

# histogram plot
graph <- ggplot(highway_openfirst,aes(x=d_open)) +
  geom_histogram() + theme_bw() +
  xlab("Year Opening") + ggtitle("Evolution of the Interstate")
ggsave(graph, filename = build_file("transportation", "highway", "interstate_histo.png"),
       width = 5, height = 6)

# map of open first
res1 <- highway_openfirst[highway_openfirst$d_open<=1950,]$geometry
res2 <- highway_openfirst[highway_openfirst$d_open<=1960,]$geometry
res3 <- highway_openfirst[highway_openfirst$d_open<=1970,]$geometry
res4 <- highway_openfirst[highway_openfirst$d_open<=2000,]$geometry

graph1 <- ggplot() + geom_sf(data=res1) + theme_bw() + ggtitle("1950")
graph2 <- ggplot() + geom_sf(data=res2) + theme_bw() + ggtitle("1960")
graph3 <- ggplot() + geom_sf(data=res3) + theme_bw() + ggtitle("1970")
graph4 <- ggplot() + geom_sf(data=res4) + theme_bw() + ggtitle("2000")

graph <- grid.arrange(arrangeGrob(graph1 + theme(legend.position="none"),
                                  graph2 + theme(legend.position="none"),
                                  graph3 + theme(legend.position="none"),
                                  graph4 + theme(legend.position="none"),
                                  ncol=2),
                      nrow=1,
                      top="Network Evolution",
                      bottom = textGrob("We identify the opening year of the first segment of each interstate in a each state.",
                                        gp= gpar(fontface=3,fontsize=9),
                                        hjust=1,x=1))
ggsave(graph, filename = build_file("transportation", "highway", "genevol2.png"),
       width = 5, height = 6)

graph <- ggplot() + geom_sf(data=highway_openfirst, aes(color=d_open),lwd=3) + 
  theme_bw() + ggtitle("Evolution of Network") + 
  scale_colour_viridis_c(option="plasma")
ggsave(graph, filename = build_file("transportation", "highway", "genevol.png"),
       width = 5, height = 6)

## 2. Distance from centroid in a given year ---- 

# load county maps
counties <- read_sf(raw_file(
  "geography", "nhgis_historical_shapefiles",
  "nhgis0001_shapefile_tl2000_us_county_2000", "US_county_2000.shp"
))
counties <- counties %>% st_transform(crs=proj_albers)
counties <- counties %>% st_centroid() # get county centroids
counties <- counties %>% subset(select=c(NHGISST,NHGISCTY))
counties <- counties %>%
  mutate(NHGISST=substr(NHGISST,1,2),
         NHGISCTY=substr(NHGISCTY,1,3))
# filter for only contiguous US
counties <- counties[!counties$NHGISST%in%c("02","15","78","72"),]
counties <- counties %>%
  mutate(NHGISST=as.numeric(NHGISST),
         NHGISCTY=as.numeric(NHGISCTY))

# Now for each year between 1940 and 2000 - compute the distance to the centroid
getdistance <- function(rank){
  # returns the distances to network from 1940 to 2000
  # rank is row no in county shapefile
  county1 <- counties[[rank]]
  county1data <- list()
  i <- 1
  for(year in seq(from=1940,to=2000,by=1)){
    
    network <- highway_openfirst[highway_openfirst$d_open<=year,]
    if(nrow(network)>0){
      distance <- min(st_distance(county1,network),na.rm=T)
    } else {
      # no interstate yet
      distance <- -1
    }
    
    county1data[[i]] <- data.frame(county1$NHGISST,
                                   county1$NHGISCTY,
                                   year,
                                   distance)
    i <- i+1
  }
  county1data <- bind_rows(county1data)
  return(county1data)
  
}

counties <- split(counties, seq(nrow(counties)))
finaldata <- mclapply(seq_along(counties),getdistance,mc.cores=numCores)
finaldata <- bind_rows(finaldata)
colnames(finaldata) <- c("state","county","year","distance")

saveRDS(finaldata, build_file("transportation", "highway", "countydistance.rds"))
