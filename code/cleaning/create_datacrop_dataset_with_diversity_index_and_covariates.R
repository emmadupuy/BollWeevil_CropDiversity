### Script to check correlations between capital and diversity ###

rm(list=ls())
packages<-c("data.table","sf","parallel","lubridate","tidyr",
            "stringr","dplyr","foreign","readxl","labelled",
            "crunch")
lapply(packages, require, character.only = TRUE)
numCores <- detectCores() - 2
# set working directory

source("2_code/helper/make_shapefile.R")



## 0. Load data ----
data <- readRDS("1_data/haines_agricultural/finaldataset_imputed_continuous.rds")
cpi <- setDT(read_excel("1_data/historical_cpi/historical_cpi.xls"))
data <- setDT(left_join(data,cpi,by=c("year")))
data <- unique(data,by=c("year","base_fips"))



# Add 1997 county area
historical_counties <- st_read(
  "1_data/US_AtlasHCB_Counties/US_HistCounties_Shapefile/US_HistCounties.shp"
)
years <- c("1997-01-01")
proj_albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 
                +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# Get shapefiles
map1997 <- make_shapefiles(year=years, atlas=historical_counties)
map1997 <- map1997[[1]] %>% st_as_sf() %>% st_drop_geometry()
map1997 <- setDT(map1997)[,c("FIPS","AREA_SQMI"),with=F]
map1997 <- map1997[,`:=`(base_fips=as.character(FIPS),area_county=640*AREA_SQMI)]
map1997 <- map1997[,c("base_fips","area_county"),with=F]
map1997 <- unique(map1997, by=c("base_fips"))
data  <- left_join(data,map1997,by=c("base_fips"))
data <- setDT(data)

## convert prices with cpi
colnames <- grep(names(data),pattern="dummy|imputed",invert=T,value=T)
data <- data[,(colnames),with=F]
data <- data[,`:=`(farmval=farmval/cpi,
                   equipval=equipval/cpi,
                   farmlab=farmlab/cpi,
                   farmwages=farmwages/cpi,
                   machine_hire=machine_hire/cpi,
                   machine_hire_sub=machine_hire_sub/cpi)]


## 1. Compute Diversity Indices----
to_keep <- c("base_fips", "year", "totacres")
cropacreage <- grep(names(data),pattern="acreage",value=T)
data <- data[,c(to_keep, cropacreage) ,with=F]
data <- data[year>=1880,] #Remove year without acreage
# Only keep same columns as in Crossley
cropacreage <- grep(names(data),
                    pattern="rice|canesug|wheat|corn|cotton_acreage|irpotat|swpotat|barley|rye|tobac|buckwhe|oats",value=T) # soybean, sorgh, peas, beans, peanut, flax
data <- data[,c(to_keep, cropacreage),with=F]

# Use sum of crops considered
cropacreage <- grep(names(data),pattern="acreage",value=T)
data <- data[,totacres_considered:=rowSums(.SD,na.rm=T),.SDcols=cropacreage]
# compute acreage as a share of agricultural land
data <- data[,(cropacreage):=lapply(.SD,function(x){x/totacres_considered}),
                     .SDcols=cropacreage]

## clean names
colnames(data) <- gsub("_acreage", "", colnames(data))
data <- data[wheat<1,] #one observation
cropacreage <- gsub("_acreage", "", cropacreage)

#### 1.1 Pielou ----------------------------------------------------------
swcols <- unlist(lapply(cropacreage,FUN=function(x)paste0(x,"sw")))
# Getting the Logs
data <- data[,(swcols):=lapply(.SD,function(x){x*log(x)}),.SDcols=cropacreage]
data <- data[,(swcols):=lapply(.SD,function(x){ifelse(is.nan(x),NA,x)}),
                     .SDcols=swcols]
# Computing the Index
data[,pielou:=rowSums(.SD,na.rm=TRUE),.SDcols=swcols,
         by=list(base_fips,year)]
data[,pielou:=-pielou/log(length(swcols))]
## Keep relevant columns
data[, (swcols):= NULL]

####  1.2 Richness ----------------------------------------------------------
countcols <- unlist(lapply(cropacreage,FUN=function(x)paste0(x,"count")))
data[,(countcols):=lapply(.SD,function(x){ifelse(x>0,1,0)}),
         .SDcols=cropacreage]
data[,richness:=rowSums(.SD,na.rm=TRUE),.SDcols=countcols,
         by=list(base_fips,year)]
data[, (countcols) := NULL] # keep relevant columns

####  1.3 Gini-Simpson ----------------------------------------------------------
sqcols <- unlist(lapply(cropacreage,FUN=function(x)paste0(x,"sq")))
data[,(sqcols):=lapply(.SD,function(x){x*x}),.SDcols=cropacreage]
data[,ginisimpson:=rowSums(.SD,na.rm=TRUE),.SDcols=sqcols,
         by=list(base_fips,year)]
data[,ginisimpson:=1-ginisimpson]
data[, (sqcols) := NULL] # keep relevant columns

# lags for the diversity measures
data <- data[order(base_fips,year),]
data[,`:=`(pielou_lag=data.table::shift(pielou,n=1L,fill=NA,type="lag"),
               richness_lag=data.table::shift(richness,n=1L,fill=NA,type="lag"),
               ginisimpson_lag=data.table::shift(ginisimpson,n=1L,fill=NA,type="lag")),
         by=list(base_fips)]


### 2. Add covariates ---- 
#### 2.1 Adding Ruggedness ----
ruggedness <- readRDS("1_data/topographics/landfire_ruggedness_county.rds")
ruggedness <- setDT(ruggedness)
ruggedness <- ruggedness[,c("FIPS","ruggedness"),with=F]
data <- left_join(data, ruggedness, by=c("base_fips"="FIPS"))

#### 2.2 Adding railroad ----
rail <- readRDS("1_data/railroads/raildata.rds")
setnames(rail,"fips","base_fips")
data <- merge(data, rail, by = c("base_fips","year"),all.x = T)
data[, min_dist_to_rail := min(distance_to_rail, na.rm = T), by = "base_fips"]
data[is.na(distance_to_rail)  & year > 1920, distance_to_rail := min_dist_to_rail]
### create year in which county centroid is less than 10km away from a rail line 
data[, year_dist_to_rail_less_than_10km := ifelse(distance_to_rail < 10000, year, NA)]
data[, year_dist_to_rail_less_than_10km := min(year_dist_to_rail_less_than_10km, na.rm = T), by = "base_fips"]


#### 2.3 Adding Highway Data: ----
# old code for mileage at the end
highway <- setDT(readRDS("1_data/highway/countydistance.rds"))
highway[,fips:=paste0(str_pad(as.character(state),width=2,pad="0",side="left"),
                      str_pad(as.character(county),width=3,pad="0",side="left"))]
highway[,c("state","county"):=NULL]
names(highway) <- c("year","distance_hw","fips")
# only 72 counties don't have a match between 1940 and 2000 - almost all in 1997 (only 12 before)
data <- left_join(data,highway,by=c("year"="year","base_fips"="fips"))


#### 2.4 Add market access -----
market_access <- fread("1_data/market_access/market_access.csv.gz")
market_access[, base_fips := str_pad(fips, 5, "left","0")]
market_access[, fips := NULL]
data <- left_join(data,market_access,by=c("year"="year","base_fips"="base_fips"))


#### 2.5 Add variance of prices ----
prices <-  fread("1_data/revenue_instruments/price_instrument.csv")
data <- data %>%
  left_join(prices, c("year"))


#### 2.6 Add variance of revenues 
## Revenues
revenues <- fread("1_data/revenue_instruments/revenue_instruments.csv")
revenues[, base_fips := str_pad(fips , 5, "left", "0")]
data <- merge(data,revenues,by=c('base_fips', "year"), all.x= T)


#### 3. Adding Quarantine Data----
quarantine <- readRDS("1_data/quarantine/allpest_data.rds")
#  give NA when out of range of the original data
quarantine <- quarantine[year>1922,`:=`(BWpresence=NA,
                                        BWneighboringpresence=NA)]
quarantine <- quarantine[year<1892,`:=`(BWpresence=NA,
                                        BWneighboringpresence=NA)]
quarantine <- quarantine[year<1918,`:=`(IFApresence=NA,
                                        IFAneighboringpresence=NA)]
quarantine <- quarantine[year<2002,`:=`(EABpresence=NA,
                                        EABneighboringpresence=NA)]
# Give data of 1901 to 1900 / 1892 to 1890
quarantine <- quarantine[year==1892,year:=1890]
quarantine <- quarantine[year==1901,year:=1900]
quarantine <- unique(quarantine,by=c("fips","year"))
data <- left_join(data,quarantine,by=c("year"="year","base_fips"="fips"))

# Cleaning for BW
data[base_fips=="48473",BWpresence:=1910]
data[base_fips=="48471",BWpresence:=1910]
data[base_fips=="48477",BWpresence:=1910]
data[base_fips=="48481",BWpresence:=1910]
data[base_fips=="48491",BWpresence:=1910]
data[base_fips=="48493",BWpresence:=1910]
data[base_fips=="47065",BWpresence:=1920]
data[,state:=substr(base_fips,1,2)]
data[state=="40"&BWpresence==1900,BWpresence:=1910]
data[,state:=NULL]

### Lag pest presence
data <- data[order(base_fips,year),]
data <- data[,`:=`(totacres_lag = shift(totacres,n=1L,fill=NA,type="lag"),
                    IFApresence_lag = shift(IFApresence,n=1L,fill=NA,type="lag"),
                    IFAneighboringpresence_lag = shift(IFAneighboringpresence,n=1L,fill=NA,type="lag"),
                   BWneighboringpresence_lag = shift(BWneighboringpresence,n=1L,fill=NA,type="lag"),
                    BWneighboringpresence_lag = shift(BWneighboringpresence,n=1L,fill=NA,type="lag")),
                     by=list(base_fips)]
data[, decade := (year %/% 10) * 10]


#### Save ------
write.csv.gz(data,"1_data/haines_agricultural/datacrop.csv.gz", row.names = F)



# balance test
to_keep <-c(cropacreage, "year", "base_fips")
temp <- data[, ..to_keep]
temp <-melt(temp,id.vars = c("year", "base_fips"))
temp <- temp[, .(value = sum(value, na.rm = T)), by = c("year", "variable")]
unique(temp[value > 0, year])
temp <-temp[value > 0]
temp[, .N, by = "year"]
      