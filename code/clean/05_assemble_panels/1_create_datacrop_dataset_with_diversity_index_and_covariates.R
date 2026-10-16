### Script to check correlations between capital and diversity ###

rm(list=ls())
source("clean/paths.R")
packages<-c("data.table","sf","parallel","lubridate","tidyr",
            "stringr","dplyr","foreign","readxl","labelled",
            "crunch")
lapply(packages, require, character.only = TRUE)
numCores <- detectCores() - 2
build_panel <- function(haines_input, panel_output) {

## 0. Load data ----
data <- readRDS(haines_input)
data <- unique(data,by=c("year","base_fips"))

colnames <- grep(names(data),pattern="dummy|imputed",invert=T,value=T)
data <- data[,(colnames),with=F]


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
ruggedness <- readRDS(build_file("covariates", "landfire_ruggedness_county.rds"))
ruggedness <- setDT(ruggedness)
ruggedness <- ruggedness[,c("FIPS","ruggedness"),with=F]
data <- left_join(data, ruggedness, by=c("base_fips"="FIPS"))

#### 2.2 Adding railroad ----
rail <- readRDS(build_file("transportation", "raildata.rds"))
setnames(rail,"fips","base_fips")
data <- merge(data, rail, by = c("base_fips","year"),all.x = T)
data[, min_dist_to_rail := min(distance_to_rail, na.rm = T), by = "base_fips"]
data[is.na(distance_to_rail)  & year > 1920, distance_to_rail := min_dist_to_rail]
### create year in which county centroid is less than 10km away from a rail line 
data[, year_dist_to_rail_less_than_10km := ifelse(distance_to_rail < 10000, year, NA)]
data[, year_dist_to_rail_less_than_10km := min(year_dist_to_rail_less_than_10km, na.rm = T), by = "base_fips"]


#### 2.3 Add market access -----
market_access <- fread(raw_file("transportation", "market_access.csv.gz"))
market_access[, base_fips := str_pad(fips, 5, "left","0")]
market_access[, fips := NULL]
data <- left_join(data,market_access,by=c("year"="year","base_fips"="base_fips"))


#### 3. Adding Quarantine Data----
quarantine <- readRDS(build_file("covariates", "boll_weevil_data.rds"))
#  give NA when out of range of the original data
quarantine <- quarantine[year>1922,`:=`(BWpresence=NA,
                                        BWneighboringpresence=NA)]
quarantine <- quarantine[year<1892,`:=`(BWpresence=NA,
                                        BWneighboringpresence=NA)]
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
                   BWneighboringpresence_lag = shift(BWneighboringpresence,n=1L,fill=NA,type="lag")),
                     by=list(base_fips)]
data[, decade := (year %/% 10) * 10]


#### Save ------
fwrite(data, panel_output)



# balance test
to_keep <-c(cropacreage, "year", "base_fips")
temp <- data[, ..to_keep]
temp <-melt(temp,id.vars = c("year", "base_fips"))
temp <- temp[, .(value = sum(value, na.rm = T)), by = c("year", "variable")]
unique(temp[value > 0, year])
temp <-temp[value > 0]
temp[, .N, by = "year"]
}

build_panel(
  build_file("haines_census", "finaldataset_imputed_continuous.rds"),
  intermediate_file("panels", "datacrop.csv.gz")
)

build_panel(
  build_file("haines_census", "finaldataset_imputed_continuous_baselinexwalk.rds"),
  intermediate_file("panels", "datacrop_altxwalk.csv.gz")
)
      
