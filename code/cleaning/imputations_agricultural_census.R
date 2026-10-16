# ---- Imputation of the Cleaned Haines Data ---- #

# Method: only imputed for var-county-year observation, if there is a year in the past and a year in the future 
# in which this county had a non-NA value for that variable. 

#### 0. Load ----

rm(list=ls())
# set local work directory
packages<-c("data.table","raster","sf","parallel","lubridate","tidyr",
            "stringr","dplyr","rlang","foreign","ggplot2","readxl","tools",
            "zoo","spatstat","gridExtra","missForest","doParallel","rngtools")
lapply(packages, require, character.only = TRUE)
numCores <- detectCores() - 2
data <- readRDS("haines_agricultural/finaldataset_continuous.rds")

# 2 duplicated rows
data <- unique(data, y=c("base_fips","year"))

#### 1. Matrix of Imputation Status ----
cols <- grep(x=names(data),pattern="year|base_fips",invert=T,value=T)
col_lags <- unlist(mclapply(cols,FUN=function(x)paste0(x,"_lag"),mc.cores=numCores))
col_leads <- unlist(mclapply(cols,FUN=function(x)paste0(x,"_lead"),mc.cores=numCores))
col_dummy <- unlist(mclapply(cols,FUN=function(x)paste0(x,"_dummy"),mc.cores=numCores))
col_isna <- unlist(mclapply(cols,FUN=function(x)paste0(x,"_isna"),mc.cores=numCores))

# Compute the lags and harmonize to 1
data[ , (col_lags) := lapply(.SD, frollsum, 5, na.rm=T), by=base_fips, .SDcols = cols]
data[ , (col_lags) := lapply(.SD, function(x){ifelse((!is.na(x)&x!=0),0,1)}), by=base_fips, .SDcols = col_lags]

# Compute the leads and harmonize to 1
data[ , (col_leads) := lapply(.SD, frollsum, 5, align="left", na.rm=T), by=base_fips, .SDcols = cols]
data[ , (col_leads) := lapply(.SD, function(x){ifelse((!is.na(x)&x!=0),0,1)}), by=base_fips, .SDcols = col_leads]

# Dummy for current value equal to 0 or NA
data[ , (col_isna) := lapply(.SD, function(x){ifelse((!is.na(x)&x!=0),0,1)}), by=base_fips, .SDcols = cols]

# Compute the dummy
for(var in cols){
  var1 <- paste0(var,"_lead")
  var2 <- paste0(var,"_lag")
  var3 <- paste0(var,"_dummy")
  var4 <- paste0(var,"_isna")
  var5 <- paste0(var,"_imputed")
  # Max of lead and lag dummy - either condition needs to fail
  data <- data[,condition1:=max(na.rm=T,.SD),.SDcols=c(var1,var2),by=list(base_fips,year)]
  # RowSums for our two necessary conditions (either no lead or no lag, and current observation is NA or 0)
  data <- data[,(var3):=ifelse(rowSums(.SD,na.rm=TRUE)>=2,1,0),.SDcols=c("condition1",var4)]
  # We get the dummy for imputed we want to keep
  data <- data[,(var4):=-get(var4)]
  data <- data[,(var5):=rowSums(.SD,na.rm=TRUE),.SDcols=c(var3,var4)] # -1 if imputed/keep, 0 else
  data <- data[,(var5):=rowSums(.SD,na.rm=TRUE),.SDcols=c(var3,var5)] # -1 if imputed/keep, 0 if non-imputed, 1 if imputed/drop
  print(var)
}  

cols <- grep(names(data),pattern="lead|lag|_isna",value=T)
data <- data[,(cols):=NULL]
cols <- union(grep(names(data),pattern="dummy",value=T),c("base_fips","year"))
data_filter <- data[,cols,with=F]
cols <- union(grep(names(data),pattern="imputed",value=T),c("base_fips","year"))
data_type <- data[,cols,with=F]
cols <- union(grep(names(data),pattern="dummy|imputed",value=T),"condition1")
data <- data[,(cols):=NULL]

# Those who got NA for the dummy get a 0, then reverse the dummy values
cols <- grep(names(data_filter),pattern="dummy",value=T)
data_filter <- data_filter[,(cols):=lapply(.SD,function(x){ifelse(is.na(x),0,x)}),
                           .SDcols=cols]
data_filter <- data_filter[,(cols):=lapply(.SD,function(x){abs(x-1)}),.SDcols=cols]

#### 2. Impute for All ----

# Replace 0 values with NAs for imputation - KEEP DOING THIS
cols <- grep(x=names(data),pattern="year|base_fips",invert=T,value=T)
data2 <- data[,(cols):=lapply(.SD,function(x){ifelse(x==0,NA,x)}),.SDcols=cols]
data2 <- data[,(cols):=lapply(.SD,function(x){ifelse(is.nan(x),NA,x)}),.SDcols=cols]
data2 <- data2[,(cols):=lapply(.SD,function(x){ifelse(is.infinite(x),NA,x)}),.SDcols=cols]

# Move base_fips to numeric to run & remove those with no base_fips
data2 <- data2[!is.na(base_fips),]
data2 <- data2[,base_fips:=as.numeric(base_fips)]

# Run mice in parallel
doParallel::registerDoParallel(cores = 8) # set based on number of CPU cores
set.seed(123, kind = "L'Ecuyer-CMRG")
dataimputed <- missForest(xmis=data2, variablewise=T,
                          parallelize="variables", verbose=T)
data3 <- dataimputed$ximp

#### 3. Filter Imputations ----

# Merge data with filtering variables
data3 <- data3[,base_fips:=str_pad(as.character(base_fips),width=5,side="left",
                                   pad="0")]
data3 <- left_join(data3,data_filter,by=c("base_fips","year"))

cols <- grep(x=names(data3),pattern="year|base_fips|dummy",invert=T,value=T)
for(var in cols){
  
  var2 <- paste0(var,"_dummy")
  # The dummy is 0 when imputation needs to be dropped, 1 otherwise - so just take production of vars
  data3 <- data3[,(var):=Reduce(`*`, .SD),.SDcols=c(var,var2)]
  print(var)
  
}  


#### 4. Save ----
cols <- grep(names(data3),pattern="dummy",value=T)
data3 <- data3[,(cols):=NULL]

saveRDS(data3,"haines_agricultural/finaldataset_imputed_continuous.rds")
saveRDS(data_type,"haines_agricultural/data_filter_continuous.rds")

