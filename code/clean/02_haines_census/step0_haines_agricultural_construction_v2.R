####### ***   SET UP   *** #######

rm(list=ls())
source("clean/paths.R")
local({
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
packages<-c("kableExtra","dplyr","readxl","withr","stringr","haven","ggplot2",
            "stats","lubridate","gridExtra","readxl","expss","sf")
lapply(packages, require, character.only = TRUE)

## Load County Crosswalk - Ag. Based
# weight_agarea1 is for stock variables (fraction relative to origin county), weight_agarea2 for averages (fraction relative to end county)
crosswalk <- readRDS(build_file("crosswalk", "mycrosswalk_1997.rds"))
crosswalk <- subset(crosswalk, select=c(beginning_FIPS,base_FIPS,base_NAME,weight_agarea1,weight_agarea2,year))
crosswalk <- setDT(crosswalk)
crosswalk <- crosswalk[is.nan(weight_agarea1),weight_agarea1:=0]
crosswalk <- crosswalk[is.nan(weight_agarea2),weight_agarea2:=0]

setwd(raw_file("agricultural_census", "ICPSR_35206"))


## Aggregating Function 
myweighted_sum <- function(colname,dataframe,weightname){
  
  # set up
  dataframe <- get(dataframe)
  dataframe <- data.frame(dataframe)
  colname2 <- enquo(colname) ; weightname2 <- enquo(weightname)
  final_colname <- quo_name(colname)
  
  # operation
  sub_data <- dataframe %>% 
    dplyr::select(!!colname2,base_FIPS,year,!!weightname2) %>%
    group_by(base_FIPS,year)  %>% 
    summarize(!!final_colname:= sum(!!sym(colname)*!!sym(weightname),na.rm =TRUE))
  
  return(sub_data)
}


####### ***     Tree Acreage   *** #######

data_1950_part1 <- read_dta(file="DS0021/35206-0021-Data.dta")
data_1950_part2 <- read_dta(file="DS0022/35206-0022-Data.dta")
data_1950_part1 <- subset(data_1950_part1, select=c("STATE","COUNTY","STATEFIP","FIPS","LEVEL",
                                                    "VAR1","VAR8","VAR9","VAR10","VAR49",
                                                    "VAR233","VAR235","VAR237","VAR245","VAR247",
                                                    "VAR249","VAR251","VAR283","VAR285","VAR301",
                                                    "VAR304","VAR443","VAR448","VAR449","VAR452",
                                                    "VAR453","VAR484","VAR485","VAR509","VAR510",
                                                    "VAR526","VAR527","VAR533","VAR534","VAR543",
                                                    "VAR544","VAR548","VAR549","VAR557","VAR558",
                                                    "VAR574","VAR577","VAR844","VAR845","VAR850",
                                                    "VAR851","VAR866","VAR867","VAR890","VAR891",
                                                    "VAR895","VAR896","VAR920","VAR921","VAR945",
                                                    "VAR946","REGION1","REGION2",
                                                    "VAR659","VAR660","VAR651","VAR652","VAR733",
                                                    "VAR734","VAR772","VAR773","VAR777","VAR778",
                                                    "VAR803","VAR804","VAR819","VAR820","VAR463",
                                                    "VAR464","VAR637","VAR640","VAR1097","VAR1100",
                                                    "VAR1199","VAR1202","VAR1106","VAR1109","VAR1126",
                                                    "VAR1129","VAR1151","VAR1154","VAR1175","VAR1178",
                                                    "VAR1301","VAR1304","VAR1215","VAR1218","VAR1379",
                                                    "VAR1382","VAR1223","VAR1226","VAR1322","VAR1326",
                                                    "VAR1268","VAR1271","VAR1385","VAR1386","VAR1365",
                                                    "VAR1369","VAR742","VAR743","VAR880","VAR881",
                                                    "VAR1051","VAR1052","VAR1065","VAR1066","VAR1069",
                                                    "VAR1070","VAR1074","VAR1075","VAR1081","VAR1083",
                                                    "VAR538","VAR539","VAR871","VAR872","VAR861",
                                                    "VAR862","VAR760","VAR761","VAR1294","VAR1297",
                                                    "VAR978","VAR980","VAR986","VAR988","VAR990",
                                                    "VAR992","VAR994","VAR996","VAR998","VAR1000",
                                                    "VAR1002","VAR1006","VAR1008","VAR1010","VAR1014",
                                                    "VAR1018","VAR1020","VAR1022","VAR1026","VAR1028",
                                                    "VAR1030","VAR1032","VAR1034","VAR1016","VAR1038",
                                                    "VAR1040","VAR1042","VAR1044","VAR1048","VAR876",
                                                    "VAR877","VAR1096","VAR1198","VAR1105","VAR1125",
                                                    "VAR1150","VAR1174","VAR1300","VAR1214","VAR1222",
                                                    "VAR1321","VAR1364","VAR253","VAR243"))

colnames(data_1950_part1)[6:172] <- c("farms","totacres","avgfarmsize","avg_farmval","irrac",
                                      "combine_num","picker_num","baler_num","tract_num","tract_wheel",
                                      "tract_garden","tract_crawl","machine_hire_sub","farmwages","horses",
                                      "mules","cropval","cornac_grain","corn_grain","cornac_silage",
                                      "corn_silage","wheatac","wheat","oatsac","oats",
                                      "barleyac","barley","ryeac","rye","flaxac",
                                      "flax","riceac","rice","buckwheatac","buckwheat",
                                      "soybeanac","soybean","irpotatac","irpotato","swpotatac",
                                      "swpotato","cottonac","cotton","hempac","hemp",
                                      "tobaccoac","tobacco","canesugac","canesug","hopsac",
                                      "hops","REGION1","REGION2",
                                      "peasac","peas","beansac","beans","cloverac1",
                                      "clover1","cloverac2","clover2","cloverac3","clover3",
                                      "cloverac4","clover4","cloverac5","clover5","sorghumgac",
                                      "sorghumg","peanutac","peanuts","appltree","apples",
                                      "apritree","apricots","chertree","cherries","peactree",
                                      "peaches","peartree","pears","plumtree","plums",
                                      "almondtree","almonds","figtree","figs","limetree",
                                      "limes","olivetree","olives","orangetree","oranges",
                                      "pecantree","pecans","pineappleac","pineapples","pomelotree",
                                      "pomeloes","alfalfac","alfalfa","SBEETAC","SBEETWT",
                                      "BLACKBAC","BLACKBER","CURRANAC","CURRANTS","RASPBAC",  
                                      "RASPBER","STRAWBAC","STRAWBER","CRANBAC","CRANBER",
                                      "emmerspletac","emmersplet","mustardac","mustard","sunflowerac",
                                      "sunflower","vetchac","vetch","hazelnultac","hazelnults",
                                      "artichokeac","asparagusac","beetac","broccoliac","brusselac",
                                      "cabbageac","cantaloupac","carrotac","cauliflowerac", "celeryac",
                                      "collardac","cucumberac","eggplantac","escaroleac","kaleac",
                                      "okraac","oniondac","oniongac","pepperac1","pepperac2",
                                      "pumpkinac","radishac","rhubarbac","lettuceromaineac","spinachac",
                                      "squashac","tomatoac","turnipac","watermelonac","canolaac",
                                      "canola","appleac","apricotac","cherryac","peachac",
                                      "pearac","plumac","almondac","figac","oliveac",
                                      "orangeac","pomeloac","automobile","motortruck")

data_1950_part1 <- setDT(data_1950_part1)
data_1950_part1 <- data_1950_part1[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horses","mules")]
data_1950_part1 <- data_1950_part1[,farmval:=farms*avg_farmval]
data_1950_part1 <- data_1950_part1[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cloverac1","cloverac2",
                                                                                "cloverac3","cloverac4","cloverac5")]
data_1950_part1 <- data_1950_part1[,clover:=rowSums(.SD,na.rm=TRUE),.SDcols=c("clover1","clover2","clover3",
                                                                              "clover4","clover5")] 
data_1950_part1 <- data_1950_part1[,pepperac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pepperac1","pepperac2")] 

data_1950_part2 <- subset(data_1950_part2, select=c("STATEICP","COUNICP","STATEFIP","COUNFIP",
                                                    "ITEM848","ITEM801","ITEM951","ITEM952","ITEM958",
                                                    "ITEM962","ITEM963","ITEM964","ITEM965","ITEM967"))

colnames(data_1950_part2)[5:14] <- c("farmfert","fert_wt","chemac_spray","chemac_insects","chemac_weeds",
                                     "chemval_insects_hay","chemval_insects_ot","chemval_nematodes","chemval_diseases","chemval_weeds")
data_1950_part2 <- setDT(data_1950_part2)
data_1950_part2 <- data_1950_part2[,chemval_crop:=rowSums(.SD,na.rm=TRUE),.SDcols=c("chemval_insects_hay",
                                                                                    "chemval_insects_ot",
                                                                                    "chemval_nematodes",
                                                                                    "chemval_diseases",
                                                                                    "chemval_weeds")]
data_1950 <- left_join(data_1950_part1,data_1950_part2, by = c("STATE" = "STATEICP","COUNTY"="COUNICP"))
rm(data_1950_part1, data_1950_part2)
data_1950$year <- 1950

pecandensity <- 17.4
lemondensity <- 87
limedensity <- 87
pineappledensity <- 30000
appledensity <- data_1950[appleac>0&appltree>0,.(mean=mean(appltree/appleac,na.rm=T))][1]
apricotdensity <- data_1950[apricotac>0&apritree>0,.(mean=mean(apritree/apricotac,na.rm=T))][1]
cherrydensity <- data_1950[cherryac>0&chertree>0,.(mean=mean(chertree/cherryac,na.rm=T))][1]
peachdensity <- data_1950[peachac>0&peactree>0,.(mean=mean(peactree/peachac,na.rm=T))][1]
peardensity <- data_1950[pearac>0&peartree>0,.(mean=mean(peartree/pearac,na.rm=T))][1]
plumensity <- data_1950[plumac>0&plumtree>0,.(mean=mean(plumtree/plumac,na.rm=T))][1]
almonddensity <- data_1950[almondac>0&almondtree>0,.(mean=mean(almondtree/almondac,na.rm=T))][1]
figdensity <- data_1950[figac>0&figtree>0,.(mean=mean(figtree/figac,na.rm=T))][1]
olivedensity <- data_1950[oliveac>0&olivetree>0,.(mean=mean(olivetree/oliveac,na.rm=T))][1]
orangedensity <- data_1950[orangeac>0&orangetree>0,.(mean=mean(orangetree/orangeac,na.rm=T))][1]
pomelodensity <- data_1950[pomeloac>0&pomelotree>0,.(mean=mean(pomelotree/pomeloac,na.rm=T))][1]
hazelnutdensity <- 108
rm(data_1950)


####### ***     1840   *** #######

data_1840 <- read_dta(file="DS0002/35206-0002-Data.dta")
data_1840 <- subset(data_1840, select = c("STATE","COUNTY","WHEAT","BARLEY","OATS",
                                          "RYE","BUCKWHEA","CORN","RICE","HOPS", 
                                          "IRPOTATO","SWPOTATO","HAY","HEMP","FLAX",
                                          "TOBACCO","COTTON","CANSUG","HORSEMU","CROPO",
                                          "TOTAGOUT","LEVEL","FIPS","REGION1","REGION2"))
colnames(data_1840)[7] <- "BUCKWHEAT"
colnames(data_1840)[18] <- "CANESUG"
colnames(data_1840)[20] <- "cropval"
colnames(data_1840)[21] <- "totval"

# moving names to lower case
names(data_1840) <- tolower(names(data_1840))
# match to common geo identifier
data_1840 <- data_1840 %>% mutate(state=str_pad(as.character(state),2,pad="0",side="left"),
                                  county=str_pad(as.character(county),4,pad="0",side="left"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1840$year <- 1840

# Keep County-Level Observations
data_1840 <- data_1840[data_1840$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1840
crosswalkuse <- semi_join(crosswalk,data_1840,by=c("beginning_FIPS"="fips","year"="year"))
data_1840 <- left_join(crosswalkuse,data_1840,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1840 <- data_1840[which(data_1840$year==1840),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1840)[!names(data_1840)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                     "weight_agarea1","weight_agarea2","state","county","fips",
                                                     "level","region1","region2","year")])
data_1840 <- lapply(colstoavg, 
                    FUN=function(x) myweighted_sum(colname=x,dataframe="data_1840",weightname="weight_agarea1"))
data_1840 <- data_1840 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))
names(data_1840) <- tolower(names(data_1840))


####### ***     1850   *** #######


data_1850 <- read_dta(file="DS0003/35206-0003-Data.dta")
data_1850 <- subset(data_1850, select=c("STATE","COUNTY","FARMS","FARMVAL","HORSES",
                                        "MULES","EQUIPVAL","WHEAT","BARLEY","OATS",
                                        "RYE","BUCKWH","CORN","RICE","TOBACCO",
                                        "COTTON","IRPOTATO","SWPOTATO","HAY","HOPS",
                                        "FLAX","CANESUG","ACIMP","ACUNIMP","LEVEL",
                                        "AGOUT3","FIPS","REGION1","REGION2","HEMPDEW",
                                        "HEMPWAT","PEASBEAN","CLOVSEED","GRASSEED"))
colnames(data_1850)[12] <- "BUCKWHEAT"
colnames(data_1850)[26] <- "totval"
data_1850 <- setDT(data_1850)
data_1850 <- data_1850[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("HORSES","MULES")]
data_1850 <- data_1850[,totacres:=rowSums(.SD,na.rm=TRUE),.SDcols=c("ACIMP","ACUNIMP")]
data_1850 <- data_1850[,avgfarmsize:=totacres/FARMS]
data_1850 <- data_1850[,hemp:=rowSums(.SD,na.rm=TRUE),.SDcols=c("HEMPDEW","HEMPWAT")]

# Remove columns and change names to lower
colstoremove <- c("HORSES","MULES","ACIMP","ACUNIMP","HEMPDEW","HEMPWAT")
data_1850 <- data_1850[,(colstoremove):=NULL]
names(data_1850) <- tolower(names(data_1850))

# match to common geo identifier
data_1850 <- data_1850 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1850$year <- 1850

# Keep County-Level Observations
data_1850 <- data_1850[data_1850$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1850
crosswalkuse <- semi_join(crosswalk,data_1850,by=c("beginning_FIPS"="fips","year"="year"))
data_1850 <- left_join(crosswalkuse,data_1850,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1850 <- data_1850[which(data_1850$year==1850),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1850)[!names(data_1850)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1850_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1850",weightname="weight_agarea1"))
data_1850_part1 <- data_1850_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1850_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1850",weightname="weight_agarea2"))
data_1850_part2 <- data_1850_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# One dataset
data_1850 <- left_join(data_1850_part1,data_1850_part2,by=c("base_FIPS","year"))
rm(data_1850_part1,data_1850_part2)
names(data_1850) <- tolower(names(data_1850))


####### ***     1860   *** #######


data_1860 <- read_dta(file="DS0004/35206-0004-Data.dta")
data_1860 <- subset(data_1860, select=c("STATE","COUNTY","ACIMP","ACUNIMP","FARMVAL",
                                        "EQUIPVAL","HORSES","MULES","WHEAT","RYE",
                                        "CORN","OATS","RICE","TOBACCO","COTTON",
                                        "IRPOTATO","SWPOTATO","BARLEY","BUCKWH","HAY",
                                        "HOPS","HEMPDEW","HEMPWAT","HEMPOTH","FLAX",
                                        "CANESUG","AGOUT3","FARM39","FARM1019","FARM2049",
                                        "FARM5099","FARM100","FARM500","FARM1000","LEVEL",
                                        "REGION1","REGION2","FIPS","PEASBEAN","CLOVSEED",
                                        "GRASSEED"))
colnames(data_1860)[19] <- "BUCKWHEAT"
colnames(data_1860)[27] <- "totval"
data_1860 <- setDT(data_1860)
data_1860 <- data_1860[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("HORSES","MULES")]
data_1860 <- data_1860[,totacres:=rowSums(.SD,na.rm=TRUE),.SDcols=c("ACIMP","ACUNIMP")]
data_1860 <- data_1860[,farms:=rowSums(.SD,na.rm=TRUE),.SDcols=c("FARM39","FARM1019","FARM2049","FARM5099","FARM100","FARM500","FARM1000")]
data_1860 <- data_1860[,hemp:=rowSums(.SD,na.rm=TRUE),.SDcols=c("HEMPDEW","HEMPWAT","HEMPOTH")]
data_1860 <- data_1860[,avgfarmsize:=totacres/farms]

# Remove columns and change names to lower
colstoremove <- c("HEMPDEW","HEMPWAT","HEMPOTH","FARM39","FARM1019","FARM2049","FARM5099",
                  "FARM100","FARM500","ACIMP","ACUNIMP","FARM1000","HORSES","MULES")
data_1860 <- data_1860[,(colstoremove):=NULL]
names(data_1860) <- tolower(names(data_1860))

# match to common geo identifier
data_1860 <- data_1860 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1860$year <- 1860

# Keep County-Level Observations
data_1860 <- data_1860[data_1860$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1860
crosswalkuse <- semi_join(crosswalk,data_1860,by=c("beginning_FIPS"="fips","year"="year"))
data_1860 <- left_join(crosswalkuse,data_1860,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1860 <- data_1860[which(data_1860$year==1860),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1860)[!names(data_1860)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1860_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1860",weightname="weight_agarea1"))
data_1860_part1 <- data_1860_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1860_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1860",weightname="weight_agarea2"))
data_1860_part2 <- data_1860_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1860 <- left_join(data_1860_part1,data_1860_part2,by=c("base_FIPS","year"))
rm(data_1860_part1,data_1860_part2)
names(data_1860) <- tolower(names(data_1860))


####### ***     1870   *** #######


# farmlab is "total annual agricultural wages paid, including value of board"

data_1870 <- read_dta(file="DS0005/35206-0005-Data.dta")
data_1870 <- subset(data_1870, select=c("STATE","COUNTY","ACIMP","ACUNIOTH","FARMVAL",
                                        "EQUIPVAL","FARMLAB","FARMOUT","HORSES","MULES",
                                        "SWHEAT","WWHEAT","RYE","CORN","OATS", 
                                        "BARLEY","BUCKWH","RICE","TOBACCO","COTTON",
                                        "IRPOTATO","SWPOTATO","HAY","HOPS","HEMP",
                                        "FLAXSEED","SUGARC","FARMS","REGION1","REGION2", 
                                        "FIPS","LEVEL","ACWOODS","PEASBEAN","CLOVSEED",
                                        "GRASSEED"))
colnames(data_1870)[17] <- "BUCKWHEAT"
colnames(data_1870)[8] <- "totval"
colnames(data_1870)[26] <- "flax" # flax seeds
data_1870 <- setDT(data_1870)
data_1870 <- data_1870[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("HORSES","MULES")]
data_1870 <- data_1870[,totacres:=rowSums(.SD,na.rm=TRUE),.SDcols=c("ACIMP","ACUNIOTH","ACWOODS")]
data_1870 <- data_1870[,wheat:=rowSums(.SD,na.rm=TRUE),.SDcols=c("SWHEAT","WWHEAT")]
data_1870 <- data_1870[,canesug:=rowSums(.SD,na.rm=TRUE),.SDcols=c("SUGARC")]
data_1870 <- data_1870[,avgfarmsize:=totacres/FARMS]

# Remove columns and change names to lower
colstoremove <- c("HORSES","MULES","ACIMP","ACUNIOTH","ACWOODS","SWHEAT","WWHEAT","SUGARC")
data_1870 <- data_1870[,(colstoremove):=NULL]
names(data_1870) <- tolower(names(data_1870))

# gen geo identifier
data_1870 <- data_1870 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1870$year <- 1870

# Keep County-Level Observations
data_1870 <- data_1870[data_1870$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1870
crosswalkuse <- semi_join(crosswalk,data_1870,by=c("beginning_FIPS"="fips","year"="year"))
data_1870 <- left_join(crosswalkuse,data_1870,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1870 <- data_1870[which(data_1870$year==1870),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1870)[!names(data_1870)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1870_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1870",weightname="weight_agarea1"))
data_1870_part1 <- data_1870_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1870_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1870",weightname="weight_agarea2"))
data_1870_part2 <- data_1870_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1870 <- left_join(data_1870_part1,data_1870_part2,by=c("base_FIPS","year"))
rm(data_1870_part1,data_1870_part2)
names(data_1870) <- tolower(names(data_1870))


####### ***     1880   *** #######


# farms and farms2 have the same values

data_1880 <- read_dta(file="DS0006/35206-0006-Data.dta")
data_1880 <- subset(data_1880, select=c("STATE","COUNTY","FARMS","FARMSIZE","ACRES",
                                        "FARMVAL","EQUIPVAL","FARMFERT","FARMOUT","HORSES",
                                        "MULES","BARLEY","BARLEYAC","BUCKWHEA","BUCKWHAC",
                                        "CORNAC","CORN","OATS","OATSAC","RYE",
                                        "RYEAC","WHEAT","WHEATAC","COTTON","COTTONAC",
                                        "FLAXSEED","HEMP","CSUGARAC","CSUGAR","HAY",
                                        "HAYAC","RICE","RICEAC","IPOTATAC","IPOTATO",
                                        "SPOTATO","SPOTATAC","HOPSAC","HOPS","REGION1",
                                        "REGION2","FIPS","LEVEL","TOBACAC","TOBACCO",
                                        "CANAPEAS","PULBEANS","CLOVSEED","GRASSEED"))
colnames(data_1880)[4] <- "avgfarmsize"
colnames(data_1880)[5] <- "totacres"
colnames(data_1880)[9] <- "totval"
colnames(data_1880)[14] <- "buckwheat"
colnames(data_1880)[15] <- "buckwheatac"
colnames(data_1880)[26] <- "flax" # flax seeds
colnames(data_1880)[28] <- "CANESUGAC"
colnames(data_1880)[29] <- "CANESUG"
colnames(data_1880)[36] <- "SWPOTATO"
colnames(data_1880)[37] <- "SWPOTATAC"
colnames(data_1880)[44] <- "TOBACCOAC"
colnames(data_1880)[35] <- "IRPOTATO"
colnames(data_1880)[34] <- "IRPOTATAC"
colnames(data_1880)[46] <- "peas"
colnames(data_1880)[47] <- "beans"
data_1880 <- setDT(data_1880)
data_1880 <- data_1880[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("HORSES","MULES")]
data_1880 <- data_1880[,peasbean:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peas","beans")]

# Remove columns and change names to lower
colstoremove <- c("HORSES","MULES","peas","beans")
data_1880 <- data_1880[,(colstoremove):=NULL]
names(data_1880) <- tolower(names(data_1880))

# match to common geo identifier
data_1880 <- data_1880 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1880$year <- 1880

# Keep County-Level Observations
data_1880 <- data_1880[data_1880$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1880
crosswalkuse <- semi_join(crosswalk,data_1880,by=c("beginning_FIPS"="fips","year"="year"))
data_1880 <- left_join(crosswalkuse,data_1880,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1880 <- data_1880[which(data_1880$year==1880),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1880)[!names(data_1880)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1880_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1880",weightname="weight_agarea1"))
data_1880_part1 <- data_1880_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1880_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1880",weightname="weight_agarea2"))
data_1880_part2 <- data_1880_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1880 <- left_join(data_1880_part1,data_1880_part2,by=c("base_FIPS","year"))
rm(data_1880_part1,data_1880_part2)
names(data_1880) <- tolower(names(data_1880))


####### ***     1890   *** #######


# 2 farm variables (farms and farms2) have identicaal values

data_1890 <- read_dta(file="DS0007/35206-0007-Data.dta")
data_1890 <- subset(data_1890, select=c("STATE","COUNTY","FARMS","FARMSIZE","ACIMP",
                                        "ACUNIMP","FARMVAL","EQUIPVAL","FARMOUT","FARMFERT",
                                        "HORSES","MULES","ASSES","BARLEYAC","BARLEY",
                                        "BUCKWHAC","BUCKWHEA","CORNAC","CORN","OATSAC",
                                        "OATS","RYE","RYEAC","WHEAT","WHEATAC",
                                        "COTTON","COTTONAC","FLAXAC","FLAXSEED","HEMPAC",
                                        'HEMP',"CSUGARAC","CSUGAR","HAYAC","HAY",
                                        "RICEAC","RICE","TOBACCO","TOBACAC","IPOTATAC",
                                        "IPOTATO","SPOTATAC","SPOTATO","HOPSAC","HOPS",
                                        "CANAPEAS","COWPEAS","BEANSDRY","CLOVSEED","GRASSEED",
                                        "PEANUTAC","PEANUTS","APPLES","APPLTREE","APRICOTS",
                                        "APRITREE","CHERRIES","CHERTREE","PEACHES","PEACTREE",
                                        "PEARS","PEARTREE","PLUMPRUN","PLUMTREE","ALMONDAC",
                                        "BANANAAC","CITRONAC","FIGAC","GUAVAAC","KAKIAC",
                                        "LEMONAC","LIMAAC","OLIVEAC","ORANGEAC","PECANAC",
                                        "PINEAPAC","POMELOAC","GRAPETAB","ALFALFAC","REGION1",
                                        "REGION2","FIPS","LEVEL","SSUGARAC","SSUGAR"))
colnames(data_1890)[4] <- "avgfarmsize"
colnames(data_1890)[9] <- "totval"
colnames(data_1890)[17] <- "BUCKWHEAT"
colnames(data_1890)[16] <- "BUCKWHEATAC"
colnames(data_1890)[29] <- "flax" # bushels of flax seed
colnames(data_1890)[32] <- "CANESUGAC"
colnames(data_1890)[33] <- "CANESUG"
colnames(data_1890)[39] <- "TOBACCOAC"
colnames(data_1890)[41] <- "IRPOTATO"
colnames(data_1890)[40] <- "IRPOTATAC"
colnames(data_1890)[42] <- "SWPOTATAC"
colnames(data_1890)[43] <- "SWPOTATO"
colnames(data_1890)[48] <- "beans"
colnames(data_1890)[84] <- "SORGHAC"
colnames(data_1890)[85] <- "SORGHUM"

data_1890 <- setDT(data_1890)
setnames(data_1890, "PLUMPRUN", "plums")
setnames(data_1890, "PINEAPAC", "pineappleac")
data_1890 <- data_1890[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("HORSES","MULES","ASSES")]
data_1890 <- data_1890[,totacres:=rowSums(.SD,na.rm=TRUE),.SDcols=c("ACIMP","ACUNIMP")]
data_1890 <- data_1890[,peasbean:=rowSums(.SD,na.rm=TRUE),.SDcols=c("CANAPEAS","beans","COWPEAS")]
data_1890 <- data_1890[,peas:=rowSums(.SD,na.rm=TRUE),.SDcols=c("CANAPEAS","COWPEAS")]

# Remove columns and change names to lower
colstoremove <- c("HORSES","MULES","ASSES","ACIMP","ACUNIMP",
                  "CANAPEAS","beans","COWPEAS")
data_1890 <- data_1890[,(colstoremove):=NULL]
names(data_1890) <- tolower(names(data_1890))

# match to common geo identifier
data_1890 <- data_1890 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1890$year <- 1890

# Keep County-Level Observations
data_1890 <- data_1890[data_1890$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1890
crosswalkuse <- semi_join(crosswalk,data_1890,by=c("beginning_FIPS"="fips","year"="year"))
data_1890 <- left_join(crosswalkuse,data_1890,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1890 <- data_1890[which(data_1890$year==1890),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1890)[!names(data_1890)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1890_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1890",weightname="weight_agarea1"))
data_1890_part1 <- data_1890_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1890_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1890",weightname="weight_agarea2"))
data_1890_part2 <- data_1890_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1890 <- left_join(data_1890_part1,data_1890_part2,by=c("base_FIPS","year"))
rm(data_1890_part1,data_1890_part2)
names(data_1890) <- tolower(names(data_1890))


####### ***     1900   *** #######


# farmlab -> average expenditure for farm labor (mean of same order of magnitude as 1870 var)
data_1900 <- read_dta(file="DS0008/35206-0008-Data.dta")
data_1900 <- subset(data_1900, select=c("STATE","COUNTY","FARMS","FARMSIZE","ACFARM",
                                        "TOTVAL","FARMEQUI","FARMOUT","FARMLAB","FARMFERT",
                                        "HORSES20","MULES0","MULES1_2","MULES20","BARLEYAC",
                                        "BARLEY","BUCKWHAC","BUCKWHEA","CORNAC","CORN",
                                        "OATSAC","OATS","RYEAC","RYE","WHEATAC",
                                        "WHEAT","RICEAC","RICE","HAYAC","HAY",
                                        "POTATOAC","POTATOES","SWPOTAC","SWPOTATO","HEMPAC",
                                        "HEMPWT","FLAXSDAC","FLAXSEED","CSUGARWT","SCANEAC",
                                        "TOBACAC","TOBACCO","HOPS","HOPSAC","IRRAC", 
                                        "LEVEL","REGION1","REGION2","FIPS","COTTONAC",
                                        "SCOTAC","COTBALE1","COTBALE2","SCOTBAL1","SCOTBAL2",
                                        "DONKEYS","FARMBUI","BALEWT","SCBALEWT","PEASAC",
                                        "PEAS","BEANSAC","BEANS","CLOVERAC","CLOVSEED",
                                        "PEANUTAC","PEANUTS","APPLET","APPLES","APRICOTT",
                                        "APRICOTS","CHERRYT","CHERRIES","PEACHT","PEACHES",
                                        "PEART","PEARS","PLUMT","PLUMS","ALMONDT",
                                        "ALMONDS","BANANAT","BANANAS","FIGTREES","FIGS",
                                        "GUAVAT","GUAVAS","LEMONT","LEMONS","LIMET",
                                        "LIMES","OLIVET","OLIVES","ORANGET","ORANGES",
                                        "PECANT","PECANS","PINEAPPT","PINEAPPL","POMELOT",
                                        "POMELOES","GRAPES","ALFALFAC","ALFALFA","SBEETAC",
                                        "SBEETWT","BLACKBAC","BLACKBER","CURRANAC","CURRANTS",
                                        "RASPBAC","RASPBER","STRAWBAC","STRAWBER","CRANBAC",
                                        "CRABER","SORGCAAC","SORGCAWT"))
# Horses not are only counted if > 2yo
colnames(data_1900)[4] <- "avgfarmsize"
colnames(data_1900)[5] <- "totacres"
colnames(data_1900)[6] <- "farmval" 
colnames(data_1900)[7] <- "equipval"
colnames(data_1900)[8] <- "totval"
colnames(data_1900)[18] <- "buckwheat"
colnames(data_1900)[17] <- "buckwheatac"
colnames(data_1900)[32] <- "irpotato"
colnames(data_1900)[31] <- "irpotatac"
colnames(data_1900)[34] <- "swpotato"
colnames(data_1900)[33] <- "swpotatac"
colnames(data_1900)[37] <- "flaxac"
colnames(data_1900)[38] <- "flax"
colnames(data_1900)[39] <- "canesug"
colnames(data_1900)[40] <- "canesugac"
colnames(data_1900)[41] <- "tobaccoac"
colnames(data_1900)[36] <- "hemp"
colnames(data_1900)[68] <- "APPLTREE"
colnames(data_1900)[70] <- "APRITREE"
colnames(data_1900)[72] <- "CHERTREE"
colnames(data_1900)[74] <- "PEACTREE"
colnames(data_1900)[76] <- "PEARTREE"
colnames(data_1900)[78] <- "PLUMTREE"
colnames(data_1900)[80] <- "ALMONDTREE"
colnames(data_1900)[82] <- "BANANATREE"
colnames(data_1900)[84] <- "FIGTREE"
colnames(data_1900)[86] <- "GUAVATREE"
colnames(data_1900)[88] <- "LEMONTREE"
colnames(data_1900)[90] <- "LIMETREE"
colnames(data_1900)[92] <- "OLIVETREE"
colnames(data_1900)[94] <- "ORANGETREE"
colnames(data_1900)[96] <- "PECANTREE"
colnames(data_1900)[98] <- "PINEAPPLETREE"
colnames(data_1900)[99] <- "PINEAPPLES"
colnames(data_1900)[100] <- "POMELOTREE"
colnames(data_1900)[101] <- "POMELOES"
colnames(data_1900)[116] <- "CRANBER"
colnames(data_1900)[117] <- "SORGHAC"
colnames(data_1900)[118] <- "SORGHUM"

data_1900 <- setDT(data_1900)
data_1900 <- data_1900[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("HORSES20","MULES0","MULES1_2","MULES20","DONKEYS")]
data_1900 <- data_1900[,COTTONAC:=rowSums(.SD,na.rm=TRUE),.SDcols=c("SCOTAC","COTTONAC")]
data_1900 <- data_1900[,`:=`(mFARMBUI=-FARMBUI,
                             mequipval=-equipval)]
data_1900 <- data_1900[,farmval:=rowSums(.SD,na.rm=TRUE),.SDcols=c("farmval","mFARMBUI","mequipval")]
data_1900 <- data_1900[,`:=`(COTBALE1=BALEWT*COTBALE1,
                             COTBALE2=COTBALE2*500,
                             SCBALEWT=SCBALEWT*SCOTBAL1,
                             SCOTBAL2=SCOTBAL2*500)]
data_1900 <- data_1900[,cotton:=rowSums(.SD,na.rm=TRUE),.SDcols=c("COTBALE1","COTBALE2","SCBALEWT","SCOTBAL2")]

# Remove columns and change names to lower
colstoremove <- c("HORSES20","MULES0","MULES1_2","MULES20","DONKEYS",
                  "SCOTAC","COTTONAC","mFARMBUI","mequipval",
                  "COTBALE1","COTBALE2","SCBALEWT","SCOTBAL2","FARMBUI","BALEWT")
data_1900 <- data_1900[,(colstoremove):=NULL]
names(data_1900) <- tolower(names(data_1900))

# match to common geo identifier
data_1900 <- data_1900 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1900$year <- 1900

# Keep County-Level Observations
data_1900 <- data_1900[data_1900$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1900
crosswalkuse <- semi_join(crosswalk,data_1900,by=c("beginning_FIPS"="fips","year"="year"))
data_1900 <- left_join(crosswalkuse,data_1900,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1900 <- data_1900[which(data_1900$year==1900),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1900)[!names(data_1900)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1900_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1900",weightname="weight_agarea1"))
data_1900_part1 <- data_1900_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1900_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1900",weightname="weight_agarea2"))
data_1900_part2 <- data_1900_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1900 <- left_join(data_1900_part1,data_1900_part2,by=c("base_FIPS","year"))
rm(data_1900_part1,data_1900_part2)
names(data_1900) <- tolower(names(data_1900))


####### ***     1910   *** #######


# farmwages -> cash expended for labor
# farebord -> rent & boad furnished for labor

data_1910 <- read_dta(file="DS0009/35206-0009-Data.dta")
data_1910 <- subset(data_1910, select=c("STATE","COUNTY","FARMS","FARMLAND","TOTVAL",
                                        "FARMEQUI","HORSES","MULES","CROPVAL","CORNAC",
                                        "CORN","OATSAC","OATS","WHEATAC","WHEAT",
                                        "BARLEYAC","BARLEY","BUCKWHAC","BUCKWHEA","RYEAC",
                                        "RYE","RICEAC","RICE","FLAXAC","FLAX",
                                        "POTATOAC","POTATOES","TOBACAC","TOBACCO","HOPS",
                                        "HOPSAC","COTTONAC","COTTON","CSUGARAC","CSUGAR",
                                        "HEMP","HEMPAC","IRRACRES","LEVEL","FIPS",
                                        "STATEFIP","REGION1","REGION2","FERTEXP","FAWAGES",
                                        "FAREBORD","ASSES","PEASAC","PEAS","BEANSAC",
                                        "BEANS","CLOVERAC","CLOVER","SORGHAC","SORGHUM", # sorghum is tons of cane sold
                                        "PEANUTAC","PEANUTS","APPLET","APPLES","APRICOTT",
                                        "APRICOTS","CHERRYT","CHERRIES","PEACHNET","PEACHNEC",
                                        "PEART","PEARS","PLUMT","PLUMS","ALMONDTR",
                                        "ALMONDS","BANANATR","BANANA","FIGTR","FIGS",
                                        "LEMONTR","LEMONS","OLIVETR","OLIVES","ORANGETR",
                                        "ORANGES","PINEAPPLEPL","PINEAPPLES","POMELOTR","POMELOES",
                                        "GRAPES","ALFALFAC","ALFALFA","SUBEETAC","SUGBEETS",
                                        "BLACKBER","BLACKBAC","CURRAC","CURRANTS","RASPBAC",
                                        "RASPBER","STRAWBAC","STRAWBER","CRANBAC","CRANBER",
                                        "PECANTR","PECANS"))

colnames(data_1910)[4] <- "totacres"
colnames(data_1910)[5] <- "farmval"
colnames(data_1910)[6] <- "EQUIPVAL"
colnames(data_1910)[18] <- "BUCKWHEATAC"
colnames(data_1910)[19] <- "BUCKWHEAT"
colnames(data_1910)[26] <- "IRPOTATAC"
colnames(data_1910)[27] <- "IRPOTATO"
colnames(data_1910)[28] <- "TOBACCOAC"
colnames(data_1910)[34] <- "CANESUGAC"
colnames(data_1910)[35] <- "CANESUG"
colnames(data_1910)[38] <- "IRRAC"
colnames(data_1910)[44] <- "FARMFERT"
colnames(data_1910)[45] <- "farmwages"
colnames(data_1910)[46] <- "farm_rentboard"
colnames(data_1910)[58] <- "APPLTREE"
colnames(data_1910)[60] <- "APRITREE"
colnames(data_1910)[62] <- "CHERTREE"
colnames(data_1910)[64] <- "PEACTREE"
colnames(data_1910)[65] <- "PEACHES"
colnames(data_1910)[66] <- "PEARTREE"
colnames(data_1910)[68] <- "PLUMTREE"
colnames(data_1910)[70] <- "ALMONDTREE"
colnames(data_1910)[72] <- "BANANATREE"
colnames(data_1910)[73] <- "BANANAS"
colnames(data_1910)[74] <- "FIGTREE"
colnames(data_1910)[76] <- "LEMONTREE"
colnames(data_1910)[78] <- "OLIVETREE"
colnames(data_1910)[80] <- "ORANGETREE"
colnames(data_1910)[82] <- "PINEAPPLETREE"
colnames(data_1910)[84] <- "POMELOTREE"
colnames(data_1910)[89] <- "SBEETAC"
colnames(data_1910)[90] <- "SBEETWT"
colnames(data_1910)[93] <- "CURRANAC"
colnames(data_1910)[101] <- "PECANTREE"

data_1910 <- setDT(data_1910)
data_1910 <- data_1910[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("HORSES","MULES","ASSES")]
data_1910 <- data_1910[,farmlab:=rowSums(.SD,na.rm=TRUE),.SDcols=c("farmwages","farm_rentboard")]
data_1910 <- data_1910[,avgfarmsize:=totacres/FARMS] # based on 1920, these two variables sum up to make total labor expenses

# Remove columns and change names to lower
colstoremove <- c("HORSES","MULES","ASSES","farmwages","farm_rentboard")
data_1910 <- data_1910[,(colstoremove):=NULL]
names(data_1910) <- tolower(names(data_1910))

# match to common geo identifier
data_1910 <- data_1910 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1910$year <- 1910

# Keep County-Level Observations
data_1910 <- data_1910[data_1910$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1910
crosswalkuse <- semi_join(crosswalk,data_1910,by=c("beginning_FIPS"="fips","year"="year"))
data_1910 <- left_join(crosswalkuse,data_1910,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1910 <- data_1910[which(data_1910$year==1910),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1910)[!names(data_1910)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1910_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1910",weightname="weight_agarea1"))
data_1910_part1 <- data_1910_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1910_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1910",weightname="weight_agarea2"))
data_1910_part2 <- data_1910_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1910 <- left_join(data_1910_part1,data_1910_part2,by=c("base_FIPS","year"))
rm(data_1910_part1,data_1910_part2)
names(data_1910) <- tolower(names(data_1910))


####### ***     1920   *** #######


# here hay is hay + forage
# labor: we have total expenditure for labor (308), cash expended for labor (309), rent and board furnished for labor (310)
# 308 is a sum of 309 and 310

data_1920 <- read_dta(file="DS0010/35206-0010-Data.dta")
data_1920 <- subset(data_1920, select=c("STATE","COUNTY","FIPS","STATEFIP","LEVEL",
                                        "VAR1","VAR18","VAR22","VAR25","VAR56",
                                        "VAR63","VAR138","VAR147","VAR148","VAR149",
                                        "VAR150","VAR151","VAR152","VAR153","VAR154",
                                        "VAR155","VAR156","VAR157","VAR158","VAR165",
                                        "VAR166", "VAR173","VAR174","VAR177","VAR178",
                                        "VAR205","VAR206","VAR207","VAR208","VAR214",
                                        "VAR215","VAR216","VAR217","VAR221","VAR222",
                                        "VAR227","VAR228","VAR308","VAR312","VAR445",
                                        "VAR310","VAR309","VAR68","VAR167","VAR168",
                                        "VAR171","VAR172","VAR169","VAR170","VAR185",
                                        "VAR186","VAR224","VAR225","VAR175","VAR176",
                                        "VAR248","VAR249","VAR250","VAR263","VAR264",
                                        "VAR265","VAR260","VAR261","VAR262","VAR251",
                                        "VAR252","VAR253","VAR254","VAR255","VAR256",
                                        "VAR257","VAR258","VAR259","VAR296","VAR297",
                                        "VAR298","VAR269","VAR270","VAR271","VAR275",
                                        "VAR276","VAR277","VAR273","VAR274","VAR275",
                                        "VAR284","VAR285","VAR286","VAR290","VAR291",
                                        "VAR292","VAR278","VAR279","VAR280","VAR187",
                                        "VAR188","VAR212","VAR213","VAR235","VAR236",
                                        "VAR239","VAR240","VAR233","VAR234","VAR231",
                                        "VAR232","VAR237","VAR238"))

colnames(data_1920)[6:113] <- c("farms","totacres","farmval","equipval","horses",
                               "mules","cropval","cornac","corn","oatsac",
                               "oats","wheatac","wheat","barleyac","barley",
                               "ryeac","rye","buckwheatac","buckwheat","riceac",
                               "rice","flaxac","flax","hayac","hay",
                               "irpotatac","irpotato","swpotatac","swpotato","tobaccoac",
                               "tobacco","cottonac","cotton","canesugac","canesug",
                               "hopsac","hops","farmlab","farmfert","irrac",
                               "farm_rentboard","farmwages","asses","soybeanac_bean","soybean_bean",
                               "peasac","peas","beansac","beans","cloverac",
                               "clover","sorghac","sorghum","peanutac","peanuts",
                               "applet1","applet2","apples","apricots1","apricots2",
                               "apricots","cherrit1","cherrit2","cherries","peacht1",
                               "peacht2","peaches","peart1","peart2","pears",
                               "plumt1","plumt2","plums","almondt1","almondt2",
                               "almonds","figt1","figt2","figs","lemont1",
                               "lemont2","lemons","oranget1","oranget2","oranges",
                               "pineapple1","pineapple2","pineapples","pecant1","pecant2",
                               "pecans","pomelot1","pomelot2","pomeloes","ALFALFAC",
                               "ALFALFA","SBEETAC","SBEETWT","BLACKBER","BLACKBAC",
                               "CURRANAC","CURRANTS","RASPBAC","RASPBER","STRAWBAC",
                               "STRAWBER","CRANBAC","CRANBER")

data_1920 <- setDT(data_1920)
names(data_1920) <- tolower(names(data_1920))
data_1920 <- data_1920[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horses","mules","asses")]
data_1920 <- data_1920[,avgfarmsize:=totacres/farms] 
data_1920 <- data_1920[,APPLTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("applet1","applet2")]
data_1920 <- data_1920[,APRITREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("apricots1","apricots2")]
data_1920 <- data_1920[,CHERTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cherrit1","cherrit2")]
data_1920 <- data_1920[,PEACTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peacht1","peacht2")]
data_1920 <- data_1920[,PEARTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peart1","peart2")]
data_1920 <- data_1920[,PLUMTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("plumt1","plumt2")]
data_1920 <- data_1920[,ALMONDTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("almondt1","almondt2")]
data_1920 <- data_1920[,FIGTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("figt1","figt2")]
data_1920 <- data_1920[,LEMONTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("lemont1","lemont2")]
data_1920 <- data_1920[,ORANGETREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("oranget1","oranget2")]
data_1920 <- data_1920[,PINEAPPLETREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pineapple1","pineapple2")]
data_1920 <- data_1920[,PECANTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pecant1","pecant2")]
data_1920 <- data_1920[,POMELOTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pomelot1","pomelot2")]
data_1920 <- data_1920[,oats:=as.numeric(oats)]

# Remove columns 
colstoremove <- c("horses","mules","asses","applet1","applet2","apricots1","apricots2",
                  "cherrit1","cherrit2","peacht1","peacht2","peart1","peart2",
                  "plumt1","plumt2","almondt1","almondt2","figt1","figt2",
                  "lemont1","lemont2","oranget1","oranget2","pineapple1","pineapple2",
                  "pecant1","pecant2","pomelot1","pomelot2")
data_1920 <- data_1920[,(colstoremove):=NULL]

# match to common geo identifier
data_1920 <- data_1920 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1920$year <- 1920

# Keep County-Level Observations
data_1920 <- data_1920[data_1920$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1920
crosswalkuse <- semi_join(crosswalk,data_1920,by=c("beginning_FIPS"="fips","year"="year"))
data_1920 <- left_join(crosswalkuse,data_1920,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1920 <- data_1920[which(data_1920$year==1920),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1920)[!names(data_1920)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1920_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1920",weightname="weight_agarea1"))
data_1920_part1 <- data_1920_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1920_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1920",weightname="weight_agarea2"))
data_1920_part2 <- data_1920_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1920 <- left_join(data_1920_part1,data_1920_part2,by=c("base_FIPS","year"))
rm(data_1920_part1,data_1920_part2)
names(data_1920) <- tolower(names(data_1920))


####### ***     1925   *** #######


# farmval def here as "value of farm: all farm property"
# wages are money wages only - no report for board + rent
# total # of horses, whatever the age
# oats is oats threshed for grain - can't get total threshed & fed for bushels (only for acres - var268-270)
# corn is corn harvested for grain (not for silage or fodder)
# var 2 and 14 - both farm # - have the same values
# var38 (totacres) empty - so we use var72

data_1925 <- read_dta(file="DS0012/35206-0012-Data.dta")
data_1925 <- subset(data_1925, select=c("STATE","COUNTY","STATEFIP","FIPS","LEVEL",
                                        "VAR2","VAR72","VAR74","VAR126","VAR129",
                                        "VAR180","VAR182","VAR188","VAR199","VAR203",
                                        "VAR266","VAR267","VAR268","VAR269","VAR271",
                                        "VAR272","VAR273","VAR274","VAR275","VAR276",
                                        "VAR277","VAR278","VAR279","VAR280","VAR288",
                                        "VAR298","VAR299","VAR300","VAR303","VAR304",
                                        "VAR306","VAR307","VAR308","VAR309","VAR310",
                                        "VAR311","VAR336","VAR259","VAR261","VAR262",
                                        "VAR263","VAR257","REGION1","REGION2","VAR291",
                                        "VAR292","VAR281","VAR282","VAR284","VAR285",
                                        "VAR320","VAR321","VAR322","VAR323","VAR324",
                                        "VAR325","VAR326","VAR330","VAR331","VAR328",
                                        "VAR329","VAR334","VAR335","VAR332","VAR333",
                                        "VAR301","VAR302","VAR312","VAR313","VAR314",
                                        "VAR315","VAR316","VAR318","VAR319"))

colnames(data_1925)[6:79] <- c("farms","totacres","avgfarmsize","farmval","equipval",
                               "farmfert","farmwages","tract_num","horse","mule",
                               "wheatac","wheat","oats","oatsac","barleyac",
                               "barley","ryeac","rye","flaxac","flax",
                               "buckwheatac","buckwheat","riceac","rice","hayac",
                               "hay","cottonac","cotton","canesugac","canesug",
                               "tobaccoac","tobacco","irpotatac","irpotato","swpotatac",
                               "swpotato","farmpop","cornac_grain","corn_grain","cornac_silage",
                               "corn_silage","cropval","REGION1","REGION2","clover1",
                               "clover2","sorghumgac","sorghumg","peanutac","peanuts",
                               "applet1","applet2","apples","PEACTREE","PEACHES",
                               "PEARTREE","PEARS","lemont1","lemont2","oranget1",
                               "oranget2","pecant1","pecant2","pomelot1","pomelot2",
                               "SBEETAC","SBEETWT","STRAWBAC","cabbageac","cantaloupac",
                               "lettuceac","oniondac","tomatoac","watermelonac")

data_1925 <- setDT(data_1925)
data_1925 <- data_1925[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horse","mule")]
data_1925 <- data_1925[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("clover1","clover2")]
data_1925 <- data_1925[,APPLTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("applet1","applet2")]
data_1925 <- data_1925[,LEMONTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("lemont1","lemont2")]
data_1925 <- data_1925[,ORANGETREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("oranget1","oranget2")]
data_1925 <- data_1925[,PECANTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pecant1","pecant2")]
data_1925 <- data_1925[,POMELOTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pomelot1","pomelot2")]

# Remove columns and change names to lower
colstoremove <- c("horse","mule","clover1","clover2","applet1","applet2",
                  "lemont1","lemont2","oranget1","oranget2","pecant1","pecant2",
                  "pomelot1","pomelot2")
data_1925 <- data_1925[,(colstoremove):=NULL]
names(data_1925) <- tolower(names(data_1925))

# match to common geo identifier - match 1925 to 1920 
data_1925 <- data_1925 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1925$year <- 1925

# Keep County-Level Observations
data_1925 <- data_1925[data_1925$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1925
crosswalkuse <- semi_join(crosswalk,data_1925,by=c("beginning_FIPS"="fips","year"="year"))
data_1925 <- left_join(crosswalkuse,data_1925,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1925 <- data_1925[which(data_1925$year==1925),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1925)[!names(data_1925)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1925_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1925",weightname="weight_agarea1"))
data_1925_part1 <- data_1925_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1925_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1925",weightname="weight_agarea2"))
data_1925_part2 <- data_1925_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1925 <- left_join(data_1925_part1,data_1925_part2,by=c("base_FIPS","year"))
rm(data_1925_part1,data_1925_part2)
names(data_1925) <- tolower(names(data_1925))


####### ***     1930   *** #######


# totacres is "all land in farms, acres"
# farmval is value of land and buildings
# corn for grain - wheat is wheat threshed - oats is oats threshed - barley is threshed
# rye is rye threshed - also: buckwheat
# sugarcane now divided between for sirup, and for sugar or sold to mills
# NOT counting sweet corn here (notes separately and a long time after the gen. corn one)
# farmlab here -> "farm expenditure for farm labor, exclusive of housework
# olives in pounds

data_1930 <- read_dta(file="DS0013/35206-0013-Data.dta")
data_1930 <- subset(data_1930, select=c("STATE","COUNTY","STATEFIP","FIPS","LEVEL",
                                        "VAR2","VAR8","VAR120","VAR125","VAR172",
                                        "VAR182","VAR237","VAR238","VAR245","VAR246",
                                        "VAR256","VAR257","VAR261","VAR262","VAR264",
                                        "VAR265","VAR267","VAR268","VAR270","VAR271",
                                        "VAR273","VAR274","VAR276","VAR277","VAR305",
                                        "VAR306","VAR309","VAR310","VAR312","VAR313",
                                        "VAR318","VAR319","VAR321","VAR322","VAR324",
                                        "VAR235","VAR523","VAR524","VAR526","VAR527",
                                        "VAR1003","VAR1007","VAR1009","VAR1018","VAR11",
                                        "VAR240","VAR241","VAR486","VAR488","REGION1",
                                        "REGION2","VAR429","VAR482","VAR494","VAR484",
                                        "VAR496","VAR490","VAR492","VAR498","VAR500",
                                        "VAR342","VAR338","VAR553","VAR554","VAR502",
                                        "VAR503","VAR478","VAR480","VAR607","VAR608",
                                        "VAR609","VAR611","VAR612","VAR613","VAR615",
                                        "VAR616","VAR617","VAR627","VAR628","VAR629",
                                        "VAR631","VAR632","VAR633","VAR635","VAR636",
                                        "VAR637","VAR666","VAR667","VAR668","VAR841",
                                        "VAR842","VAR843","VAR619","VAR620","VAR621",
                                        "VAR865","VAR866","VAR867","VAR792","VAR795",
                                        "VAR797","VAR798","VAR799","VAR756","VAR757",
                                        "VAR758","VAR761","VAR764","VAR682","VAR683",
                                        "VAR684","VAR849","VAR850","VAR851","VAR737",
                                        "VAR740","VAR287","VAR288","VAR302","VAR303",
                                        "VAR698","VAR699","VAR707","VAR708","VAR716",
                                        "VAR717","VAR719","VAR720","VAR704","VAR705",
                                        "VAR508","VAR509","VAR517","VAR518","VAR565",
                                        "VAR566","VAR568","VAR569","VAR574","VAR575",
                                        "VAR577","VAR578","VAR674","VAR675","VAR676",
                                        "VAR357","VAR361","VAR405","VAR413","VAR417",
                                        "VAR457","VAR465","VAR330","VAR334","VAR345",
                                        "VAR349","VAR353","VAR365","VAR369","VAR373",
                                        "VAR377","VAR385","VAR389","VAR393","VAR401",
                                        "VAR409","VAR433","VAR437","VAR441","VAR445",
                                        "VAR449","VAR453","VAR461","VAR1014","VAR1016",
                                        "VAR1020","VAR1022"))

colnames(data_1930)[6:182] <- c("farms","totacres","farmval","equipval","horse",
                               "mule","cornac_grain","corn_grain","wheatac","wheat",
                               "oatsac","oats","barleyac","barley","ryeac",
                               "rye","buckwheatac","buckwheat","flaxac","flax",
                               "riceac","rice","hayac","hay","cottonac",
                               "cotton","canesugac_sir","canesug_sir","canesugac","canesug",
                               "tobaccoac","tobacco","irpotatac","irpotato","swpotatac",
                               "swpotato","hempac","hemp","hopsac","hops",
                               "farmfert","farmlab","farm_labdays","tract_num","avgfarmsize",
                               "cornac_silage","corn_silage","soybeanac","soybean","REGION1",
                               "REGION2","peasac1","peasac2","peasac3","peas2",
                               "peas3","beansac1","beans1","beansac2","beans2",
                               "beansac3","beans_snap","cloverac","clover","sorghumgac",
                               "sorghumg","peanutac","peanuts","applet1","applet2",
                               "apples","apricotst1","apricotst2","apricots","cherriest1",
                               "cherriest2","cherries","peacht1","peacht2","peaches",
                               "peart1","peart2","pears","plumt1","plumt2",
                               "plums","almondt1","almondt2","almonds","bananat1",
                               "bananat2","bananas","figt1","figt2","figs",
                               "guavat1","guavat2","guavas","lemonac","lemons",
                               "limet1","limet2","limes","olivet1","olivet2",
                               "olives","orangeac","oranges","pecant1","pecant2",
                               "pecans","pineapple1","pineapple2","pineapples","pomeloac",
                               "pomeloes","alfalfac","alfalfa","SBEETAC","SBEETWT",
                               "BLACKBAC","BLACKBER","CURRANTS","CURRANAC","RASPBAC",
                               "RASPBER","STRAWBAC","STRAWBER","CRANBAC","CRANBER",
                               "chicoryac","chicory","emmerspletac","emmersplet","milletac",
                               "millet","mustardac","mustard","sunflowerac","sunflower",
                               "vetchac","vetch","hazelnult1","hazelnult2","hazelnults",
                               "cabbageac","cantaloupac","lettuceac","oniondac","oniongac",
                               "tomatoac","watermelonac","artichokeac","asparagusac","beetac",
                               "broccoliac","brusselac","carrotac","cauliflowerac","celeryac",
                               "collardac","cucumberac","eggplantac","escaroleac","kaleac",
                               "okraac","pepperac","pumpkinac","radishac","rhubarbac",
                               "spinachac","squashac","turnipac","automobile","motortruck",
                               "electricmotor","gasengine")

data_1930 <- setDT(data_1930)
data_1930 <- data_1930[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horse","mule")]
data_1930 <- data_1930[,peasac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peasac2","peasac3")]
data_1930 <- data_1930[,peas:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peas2","peas3")]
data_1930 <- data_1930[,beansac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beansac1","beansac2","beansac3")]
data_1930 <- data_1930[,APPLTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("applet1","applet2")]
data_1930 <- data_1930[,APRITREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("apricotst1","apricotst2")]
data_1930 <- data_1930[,CHERTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cherriest1","cherriest2")]
data_1930 <- data_1930[,PEACTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peacht1","peacht2")]
data_1930 <- data_1930[,PEARTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peart1","peart2")]
data_1930 <- data_1930[,PLUMTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("plumt1","plumt2")]
data_1930 <- data_1930[,ALMONDTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("almondt1","almondt2")]
data_1930 <- data_1930[,BANANATREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("bananat1","bananat2")]
data_1930 <- data_1930[,FIGTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("figt1","figt2")]
data_1930 <- data_1930[,GUAVATREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("guavat1","guavat2")]
data_1930 <- data_1930[,limetree:=rowSums(.SD,na.rm=TRUE),.SDcols=c("limet1","limet2")]
data_1930 <- data_1930[,olivetree:=rowSums(.SD,na.rm=TRUE),.SDcols=c("olivet1","olivet2")]
data_1930 <- data_1930[,PECANTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pecant1","pecant2")]
data_1930 <- data_1930[,PINEAPPLETREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pineapple1","pineapple2")]
data_1930 <- data_1930[,hazelnuttree:=rowSums(.SD,na.rm=TRUE),.SDcols=c("hazelnult1","hazelnult2")]

# Remove columns and change names to lower
colstoremove <- c("horse","mule","peasac2","peasac3","peas2","peas3","beansac1","beansac2",
                  "beansac3","applet1","applet2","apricotst1","apricotst2","cherriest1","cherriest2",
                  "peacht1","peacht2","peart1","peart2","plumt1","plumt2","almondt1","almondt2",
                  "bananat1","bananat2","figt1","figt2","guavat1","guavat2","limet1","limet2",
                  "olivet1","olivet2","pecant1","pecant2","pineapple1","pineapple2","hazelnult1",
                  "hazelnult2")
data_1930 <- data_1930[,(colstoremove):=NULL]
names(data_1930) <- tolower(names(data_1930))

# match to common geo identifier
data_1930 <- data_1930 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1930$year <- 1930

# Keep County-Level Observations
data_1930 <- data_1930[data_1930$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1930
crosswalkuse <- semi_join(crosswalk,data_1930,by=c("beginning_FIPS"="fips","year"="year"))
data_1930 <- left_join(crosswalkuse,data_1930,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1930 <- data_1930[which(data_1930$year==1930),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1930)[!names(data_1930)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1930_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1930",weightname="weight_agarea1"))
data_1930_part1 <- data_1930_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1930_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1930",weightname="weight_agarea2"))
data_1930_part2 <- data_1930_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1930 <- left_join(data_1930_part1,data_1930_part2,by=c("base_FIPS","year"))
rm(data_1930_part1,data_1930_part2)
names(data_1930) <- tolower(names(data_1930))


####### ***     1935   *** #######


# totacres is "all land in farms, acres"
# farmval is "value of farms (land and buildings)"
# corn for grain & for all
# irrigated land is "irrigated land from which crops were harvested"

data_1935 <- read_dta(file="DS0015/35206-0015-Data.dta")
data_1935 <- subset(data_1935, select=c("STATE","COUNTY","STATEFIP","FIPS","LEVEL",
                                        "VAR2","VAR12","VAR11","VAR19","VAR95",
                                        "VAR100","VAR123","VAR124","VAR126","VAR127",
                                        "VAR130","VAR131","VAR135","VAR136","VAR138",
                                        "VAR139","VAR144","VAR145","VAR147","VAR148",
                                        "VAR176","VAR178","VAR179","VAR180","VAR182",
                                        "VAR183","VAR185","VAR186","VAR191","VAR192",
                                        "VAR194","VAR195","VAR197","VAR198","VAR238",
                                        "VAR398","REGION1","REGION2","VAR321","VAR325",
                                        "VAR328","VAR332","VAR334","VAR337","VAR339",
                                        "VAR341","VAR344","VAR345","VAR161","VAR162",
                                        "VAR150","VAR151","VAR314","VAR318","VAR366",
                                        "VAR367","VAR368","VAR370","VAR371","VAR372",
                                        "VAR374","VAR375","VAR376","VAR378","VAR379",
                                        "VAR380","VAR382","VAR383","VAR384","VAR390",
                                        "VAR391","VAR392","VAR394","VAR395","VAR396",
                                        "VAR158","VAR159","VAR188","VAR189"))

colnames(data_1935)[6:84] <- c("farms","totacres","avgfarmsize","farmval","horses",
                               "mules","wint_wheatac","wint_wheat","spring_wheatac","spring_wheat",
                               "oatsac","oats","barleyac","barley","ryeac",
                               "rye","riceac","rice","flaxac","flax",
                               "cornac_all","cornac_grain","corn_grain","cornac_other","cottonac",
                               "cotton","canesugac","canesug","tobaccoac","tobacco",
                               "irpotatac","irpotato","swpotatac","swpotato","farmpop",
                               "irrac","REGION1","REGION2","soybeanac","soybean",
                               "peasac1","peas1","peasac2","peas2","beansac1",
                               "beans1","beansac2","beans2","cloverac","clover",
                               "sorghumgac","sorghumg","peanutac","peanuts","applet1",
                               "applet2","apples","cherriest1","cherriest2","cherries",
                               "peacht1","peacht2","peaches","peart1","peart2",
                               "pears","plumt1","plumt2","plums","oranget1",
                               "oranget2","oranges","pomelot1","pomelot2","pomeloes",
                               "alfalfac","alfalfa","SBEETAC","SBEETWT")

data_1935 <- setDT(data_1935)
data_1935 <- data_1935[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horses","mules")]
data_1935 <- data_1935[,wheatac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("wint_wheatac","spring_wheatac")]
data_1935 <- data_1935[,wheat:=rowSums(.SD,na.rm=TRUE),.SDcols=c("wint_wheat","spring_wheat")]
data_1935 <- data_1935[,peasac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peasac1","peasac2")]
data_1935 <- data_1935[,peas:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peas1","peas2")]
data_1935 <- data_1935[,beansac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beansac1")]
data_1935 <- data_1935[,beans:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beans1")]
data_1935 <- data_1935[,APPLTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("applet1","applet2")]
data_1935 <- data_1935[,CHERTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cherriest1","cherriest2")]
data_1935 <- data_1935[,PEACTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peacht1","peacht2")]
data_1935 <- data_1935[,PEARTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peart1","peart2")]
data_1935 <- data_1935[,PLUMTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("plumt1","plumt2")]
data_1935 <- data_1935[,ORANGETREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("oranget1","oranget2")]
data_1935 <- data_1935[,POMELOTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pomelot1","pomelot2")]

# Remove columns and change names to lower
colstoremove <- c("horses","mules","wint_wheatac","spring_wheatac","wint_wheat","spring_wheat",
                  "peasac1","peasac2","peas1","peas2","beansac1","beans1","applet1","applet2",
                  "cherriest1","cherriest2","peacht1","peacht2","peart1","peart2","plumt1","plumt2",
                  "oranget1","oranget2","pomelot1","pomelot2")
data_1935 <- data_1935[,(colstoremove):=NULL]
names(data_1935) <- tolower(names(data_1935))

# match to common geo identifier
data_1935 <- data_1935 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1935$year <- 1935

# Keep County-Level Observations
data_1935 <- data_1935[data_1935$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1935
crosswalkuse <- semi_join(crosswalk,data_1935,by=c("beginning_FIPS"="fips","year"="year"))
data_1935 <- left_join(crosswalkuse,data_1935,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1935 <- data_1935[which(data_1935$year==1935),]

data_1935 <- data_1935 %>%
  mutate(pears=as.numeric(pears))

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1935)[!names(data_1935)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1935_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1935",weightname="weight_agarea1"))
data_1935_part1 <- data_1935_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1935_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1935",weightname="weight_agarea2"))
data_1935_part2 <- data_1935_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1935 <- left_join(data_1935_part1,data_1935_part2,by=c("base_FIPS","year"))
rm(data_1935_part1,data_1935_part2)
names(data_1935) <- tolower(names(data_1935))


####### ***     1940   *** #######


# irrigated cropland harvested / irrigated pasture harvested
# rice is now rough of paddy
# cane sugar not accounting for the one for seed
# here expenses for labor are "cash wages paid for hired labor"
# soybeans are soybeans for beans only
# beans are all dry field and seed beans

data_1940 <- read_dta(file="DS0016/35206-0016-Data.dta")
data_1940 <- subset(data_1940, select=c("STATE","COUNTY","STATEFIP","FIPS","LEVEL",
                                        "VAR4","VAR7","VAR8","VAR26","VAR28",
                                        "VAR29","VAR33","VAR188","VAR264","VAR266",
                                        "VAR267","VAR269","VAR270","VAR272","VAR294",
                                        "VAR295","VAR299","VAR300","VAR302","VAR303",
                                        "VAR305","VAR306","VAR309","VAR310","VAR327",
                                        "VAR328","VAR330","VAR331","VAR359","VAR360",
                                        "VAR362","VAR363","VAR365","VAR366","VAR368",
                                        "VAR369","VAR371","VAR372","VAR374","VAR375",
                                        "VAR391","VAR392","VAR560","VAR570","VAR577",
                                        "VAR594","VAR653","VAR654","VAR655","VAR580",
                                        "REGION1", "REGION2","VAR658","VAR664","VAR683",
                                        "VAR688","VAR708","VAR713","VAR778","VAR832",
                                        "VAR676","VAR680","VAR683","VAR688","VAR691",
                                        "VAR696","VAR699","VAR704","VAR724","VAR725",
                                        "VAR276","VAR277","VAR667","VAR673","VAR941",
                                        "VAR942","VAR944","VAR1020","VAR1021","VAR1023",
                                        "VAR946","VAR947","VAR949","VAR959","VAR960",
                                        "VAR962","VAR972","VAR973","VAR975","VAR985",
                                        "VAR986","VAR988","VAR1116","VAR1117","VAR1119",
                                        "VAR1015","VAR1016","VAR1018","VAR1075","VAR1076",
                                        "VAR1078","VAR1176","VAR1177","VAR1179","VAR1181",
                                        "VAR1182","VAR1184","VAR1030","VAR1031","VAR1033",
                                        "VAR1134","VAR1135","VAR1137","VAR1103","VAR1104",
                                        "VAR1106","VAR1070","VAR1071","VAR1073","VAR1163",
                                        "VAR1164","VAR1166","VAR715","VAR716","VAR1494",
                                        "VAR1495","VAR1497","VAR1498","VAR894","VAR895",
                                        "VAR912","VAR913","VAR921","VAR922","VAR930",
                                        "VAR931","VAR909","VAR910","VAR1313","VAR1314",
                                        "VAR1317","VAR1318","VAR1098","VAR1099","VAR1101",
                                        "VAR733","VAR736","VAR745","VAR748","VAR751",
                                        "VAR754","VAR757","VAR760","VAR763","VAR766",
                                        "VAR769","VAR772","VAR781","VAR784","VAR790",
                                        "VAR793","VAR796","VAR799","VAR808","VAR811",
                                        "VAR817","VAR820","VAR823","VAR835","VAR838",
                                        "VAR841","VAR844","VAR847","VAR850","VAR856",
                                        "VAR859","VAR862","VAR865","VAR871","VAR582",
                                        "VAR588"))

colnames(data_1940)[6:186] <- c("farms","totacres","avgfarmsize","irrac","irrac_pasture",
                               "farmval","equipval","horsemu","cornac_all","cornac_grain",
                               "corn_grain","cornac_silage","corn_silage","cornac_other","oatsac",
                               "oats","barleyac","barley","ryeac","rye",
                               "flaxac","flax","wheatac","wheat","buckwheatac",
                               "buckwheat","riceac","rice","irpotatac","irpotato",
                               "swpotatac","swpotato","cottonac","cotton","tobaccoac",
                               "tobacco","canesugac_sir","canesug_sir","canesugac","canesug",
                               "hopsac","hops","farmwages","equipval2","farmfert",
                               "tract_num","soybeanac","soybeanac_other","soybean","farmfert_lime",
                               "REGION1", "REGION2","peasac1","peas1","peasac2",
                               "peas2","peasac3","peas3","peasac4","peasac5",
                               "beansac1","beans1","beansac2","beans2","beansac3",
                               "beans3","beansac4","beans4","cloverac","clover",
                               "sorghumgac","sorghumg","peanutac","peanuts","applet1",
                               "applet2","apples","apricotst1","apricotst2","apricots",
                               "cherriest1","cherriest2","cherries","peacht1","peacht2",
                               "peaches","peart1","peart2","pears","plumt1",
                               "plumt2","plums","almondt1","almondt2","almonds",
                               "figt1","figt2","figs","guavat1","guavat2",
                               "guavas","lemont1","lemont2","lemons","limet1",
                               "limet2","limes","olivet1","olivet2","olives",
                               "oranget1","oranget2","oranges","pecant1","pecant2",
                               "pecans","pineapple1","pineapple2","pineapples","pomelot1",
                               "pomelot2","pomeloes","alfalfac","alfalfa","SBEETAC1",
                               "SBEETWT1","SBEETAC2","SBEETWT2","BLACKBAC","BLACKBER",
                               "CURRANTS","CURRANAC","RASPBAC","RASPBER","STRAWBAC",
                               "STRAWBER","CRANBAC","CRANBER","emmerspletac1","emmersplet1",
                               "emmerspletac2","emmersplet2","hazelnult1","hazelnut2","hazelnuts",
                               "artichokeac","asparagusac","beetac","broccoliac","brusselac",
                               "cabbageac","cantaloupac","carrotac","cauliflowerac","celeryac",
                               "ccabbageac","collardac","watercressac","cucumberac","eggplantac",
                               "endiveac","escaroleac","garlicac","kaleac","lettuceac",
                               "okraac","oniondac","oniongac","pepperac1","pepperac2",
                               "pumpkinac","radishac","rhubarbac","romaineac","spinachac",
                               "squashac","tomatoac","turnipac","watermelonac","automobile",
                               "motortruck")

data_1940 <- setDT(data_1940)
data_1940 <- data_1940[,irrac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("irrac","irrac_pasture")]
data_1940 <- data_1940[,farmfert:=rowSums(.SD,na.rm=TRUE),.SDcols=c("farmfert","farmfert_lime")]
data_1940 <- data_1940[,peasac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peasac3")] # peasac1+peasac2+peasac3+peasac4+peasac5
data_1940 <- data_1940[,beansac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beansac1","beansac2","beansac3","beansac4")]
data_1940 <- data_1940[,APPLTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("applet1","applet2")]
data_1940 <- data_1940[,APRITREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("apricotst1","apricotst2")]
data_1940 <- data_1940[,CHERTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cherriest1","cherriest2")]
data_1940 <- data_1940[,PEACTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peacht1","peacht2")]
data_1940 <- data_1940[,PEARTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peart1","peart2")]
data_1940 <- data_1940[,PLUMTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("plumt1","plumt2")]
data_1940 <- data_1940[,ALMONDTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("almondt1","almondt2")]
data_1940 <- data_1940[,FIGTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("figt1","figt2")]
data_1940 <- data_1940[,guavatree:=rowSums(.SD,na.rm=TRUE),.SDcols=c("guavat1","guavat2")]
data_1940 <- data_1940[,lemontree:=rowSums(.SD,na.rm=TRUE),.SDcols=c("lemont1","lemont2")]
data_1940 <- data_1940[,limetree:=rowSums(.SD,na.rm=TRUE),.SDcols=c("limet1","limet2")]
data_1940 <- data_1940[,olivetree:=rowSums(.SD,na.rm=TRUE),.SDcols=c("olivet1","olivet2")]
data_1940 <- data_1940[,orangetree:=rowSums(.SD,na.rm=TRUE),.SDcols=c("oranget1","oranget2")]
data_1940 <- data_1940[,PECANTREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pecant1","pecant2")]
data_1940 <- data_1940[,PINEAPPLETREE:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pineapple1","pineapple2")]
data_1940 <- data_1940[,pomelotree:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pomelot1","pomelot2")]
data_1940 <- data_1940[,SBEETAC:=rowSums(.SD,na.rm=TRUE),.SDcols=c("SBEETAC1","SBEETAC2")]
data_1940 <- data_1940[,SBEETWT:=rowSums(.SD,na.rm=TRUE),.SDcols=c("SBEETWT1","SBEETWT2")]
data_1940 <- data_1940[,emmerspletac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("emmerspletac1","emmerspletac2")]
data_1940 <- data_1940[,emmersplet:=rowSums(.SD,na.rm=TRUE),.SDcols=c("emmersplet1","emmersplet2")]
data_1940 <- data_1940[,hazelnuttree:=rowSums(.SD,na.rm=TRUE),.SDcols=c("hazelnult1","hazelnut2")]
data_1940 <- data_1940[,pepperac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pepperac1","pepperac2")]

# Remove columns and change names to lower
colstoremove <- c("irrac","irrac_pasture","farmfert","farmfert_lime","peasac3","peasac1","peasac2",
                  "peasac4","peasac5","beansac1","beansac2","beansac3","beansac4","applet1","applet2",
                  "apricotst1","apricotst2","cherriest1","cherriest2","peacht1","peacht2","peart1",
                  "peart2","plumt1","plumt2","almondt1","almondt2","figt1","figt2","guavat1","guavat2",
                  "lemont1","lemont2","limet1","limet2","olivet1","olivet2","oranget1","oranget2",
                  "pecant1","pecant2","pineapple1","pineapple2","pomelot1","pomelot2","SBEETAC1",
                  "SBEETAC2","SBEETWT1","SBEETWT2","emmerspletac1","emmerspletac2","emmersplet1",
                  "emmersplet2","hazelnult1","hazelnut2","pepperac1","pepperac2")
data_1940 <- data_1940[,(colstoremove):=NULL]
names(data_1940) <- tolower(names(data_1940))

# match to common geo identifier
data_1940 <- data_1940 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1940$year <- 1940

# Keep County-Level Observations
data_1940 <- data_1940[data_1940$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1940
crosswalkuse <- semi_join(crosswalk,data_1940,by=c("beginning_FIPS"="fips","year"="year"))
data_1940 <- left_join(crosswalkuse,data_1940,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1940 <- data_1940[which(data_1940$year==1940),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1940)[!names(data_1940)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1940_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1940",weightname="weight_agarea1"))
data_1940_part1 <- data_1940_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1940_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1940",weightname="weight_agarea2"))
data_1940_part2 <- data_1940_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1940 <- left_join(data_1940_part1,data_1940_part2,by=c("base_FIPS","year"))
rm(data_1940_part1,data_1940_part2)
names(data_1940) <- tolower(names(data_1940))


####### ***     1945   *** #######


# farmval is value of land and building here
# equipval 2 variables with different values (43 and 44)
# farmwages is "cash wages paid for hired labor"
# oats is now oats threshed or combined (vs cut and fed - which was the other category - not kept - before)
# rice is rough or paddy - counted in barrels (output 1) or bags (output 2)
# soybeans are soybeans for beans only
# var 39 and 587 both farmval - same values
# apples bushels, apricots bushels, peaches bushels, pears bushels, oranges tons
# beans is all dry field and seed beans (probably including lima)

data_1945 <- read_dta(file="DS0020/35206-0020-Data.dta")
data_1945 <- subset(data_1945, select=c("STATE","COUNTY","STATEFIP","FIPS","LEVEL",
                                        "VAR1","VAR2","VAR4","VAR7","VAR39",
                                        "VAR43","VAR44","VAR105","VAR119","VAR132",
                                        "VAR133","VAR148","VAR149","VAR155","VAR156",
                                        "VAR159","VAR160","VAR163","VAR164","VAR167",
                                        "VAR168","VAR187","VAR188","VAR189","VAR196",
                                        "VAR197","VAR285","VAR286","VAR290","VAR291",
                                        "VAR294","VAR295","VAR299","VAR300","VAR304",
                                        "VAR305","VAR312","VAR313","VAR316","VAR317",
                                        "VAR458","VAR461","REGION1","REGION2","VAR214",
                                        "VAR215","VAR450","VAR225","VAR226","VAR200",
                                        "VAR201","VAR277","VAR278","VAR138","VAR139",
                                        "VAR238","VAR239","VAR352","VAR353","VAR381",
                                        "VAR382","VAR357","VAR358","VAR361","VAR362",
                                        "VAR366","VAR367","VAR371","VAR372","VAR415",
                                        "VAR416","VAR403","VAR404","VAR407","VAR408",
                                        "VAR387","VAR388","VAR419","VAR420","VAR398",
                                        "VAR399","VAR273","VAR274","VAR308","VAR309",
                                        "VAR339","VAR340","VAR334","VAR335","VAR329",
                                        "VAR330","VAR269","VAR270","VAR440","VAR442",
                                        "VAR444","VAR448","VAR103","VAR107"))

colnames(data_1945)[6:104] <- c("farmpop","farms","totacres","avgfarmsize","farmval",
                               "equipval1","equipval2","tract_num","farmwages","cornac_grain",
                               "corn_grain","oatsac","oats","barleyac","barley",
                               "ryeac","rye","flaxac","flax","wheatac",
                               "wheat","riceac","rice_barrel","rice_bag","soybeanac",
                               "soybean","irpotatac","irpotato","hopsac","hops",
                               "cottonac","cotton","swpotatac","swpotato","tobaccoac",
                               "tobacco","canesugac_sir","canesug_sir","canesugac","canesug",
                               "mule","horse","REGION1","REGION2","peasac1",
                               "peas1","peasac2","peasac3","peas3","beansac",
                               "beans","cloverac","clover","sorghumgac","sorghumg",
                               "peanutac","peanuts","appltree","apples","apritree",
                               "apricots","chertree","cherries","peactree","peaches",
                               "peartree","pears","plumtree","plums","almondtree",
                               "almonds","lemontree","lemons","limetree","limes",
                               "orangetree","oranges","pecantree","pecans","pomelotree",
                               "pomeloes","alfalfac","alfalfa","SBEETAC","SBEETWT",
                               "BLACKBAC","BLACKBER","RASPBAC","RASPBER","STRAWBAC",
                               "STRAWBER","vetchac","vetch","cabbageac","celeryac",
                               "lettuceac","tomatoac","motortruck","automobile")

data_1945 <- setDT(data_1945)
data_1945 <- data_1945[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horse","mule")]
data_1945 <- data_1945[,`:=`(rice_barrel=rice_barrel*162,
                             rice_bag=rice_bag*100)]
data_1945 <- data_1945[,rice:=rowSums(.SD,na.rm=TRUE),.SDcols=c("rice_barrel","rice_bag")]
data_1945 <- data_1945[,peasac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("peasac1","peasac3")] #  + peasac2 which is green beans
data_1945 <- data_1945[,equipval:=rowSums(.SD,na.rm=TRUE),.SDcols=c("equipval1","equipval2")]

# Remove columns and change names to lower
colstoremove <- c("horse","mule","rice_barrel","rice_bag","peasac1","peasac3","equipval1","equipval2")
data_1945 <- data_1945[,(colstoremove):=NULL]
names(data_1945) <- tolower(names(data_1945))

# match to common geo identifier
data_1945 <- data_1945 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1945$year <- 1945

# Keep County-Level Observations
data_1945 <- data_1945[data_1945$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1945
crosswalkuse <- semi_join(crosswalk,data_1945,by=c("beginning_FIPS"="fips","year"="year"))
data_1945 <- left_join(crosswalkuse,data_1945,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1945 <- data_1945[which(data_1945$year==1945),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1945)[!names(data_1945)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1945_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1945",weightname="weight_agarea1"))
data_1945_part1 <- data_1945_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1945_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1945",weightname="weight_agarea2"))
data_1945_part2 <- data_1945_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1945 <- left_join(data_1945_part1,data_1945_part2,by=c("base_FIPS","year"))
rm(data_1945_part1,data_1945_part2)
names(data_1945) <- tolower(names(data_1945))


####### ***     1950   *** #######


# On the 1950 change to a more restrictive def of farm: https://www2.census.gov/prod2/decennial/documents/21895591v2ch02.pdf

# different types of tractors - to be sure we keep all relevant var: tract_num for the gen one, 
# tract_wheel, tract_garden and tract_crawl for the specialized counts
# also add machine hire - expenses in $ for machine hire: machine_hire
# farmwages here -> "specified farm expenditures: hired labor"
# soybeans for beans
# surgarcane for sirup mixed with sorghum here - so only keep sugarcane for sugar
# at least from 1950 onwards - also ton of fertilizer used - fert_wt
# chemicals can be value of expenses - or acres where applied
# for acres=> chemac_spray (acres for spray, dust and fumigants) // chemac_insects (acres for chemicals on insects and diseases)
# chemac_weeds (acres - chem for weeds or grass IN CROPS)
# We have the same for value with chemval_"" -- chemval_weeds does not include value used for pasture - sum is chemval_crops for all crop-related expenses
# sorghum bushels, apples bushels, apricots bushels, cherries pounds,
# peaches and pears in bushels, plums in bushels, figs in pounds, olives in pounds
# orange in tons, blackberries in quarts, raspb in quarts, strawb in quarts
# cranberries in pounds, 
# beans is dry field and seed beans harvested for beans

data_1950_part1 <- read_dta(file="DS0021/35206-0021-Data.dta")
data_1950_part2 <- read_dta(file="DS0022/35206-0022-Data.dta")
data_1950_part1 <- subset(data_1950_part1, select=c("STATE","COUNTY","STATEFIP","FIPS","LEVEL",
                                                    "VAR1","VAR8","VAR9","VAR10","VAR49",
                                                    "VAR233","VAR235","VAR237","VAR245","VAR247",
                                                    "VAR249","VAR251","VAR283","VAR285","VAR301",
                                                    "VAR304","VAR443","VAR448","VAR449","VAR452",
                                                    "VAR453","VAR484","VAR485","VAR509","VAR510",
                                                    "VAR526","VAR527","VAR533","VAR534","VAR543",
                                                    "VAR544","VAR548","VAR549","VAR557","VAR558",
                                                    "VAR574","VAR577","VAR844","VAR845","VAR850",
                                                    "VAR851","VAR866","VAR867","VAR890","VAR891",
                                                    "VAR895","VAR896","VAR920","VAR921","VAR945",
                                                    "VAR946","REGION1","REGION2",
                                                    "VAR659","VAR660","VAR651","VAR652","VAR733",
                                                    "VAR734","VAR772","VAR773","VAR777","VAR778",
                                                    "VAR803","VAR804","VAR819","VAR820","VAR463",
                                                    "VAR464","VAR637","VAR640","VAR1097","VAR1100",
                                                    "VAR1199","VAR1202","VAR1106","VAR1109","VAR1126",
                                                    "VAR1129","VAR1151","VAR1154","VAR1175","VAR1178",
                                                    "VAR1301","VAR1304","VAR1215","VAR1218","VAR1379",
                                                    "VAR1382","VAR1223","VAR1226","VAR1322","VAR1326",
                                                    "VAR1268","VAR1271","VAR1385","VAR1386","VAR1365",
                                                    "VAR1369","VAR742","VAR743","VAR880","VAR881",
                                                    "VAR1051","VAR1052","VAR1065","VAR1066","VAR1069",
                                                    "VAR1070","VAR1074","VAR1075","VAR1081","VAR1083",
                                                    "VAR538","VAR539","VAR871","VAR872","VAR861",
                                                    "VAR862","VAR760","VAR761","VAR1294","VAR1297",
                                                    "VAR978","VAR980","VAR986","VAR988","VAR990",
                                                    "VAR992","VAR994","VAR996","VAR998","VAR1000",
                                                    "VAR1002","VAR1006","VAR1008","VAR1010","VAR1014",
                                                    "VAR1018","VAR1020","VAR1022","VAR1026","VAR1028",
                                                    "VAR1030","VAR1032","VAR1034","VAR1016","VAR1038",
                                                    "VAR1040","VAR1042","VAR1044","VAR1048","VAR876",
                                                    "VAR877","VAR1096","VAR1198","VAR1105","VAR1125",
                                                    "VAR1150","VAR1174","VAR1300","VAR1214","VAR1222",
                                                    "VAR1321","VAR1364","VAR253","VAR243"))

colnames(data_1950_part1)[6:172] <- c("farms","totacres","avgfarmsize","avg_farmval","irrac",
                                     "combine_num","picker_num","baler_num","tract_num","tract_wheel",
                                     "tract_garden","tract_crawl","machine_hire_sub","farmwages","horses",
                                     "mules","cropval","cornac_grain","corn_grain","cornac_silage",
                                     "corn_silage","wheatac","wheat","oatsac","oats",
                                     "barleyac","barley","ryeac","rye","flaxac",
                                     "flax","riceac","rice","buckwheatac","buckwheat",
                                     "soybeanac","soybean","irpotatac","irpotato","swpotatac",
                                     "swpotato","cottonac","cotton","hempac","hemp",
                                     "tobaccoac","tobacco","canesugac","canesug","hopsac",
                                     "hops","REGION1","REGION2",
                                     "peasac","peas","beansac","beans","cloverac1",
                                     "clover1","cloverac2","clover2","cloverac3","clover3",
                                     "cloverac4","clover4","cloverac5","clover5","sorghumgac",
                                     "sorghumg","peanutac","peanuts","appltree","apples",
                                     "apritree","apricots","chertree","cherries","peactree",
                                     "peaches","peartree","pears","plumtree","plums",
                                     "almondtree","almonds","figtree","figs","limetree",
                                     "limes","olivetree","olives","orangetree","oranges",
                                     "pecantree","pecans","pineappleac","pineapples","pomelotree",
                                     "pomeloes","alfalfac","alfalfa","SBEETAC","SBEETWT",
                                     "BLACKBAC","BLACKBER","CURRANAC","CURRANTS","RASPBAC",  
                                     "RASPBER","STRAWBAC","STRAWBER","CRANBAC","CRANBER",
                                     "emmerspletac","emmersplet","mustardac","mustard","sunflowerac",
                                     "sunflower","vetchac","vetch","hazelnultac","hazelnults",
                                     "artichokeac","asparagusac","beetac","broccoliac","brusselac",
                                     "cabbageac","cantaloupac","carrotac","cauliflowerac", "celeryac",
                                     "collardac","cucumberac","eggplantac","escaroleac","kaleac",
                                     "okraac","oniondac","oniongac","pepperac1","pepperac2",
                                     "pumpkinac","radishac","rhubarbac","lettuceromaineac","spinachac",
                                     "squashac","tomatoac","turnipac","watermelonac","canolaac",
                                     "canola","appleac","apricotac","cherryac","peachac",
                                     "pearac","plumac","almondac","figac","oliveac",
                                     "orangeac","pomeloac","automobile","motortruck")

data_1950_part1 <- setDT(data_1950_part1)
data_1950_part1 <- data_1950_part1[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horses","mules")]
data_1950_part1 <- data_1950_part1[,farmval:=farms*avg_farmval]
data_1950_part1 <- data_1950_part1[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cloverac1","cloverac2",
                                                                                "cloverac3","cloverac4","cloverac5")]
data_1950_part1 <- data_1950_part1[,clover:=rowSums(.SD,na.rm=TRUE),.SDcols=c("clover1","clover2","clover3",
                                                                              "clover4","clover5")] 
data_1950_part1 <- data_1950_part1[,pepperac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pepperac1","pepperac2")] 

data_1950_part2 <- subset(data_1950_part2, select=c("STATEICP","COUNICP","STATEFIP","COUNFIP",
                                                    "ITEM848","ITEM801","ITEM951","ITEM952","ITEM958",
                                                    "ITEM962","ITEM963","ITEM964","ITEM965","ITEM967"))

colnames(data_1950_part2)[5:14] <- c("farmfert","fert_wt","chemac_spray","chemac_insects","chemac_weeds",
                                     "chemval_insects_hay","chemval_insects_ot","chemval_nematodes","chemval_diseases","chemval_weeds")
data_1950_part2 <- setDT(data_1950_part2)
data_1950_part2 <- data_1950_part2[,chemval_crop:=rowSums(.SD,na.rm=TRUE),.SDcols=c("chemval_insects_hay",
                                                                                    "chemval_insects_ot",
                                                                                    "chemval_nematodes",
                                                                                    "chemval_diseases",
                                                                                    "chemval_weeds")]
data_1950 <- left_join(data_1950_part1,data_1950_part2, by = c("STATE" = "STATEICP","COUNTY"="COUNICP"))
rm(data_1950_part1, data_1950_part2)

# Remove columns and change names to lower
colstoremove <- c("horses","mules","cloverac1","cloverac2",
                  "cloverac3","cloverac4","cloverac5","clover1","clover2","clover3",
                  "clover4","clover5","pepperac1","pepperac2","avg_farmval","chemval_insects_hay",
                  "chemval_insects_ot","chemval_nematodes","chemval_diseases","chemval_weeds",
                  "tract_wheel","tract_garden","tract_crawl")
data_1950 <- data_1950[,(colstoremove):=NULL]
names(data_1950) <- tolower(names(data_1950))

# match to common geo identifier
data_1950 <- data_1950 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1950$year <- 1950 

# Keep County-Level Observations
data_1950 <- data_1950[data_1950$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1950
crosswalkuse <- semi_join(crosswalk,data_1950,by=c("beginning_FIPS"="fips","year"="year"))
data_1950 <- left_join(crosswalkuse,data_1950,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1950 <- data_1950[which(data_1950$year==1950),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1950)[!names(data_1950)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1950_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1950",weightname="weight_agarea1"))
data_1950_part1 <- data_1950_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1950_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1950",weightname="weight_agarea2"))
data_1950_part2 <- data_1950_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1950 <- left_join(data_1950_part1,data_1950_part2,by=c("base_FIPS","year"))
rm(data_1950_part1,data_1950_part2)
names(data_1950) <- tolower(names(data_1950))


####### ***     1954   *** #######


# irrigated land here is "irrigated land in farms, reporting" - there are several variables like this with close names
# the others are kept too
# farmwages -> hired labor ($)
# rice is now only one category for threshed or combined
# soybean is soybean for beans - soybean all cat. does not have acres
# sugarcane is for sugar - otherwise sugarcane for sirup is aggregated with sorghum for sirup
# chemicals can be value of expenses - or acres where applied
# for acres=> chemac_spray (acres for spray, dust and fumigants) // chemac_insects (acres for chemicals on insects and diseases)
# chemac_weeds (acres - chem for weeds or grass or bush (is the bush part new?))
# We have the same for value with chemval_"" -- chemval_weeds does not include value used for pasture - sum is chemval_crops for all crop-related expenses
# beans bushels (dry field and seed harvested for beans), clover in pounds, apples, apricots in bushels, cherries in pounds
# peaches, pears, plums in bushels, almonds in pounds, lemon in boxes, limes in pounds
# olives in pounds, oranges in tons, grapefruits in tons, alfalfa in pounds
# blackberries in wuarts, raspb in quarts, strawb in quarts
# cranberries in 100pounds barrels, 

data_1954_part1 <- read_dta(file="DS0025/35206-0025-Data.dta")
data_1954_part2 <- read_dta(file="DS0026/35206-0026-Data.dta")

data_1954_part1 <- subset(data_1954_part1, select=c("STATE","COUNTY","LEVEL",
                                                    "VAR1","VAR8","VAR9","VAR10","VAR51",
                                                    "VAR62","VAR84","VAR267","VAR269","VAR271",
                                                    "VAR279","VAR281","VAR283","VAR285","VAR309",
                                                    "VAR311","VAR323","VAR324","VAR386","VAR512",
                                                    "VAR513","VAR515","VAR516","VAR544","VAR545",
                                                    "VAR564","VAR655","VAR575","VAR576","VAR581",
                                                    "VAR582","VAR589","VAR590","VAR597","VAR598",
                                                    "VAR605","VAR606","VAR617","VAR620","VAR730",
                                                    "VAR731","VAR734","VAR735","VAR738","VAR739",
                                                    "VAR742","VAR743","VAR772","VAR773","VAR786",
                                                    "VAR787","VAR617","VAR620","VAR675","VAR676",
                                                    "VAR671","VAR673","VAR814","VAR815","VAR822",
                                                    "VAR823","VAR826","VAR827","VAR818","VAR819",
                                                    "VAR829","VAR830","VAR832","VAR833","VAR1286",
                                                    "VAR1287","VAR662","VAR999","VAR1002","VAR1121",
                                                    "VAR1124","VAR1099","VAR1102","VAR1007","VAR1010",
                                                    "VAR1029","VAR1032","VAR1078","VAR1081","VAR1194",
                                                    "VAR1197","VAR1129","VAR1132","VAR1255","VAR1258",
                                                    "VAR1260","VAR1263","VAR1149","VAR1152","VAR1218",
                                                    "VAR1222","VAR1166","VAR1169","VAR1211","VAR1215",
                                                    "VAR810","VAR811","VAR766","VAR767","VAR968",
                                                    "VAR969","VAR972","VAR973","VAR962","VAR963",
                                                    "VAR992","VAR993","VAR585","VAR586","VAR593",
                                                    "VAR594","VAR795","VAR796","VAR860","VAR861",
                                                    "VAR1200","VAR1203","VAR897","VAR905","VAR907",
                                                    "VAR909","VAR911","VAR913","VAR915","VAR953",
                                                    "VAR1470","VAR1472","VAR1474","VAR923","VAR925",
                                                    "VAR927","VAR929","VAR931","VAR935","VAR937",
                                                    "VAR939","VAR941","VAR943","VAR945","VAR947",
                                                    "VAR949","VAR951","VAR798","VAR799","VAR998",
                                                    "VAR1120","VAR1098","VAR1006","VAR1028","VAR1077",
                                                    "VAR1193","VAR1128","VAR1254","VAR1148","VAR1217",
                                                    "VAR1210","VAR273","VAR277","VAR287"))

colnames(data_1954_part1)[4:167] <- c("farms","totacres","avgfarmsize","avg_farmval","irrac",
                                     "irrac2","irrac3","combine_num","picker_num","baler_num",
                                     "tract_num","tract_wheel","tract_garden","tract_crawl","machine_hire_sub",
                                     "farmwages","farmfert","fert_wt","horsemu","cornac_grain",
                                     "corn_grain","cornac_silage","corn_silage","wheatac","wheat",
                                     "oatsac","oats","barleyac","barley","ryeac",
                                     "rye","flaxac","flax","riceac","rice",
                                     "buckwheatac","buckwheat","soyebeanac_bean","soybean_bean","irpotatac",
                                     "irpotato","swpotatac","swpotato","cottonac","cotton",
                                     "tobaccoac","tobacco","canesugac","canesug","hopsac",
                                     "hops","soybeanac","soybean","peasac","peas",
                                     "beansac","beans","cloverac1","clover1","cloverac2",
                                     "clover2","cloverac3","clover3","cloverac4","clover4",
                                     "cloverac5","clover5","cloverac6","clover6","sorghumgac",
                                     "sorghumg","peanutac","appltree","apples","apritree",
                                     "apricots","chertree","cherries","peactree","peaches",
                                     "peartree","pears","plumtree","plums","almondtree",
                                     "almonds","figtree","figs","lemontree","lemons",
                                     "limetree","limes","olivetree","olives","orangetree",
                                     "oranges","pecantree","pecans","pomelotree","pomeloes",
                                     "alfalfac","alfalfa","SBEETAC","SBEETWT","BLACKBAC",
                                     "BLACKBER","RASPBAC","RASPBER","STRAWBAC","STRAWBER",
                                     "CRANBAC","CRANBER","emmerspletac","emmersplet","milletac",
                                     "millet","mustardac","mustard","vetchac","vetch",
                                     "hazelnuttree","hazelnults","asparagusac","broccoliac","cabbageac",
                                     "cantaloupac","carrotac","cauliflowerac","celeryac","collardac",
                                     "cucumberac","eggplantac","escaroleac","kaleac","lettuceromaineac",
                                     "okraac","oniondac","oniongac","pepperac1","pepperac2",
                                     "pumpkinac","radishac","spinachac","squashac","tomatoac",
                                     "turnipac","watermelonac","canolaac","canola","appleac",
                                     "apricotac","cherryac","peachac","pearac","plumac",
                                     "almondac","figac","lemonac","oliveac","orangeac",
                                     "pomeloac","forageharvester","motortruck","automobile")

data_1954_part1 <- setDT(data_1954_part1)
data_1954_part1 <- data_1954_part1[,farmval:=farms*avg_farmval]
data_1954_part1 <- data_1954_part1[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cloverac1","cloverac2",
                                                                                "cloverac3","cloverac4","cloverac5",
                                                                                "cloverac6")]
data_1954_part1 <- data_1954_part1[,clover:=rowSums(.SD,na.rm=TRUE),.SDcols=c("clover1","clover2","clover3",
                                                                              "clover4","clover5","clover6")] 
data_1954_part1 <- data_1954_part1[,pepperac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pepperac1","pepperac2")] 

data_1954_part2 <- subset(data_1954_part2, select=c("STATEICP","COUNICP","FIPS",
                                                    "ITEM951","ITEM952","ITEM958","ITEM962","ITEM963",
                                                    "ITEM964","ITEM965","ITEM967"))
colnames(data_1954_part2)[4:11] <- c("chemac_spray","chemac_insects","chemac_weeds","chemval_insects_hay","chemval_insects_ot",
                                     "chemval_nematodes","chemval_diseases","chemval_weeds")
data_1954_part2 <- setDT(data_1954_part2)
data_1954_part2 <- data_1954_part2[,chemval_crop:=rowSums(.SD,na.rm=TRUE),.SDcols=c("chemval_insects_hay","chemval_insects_ot",
                                                                                    "chemval_nematodes","chemval_diseases",
                                                                                    "chemval_weeds")] 

data_1954 <- left_join(data_1954_part1,data_1954_part2, by = c("STATE"="STATEICP","COUNTY"="COUNICP"))

# Remove columns and change names to lower
colstoremove <- c("irrac2","irrac3","cloverac1","cloverac2",
                  "cloverac3","cloverac4","cloverac5",
                  "cloverac6","avg_farmval","clover1","clover2","clover3",
                  "clover4","clover5","clover6","pepperac1","pepperac2",
                  "chemval_insects_hay","chemval_insects_ot",
                  "chemval_nematodes","chemval_diseases","chemval_weeds",
                  "tract_wheel","tract_garden","tract_crawl")
data_1954 <- data_1954[,(colstoremove):=NULL]
names(data_1954) <- tolower(names(data_1954))

rm(data_1954_part2,data_1954_part1)

# match to common geo identifier
data_1954 <- data_1954 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1954$year <- 1954

# Keep County-Level Observations
data_1954 <- data_1954[data_1954$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1954
crosswalkuse <- semi_join(crosswalk,data_1954,by=c("beginning_FIPS"="fips","year"="year"))
data_1954 <- left_join(crosswalkuse,data_1954,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1954 <- data_1954[which(data_1954$year==1954),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1954)[!names(data_1954)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1954_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1954",weightname="weight_agarea1"))
data_1954_part1 <- data_1954_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1954_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1954",weightname="weight_agarea2"))
data_1954_part2 <- data_1954_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1954 <- left_join(data_1954_part1,data_1954_part2,by=c("base_FIPS","year"))
rm(data_1954_part1,data_1954_part2)
names(data_1954) <- tolower(names(data_1954))


####### ***     1959   *** #######


# change_farms records the chage in farm # due to change in def of a farm between 1954 and 1959
# corn for grain is in bushels, corn for silage is tons of green weight
# oats is oats harvested for grain
# for acres=> chemac_spray (acres for spray, dust and fumigants) // chemac_insects (acres for chemicals on insects and diseases)
# chemac_weeds (acres - chem for weeds or grass or bush (is the bush part new?))
# We have the same for value with chemval_"" -- chemval_weeds does not include value used for pasture - sum is chemval_crops for all crop-related expenses
# beans in bags of 100lbs - dry field and seed, clover in bushels, apples/apricots in bushels, cherries in pounds
# peaches, pears in bushels, almonds in tons, bananas in pounds, figs in pounds, guavas in lbs
# olives in lbs, orange in tons, grapefruits in tons, alfalfa in bushels
# blackberries, raspberries, strawb in quarts, cranberries in lbs
# sunflower bushels

data_1959_part1 <- read_dta(file="DS0027/35206-0027-Data.dta")
data_1959_part2 <- read_dta(file="DS0028/35206-0028-Data.dta")

data_1959_part1 <- subset(data_1959_part1, select=c("STATE","COUNTY",
                                                    "VAR1","VAR2","VAR5","VAR6","VAR7",
                                                    "VAR54","VAR504","VAR506","VAR508","VAR522",
                                                    "VAR528","VAR532","VAR530","VAR570","VAR692",
                                                    "VAR697","VAR708","VAR747","VAR877","VAR878",
                                                    "VAR882","VAR883","VAR915","VAR916","VAR965",
                                                    "VAR966","VAR980","VAR981","VAR992","VAR993",
                                                    "VAR997","VAR998","VAR1009","VAR1010","VAR1030",
                                                    "VAR1031","VAR1060","VAR1063","VAR1333","VAR1334",
                                                    "VAR1345","VAR1346","VAR1350","VAR1351","VAR1362",
                                                    "VAR1363","VAR1375","VAR1376","VAR1378","VAR1379",
                                                    "VAR1147","VAR1148","VAR1126",
                                                    "VAR1127","VAR1235","VAR1236","VAR1243","VAR1244",
                                                    "VAR1247","VAR1248","VAR1250","VAR1251","VAR1253",
                                                    "VAR1254","VAR1326","VAR1327","VAR895","VAR896",
                                                    "VAR1110","VAR1113","VAR1608","VAR1611","VAR1734",
                                                    "VAR1737","VAR1709","VAR1712","VAR1617","VAR1620",
                                                    "VAR1639","VAR1642",'VAR1688',"VAR1691","VAR1802",
                                                    "VAR1806","VAR1941","VAR1948","VAR1742","VAR1745",
                                                    "VAR1913","VAR1916","VAR1880","VAR1883","VAR1885",
                                                    "VAR1888","VAR1763","VAR1766","VAR1833","VAR1837",
                                                    "VAR1769","VAR1772","VAR1446","VAR1447","VAR1826",
                                                    "VAR1829","VAR1231","VAR1232","VAR1358","VAR1359",
                                                    "VAR1588","VAR1589","VAR1575","VAR1576","VAR1569",
                                                    "VAR1570","VAR1596","VAR1597","VAR1553","VAR1014",
                                                    "VAR1015","VAR1019","VAR1020","VAR1044","VAR1045",
                                                    "VAR1329","VAR1330","VAR1299","VAR1300","VAR1813",
                                                    "VAR1816","VAR1513","VAR1542","VAR1491","VAR1536",
                                                    "VAR1515","VAR1527","VAR1549","VAR1494","VAR1555",
                                                    "VAR1485","VAR1531","VAR1549","VAR1518","VAR1508",
                                                    "VAR1510","VAR1523","VAR1497","VAR1551","VAR1499",
                                                    "VAR1529","VAR1545","VAR1559","VAR1525","VAR1506",
                                                    "VAR1479","VAR1547","VAR1533","VAR1607","VAR1733",
                                                    "VAR1708","VAR1616","VAR1638","VAR1687","VAR1801",
                                                    "VAR1940","VAR1741","VAR1912","VAR1879","VAR1762",
                                                    "VAR1832","VAR1825","VAR510","VAR512","VAR518",
                                                    "VAR520","VAR534","VAR540"))

colnames(data_1959_part1)[3:178] <- c("farms","farms_defchange","totacres","avgfarmsize","avg_farmval",
                                     "irrac","combine_num","picker_num","baler_num","tract_num",
                                     "tract_wheel","tract_garden","tract_crawl","fert_wt","machine_hire_sub",
                                     "farmwages","farmfert","horsemu","cornac_grain","corn_grain",
                                     "cornac_silage","corn_silage","wheatac","wheat","oatsac",
                                     "oats","barleyac","barley","ryeac","rye",
                                     "flaxac","flax","buckwheatac","buckwheat","riceac",
                                     "rice","soyebeanac_bean","soybean_bean","irpotatac","irpotato",
                                     "swpotatac","swpotato","cottonac","cotton","canesugac",
                                     "canesug","hopsac","hops","tobaccoac","tobacco",
                                     "peasac","peas","beansac",
                                     "beans","cloverac1","clover1","cloverac2","clover2",
                                     "cloverac3","clover3","cloverac4","clover4","cloverac5",
                                     "clover5","cloverac6","clover6","sorghumgac","sorghumg",
                                     "peanutac","peanuts","appltree","apples","apritree",
                                     "apricots","chertree","cherries","peactree","peaches",
                                     "peartree","pears","plumtree","plums","almondtree",
                                     "almonds","bananatree","bananas","figtree","figs",
                                     "guavatree","guavas","lemontree","lemons","limetree",
                                     "limes","olivetree","olives","orangetree","oranges",
                                     "pecantree","pecans","pineappleac","pineapples","pomelotree",
                                     "pomeloes","alfalfac","alfalfa","SBEETAC","SBEETWT",
                                     "BLACKBAC","BLACKBER","RASPBAC","RASPBER","STRAWBAC",
                                     "STRAWBER","CRANBAC","CRANBER","escaroleac","emmerspletac",
                                     "emmersplet","milletac","millet","mustardac","mustard",
                                     "sunflowerac","sunflower","vetchac","vetch","hazelnuttree",
                                     "hazelnults","asparagusac","broccoliac","cabbageac","cantaloupac",
                                     "carrotac","cauliflowerac","celeryac","ccabbageac","collardac",
                                     "cucumberac","eggplantac","kaleac","lettuceromaineac","okraac",
                                     "oniondac","oniongac","pepperac1","pepperac2","pepperac3",
                                     "pumpkinac","radishac","rhubarbac","spinachac","squashac",
                                     "tomatoac","turnipac","watermelonac","appleac","apricotac",
                                     "cherryac","peachac","pearac","plumac","almondac",
                                     "bananaac","figac","guavaac","lemonac","oliveac",
                                     "orangeac","pomeloac","forageharvester","potatodigger","sprayerduster",
                                     "motortruck","automobile","nbfarms_cropdrier")

#data_1959_part1 <- subset(data_1959_part1, select= -c(hopsac,hops)) 
data_1959_part2 <- subset(data_1959_part2, select=c("STATEICP","COUNICP","FIPS","LEVEL",
                                                    "ITEM951","ITEM952","ITEM958","ITEM962","ITEM963",
                                                    "ITEM964","ITEM965","ITEM967"))

colnames(data_1959_part2)[5:12] <- c("chemac_spray","chemac_insects","chemac_weeds","chemval_insects_hay","chemval_insects_ot",
                                     "chemval_nematodes","chemval_diseases","chemval_weeds")
data_1959 <- left_join(data_1959_part1,data_1959_part2, by = c("STATE" = "STATEICP","COUNTY"="COUNICP"))


data_1959 <- setDT(data_1959)
data_1959 <- data_1959[,farmval:=farms*avg_farmval]
data_1959 <- data_1959[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cloverac1","cloverac2",
                                                                                "cloverac3","cloverac4","cloverac5",
                                                                                "cloverac6")]
data_1959 <- data_1959[,clover:=rowSums(.SD,na.rm=TRUE),.SDcols=c("clover1","clover2",
                                                                    "clover3","clover4","clover5",
                                                                    "clover6")]
data_1959 <- data_1959[,pepperac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pepperac1","pepperac2",
                                                                  "pepperac3")]
data_1959 <- data_1959[,chemval_crop:=rowSums(.SD,na.rm=TRUE),.SDcols=c("chemval_insects_hay","chemval_insects_ot",
                                                                    "chemval_nematodes","chemval_diseases",
                                                                    "chemval_weeds")]

# Remove columns and change names to lower
colstoremove <- c("avg_farmval","cloverac1","cloverac2",
                  "cloverac3","cloverac4","cloverac5",
                  "cloverac6","clover1","clover2",
                  "clover3","clover4","clover5",
                  "clover6","pepperac1","pepperac2",
                  "pepperac3","chemval_insects_hay","chemval_insects_ot",
                  "chemval_nematodes","chemval_diseases",
                  "chemval_weeds","tract_wheel","tract_garden","tract_crawl")
data_1959 <- data_1959[,(colstoremove):=NULL]
names(data_1959) <- tolower(names(data_1959))

rm(data_1959_part1, data_1959_part2)

# match to common geo identifier
data_1959 <- data_1959 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1959$year <- 1959

# Keep County-Level Observations
data_1959 <- data_1959[data_1959$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1959
crosswalkuse <- semi_join(crosswalk,data_1959,by=c("beginning_FIPS"="fips","year"="year"))
data_1959 <- left_join(crosswalkuse,data_1959,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1959 <- data_1959[which(data_1959$year==1959),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1959)[!names(data_1959)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1959_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1959",weightname="weight_agarea1"))
data_1959_part1 <- data_1959_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1959_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1959",weightname="weight_agarea2"))
data_1959_part2 <- data_1959_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1959 <- left_join(data_1959_part1,data_1959_part2,by=c("base_FIPS","year"))
rm(data_1959_part1,data_1959_part2)
names(data_1959) <- tolower(names(data_1959))


####### ***     1964   *** #######

# cropval is "All crops sold, $"
# garden tractors (interpreted as for lawn) - John Deere introduced its first model in 1963 https://blog.machinefinder.com/26612/history-john-deere-riding-mower-1960s-to-present
# TO BE compared with # of tractors - garden tractors for previous years (and probably ok for the very early years to just use # tractors)
# here tract_garden correponds to garden tractors & motor tillers
# for acres=> chemac_spray (acres for spray, dust and fumigants) // chemac_insects (acres for chemicals on insects and diseases)
# chemac_weeds (acres - chem for weeds or grass or bush (is the bush part new?))
# We have the same for value with chemval_"" -- chemval_weeds does not include value used for pasture - sum is chemval_crops for all crop-related expenses
# machine hire -> expenses for machine hire, custom and contract work
# corn silage - weight is green weight in tons
# rice is all type of rice (one category)
# peas in pounds, beans in bushels of dry field and seed, sorghum bushels
# apples/apricots/peaches/pears/plums bushels, cherries/almonds/figs/lemons/lime is lbs, 
# olives/oranges/grapefruits in lbs
# blackberries/raspberries/strawberries in quarts, cranberries in lbs

data_1964_part1 <- read_dta(file="DS0030/35206-0030-Data.dta")
data_1964_part2 <- read_dta(file="DS0031/35206-0031-Data.dta") # chemicals
data_1964_part3 <- read_dta(file="DS0032/35206-0032-Data.dta") # horsemu

data_1964_part1 <- subset(data_1964_part1, select=c("STATE","COUNTY",
                                                    "VAR1","VAR4","VAR5","VAR6","VAR50",
                                                    "VAR375","VAR466","VAR470","VAR472","VAR474",
                                                    "VAR476","VAR482","VAR488","VAR534","VAR663",
                                                    "VAR675","VAR677","VAR860","VAR861","VAR867",
                                                    "VAR868","VAR907","VAR908","VAR938","VAR939",
                                                    "VAR977","VAR978","VAR993","VAR994","VAR1005",
                                                    "VAR1007","VAR1016","VAR1017","VAR1026","VAR1027",
                                                    "VAR1036","VAR1037","VAR1199","VAR1200","VAR1219",
                                                    "VAR1220","VAR1223","VAR1224","VAR1231","VAR1232",
                                                    "VAR1239","VAR1240","VAR1242","VAR1243","LEVEL",
                                                    "VAR1215","VAR1217","VAR1204","VAR1205","VAR1111",
                                                    "VAR1112","VAR1117","VAR1118","VAR1120","VAR1121",
                                                    "VAR1123","VAR1124","VAR1126","VAR1127","VAR1129",
                                                    "VAR1130","VAR883","VAR884","VAR928","VAR929",
                                                    "VAR1416","VAR1423","VAR1586","VAR1593","VAR1560",
                                                    "VAR1567","VAR1427","VAR1434","VAR1460","VAR1467",
                                                    "VAR1529","VAR1536","VAR1695","VAR1702","VAR1837",
                                                    "VAR1841","VAR1616","VAR1623","VAR1802","VAR1809",
                                                    "VAR1812","VAR1818","VAR1635","VAR1642","VAR1747",
                                                    "VAR1645","VAR1651","VAR1286","VAR1287","VAR1731",
                                                    "VAR1738","VAR1108","VAR1109","VAR1226","VAR1227",
                                                    "VAR1393","VAR1394","VAR1383","VAR1384","VAR1378",
                                                    "VAR1379","VAR1410","VAR1411","VAR1354","VAR1021",
                                                    "VAR1022","VAR1031","VAR1032","VAR1048","VAR1049",
                                                    "VAR1273","VAR1274","VAR1184","VAR1185","VAR1328",
                                                    "VAR1334","VAR1350","VAR1358","VAR1330","VAR1342",
                                                    "VAR1352","VAR1372","VAR1346","VAR1304","VAR1344",
                                                    "VAR1332","VAR1324","VAR1326","VAR1338","VAR1312",
                                                    "VAR1363","VAR1336","VAR1348","VAR1340","VAR1322",
                                                    "VAR1300","VAR1362","VAR1356","VAR1751","VAR1762",
                                                    "VAR1781","VAR1791","VAR484","VAR462","VAR464",
                                                    "VAR486","VAR490","VAR492"))

colnames(data_1964_part1)[3:160] <- c("farms","totacres","avgfarmsize","avg_farmval","irrac",
                                   "cropval","tract_num_nongarden","tract_wheel","tract_crawl","tract_garden",
                                    "combine_num","baler_num","picker_num","fert_wt","farmfert",
                                    "machine_hire","farmwages","cornac_grain","corn_grain","cornac_silage",
                                    "corn_silage","soybeanac_bean","soybean_bean","wheatac","wheat",
                                    "oatsac","oats","barleyac","barley","riceac",
                                    "rice","ryeac","rye","buckwheatac","buckwheat",
                                    "flaxac","flax","irpotatac","irpotato","swpotatac",
                                    "swpotato","cottonac","cotton","canesugac","canesug",
                                    "hopsac","hops","tobaccoac","tobacco","LEVEL",
                                    "peasac","peas","beansac","beans","cloverac1",
                                    "clover1","cloverac2","clover2","cloverac3","clover3",
                                   "cloverac4","clover4","cloverac5","clover5","cloverac6",
                                   "clover6","sorghumgac","sorghumg","peanutac","peanuts",
                                   "appleac","apples","apricotac","apricots","cherryac",
                                   "cherries","peachac","peaches","pearac","pears",
                                   "plumac","plums","almondac","almonds","bananaac",
                                   "bananas","figac","figs","lemonac","lemons",
                                   "limetree","limes","oliveac","olives","oranges",
                                   "pecantree","pecans","pineappleac","pineapples","pomeloac",
                                   "pomeloes","alfalfac","alfalfa","SBEETAC","SBEETWT",
                                   "BLACKBAC","BLACKBER","RASPBAC","RASPBER","STRAWBAC",
                                   "STRAWBER","CRANBAC","CRANBER","escaroleac","emmerspletac",
                                   "emmersplet","milletac","millet","mustardac","mustard",
                                   "sunflowerac","sunflower","vetchac","vetch","asparagusac",
                                   "beetac","broccoliac","cantaloupac","carrotac","cauliflowerac",
                                   "celeryac","ccabbageac","collardac","cucumberac","eggplantac",
                                   "lettuceromaineac","okraac","oniondac","oniongac","pepperac1",
                                   "pepperac2","pumpkinac","radishac","spinachac","squashac",
                                   "tomatoac","turnipac","watermelonac","orangeac1","orangeac2",
                                   "orangeac3","orangeac4","mowerconditioner","automobile","motortruck",
                                   "cropdrier","forageharvester1","forageharvester2")

data_1964_part2 <- subset(data_1964_part2, select=c("STATEICP","COUNICP","FIPS",
                                                    "ITEM951","ITEM952","ITEM957","ITEM962","ITEM963",
                                                    "ITEM964","ITEM965","ITEM967")) #,"ITEM744A" ITEM744 (empty)
colnames(data_1964_part2)[4:11] <- c("chemac_spray","chemac_insects","chemac_weeds","chemval_insects_hay","chemval_insects_ot",
                                     "chemval_nematodes","chemval_diseases","chemval_weeds") # ,"horsemu" 

#data_1964_part3 <- subset(data_1964_part3, select=c("STATE","COUNTY","VAR204"))
#colnames(data_1964_part3)[3] <- c("horsemu")

data_1964 <- left_join(data_1964_part1,data_1964_part2, by = c("STATE"="STATEICP","COUNTY"="COUNICP"))
#data_1964 <- left_join(data_1964,data_1964_part3, by = c("STATE"="STATE","COUNTY"="COUNTY"))

data_1964 <- setDT(data_1964)
data_1964 <- data_1964[,farmval:=farms*avg_farmval]
data_1964 <- data_1964[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cloverac1","cloverac2",
                                                                    "cloverac3","cloverac4","cloverac5",
                                                                    "cloverac6")]
data_1964 <- data_1964[,tract_num:=rowSums(.SD,na.rm=TRUE),.SDcols=c("tract_num_nongarden","tract_garden")] # note that tract garden contains moto tillers as well
data_1964 <- data_1964[,clover:=rowSums(.SD,na.rm=TRUE),.SDcols=c("clover1","clover2","clover3",
                                                                  "clover4","clover5","clover6")]
data_1964 <- data_1964[,chemval_crop:=rowSums(.SD,na.rm=TRUE),.SDcols=c("chemval_insects_hay",
                                                                        "chemval_insects_ot",
                                                                        "chemval_nematodes",
                                                                        "chemval_diseases",
                                                                        "chemval_weeds")]
data_1964 <- data_1964[,pepperac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pepperac1","pepperac2")]
data_1964 <- data_1964[,orangeac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("orangeac1","orangeac2","orangeac3",
                                                                    "orangeac4")]
data_1964 <- data_1964[,orangeac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("forageharvester1","forageharvester2")]


# Remove columns and change names to lower
colstoremove <- c("cloverac1","cloverac2",
                  "cloverac3","cloverac4","cloverac5",
                  "cloverac6","tract_num_nongarden","tract_garden",
                  "clover1","clover2","clover3",
                  "clover4","clover5","clover6",
                  "chemval_insects_hay","chemval_insects_ot",
                  "chemval_nematodes","chemval_diseases",
                  "chemval_weeds","pepperac1","pepperac2","tract_wheel","tract_crawl",
                  "orangeac1","orangeac2","orangeac3",
                  "orangeac4")
data_1964 <- data_1964[,(colstoremove):=NULL]
names(data_1964) <- tolower(names(data_1964))

rm(data_1964_part1, data_1964_part2, data_1964_part3)

# match to common geo identifier
data_1964 <- data_1964 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1964$year <- 1964

# Keep County-Level Observations
data_1964 <- data_1964[data_1964$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1964
crosswalkuse <- semi_join(crosswalk,data_1964,by=c("beginning_FIPS"="fips","year"="year"))
data_1964 <- left_join(crosswalkuse,data_1964,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1964 <- data_1964[which(data_1964$year==1964),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1964)[!names(data_1964)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1964_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1964",weightname="weight_agarea1"))
data_1964_part1 <- data_1964_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1964_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1964",weightname="weight_agarea2"))
data_1964_part2 <- data_1964_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1964 <- left_join(data_1964_part1,data_1964_part2,by=c("base_FIPS","year"))
rm(data_1964_part1,data_1964_part2)
names(data_1964) <- tolower(names(data_1964))


####### ***     1969   *** #######


# machine hire -> contract labor, machine hire and custom work
# tractor - no more one gen. category - no garden tractors anymore
# combines are now only self-propelled
# 2 cat for pickers now: corn heads for combines, and other cornpickers and picker shellers
# horse is horse & ponies - no mules
# corn for grain is now for grain and seed -- silage is for silage or green chop
# beans are dry field and seed including lima

data_1969_part1 <- read_dta(file="DS0033/35206-0033-Data.dta")
data_1969_part2 <- read_dta(file="DS0035/35206-0035-Data.dta")
data_1969_part3 <- read_dta(file="DS0034/35206-0034-Data.dta") # farms making 2,5k or more a year only 
data_1969_part1 <- subset(data_1969_part1, select=c("STATE","COUNTY","FIPS","LEVEL",
                                                    "ITEM01001","ITEM01002","ITEM01003","ITEM01007","ITEM01057",
                                                    "ITEM03015","ITEM05029","ITEM05045","ITEM05049","ITEM06002",
                                                    "ITEM06019","ITEM06022","ITEM06025","ITEM06037","ITEM06028",
                                                    "ITEM06031","ITEM08029","ITEM10008","ITEM10009","ITEM10014",
                                                    "ITEM10015","ITEM10020","ITEM10021","ITEM10023","ITEM10024",
                                                    "ITEM10026","ITEM10027","ITEM10029","ITEM10030","ITEM10047",
                                                    "ITEM10048","ITEM10052","ITEM10053","ITEM10008","ITEM10009",
                                                    "ITEM10011","ITEM10012"))

colnames(data_1969_part1)[5:41] <- c("farms","totacres","avgfarmsize","avg_farmval","irrac",
                                     "cropval","farmfert","farmwages","machine_hire","equipval",
                                     "tract_wheel","tract_crawl","combine_num","baler_num","picker_num1",
                                     "picker_num2","horse","soybeanac_bean","soybean","wheatac",
                                     "wheat","cottonac","cotton","tobaccoac","tobacco",
                                     "irpotatac","irpotato","swpotatac","swpotato","cornac_grain",
                                     "corn_grain","cornac_silage","corn_silage","soyebeanac_bean","soybean_bean",
                                     "peanutac","peanuts")
data_1969_part1 <- subset(data_1969_part1, select=-c(tract_wheel,tract_crawl,combine_num,baler_num,picker_num1,
                                                    picker_num2,horse))

data_1969_part2 <- subset(data_1969_part2, select=c("STATEICP","COUNICP",
                                                    "ITEM119","ITEM120","ITEM121","ITEM122","ITEM124",
                                                    "ITEM187","ITEM419","ITEM420","ITEM421","ITEM422",
                                                    "ITEM424","ITEM487","ITEM951","ITEM952","ITEM957",
                                                    "ITEM962","ITEM963","ITEM964","ITEM965","ITEM967",
                                                    "ITEM183","ITEM483","ITEM481","ITEM180","ITEM192",
                                                    "ITEM492","ITEM106","ITEM406","ITEM161","ITEM461",
                                                    "ITEM464","ITEM164","ITEM162","ITEM462","ITEM163",
                                                    "ITEM463","ITEM165","ITEM165","ITEM157","ITEM457",
                                                    "ITEM152","ITEM452","ITEM158","ITEM458","ITEM153",
                                                    "ITEM453","ITEM191","ITEM491","ITEM184","ITEM484",
                                                    "ITEM169","ITEM469","ITEM125","ITEM425","ITEM188",
                                                    "ITEM488","ITEM147","ITEM148","ITEM142",'ITEM146',
                                                    "ITEM140","ITEM143"))

colnames(data_1969_part2)[3:64] <- c("oatsac","barleyac","ryeac","riceac","flaxac",
                                     "canesugac","oats","barley","rye","rice",
                                     "flax","cansug","chemac_spray","chemac_insects","chemac_weeds",
                                     "chemval_insects_hay","chemval_insects_ot","chemval_nematodes","chemval_diseases","chemval_weeds",
                                     "peasac","peas","beans","beansac","cloverac1",
                                     "clover1","sorghumgac","sorghumg","appleac","apples",
                                     "cherries","cherryac","peachac","peaches","pearac",
                                     "pears","plumac","plums","almondac","almonds",
                                     "orangeac","oranges","pecanac","pecans","pomeloac",
                                     "pomeloes","alfalfac","alfalfa","SBEETAC","SBEETWT",
                                     "STRAWBAC","STRAWBER","milletac","millet","sunflowerac",
                                     "sunflower","asparagusac","cantaloupac","cucumberac","lettuceromaineac",
                                     "tomatoac","watermelonac")

data_1969_part3 <- subset(data_1969_part3, select=c("STATE","COUNTY",
                                                    "ITEM06019","ITEM06022","ITEM06025","ITEM06037","ITEM06031",
                                                    "ITEM06028","ITEM06040","ITEM06034","ITEM08030","ITEM06013",
                                                    "ITEM06016","ITEM06043"))
colnames(data_1969_part3)[3:12] <- c("tract_wheel","tract_crawl","combine_num","baler_num","picker_num1",
                                   "picker_num2","windrower","mowerconditioner","horsemu","automobile",
                                   "motortruck","forageharvester")

data_1969 <- left_join(data_1969_part1,data_1969_part2,by=c("STATE"="STATEICP","COUNTY"="COUNICP"))
data_1969 <- left_join(data_1969,data_1969_part3,by=c("STATE","COUNTY"))
data_1969 <- setDT(data_1969)
data_1969 <- data_1969[,farmval:=farms*avg_farmval]
data_1969 <- data_1969[,picker_num:=rowSums(.SD,na.rm=TRUE),.SDcols=c("picker_num1","picker_num2")]
data_1969 <- data_1969[,chemval_crop:=rowSums(.SD,na.rm=TRUE),.SDcols=c("chemval_insects_hay",
                                                                        "chemval_insects_ot",
                                                                        "chemval_nematodes",
                                                                        "chemval_diseases",
                                                                        "chemval_weeds")]
data_1969 <- data_1969[,tract_num:=rowSums(.SD,na.rm=TRUE),.SDcols=c("tract_wheel","tract_crawl")] # no garden tractors here 
data_1969 <- data_1969[,cropval:=cropval*1000]
data_1969 <- data_1969[,farmfert:=farmfert*1000]
data_1969 <- data_1969[,farmwages:=farmwages*1000]
data_1969 <- data_1969[,machine_hire:=machine_hire*1000]
data_1969 <- data_1969[,equipval:=equipval*1000]

# Remove columns and change names to lower
colstoremove <- c("picker_num1","picker_num2","chemval_insects_hay",
                  "chemval_insects_ot","chemval_nematodes",
                  "chemval_diseases","chemval_weeds",
                  "tract_wheel","tract_crawl")
data_1969 <- data_1969[,(colstoremove):=NULL]
names(data_1969) <- tolower(names(data_1969))
rm(data_1969_part1, data_1969_part2,data_1969_part3)

# match to common geo identifier
data_1969 <- data_1969 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1969$year <- 1969

# Keep County-Level Observations
data_1969 <- data_1969[data_1969$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1969
crosswalkuse <- semi_join(crosswalk,data_1969,by=c("beginning_FIPS"="fips","year"="year"))
data_1969 <- left_join(crosswalkuse,data_1969,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1969 <- data_1969[which(data_1969$year==1969),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1969)[!names(data_1969)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1969_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1969",weightname="weight_agarea1"))
data_1969_part1 <- data_1969_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1969_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1969",weightname="weight_agarea2"))
data_1969_part2 <- data_1969_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1969 <- left_join(data_1969_part1,data_1969_part2,by=c("base_FIPS","year"))
rm(data_1969_part1,data_1969_part2)
names(data_1969) <- tolower(names(data_1969))


####### ***     1974   *** #######


# cropval includes hay here
# add farmwages_contract for expenses for contract labor
# machine hire: contract labor, machine hire, custom work
# combines are self-propelled for grain and beans
# 2 cat for pickers now: corn heads for combines, and other cornpickers and picker shellers
# horse is horse & ponies - no mules
# wheat is wheat without more precisions
# peas is dry field and seed, beans is id dry field and seed including lima

data_1974_part1 <- read_dta(file="DS0036/35206-0036-Data.dta")
data_1974_part2 <- read_dta(file="DS0038/35206-0038-Data.dta")

data_1974_part1 <- subset(data_1974_part1, select=c("STATE","COUNTY","FIPS","LEVEL","STATEFIP",
                                                    "COUNFIP",
                                                    "ITEM01001","ITEM01002","ITEM01003","ITEM01006","ITEM01057",
                                                    "ITEM03012","ITEM03015","ITEM05029","ITEM05045","ITEM05047",
                                                    "ITEM05049","ITEM06002","ITEM06019","ITEM06022","ITEM06025",
                                                    "ITEM06028","ITEM06031","ITEM06037","ITEM08030","ITEM10008",
                                                    "ITEM10009","ITEM10014","ITEM10015","ITEM10020","ITEM10021",
                                                    "ITEM10023","ITEM10024","ITEM10026","ITEM10027","ITEM10029",
                                                    "ITEM10030","ITEM10047","ITEM10048","ITEM10052","ITEM10053",
                                                    "ITEM05047","ITEM06040","ITEM06034","ITEM06013","ITEM06016",
                                                    "ITEM06043"))

colnames(data_1974_part1)[7:47] <- c("farms","totacres","avgfarmsize","farmval","irrac",
                                     "totval","cropval","farmfert","farmwages","farmwages_contract",
                                     "machine_hire","equipval","tract_wheel","tract_crawl","combine_num",
                                     "picker_num1","picker_num2","baler_num","horse","soybeanac_bean",
                                     "soybean_bean","wheatac","wheat","cottonac","cotton",
                                     "tobaccoac","tobacco","irpotatac","irpotato","swpotatac",
                                     "swpotato","cornac_grain","corn_grain","cornac_silage","corn_silage",
                                     "hire_contract","windrower","mowerconditioner","automobile","motortruck",
                                     "forageharvester")

data_1974_part2 <- subset(data_1974_part2, select=c("STATEICP","COUNICP",
                                                    "ITEM119","ITEM120","ITEM121","ITEM122","ITEM124",
                                                    "ITEM187","ITEM419","ITEM420","ITEM421","ITEM422",
                                                    "ITEM424","ITEM487","ITEM951","ITEM952","ITEM957",
                                                    "ITEM962","ITEM963","ITEM964","ITEM965","ITEM967",
                                                    "ITEM183","ITEM483","ITEM180","ITEM481","ITEM192",
                                                    "ITEM492","ITEM106","ITEM406","ITEM174","ITEM474",
                                                    "ITEM161","ITEM461","ITEM164","ITEM464","ITEM162",
                                                    "ITEM462","ITEM163","ITEM463","ITEM165","ITEM465",
                                                    "ITEM157","ITEM457","ITEM152","ITEM452","ITEM158",
                                                    "ITEM458","ITEM153","ITEM453","ITEM191","ITEM491",
                                                    "ITEM184","ITEM484","ITEM169","ITEM469","ITEM125",
                                                    "ITEM425","ITEM188","ITEM488","ITEM147","ITEM148",
                                                    "ITEM142","ITEM146","ITEM140","ITEM143"))

colnames(data_1974_part2)[3:66] <- c("oatsac","barleyac","ryeac","riceac","flaxac",
                                     "canesugac","oats","barley","rye","rice",
                                     "flax","cansug","chemac_spray","chemac_insects","chemac_weeds",
                                     "chemval_insects_hay","chemval_insects_ot","chemval_nematodes","chemval_diseases","chemval_weeds",
                                     "peasac","peas","beansac","beans","cloverac1",
                                     "clover1","sorghumgac","sorghumg","peanutac","peanuts",
                                     "appleac","apples","cherries","cherryac","peachac",
                                     "peaches","pearac","pears","plumac","plums",
                                     "almondac","almonds","orangeac","oranges","pecanac",
                                     "pecans","pomeloac","pomeloes","alfalfac","alfalfa",
                                     "SBEETAC","SBEETWT","STRAWBAC","STRAWBER","milletac",
                                     "millet","sunflowerac","sunflower","asparagusac","cantaloupac",
                                     "cucumberac","lettuceromaineac","tomatoac","watermelonac")

data_1974 <- left_join(data_1974_part1,data_1974_part2, by = c("STATE" = "STATEICP","COUNTY"="COUNICP"))
data_1974 <- setDT(data_1974)
data_1974 <- data_1974[,`:=`(farmval=farmval*1000,
                             cropval=cropval*1000,
                             farmfert=farmfert*1000,
                             farmwages=farmwages*1000,
                             machine_hire=machine_hire*1000,
                             hire_contract=hire_contract*1000,
                             equipval=equipval*1000,
                             horsemu=horse)]
data_1974 <- data_1974[,hire_contract:=-hire_contract]
data_1974 <- data_1974[,machine_hire_sub:=rowSums(.SD,na.rm=TRUE),.SDcols=c("machine_hire","hire_contract")]
data_1974 <- data_1974[,chemval_crop:=rowSums(.SD,na.rm=TRUE),.SDcols=c("chemval_insects_hay","chemval_insects_ot",
                                                                         "chemval_nematodes","chemval_diseases",
                                                                         "chemval_weeds")]
data_1974 <- data_1974[,picker_num:=rowSums(.SD,na.rm=TRUE),.SDcols=c("picker_num1","picker_num2")]
data_1974 <- data_1974[,tract_num:=rowSums(.SD,na.rm=TRUE),.SDcols=c("tract_wheel","tract_crawl")]
data_1974 <- data_1974[machine_hire_sub<0,machine_hire_sub:=0]


# Remove columns and change names to lower
colstoremove <- c("machine_hire","hire_contract",
                  "chemval_insects_hay","chemval_insects_ot",
                  "chemval_nematodes","chemval_diseases",
                  "chemval_weeds","picker_num1","picker_num2",
                  "tract_wheel","tract_crawl","cloverac1","clover1","horse")
data_1974 <- data_1974[,(colstoremove):=NULL]
names(data_1974) <- tolower(names(data_1974))
rm(data_1974_part1, data_1974_part2)

# Keep ratio of machine_hire_sub over total cat. 
check <- data.table(cbind(data_1974$machine_hire,data_1974$machine_hire_sub))

# match to common geo identifier
data_1974 <- data_1974 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1974$year <- 1974

# Keep County-Level Observations
data_1974 <- data_1974[data_1974$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1974
crosswalkuse <- semi_join(crosswalk,data_1974,by=c("beginning_FIPS"="fips","year"="year"))
data_1974 <- left_join(crosswalkuse,data_1974,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1974 <- data_1974[which(data_1974$year==1974),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1974)[!names(data_1974)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1974_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1974",weightname="weight_agarea1"))
data_1974_part1 <- data_1974_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1974_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1974",weightname="weight_agarea2"))
data_1974_part2 <- data_1974_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1974 <- left_join(data_1974_part1,data_1974_part2,by=c("base_FIPS","year"))
rm(data_1974_part1,data_1974_part2)
names(data_1974) <- tolower(names(data_1974))


####### ***     1978   *** #######


# chemval for total expenses for agricultural chemicals - not comparable to the sum of previously kept var on this as they were a subset
# farmrepair for repair and maintenance costs
# machine_hire_sub: machine hire and rental of machinery and equipment - no contract or specialized work here
# for chemicals : we add chemicals for insects, nematodes and chemicals for diseases to get the old chemac_insects
# peas are dry edible, beans are dry including lima
# escarole as sum of escarole, chicory and endive

data_1978 <- read_dta(file="DS0039/35206-0039-Data.dta")

data_1978 <- subset(data_1978, select=c("STATE","COUNTY","FIPS","LEVEL","STATEFIP",
                                        "COUNFIP",
                                        "ITEM01001","ITEM01002","ITEM01003","ITEM01004","ITEM01006",
                                        "ITEM01018","ITEM01019","ITEM01021","ITEM01056","ITEM01057",
                                        "ITEM01059","ITEM01060","ITEM01065","ITEM01066","ITEM01068",
                                        "ITEM01069","ITEM01071","ITEM01072","ITEM01074","ITEM01075",
                                        "ITEM01080","ITEM01081","ITEM01083","ITEM01084","ITEM01086",
                                        "ITEM01087","ITEM01092","ITEM01093","ITEM01098","ITEM01099",
                                        "ITEM03013","ITEM03015","ITEM03029","ITEM03031","ITEM03033",
                                        "ITEM03035","ITEM08017","ITEM08023","ITEM08029","ITEM09011",
                                        "ITEM09013","ITEM09015","ITEM09017","ITEM13018","ITEM24022",
                                        "ITEM24023","ITEM24032","ITEM24033","ITEM24067","ITEM24068",
                                        "ITEM25047","ITEM25048","ITEM23002",
                                        "ITEM25027","ITEM25028","ITEM25017","ITEM25018","ITEM26012",
                                        "ITEM26013","ITEM26047","ITEM26048","ITEM26067","ITEM26068",
                                        "ITEM26082","ITEM26083","ITEM26102","ITEM26103","ITEM26122",
                                        "ITEM26123","ITEM01062","ITEM01063","ITEM01104","ITEM01105",
                                        "ITEM28011","ITEM28018","ITEM28020","ITEM28027","ITEM28047",
                                        "ITEM28054","ITEM28182","ITEM28189","ITEM28191","ITEM28198",
                                        "ITEM28209","ITEM28216","ITEM28326","ITEM28333","ITEM28038",
                                        "ITEM28045","ITEM28101","ITEM28108","ITEM28119","ITEM28126",
                                        "ITEM28263","ITEM28270","ITEM09008","ITEM09009","ITEM28155",
                                        "ITEM28162","ITEM28281","ITEM28288",'ITEM28353',"ITEM28360",
                                        "ITEM01101","ITEM01102","ITEM28245","ITEM28252","ITEM26007",
                                        "ITEM26008","ITEM01095","ITEM01096","ITEM29007","ITEM29008",
                                        "ITEM29032","ITEM29033","ITEM29042","ITEM29043","ITEM29047",
                                        "ITEM29048","ITEM29027","ITEM29028","ITEM27062","ITEM27090",
                                        "ITEM27094","ITEM24027","ITEM24028","ITEM24057","ITEM24058",
                                        "ITEM24042","ITEM24043","ITEM01077","ITEM01078","ITEM26112",
                                        "ITEM26113","ITEM28335","ITEM28242","ITEM27006","ITEM27010",
                                        "ITEM27022","ITEM27026","ITEM27030","ITEM27038","ITEM27042",
                                        "ITEM27046","ITEM27050","ITEM27054","ITEM27058","ITEM27034",
                                        "ITEM27070","ITEM27194","ITEM27078","ITEM27086","ITEM27098",
                                        "ITEM27106","ITEM27110","ITEM27126","ITEM27118","ITEM27122",
                                        "ITEM27138","ITEM27142","ITEM27146","ITEM27150","ITEM27154",
                                        "ITEM27158","ITEM27170","ITEM27178","ITEM27182","ITEM27198",
                                        "ITEM31067","ITEM31068","ITEM25022","ITEM25023","ITEM08025",
                                        "ITEM08027","ITEM08015"))

colnames(data_1978)[7:181] <- c("farms","totacres","avgfarmsize","avg_farmval","avg_equipval",
                               "irrac","totval","cropval","cornac_grain","corn_grain",
                               "cornac_silage","corn_silage","wheatac","wheat","barleyac",
                               "barley","oatsac","oats","riceac","rice",
                               "cottonac","cotton","tobaccoac","tobacco","soybeanac_bean",
                               "soybean_bean","irpotatac","irpotato","canesugac","canesug",
                               "farmfert","chemval","farmwages","farmwages_contract","farmrepair",
                               "machine_hire_sub","tract_wheel","combine_num","baler_num","chemac_insects1",
                               "chemac_nematodes","chemac_diseases","chemac_weeds","horse","buckwheatac",
                               "buckwheat","flaxac","flax","ryeac","rye",
                               "swpotatac","swpotato","mule",
                               "peasac","peas","beansac","beans","cloverac1",
                               "clover1","cloverac2","clover2","cloverac3","clover3",
                               "cloverac4","clover4","cloverac5","clover5","cloverac6",
                               "clover6","sorghumgac","sorghumg","peanutac","peanuts",
                               "appleac","apples","apricotac","apricots","cherryac",
                               "cherries","peachac","peaches","pearac","pears",
                               "plumac","plums","almondac","almonds","bananaac",
                               "bananas","figac","figs","guavaac","guavas",
                               "lemonac","lemons","limeac","limes","oliveac",
                               "olives","orangeac","oranges","pecanac","pecans",
                               "pineappleac","pineapples","pomeloac","pomeloes","alfalfac",
                               "alfalfa","SBEETAC","SBEETWT","BLACKBAC","BLACKBER",
                               "curranac","currants","RASPBAC","RASPBER","STRAWBAC",
                               "STRAWBER","CRANBAC","CRANBER","escaroleac1","escaroleac2",
                               "escaroleac3","emmerspletac","emmersplet","milletac","millet",
                               "mustardac","mustard","sunflowerac","sunflower","vetchac",
                               "vetch","hazelnultac","hazelnults","artichokeac","asparagusac",
                               "beetac","broccoliac","brusselac","cabbageac1","cabbageac2",
                               "cantaloupac","carrotac","cauliflowerac","celeryac","ccabbageac",
                               "collardac","watercressac","cucumberac","eggplantac","garlicac",
                               "kaleac","lettuceromaineac","okraac","oniondac","oniongac",
                               "pepperac1","pepperac2","pepperac3","pumpkinac","radishac",
                               "rhubarbac","squashac","tomatoac","turnipac","watermelonac",
                               "canolaac","canola","limaac","lima","cottonstripper",
                               "mowerconditioner","motortruck")

data_1978 <- setDT(data_1978)
data_1978 <- data_1978[,`:=`(farmval=farms*avg_farmval,
                             equipval=farms*avg_equipval,
                             totval=totval*1000,
                             cropval=cropval*1000,
                             farmfert=farmfert*1000,
                             farmwages=farmwages*1000,
                             farmwages_contract=farmwages_contract*1000,
                             farmrepair=farmrepair*1000,
                             machine_hire_sub=machine_hire_sub*1000,
                             tract_num=tract_wheel,
                             horsemu=horse+mule)]
data_1978 <- data_1978[,ID:=.I]
data_1978 <- data_1978[,chemac_insects:=max(na.rm=T,.SD),.SDcols=c("chemac_insects1","chemac_nematodes",
                                                                          "chemac_diseases"),by=ID]
data_1978 <- data_1978[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horse","mule")]
data_1978 <- data_1978[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cloverac1","cloverac2",
                                                                    "cloverac3","cloverac4",
                                                                    "cloverac5","cloverac6")]
data_1978 <- data_1978[,clover:=rowSums(.SD,na.rm=TRUE),.SDcols=c("clover1","clover2","clover3",
                                                                  "clover4","clover5","clover6")]
data_1978 <- data_1978[,escaroleac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("escaroleac1","escaroleac2","escaroleac3")]
data_1978 <- data_1978[,cabbageac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cabbageac1","cabbageac2")]
data_1978 <- data_1978[,pepperac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("pepperac1","pepperac2","pepperac3")]
data_1978 <- data_1978[,beansac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beansac","limaac")]
data_1978 <- data_1978[,beans:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beans","lima")]

# Remove columns and change names to lower
colstoremove <- c("avg_farmval","avg_equipval",
                  "chemac_insects1","chemac_nematodes",
                  "chemac_diseases","horse","mule",
                  "cloverac1","cloverac2",
                  "cloverac3","cloverac4",
                  "cloverac5","cloverac6",
                  "clover1","clover2","clover3",
                  "clover4","clover5","clover6",
                  "escaroleac1","escaroleac2","escaroleac3",
                  "cabbageac1","cabbageac2",
                  "pepperac1","pepperac2","pepperac3",
                  "beansac","limaac","beans","lima","ID","tract_wheel")
data_1978 <- data_1978[,(colstoremove):=NULL]
names(data_1978) <- tolower(names(data_1978))

# match to common geo identifier
data_1978 <- data_1978 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1978$year <- 1978

# Keep County-Level Observations
data_1978 <- data_1978[data_1978$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1978
crosswalkuse <- semi_join(crosswalk,data_1978,by=c("beginning_FIPS"="fips","year"="year"))
data_1978 <- left_join(crosswalkuse,data_1978,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1978 <- data_1978[which(data_1978$year==1978),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1978)[!names(data_1978)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1978_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1978",weightname="weight_agarea1"))
data_1978_part1 <- data_1978_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1978_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1978",weightname="weight_agarea2"))
data_1978_part2 <- data_1978_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1978 <- left_join(data_1978_part1,data_1978_part2,by=c("base_FIPS","year"))
rm(data_1978_part1,data_1978_part2)
names(data_1978) <- tolower(names(data_1978))


####### ***     1982   *** #######


# beans is dry beans with lima, peas is dry edible peas

data_1982 <- read_dta(file="DS0040/35206-0040-Data.dta")

data_1982 <- subset(data_1982, select=c("STATE","COUNTY","FIPS","LEVEL","STATEFIP",
                                        "COUNFIP",
                                        "ITEM01001","ITEM01002","ITEM01003","ITEM01004","ITEM01006",
                                        "ITEM01019","ITEM01021","ITEM01056","ITEM01057","ITEM01059",
                                        "ITEM01060","ITEM01065","ITEM01066","ITEM01068","ITEM01069",
                                        "ITEM01071","ITEM01072","ITEM01074","ITEM01075","ITEM01080",
                                        "ITEM01081","ITEM01083","ITEM01084","ITEM01086","ITEM01087",
                                        "ITEM01092","ITEM01093","ITEM01098","ITEM01099","ITEM03013",
                                        "ITEM03015","ITEM03029","ITEM03031","ITEM03033","ITEM03035",
                                        "ITEM08017","ITEM08023","ITEM08029","ITEM09011","ITEM09013",
                                        "ITEM09015","ITEM09017","ITEM13018","ITEM23002","ITEM24022",
                                        "ITEM24023","ITEM24032","ITEM24033","ITEM24067","ITEM24068",
                                        "ITEM25047","ITEM25048","ITEM01018",
                                        "ITEM25017","ITEM25018","ITEM25027","ITEM25028","ITEM26012",
                                        "ITEM26013","ITEM26047","ITEM26048","ITEM26067","ITEM26068",
                                        "ITEM26082","ITEM26083","ITEM26102","ITEM26103","ITEM26122",
                                        "ITEM26123","ITEM01062","ITEM01063","ITEM01104","ITEM01105",
                                        "ITEM28011","ITEM28018","ITEM28020","ITEM28027","ITEM28047",
                                        "ITEM28054","ITEM28182","ITEM28189","ITEM28191","ITEM28198",
                                        "ITEM28209","ITEM28216","ITEM28326","ITEM28333","ITEM28038",
                                        "ITEM28045","ITEM28101","ITEM28108","ITEM28119","ITEM28126",
                                        "ITEM28263","ITEM28270","ITEM09008","ITEM09009","ITEM28155",
                                        "ITEM28162","ITEM28281","ITEM28288","ITEM28353","ITEM28360",
                                        "ITEM01101","ITEM01102","ITEM28245","ITEM28252","ITEM26007",
                                        "ITEM26008","ITEM01095","ITEM01096","ITEM29007","ITEM29008",
                                        "ITEM29032","ITEM29033","ITEM29042","ITEM29043","ITEM29037",
                                        "ITEM29038","ITEM29047","ITEM29048","ITEM29027","ITEM29028",
                                        "ITEM27062","ITEM27090","ITEM27094","ITEM24027","ITEM24028",
                                        "ITEM24057","ITEM24058","ITEM24042","ITEM24043","ITEM01077",
                                        "ITEM01078","ITEM26112","ITEM26113","ITEM28335","ITEM28342",
                                        "ITEM27006","ITEM27010","ITEM27022","ITEM27026","ITEM27030",
                                        "ITEM27038","ITEM27042","ITEM27046","ITEM27050","ITEM27054",
                                        "ITEM27058","ITEM27034","ITEM27070","ITEM27194","ITEM27078",
                                        "ITEM27086","ITEM27098","ITEM27106","ITEM27110","ITEM27126",
                                        "ITEM27118","ITEM27122","ITEM27138","ITEM27142","ITEM27146",
                                        "ITEM27150","ITEM27154","ITEM27158","ITEM27170","ITEM27178",
                                        "ITEM27182","ITEM27198","ITEM31067","ITEM31068","ITEM25022",
                                        "ITEM25023","ITEM08025","ITEM08027","ITEM08015"))

colnames(data_1982)[7:183] <- c("farms","totacres","avgfarmsize","avg_farmval","avg_equipval",
                               "totval","cropval","cornac_grain","corn_grain","cornac_silage",
                               "corn_silage","wheatac","wheat","barleyac","barley",
                               "oatsac","oats","riceac","rice","cottonac",
                               "cotton","tobaccoac","tobacco","soybeanac_bean","soybean_bean",
                               "irpotatac","irpotato","canesugac","canesug","farmfert",
                               "chemval","farmwages","farmwages_contract","farmrepair","machine_hire_sub",
                               "tract_wheel","combine_num","baler_num","chemac_insects1","chemac_nematodes",
                               "chemac_diseases","chemac_weeds","horse","mule","buckwheatac",
                               "buckwheat","flaxac","flax","ryeac","rye",
                               "swpotatac","swpotato","irrac",
                               "beansac","beans","peasac","peas","cloverac1",
                               "clover1","cloverac2","clover2","cloverac3","clover3",
                               "cloverac4","clover4","cloverac5","clover5","cloverac6",
                               "clover6","sorghumgac","sorghumg","peanutac","peanuts",
                               "appleac","apples","apricotac","apricots","cherryac",
                               "cherries","peachac","peaches","pearac","pears",
                               "plumac","plums","almondac","almonds","bananaac",
                               "bananas","figac","figs","guavaac","guavas",
                               "lemonac","lemons","limeac","limes","oliveac",
                               "olives","orangeac","oranges","pecanac","pecans",
                               "pineappleac","pineapples","pomeloac","pomeloes","alfalfac",
                               "alfalfa","SBEETAC","SBEETWT","BLACKBAC","BLACKBER",
                               "curranac","currants","RASPBAC1","RASPBER1","RASPBAC2",
                               "RASPBER2","STRAWBAC","STRAWBER","CRANBAC","CRANBER",
                               "escaroleac1","escaroleac2","escaroleac3","emmerspletac","emmersplet",
                               "milletac","millet","mustardac","mustard","sunflowerac",
                               "sunflower","vetchac","vetch","hazelnultac","hazelnults",
                               "artichokeac","asparagusac","beetac","broccoliac","brusselac",
                               "cabbageac1","cabbageac2","cantaloupac","carrotac","cauliflowerac",
                               "celeryac","ccabbageac","collardac","watercressac","cucumberac",
                               "eggplantac","garlicac","kaleac","lettuceromaineac","okraac",
                               "oniondac","oniongac","pepperac1","pepperac2","pepperac3",
                               "pumpkinac","radishac","rhubarbac","squashac","tomatoac",
                               "turnipac","watermelonac","canolaac","canola","limaac",
                               "lima","cottonstripper","mowerconditioner","motortruck")

data_1982 <- setDT(data_1982)
data_1982 <- data_1982[,`:=`(farmval=farms*avg_farmval,
                             equipval=farms*avg_equipval,
                             totval=totval*1000,
                             farmfert=farmfert*1000,
                             chemval=chemval*1000,
                             farmwages=farmwages*1000,
                             farmwages_contract=farmwages_contract*1000,
                             farmrepair=farmrepair*1000,
                             machine_hire_sub=machine_hire_sub*1000,
                             tract_num=tract_wheel)]
data_1982 <- data_1982[,ID:=.I]
data_1982 <- data_1982[,chemac_insects:=max(na.rm=T,.SD),.SDcols=c("chemac_insects1","chemac_nematodes",
                                                                    "chemac_diseases"),by=ID]
data_1982 <- data_1982[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horse","mule")]
data_1982 <- data_1982[,RASPBER:=rowSums(.SD,na.rm=TRUE),.SDcols=c("RASPBER1","RASPBER2")]
data_1982 <- data_1982[,RASPBAC:=rowSums(.SD,na.rm=TRUE),.SDcols=c("RASPBAC1","RASPBAC2")]
data_1982 <- data_1982[,escaroleac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("escaroleac1","escaroleac2","escaroleac3")]
data_1982 <- data_1982[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cloverac1","cloverac2",
                                                                    "cloverac3","cloverac4",
                                                                    "cloverac5","cloverac6")]
data_1982 <- data_1982[,clover:=rowSums(.SD,na.rm=TRUE),.SDcols=c("clover1","clover2",
                                                                    "clover3","clover4",
                                                                    "clover5","clover6")]
data_1982 <- data_1982[,cabbageac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cabbageac1","cabbageac2")]
data_1982 <- data_1982[,beansac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beansac","limaac")]
data_1982 <- data_1982[,beans:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beans","lima")]

# Remove columns and change names to lower
colstoremove <- c("avg_farmval","avg_equipval",
                  "chemac_insects1","chemac_nematodes",
                  "chemac_diseases","horse","mule",
                  "RASPBER1","RASPBER2","RASPBAC1","RASPBAC2",
                  "escaroleac1","escaroleac2","escaroleac3",
                  "cloverac1","cloverac2",
                  "cloverac3","cloverac4",
                  "cloverac5","cloverac6",
                  "clover1","clover2",
                  "clover3","clover4",
                  "clover5","clover6",
                  "cabbageac1","cabbageac2",
                  "beansac","limaac","beans","lima","ID","tract_wheel")
data_1982 <- data_1982[,(colstoremove):=NULL]
names(data_1982) <- tolower(names(data_1982))

# match to common geo identifier
data_1982 <- data_1982 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1982$year <- 1982 

# Keep County-Level Observations
data_1982 <- data_1982[data_1982$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1982
crosswalkuse <- semi_join(crosswalk,data_1982,by=c("beginning_FIPS"="fips","year"="year"))
data_1982 <- left_join(crosswalkuse,data_1982,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1982 <- data_1982[which(data_1982$year==1982),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1982)[!names(data_1982)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1982_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1982",weightname="weight_agarea1"))
data_1982_part1 <- data_1982_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1982_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1982",weightname="weight_agarea2"))
data_1982_part2 <- data_1982_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1982 <- left_join(data_1982_part1,data_1982_part2,by=c("base_FIPS","year"))
rm(data_1982_part1,data_1982_part2)
names(data_1982) <- tolower(names(data_1982))


####### ***     1987   *** #######


# Dry edible peas are all, beans are dry edible including lima

data_1987 <- read_dta(file="DS0041/35206-0041-Data.dta")
data_1987 <- subset(data_1987, select=c("STATE","COUNTY","FIPS","LEVEL","STATEFIP",
                                        "COUNFIP",
                                        "ITEM01001","ITEM01002","ITEM01003","ITEM01004","ITEM01006",
                                        "ITEM01019","ITEM01021","ITEM01056","ITEM01057","ITEM01059",
                                        "ITEM01060","ITEM01065","ITEM01066","ITEM01068","ITEM01069",
                                        "ITEM01071","ITEM01072","ITEM01074","ITEM01075","ITEM01080",
                                        "ITEM01081","ITEM01083","ITEM01084","ITEM01086","ITEM01087",
                                        "ITEM01092","ITEM01093","ITEM01098","ITEM01099","ITEM03013",
                                        "ITEM03015","ITEM03029","ITEM03031","ITEM03033","ITEM03035",
                                        "ITEM08017","ITEM08023","ITEM08029","ITEM09011","ITEM09013",
                                        "ITEM09015","ITEM09017","ITEM13018","ITEM23002","ITEM24022",
                                        "ITEM24023","ITEM24032","ITEM24033","ITEM24067","ITEM24068",
                                        "ITEM25047","ITEM25048","ITEM01018",
                                        "ITEM25027","ITEM25028","ITEM25017","ITEM25018","ITEM26012",
                                        "ITEM26013","ITEM26047","ITEM26048","ITEM26067","ITEM26068",
                                        "ITEM26082","ITEM26083","ITEM26102","ITEM26103","ITEM26122",
                                        "ITEM26123","ITEM15024","ITEM15025","ITEM01104","ITEM01105",
                                        "ITEM28011","ITEM28018","ITEM28020","ITEM28027","ITEM28047",
                                        "ITEM28054","ITEM28182","ITEM28189","ITEM28191","ITEM28198",
                                        "ITEM28209","ITEM28216","ITEM28326","ITEM28333","ITEM28038",
                                        "ITEM28045","ITEM28101","ITEM28108","ITEM28119","ITEM28126",
                                        "ITEM28263","ITEM28270","ITEM09008","ITEM09009","ITEM28155",
                                        "ITEM28162","ITEM28281","ITEM28288","ITEM28353","ITEM28360",
                                        "ITEM01101","ITEM01102","ITEM28245","ITEM28252","ITEM26007",
                                        "ITEM26008","ITEM15125","ITEM15126","ITEM29007","ITEM29008",
                                        "ITEM29032","ITEM29033","ITEM29042","ITEM29043","ITEM29037",
                                        "ITEM29038","ITEM29047","ITEM29048","ITEM29027","ITEM29028",
                                        "ITEM27062","ITEM27094","ITEM27090","ITEM24027","ITEM24028",
                                        "ITEM24057","ITEM24058","ITEM24042","ITEM24043","ITEM01077",
                                        "ITEM01078","ITEM26112","ITEM26113","ITEM28335","ITEM28342",
                                        "ITEM27006","ITEM27010","ITEM27022","ITEM27026","ITEM27030",
                                        "ITEM27038","ITEM27042","ITEM27046","ITEM27050","ITEM27054",
                                        "ITEM27058","ITEM27034","ITEM27070","ITEM27194","ITEM27078",
                                        "ITEM27086","ITEM27098","ITEM27106","ITEM27110","ITEM27126",
                                        "ITEM27118","ITEM27122","ITEM27138","ITEM27142","ITEM27146",
                                        "ITEM27150","ITEM27154","ITEM27158","ITEM27170","ITEM27178",
                                        "ITEM27182","ITEM27198","ITEM31067","ITEM31068","ITEM25022",
                                        "ITEM25023","ITEM08027","ITEM08025","ITEM08015"))

colnames(data_1987)[7:183] <- c("farms","totacres","avgfarmsize","avg_farmval","avg_equipval",
                               "totval","cropval","cornac_grain","corn_grain","cornac_silage",
                               "corn_silage","wheatac","wheat","barleyac","barley",
                               "oatsac","oats","riceac","rice","cottonac",
                               "cotton","tobaccoac","tobacco","soybeanac_bean","soybean_bean",
                               "irpotatac","irpotato","canesugac","canesug","farmfert",
                               "chemval","farmwages","farmwages_contract","farmrepair","machine_hire_sub",
                               "tract_wheel","combine_num","baler_num","chemac_insects1","chemac_nematodes",
                               "chemac_diseases","chemac_weeds","horse","mule","buckwheatac",
                               "buckwheat","flaxac","flax","ryeac","rye",
                               "swpotatac","swpotato","irrac",
                               "peasac","peas","beansac","beans","cloverac1",
                               "clover1","cloverac2","clover2","cloverac3","clover3",
                               "cloverac4","clover4","cloverac5","clover5","cloverac6",
                               "clover6","sorghumgac","sorghumg","peanutac","peanuts",
                               "appleac","apples","apricotac","apricots","cherryac",
                               "cherries","peachac","peaches","pearac","pears",
                               "plumac","plums","almondac","almonds","bananaac",
                               "bananas","figac","figs","guavaac","guavas",
                               "lemonac","lemons","limeac","limes","oliveac",
                               "olives","orangeac","oranges","pecanac","pecans",
                               "pineappleac","pineapples","pomeloac","pomeloes","alfalfac",
                               "alfalfa","SBEETAC","SBEETWT","BLACKBAC","BLACKBER",
                               "curranac","currants","RASPBAC1","RASPBER1","RASPBAC2",
                               "RASPBER2","STRAWBAC","STRAWBER","CRANBAC","CRANBER",
                               "escaroleac1","escaroleac2","escaroleac3","emmerspletac","emmersplet",
                               "milletac","millet","mustardac","mustard","sunflowerac",
                               "sunflower","vetchac","vetch","hazelnultac","hazelnults",
                               "artichokeac","asparagusac","beetac","broccoliac","brusselac",
                               "cabbageac1","cabbageac2","cantaloupac","carrotac","cauliflowerac",
                               "celeryac","ccabbageac","collardac","watercressac","cucumberac",
                               "eggplantac","garlicac","kaleac","lettuceromaineac","okraac",
                               "oniondac","oniongac","pepperac1","pepperac2","pepperac3",
                               "pumpkinac","radishac","rhubarbac","squashac","tomatoac",
                               "turnipac","watermelonac","canolaac","canola","limaac",
                               "lima","mowerconditioner","cottonstripper","motortruck")

data_1987 <- setDT(data_1987)
data_1987 <- data_1987[,`:=`(farmval=farms*avg_farmval,
                             equipval=farms*avg_equipval,
                             totval=totval*1000,
                             cropval=cropval*1000,
                             farmfert=farmfert*1000,
                             chemval=chemval*1000,
                             farmwages=farmwages*1000,
                             farmwages_contract=farmwages_contract*1000,
                             farmrepair=farmrepair*1000,
                             machine_hire_sub=machine_hire_sub*1000,
                             tract_num=tract_wheel)]
data_1987 <- data_1987[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horse","mule")]
data_1987 <- data_1987[,ID:=.I]
data_1987 <- data_1987[,chemac_insects:=max(na.rm=T,.SD),.SDcols=c("chemac_insects1","chemac_nematodes",
                                                                    "chemac_diseases"),by=ID]
data_1987 <- data_1987[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cloverac1","cloverac2",
                                                                    "cloverac3","cloverac4",
                                                                    "cloverac5","cloverac6")]
data_1987 <- data_1987[,clover:=rowSums(.SD,na.rm=TRUE),.SDcols=c("clover1","clover2","clover3",
                                                                   "clover4","clover5","clover6")]
data_1987 <- data_1987[,RASPBER:=rowSums(.SD,na.rm=TRUE),.SDcols=c("RASPBER1","RASPBER2")]
data_1987 <- data_1987[,RASPBAC:=rowSums(.SD,na.rm=TRUE),.SDcols=c("RASPBAC1","RASPBAC2")]
data_1987 <- data_1987[,escaroleac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("escaroleac1","escaroleac2","escaroleac3")]
data_1987 <- data_1987[,cabbageac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cabbageac1","cabbageac2")]
data_1987 <- data_1987[,beansac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beansac","limaac")]
data_1987 <- data_1987[,beans:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beans","lima")]

# Remove columns and change names to lower
colstoremove <- c("avg_farmval","avg_equipval",
                  "horse","mule","chemac_insects1","chemac_nematodes",
                  "chemac_diseases","cloverac1","cloverac2",
                  "cloverac3","cloverac4",
                  "cloverac5","cloverac6",
                  "clover1","clover2","clover3",
                  "clover4","clover5","clover6",
                  "RASPBER1","RASPBER2","RASPBAC1","RASPBAC2",
                  "escaroleac1","escaroleac2","escaroleac3",
                  "cabbageac1","cabbageac2","beansac","limaac","beans","lima","ID","tract_wheel")
data_1987 <- data_1987[,(colstoremove):=NULL]
names(data_1987) <- tolower(names(data_1987))

# match to common geo identifier
data_1987 <- data_1987 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1987$year <- 1987

# Keep County-Level Observations
data_1987 <- data_1987[data_1987$level==1,]

# Match to 1997 crosswalk
# We have one row per subset of county from 1987
crosswalkuse <- semi_join(crosswalk,data_1987,by=c("beginning_FIPS"="fips","year"="year"))
data_1987 <- left_join(crosswalkuse,data_1987,by=c("beginning_FIPS"="fips","year"="year")) ; rm(crosswalkuse)
data_1987 <- data_1987[which(data_1987$year==1987),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_1987)[!names(data_1987)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize")])
data_1987_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1987",weightname="weight_agarea1"))
data_1987_part1 <- data_1987_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_1987_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_1987",weightname="weight_agarea2"))
data_1987_part2 <- data_1987_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_1987 <- left_join(data_1987_part1,data_1987_part2,by=c("base_FIPS","year"))
rm(data_1987_part1,data_1987_part2)
names(data_1987) <- tolower(names(data_1987))


####### ***     1992   *** #######


# Peas are dry edible, beans are dry edible including lima
# One less clover category - no alsike

data_1992 <- read_dta(file="DS0042/35206-0042-Data.dta")
data_1992 <- subset(data_1992, select=c("STATE","COUNTY","FIPS","LEVEL","STATEFIP",
                                        "COUNFIP",
                                        "ITEM010001","ITEM010002","ITEM010003","ITEM010004","ITEM010006",
                                        "ITEM010019","ITEM010021","ITEM010058","ITEM010059","ITEM010061",
                                        "ITEM010062","ITEM010067","ITEM010068","ITEM010070","ITEM010071",
                                        "ITEM010073","ITEM010074","ITEM010076","ITEM010077","ITEM010082",
                                        "ITEM010083","ITEM010085","ITEM010086","ITEM010088","ITEM010089",
                                        "ITEM010094","ITEM010095","ITEM010100","ITEM010101","ITEM030013",
                                        "ITEM030015","ITEM030029","ITEM030031","ITEM030033","ITEM030035",
                                        "ITEM090016","ITEM090022","ITEM090028","ITEM100008","ITEM100010",
                                        "ITEM100012","ITEM100014","ITEM180002","ITEM250002","ITEM260047",
                                        "ITEM260048","ITEM260072","ITEM260073","ITEM260102","ITEM260103",
                                        "ITEM270047","ITEM270048","ITEM010018",
                                        "ITEM270027","ITEM270028","ITEM270017","ITEM270018","ITEM280042",
                                        "ITEM280043","ITEM280062","ITEM280063","ITEM280077","ITEM280078",
                                        "ITEM280097","ITEM280098","ITEM280117","ITEM280118","ITEM010064",
                                        "ITEM010065","ITEM310002","ITEM310009","ITEM010106","ITEM010107",
                                        "ITEM310012","ITEM310018","ITEM310038","ITEM310045","ITEM310073",
                                        "ITEM310080","ITEM310182","ITEM310190","ITEM310200","ITEM310207",
                                        "ITEM310317","ITEM310324","ITEM310029","ITEM310036","ITEM310092",
                                        "ITEM310099","ITEM310110","ITEM310117","ITEM310254","ITEM310261",
                                        "ITEM310263","ITEM310270","ITEM310146","ITEM310153","ITEM310272",
                                        "ITEM310279","ITEM310344","ITEM310351","ITEM010103","ITEM010104",
                                        "ITEM310236","ITEM310243","ITEM280007","ITEM280008","ITEM010097",
                                        "ITEM010098","ITEM320007","ITEM320008","ITEM320032","ITEM320033",
                                        "ITEM320042","ITEM320043","ITEM320037","ITEM320038","ITEM320047",
                                        "ITEM320048","ITEM320027","ITEM320028","ITEM290066","ITEM290094",
                                        "ITEM290098","ITEM260067","ITEM260068","ITEM260092","ITEM260093",
                                        "ITEM260077","ITEM260078","ITEM010079","ITEM010080","ITEM280107",
                                        "ITEM280108","ITEM310326","ITEM310333","ITEM290010","ITEM290014",
                                        "ITEM290026","ITEM290030","ITEM290034","ITEM290042","ITEM290046",
                                        "ITEM290050","ITEM290054","ITEM290058","ITEM290062","ITEM290038",
                                        "ITEM290074","ITEM290198","ITEM290082","ITEM290090","ITEM290102",
                                        "ITEM290114","ITEM290118","ITEM290134","ITEM290126","ITEM290130",
                                        "ITEM290146","ITEM290150","ITEM290154","ITEM290158","ITEM290162",
                                        "ITEM290166","ITEM290174","ITEM290182","ITEM290186","ITEM290202",
                                        "ITEM260052","ITEM260053","ITEM270022","ITEM270023","ITEM090026",
                                        "ITEM090024","ITEM090014"))

colnames(data_1992)[7:181] <- c("farms","totacres","avgfarmsize","avg_farmval","avg_equipval",
                             "totval","cropval","cornac_grain","corn_grain","cornac_silage",
                             "corn_silage","wheatac","wheat","barleyac","barley",
                             "oatsac","oats","riceac","rice","cottonac",
                             "cotton","tobaccoac","tobacco","soybeanac_bean","soybean_bean",
                             "irpotatac","irpotato","canesugac","canesug","farmfert",
                             "chemval","farmwages","farmwages_contract","farmrepair","machine_hire_sub",
                             "tract_wheel","combine_num","baler_num","chemac_insects1","chemac_nematodes",
                             "chemac_diseases","chemac_weeds","horse","mule","buckwheatac",
                             "buckwheat","flaxac","flax","ryeac","rye",
                             "swpotatac","swpotato","irrac",
                             "peasac","peas","beansac","beans","cloverac1",
                             "clover1","cloverac2","clover2","cloverac3","clover3",
                             "cloverac4","clover4","cloverac5","clover5","sorghumgac",
                             "sorghumg","appleac","apples","peanutac","peanuts",
                             "apricotac","apricots","cherryac","cherries","peachac",
                             "peaches","pearac","pears","plumac","plums",
                             "almondac","almonds","bananaac","bananas","figac",
                             "figs","guavaac","guavas","lemonac","lemons",
                             "limeac","limes","oliveac","olives","orangeac",
                             "oranges","pecanac","pecans","pineappleac","pineapples",
                             "pomeloac","pomeloes","alfalfac","alfalfa","SBEETAC",
                             "SBEETWT","BLACKBAC","BLACKBER","curranac","currants",
                             "RASPBAC1","RASPBER1","RASPBAC2","RASPBER2","STRAWBAC",
                             "STRAWBER","CRANBAC","CRANBER","escaroleac1","escaroleac2",
                             "escaroleac3","emmerspletac","emmersplet","milletac","millet",
                             "mustardac","mustard","sunflowerac","sunflower","vetchac",
                             "vetch","hazelnultac","hazelnults","artichokeac","asparagusac",
                             "beetac","broccoliac","brusselac","cabbageac1","cabbageac2",
                             "cantaloupac","carrotac","cauliflowerac","celeryac","ccabbageac",
                             "collardac","watercressac","cucumberac","eggplantac","garlicac",
                             "kaleac","lettuceromaineac","okraac","oniondac","oniongac",
                             "pepperac1","pepperac2","pepperac3","pumpkinac","radishac",
                             "rhubarbac","squashac","tomatoac","turnipac","watermelonac",
                             "canolaac","canola","limaac","lima","mowerconditioner",
                             "cottonstripper","motortruck")

data_1992 <- setDT(data_1992)
data_1992 <- data_1992[,`:=`(farmval=farms*avg_farmval,
                             equipval=farms*avg_equipval,
                             totval=totval*1000,
                             cropval=cropval*1000,
                             farmfert=farmfert*1000,
                             chemval=chemval*1000,
                             farmwages=farmwages*1000,
                             farmwages_contract=farmwages_contract*1000,
                             farmrepair=farmrepair*1000,
                             machine_hire_sub=machine_hire_sub*1000,
                             tract_num=tract_wheel)]
data_1992 <- data_1992[,ID:=.I]
data_1992 <- data_1992[,chemac_insects:=max(na.rm=T,.SD),.SDcols=c("chemac_insects1","chemac_nematodes",
                                                                    "chemac_diseases"),by=ID]
data_1992 <- data_1992[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cloverac1","cloverac2",
                                                                    "cloverac3","cloverac4",
                                                                    "cloverac5")]
data_1992 <- data_1992[,clover:=rowSums(.SD,na.rm=TRUE),.SDcols=c("clover1","clover2",
                                                                    "clover3","clover4",
                                                                    "clover5")]
data_1992 <- data_1992[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horse","mule")]
data_1992 <- data_1992[,RASPBER:=rowSums(.SD,na.rm=TRUE),.SDcols=c("RASPBER1","RASPBER2")]
data_1992 <- data_1992[,RASPBAC:=rowSums(.SD,na.rm=TRUE),.SDcols=c("RASPBAC1","RASPBAC2")]
data_1992 <- data_1992[,escaroleac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("escaroleac1","escaroleac2","escaroleac3")]
data_1992 <- data_1992[,cabbageac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cabbageac1","cabbageac2")]
data_1992 <- data_1992[,beansac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beansac","limaac")]
data_1992 <- data_1992[,beans:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beans","lima")]

# Remove columns and change names to lower
colstoremove <- c("avg_farmval","avg_equipval",
                  "chemac_insects1","chemac_nematodes",
                  "chemac_diseases","cloverac1","cloverac2",
                  "cloverac3","cloverac4",
                  "cloverac5",
                  "clover1","clover2",
                  "clover3","clover4",
                  "clover5","horse","mule",
                  "RASPBER1","RASPBER2","RASPBAC1","RASPBAC2",
                  "escaroleac1","escaroleac2","escaroleac3",
                  "cabbageac1","cabbageac2","beansac","limaac","beans","lima","ID","tract_wheel")
data_1992 <- data_1992[,(colstoremove):=NULL]
names(data_1992) <- tolower(names(data_1992))

# match to common geo identifier
data_1992 <- data_1992 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1992$year <- 1992

# Keep County-Level Observations
data_1992 <- data_1992[data_1992$level==1,]
names(data_1992) <- tolower(names(data_1992))
# Add base_fips
# NOTE: no crosswalk to 1997 county definitions
data_1992 <- data_1992[,base_fips:=fips]



####### ***     1997   *** #######


# peas are dry edible, beans are dry edible including lima
# clover - no alsike

data_1997 <- read_dta(file="DS0043/35206-0043-Data.dta")
data_1997 <- subset(data_1997, select=c("STATE","COUNTY","FIPS","LEVEL","STATEFIP",
                                        "COUNFIP",
                                        "ITEM01001","ITEM01002","ITEM01003","ITEM01005","ITEM01007",
                                        "ITEM01020","ITEM01022","ITEM01059","ITEM01060","ITEM01062",
                                        "ITEM01063","ITEM01068","ITEM01069","ITEM01071","ITEM01072",
                                        "ITEM01074","ITEM01075","ITEM01077","ITEM01078","ITEM01083",
                                        "ITEM01084","ITEM01086","ITEM01087","ITEM01089","ITEM01090",
                                        "ITEM01095","ITEM01096","ITEM01101","ITEM01102","ITEM03013",
                                        "ITEM03015","ITEM03029","ITEM03031","ITEM03033","ITEM03035",
                                        "ITEM09016","ITEM09026","ITEM09032","ITEM10008","ITEM10010",
                                        "ITEM10012","ITEM10014","ITEM18002","ITEM25002","ITEM26047",
                                        "ITEM26048","ITEM26072","ITEM26073","ITEM26102","ITEM26103",
                                        "ITEM27047","ITEM27048","ITEM01019",
                                        "ITEM27027","ITEM27028","ITEM01092","ITEM01093","ITEM28042",
                                        "ITEM28043","ITEM28062","ITEM28063","ITEM28077","ITEM28078",
                                        "ITEM28097","ITEM28098","ITEM28117","ITEM28118","ITEM01065",
                                        "ITEM01066","ITEM01107","ITEM01108","ITEM31002","ITEM31009",
                                        "ITEM31011","ITEM31018","ITEM31038","ITEM31045","ITEM31173",
                                        "ITEM31179","ITEM31182","ITEM31189","ITEM31200","ITEM31207",
                                        "ITEM31317","ITEM31324","ITEM31029","ITEM31036","ITEM31092",
                                        "ITEM31099","ITEM31110","ITEM31117","ITEM31254","ITEM31261",
                                        "ITEM31263","ITEM31270","ITEM31146","ITEM31153","ITEM31272",
                                        "ITEM31279","ITEM31344","ITEM31351","ITEM01104","ITEM01105",
                                        "ITEM31236","ITEM31243","ITEM28007","ITEM28008","ITEM01098",
                                        "ITEM01099","ITEM32007","ITEM32008","ITEM32032","ITEM32033",
                                        "ITEM32042","ITEM32043","ITEM32037","ITEM32038","ITEM32047",
                                        "ITEM32048","ITEM32027","ITEM32028","ITEM29066","ITEM29098",
                                        "ITEM29094","ITEM26067","ITEM26068","ITEM26092","ITEM26093",
                                        "ITEM26077","ITEM26078","ITEM01080","ITEM01081","ITEM28107",
                                        "ITEM28108","ITEM31326","ITEM31333","ITEM29010","ITEM29014",
                                        "ITEM29026","ITEM29030","ITEM29034","ITEM29042","ITEM29046",
                                        "ITEM29050","ITEM29054","ITEM29058","ITEM29062","ITEM29038",
                                        "ITEM29074","ITEM29198","ITEM29082","ITEM29090","ITEM29102",
                                        "ITEM29114","ITEM29118","ITEM29134","ITEM29126","ITEM29130",
                                        "ITEM29146","ITEM29150","ITEM29154","ITEM29158","ITEM29162",
                                        "ITEM29166","ITEM29174","ITEM29182","ITEM29186","ITEM29202",
                                        "ITEM26052","ITEM26053","ITEM27022","ITEM27023","ITEM09030",
                                        "ITEM09028","ITEM09034"))

colnames(data_1997)[7:181] <- c("farms","totacres","avgfarmsize","avg_farmval","avg_equipval",
                               "totval","cropval","cornac_grain","corn_grain","cornac_silage",
                               "corn_silage","wheatac","wheat","barleyac","barley",
                               "oatsac","oats","riceac","rice","cottonac",
                               "cotton","tobaccoac","tobacco","soybeanac_bean","soybean_bean",
                               "irpotatac","irpotato","canesugac","canesug","farmfert",
                               "chemval","farmwages","farmwages_contract","farmrepair","machine_hire_sub",
                               "tract_wheel","combine_num","baler_num","chemac_insects1","chemac_nematodes",
                               "chemac_diseases","chemac_weeds","horse","mule","buckwheatac",
                               "buckwheat","flaxac","flax","ryeac","rye",
                               "swpotatac","swpotato","irrac",
                               "peasac","peas","beansac","beans","cloverac1",
                               "clover1","cloverac2","clover2","cloverac3","clover3",
                               "cloverac4","clover4","cloverac5","clover5","sorghumgac",
                               "sorghumg","peanutac","peanuts","appleac","apples",
                               "apricotac","apricots","cherryac","cherries","peachac",
                               "peaches","pearac","pears","plumac","plums",
                               "almondac","almonds","bananaac","bananas","figac",
                               "figs", "guavaac","guavas","lemonac","lemons",
                               "limeac","limes","oliveac","olives","orangeac",
                               "oranges","pecanac","pecans","pineappleac","pineapples",
                               "pomeloac","pomeloes","alfalfac","alfalfa","SBEETAC",
                               "SBEETWT","BLACKBAC","BLACKBER","curranac","currants",
                               "RASPBAC1","RASPBER1","RASPBAC2","RASPBER2","STRAWBAC",
                               "STRAWBER","CRANBAC","CRANBER","escaroleac1","escaroleac2",
                               "escaroleac3","emmerspletac","emmersplet","milletac","millet",
                               "mustardac","mustard","sunflowerac","sunflower","vetchac",
                               "vetch","hazelnultac","hazelnults","artichokeac","asparagusac",
                               "beetac","broccoliac","brusselac","cabbageac1","cabbageac2",
                               "cantaloupac","carrotac","cauliflowerac","celeryac","ccabbageac",
                               "collardac","watercressac","cucumberac","eggplantac","garlicac",
                               "kaleac","lettuceromaineac","okraac","oniondac","oniongac",
                               "pepperac1","pepperac2","pepperac3","pumpkinac","radishac",
                               "rhubarbac","squashac","tomatoac","turnipac","watermelonac",
                               "canolaac","canola","limaac","lima","mowerconditioner",
                               "cottonstripper","motortruck")

data_1997 <- setDT(data_1997)
data_1997 <- data_1997[,`:=`(farmval=farms*avg_farmval,
                             equipval=farms*avg_equipval,
                             totval=totval*1000,
                             cropval=cropval*1000,
                             farmfert=farmfert*1000,
                             chemval=chemval*1000,
                             farmwages=farmwages*1000,
                             farmwages_contract=farmwages_contract*1000,
                             farmrepair=farmrepair*1000,
                             machine_hire_sub=machine_hire_sub*1000,
                             tract_num=tract_wheel)]
data_1997 <- data_1997[,ID:=.I]
data_1997 <- data_1997[,chemac_insects:=max(na.rm=T,.SD),.SDcols=c("chemac_insects1","chemac_nematodes",
                                                                    "chemac_diseases"),by=ID]
data_1997 <- data_1997[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cloverac1","cloverac2",
                                                                    "cloverac3","cloverac4",
                                                                    "cloverac5")]
data_1997 <- data_1997[,clover:=rowSums(.SD,na.rm=TRUE),.SDcols=c("clover1","clover2",
                                                                  "clover3","clover4",
                                                                  "clover5")]
data_1997 <- data_1997[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horse","mule")]
data_1997 <- data_1997[,RASPBER:=rowSums(.SD,na.rm=TRUE),.SDcols=c("RASPBER1","RASPBER2")]
data_1997 <- data_1997[,RASPBAC:=rowSums(.SD,na.rm=TRUE),.SDcols=c("RASPBAC1","RASPBAC2")]
data_1997 <- data_1997[,escaroleac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("escaroleac1","escaroleac2","escaroleac3")]
data_1997 <- data_1997[,cabbageac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cabbageac1","cabbageac2")]
data_1997 <- data_1997[,beansac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beansac","limaac")]
data_1997 <- data_1997[,beans:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beans","lima")]

# Remove columns and change names to lower
colstoremove <- c("avg_farmval","avg_equipval",
                  "chemac_insects1","chemac_nematodes",
                  "chemac_diseases","cloverac1","cloverac2",
                  "cloverac3","cloverac4",
                  "cloverac5",
                  "clover1","clover2",
                  "clover3","clover4",
                  "clover5",
                  "horse","mule","RASPBER1","RASPBER2",
                  "RASPBAC1","RASPBAC2","escaroleac1","escaroleac2","escaroleac3",
                  "cabbageac1","cabbageac2","beansac","limaac","beans","lima","ID","tract_wheel")
data_1997 <- data_1997[,(colstoremove):=NULL]
names(data_1997) <- tolower(names(data_1997))


# match to common geo identifier
data_1997 <- data_1997 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_1997$year <- 1997

# Keep County-Level Observations
data_1997 <- data_1997[data_1997$level==1,]
names(data_1997) <- tolower(names(data_1997))

# Add base_fips
data_1997 <- data_1997[,base_fips:=fips]



####### ***   2002   *** #######


# fertilizer is now bundled with lime and soil conditioners
# farmrepair is now supplies, repair and maintenance
# farm_custom for expenses for customwork and custom hauling
# machine_hire_sub is: rent and lease expenses for machinery, equipment, and farm share of vehicles
# there is a second variable for expenses for hired labor - which we don't keep 

data_2002 <- read_dta(file="DS0044/35206-0044-Data.dta")
data_2002 <- subset(data_2002, select=c("STATE","COUNTY","FIPS","LEVEL","STATEFIP",
                                        "COUNFIP",
                                        "ITEM01001","ITEM01002","ITEM01003","ITEM01005","ITEM01007",
                                        "ITEM01020","ITEM01022","ITEM01063","ITEM01064","ITEM01066",
                                        "ITEM01067","ITEM01069","ITEM01070","ITEM01084","ITEM01085",
                                        "ITEM01081","ITEM01082","ITEM01099","ITEM01100","ITEM01108",
                                        "ITEM01109","ITEM01111","ITEM01112","ITEM01114","ITEM01115",
                                        "ITEM01120","ITEM01121","ITEM01129","ITEM01130","ITEM03009",
                                        "ITEM03013","ITEM03045","ITEM03049","ITEM03053","ITEM03057",
                                        "ITEM03065","ITEM22050","ITEM23185","ITEM23187","ITEM24072",
                                        "ITEM24073","ITEM24112","ITEM24113","ITEM24172","ITEM24173",
                                        "ITEM38027","ITEM38028","ITEM38042","ITEM38055","ITEM39019",
                                        "ITEM39023","ITEM39027","ITEM39031","ITEM15002","ITEM01019",
                                        "ITEM25077","ITEM25078","ITEM01096","ITEM01097","ITEM26082",
                                        "ITEM26122","ITEM26152","ITEM26192","ITEM26232","ITEM01087",
                                        "ITEM01135","ITEM31002","ITEM31014","ITEM31050","ITEM31062",
                                        "ITEM31206","ITEM31242","ITEM31290","ITEM31506","ITEM31038",
                                        "ITEM31098","ITEM31122","ITEM31398","ITEM31410","ITEM31170",
                                        "ITEM31422","ITEM31542","ITEM01132","ITEM31362","ITEM26012",
                                        "ITEM01126","ITEM33002","ITEM33022","ITEM33026","ITEM33030",
                                        "ITEM33042","ITEM33018","ITEM29122","ITEM29092","ITEM24102",
                                        "ITEM24152","ITEM26102","ITEM24122","ITEM01123","ITEM26212",
                                        "ITEM31518","ITEM29008","ITEM29014","ITEM29032","ITEM29038",
                                        "ITEM29044","ITEM29050","ITEM29056","ITEM29062","ITEM29068",
                                        "ITEM29074","ITEM29080","ITEM29086","ITEM29098","ITEM29308",
                                        "ITEM29104","ITEM29116","ITEM29128","ITEM29146","ITEM29152",
                                        "ITEM29182","ITEM29188","ITEM29194","ITEM29224","ITEM29230",
                                        "ITEM29236","ITEM29242","ITEM29248","ITEM29254","ITEM29266",
                                        "ITEM29278","ITEM29284","ITEM29314","ITEM24082","ITEM25062",
                                        "ITEM38047"))

colnames(data_2002)[7:142] <- c("farms","totacres","avgfarmsize","avg_farmval","avg_equipval",
                               "totval","cropval","cornac_grain","corn_grain","cornac_silage",
                               "corn_silage","wheatac","wheat","barleyac","barley",
                               "oatsac","oats","cottonac","cotton","tobaccoac",
                               "tobacco","irpotatac","irpotato","swpotatac","swpotato",
                               "riceac","rice","canesugac","canesug","farmfert",
                               "chemval","farmrepair","farmwages","farmwages_contract","farm_custom",
                               "machine_hire_sub","mule","soybeanac_bean","soybean_bean","buckwheatac",
                               "buckwheat","flaxac","flax","ryeac","rye",
                               "tract_num","tract_num1997","combine_num","baler_num","chemac_insects1",
                               "chemac_weeds","chemac_nematodes","chemac_diseases","horse","irrac",
                               "peasac","peas","beansac","beans","cloverac1",
                               "cloverac2","cloverac3","cloverac4","cloverac5","sorghumgac",
                               "peanutac","appleac","apricotac","cherryac1","cherryac2",
                               "peachac","pearac","plumac","almondac","bananaac",
                               "figac","guavaac","lemonac","limeac","oliveac",
                               "orangeac","pecanac","pineappleac","pomeloac","alfalfac",
                               "SBEETAC","BLACKBAC","curranac","RASPBAC1","RASPBAC2",
                               "STRAWBAC","CRANBAC","escaroleac1","escaroleac2","emmerspletac",
                               "milletac1","milletac2","mustardac","sunflowerac","vetchac",
                               "hazelnultac","artichokeac","asparagusac","beetac","broccoliac",
                               "brusselac","cabbageac1","cabbageac2","cabbageac3","cantaloupac",
                               "carrotac","cauliflowerac","celeryac","collardac","watercressac",
                               "cucumberac","eggplantac","garlicac","kaleac","lettuceromaineac",
                               "okraac","oniondac","oniongac","pepperac1","pepperac2",
                               "pepperac3","pumpkinac","radishac","rhubarbac","squashac",
                               "tomatoac","turnipac","watermelonac","canolaac","limaac",
                               "cottonstripper")
  
data_2002 <- setDT(data_2002)
data_2002 <- data_2002[,`:=`(farmval=farms*avg_farmval,
                             equipval=farms*avg_equipval,
                             totval=totval*1000,
                             cropval=cropval*1000,
                             farmfert=farmfert*1000,
                             chemval=chemval*1000,
                             farmwages=farmwages*1000,
                             farmwages_contract=farmwages_contract*1000,
                             farmrepair=farmrepair*1000,
                             machine_hire_sub=machine_hire_sub*1000)]

data_2002 <- data_2002[,ID:=.I]
data_2002 <- data_2002[,chemac_insects:=max(na.rm=T,.SD),.SDcols=c("chemac_insects1","chemac_nematodes",
                                                                   "chemac_diseases","chemac_weeds"),by=ID]
data_2002 <- data_2002[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cloverac1","cloverac2",
                                                                    "cloverac3","cloverac4",
                                                                    "cloverac5")]
data_2002 <- data_2002[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horse","mule")]
data_2002 <- data_2002[,RASPBAC:=rowSums(.SD,na.rm=TRUE),.SDcols=c("RASPBAC1","RASPBAC2")]
data_2002 <- data_2002[,escaroleac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("escaroleac1","escaroleac2")]
data_2002 <- data_2002[,cabbageac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cabbageac1","cabbageac2","cabbageac3")]
data_2002 <- data_2002[,beansac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beansac","limaac")]
data_2002 <- data_2002[,milletac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("milletac1","milletac2")]
data_2002 <- data_2002[,cherryac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cherryac1","cherryac2")]

# Remove columns and change names to lower
colstoremove <- c("avg_farmval","avg_equipval",
                  "chemac_insects1","chemac_nematodes",
                  "chemac_diseases","cloverac1","cloverac2",
                  "cloverac3","cloverac4","chemac_weeds",
                  "cloverac5","milletac2","cherryac1","cherryac2",
                  "horse","mule","cabbageac3",
                  "RASPBAC1","RASPBAC2","escaroleac1","escaroleac2",
                  "cabbageac1","cabbageac2","beansac","limaac","ID","milletac1")
data_2002 <- data_2002[,(colstoremove):=NULL]
names(data_2002) <- tolower(names(data_2002))

# match to common geo identifier
data_2002 <- data_2002 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_2002$year <- 2002

# Keep County-Level Observations
data_2002 <- data_2002[data_2002$level==1,]
names(data_2002) <- tolower(names(data_2002))

# Match to 1997 crosswalk
# We have one row per subset of county from 1992
crosswalkuse <- semi_join(crosswalk,data_2002,by=c("beginning_FIPS"="fips","year"="year"))
data_2002 <- setDT(left_join(crosswalkuse,data_2002,by=c("beginning_FIPS"="fips","year"="year"))) ; rm(crosswalkuse)
data_2002 <- data_2002[which(data_2002$year==2002),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_2002)[!names(data_2002)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize",
                                                             "statefip","counfip")])
data_2002 <- data_2002[is.nan(weight_agarea1),weight_agarea1:=0]
data_2002 <- data_2002[is.nan(weight_agarea2),weight_agarea2:=0]

data_2002_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_2002",weightname="weight_agarea1"))

data_2002_part1 <- data_2002_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_2002_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_2002",weightname="weight_agarea2"))
data_2002_part2 <- data_2002_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_2002 <- left_join(data_2002_part1,data_2002_part2,by=c("base_FIPS","year"))
rm(data_2002_part1,data_2002_part2)
names(data_2002) <- tolower(names(data_2002))



####### ***   2007   *** #######


# farm_fert: a bundle of fertilizer, lime and soil conditioner
# there is a second variable for expenses for hired labor - which we don't keep 
# no "irpotato","swpotato",

data_2007 <- read_dta(file="DS0045/35206-0045-Data.dta")
data_2007 <- subset(data_2007, select=c("STATE","COUNTY","FIPS","LEVEL","STATEFIP",
                                        "COUNTYFIP",
                                        "DATA1_1","DATA1_2","DATA1_3","DATA1_5","DATA1_7",
                                        "DATA1_19","DATA1_20","DATA1_22","DATA1_63","DATA1_64",
                                        "DATA1_66","DATA1_67","DATA1_69","DATA1_70","DATA1_81",
                                        "DATA1_82","DATA1_84","DATA1_85","DATA1_93","DATA1_94",
                                        "DATA1_99","DATA1_100","DATA1_108","DATA1_109","DATA1_114",
                                        "DATA1_115","DATA1_123","DATA1_124","DATA3_9","DATA3_13",
                                        "DATA3_45","DATA3_49","DATA3_53","DATA3_57","DATA3_65",
                                        "DATA15_210","DATA24_260","DATA26_230","DATA26_330","DATA26_2140",
                                        "DATA26_3140","DATA26_2230","DATA26_3230","DATA30_2420","DATA30_2510",
                                        "DATA41_31","DATA41_47","DATA41_59","DATA42_19","DATA42_23",
                                        "DATA42_27","DATA42_31","DATA26_2110","DATA1_96","DATA27_280",
                                        "DATA27_2110","DATA27_2140","DATA27_2200","DATA1_87","DATA1_129",
                                        "DATA32_220","DATA32_320","DATA32_260","DATA32_271","DATA32_2190",
                                        "DATA32_2220","DATA32_2270","DATA32_2450","DATA32_250","DATA32_2100",
                                        "DATA32_2120","DATA32_2350","DATA32_2360","DATA32_2160","DATA32_2370",
                                        "DATA32_2490","DATA1_126","DATA32_2330","DATA27_220","DATA1_120",
                                        "DATA34_210","DATA34_260","DATA34_280","DATA34_2120","DATA34_250",
                                        "DATA30_2210","DATA30_2160","DATA26_2130","DATA26_2200","DATA26_2160",
                                        "DATA1_117","DATA27_2180","DATA32_2470","DATA30_220","DATA30_230",
                                        "DATA30_260","DATA30_270","DATA30_280","DATA30_290","DATA30_2100",
                                        "DATA30_2110","DATA30_2120","DATA30_2130","DATA30_2140","DATA30_2150",
                                        "DATA30_2170","DATA30_2550","DATA30_2180","DATA30_2200","DATA30_2220",
                                        "DATA30_2270","DATA30_2280","DATA30_2330","DATA30_2340","DATA30_2350",
                                        "DATA30_2400","DATA30_2410","DATA30_2430","DATA30_2440","DATA30_2450",
                                        "DATA30_2470","DATA30_2520","DATA30_2530","DATA30_2560","DATA26_240",
                                        "DATA1_96","DATA41_51"))

colnames(data_2007)[7:133] <- c("farms","totacres","avgfarmsize","avg_farmval","avg_equipval",
                             "irrac","totval","cropval","cornac_grain","corn_grain",
                             "cornac_silage","corn_silage","wheatac","wheat","oatsac",
                             "oats","barleyac","barley","soybeanac_bean","soybean_bean",
                             "cottonac","cotton","tobaccoac","tobacco","riceac",
                             "rice","canesugac","canesug","farmfert","chemval",
                             "farmrepair","farmwages","farmwages_contract","farm_custom","machine_hire_sub",
                             "horse","mule","buckwheatac","buckwheat","flaxac",
                             "flax","ryeac","rye","irpotatac","swpotatac",
                             "tract_num","combine_num","baler_num","chemac_insects1","chemac_weeds",
                             "chemac_nematodes","chemac_diseases","peasac","beansac","cloverac1",
                             "cloverac2","cloverac3","cloverac4","sorghumgac","peanutac",
                             "appleac","apricotac","cherryac1","cherryac2","peachac",
                             "pearac","plumac","almondac","bananaac","figac",
                             "guavaac","lemonac","limeac","oliveac","orangeac",
                             "pecanac","pineappleac","pomeloac","alfalfac","SBEETAC",
                             "BLACKBAC","curranac","RASPBAC","STRAWBAC","CRANBAC",
                             "escaroleac1","escaroleac2","emmerspletac","milletac","mustardac",
                             "sunflowerac","vetchac","hazelnultac","artichokeac","asparagusac",
                             "beetac","broccoliac","brusselac","cabbageac1","cabbageac2",
                             "cabbageac3","cantaloupac","carrotac","cauliflowerac","celeryac",
                             "collardac","watercressac","cucumberac","eggplantac","garlicac",
                             "kaleac","lettuceromaineac","okraac","oniondac","oniongac",
                             "pepperac1","pepperac2","pumpkinac","radishac","rhubarbac",
                             "squashac","tomatoac","turnipac","watermelonac","canolaac",
                             "limaac","cottonstripper")

data_2007 <- setDT(data_2007)
data_2007 <- data_2007[,`:=`(farmval=farms*avg_farmval,
                             equipval=farms*avg_equipval,
                             totval=totval*1000,
                             cropval=cropval*1000,
                             farmfert=farmfert*1000,
                             chemval=chemval*1000,
                             farmwages=farmwages*1000,
                             farmwages_contract=farmwages_contract*1000,
                             farmrepair=farmrepair*1000,
                             machine_hire_sub=machine_hire_sub*1000)]

data_2007 <- data_2007[,ID:=.I]
data_2007 <- data_2007[,chemac_insects:=max(na.rm=T,.SD),.SDcols=c("chemac_insects1","chemac_nematodes",
                                                                   "chemac_diseases","chemac_weeds"),by=ID]
data_2007 <- data_2007[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cloverac1","cloverac2",
                                                                    "cloverac3","cloverac4")]
data_2007 <- data_2007[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horse","mule")]
data_2007 <- data_2007[,escaroleac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("escaroleac1","escaroleac2")]
data_2007 <- data_2007[,cabbageac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cabbageac1","cabbageac2","cabbageac3")]
data_2007 <- data_2007[,beansac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beansac","limaac")]
data_2007 <- data_2007[,cherryac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cherryac1","cherryac2")]

# Remove columns and change names to lower
colstoremove <- c("avg_farmval","avg_equipval",
                  "chemac_insects1","chemac_nematodes",
                  "chemac_diseases","cloverac1","cloverac2",
                  "cloverac3","cloverac4","chemac_weeds",
                  "cherryac1","cherryac2",
                  "horse","mule","cabbageac3",
                  "escaroleac1","escaroleac2",
                  "cabbageac1","cabbageac2","beansac","limaac","ID")
data_2007 <- data_2007[,(colstoremove):=NULL]
names(data_2007) <- tolower(names(data_2007))

# match to common geo identifier
data_2007 <- data_2007 %>% mutate(state=str_pad(as.character(state),2,pad="0"),
                                  county=str_pad(as.character(county),4,pad="0"),
                                  fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_2007$year <- 2007

# Keep County-Level Observations
data_2007 <- data_2007[data_2007$level==1,]
names(data_2007) <- tolower(names(data_2007))

# Match to 1997 crosswalk
# We have one row per subset of county from 2007
crosswalkuse <- semi_join(crosswalk,data_2007,by=c("beginning_FIPS"="fips","year"="year"))
data_2007 <- setDT(left_join(crosswalkuse,data_2007,by=c("beginning_FIPS"="fips","year"="year"))) ; rm(crosswalkuse)
data_2007 <- data_2007[which(data_2007$year==2007),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_2007)[!names(data_2007)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize",
                                                             "statefip","counfip")])
data_2007 <- data_2007[is.nan(weight_agarea1),weight_agarea1:=0]
data_2007 <- data_2007[is.nan(weight_agarea2),weight_agarea2:=0]

data_2007_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_2007",weightname="weight_agarea1"))

data_2007_part1 <- data_2007_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_2007_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_2007",weightname="weight_agarea2"))
data_2007_part2 <- data_2007_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_2007 <- left_join(data_2007_part1,data_2007_part2,by=c("base_FIPS","year"))
rm(data_2007_part1,data_2007_part2)
names(data_2007) <- tolower(names(data_2007))





####### ***   2012   *** #######


data_2012 <- read_dta(file="DS0047/35206-0047-Data.dta")
data_2012 <- subset(data_2012, select=c("STATEICP","COUNICP","FIPS","LEVEL","STATEFIP",
                                        "COFIPS",
                                        "DATA1_1","DATA1_2","DATA1_3","DATA1_5","DATA1_7",
                                        "DATA1_20","DATA1_21","DATA1_23","DATA1_64","DATA1_65",
                                        "DATA1_67","DATA1_68","DATA1_70","DATA1_71","DATA1_82",
                                        "DATA1_83","DATA1_85","DATA1_86","DATA1_94","DATA1_95",
                                        "DATA1_100","DATA1_101","DATA1_109","DATA1_110","DATA1_115",
                                        "DATA1_116","DATA1_124","DATA1_125","DATA3_9","DATA3_13",
                                        "DATA3_45","DATA3_49","DATA3_53","DATA3_57","DATA3_65",
                                        "DATA18_2","DATA18_22","DATA25_23","DATA25_33","DATA25_214",
                                        "DATA25_314","DATA25_225","DATA25_325","DATA29_242","DATA29_251",
                                        "DATA40_31","DATA40_47","DATA40_59","DATA41_19","DATA41_23",
                                        "DATA41_27","DATA41_31","DATA25_21","DATA1_97","DATA26_210",
                                        "DATA26_217","DATA26_223","DATA26_231","DATA1_88","DATA1_130",
                                        "DATA31_23","DATA31_25","DATA31_211","DATA31_213","DATA31_267",
                                        "DATA31_273","DATA31_291","DATA31_21","DATA31_29","DATA31_223",
                                        "DATA31_229","DATA31_237","DATA31_239","DATA31_251","DATA31_253",
                                        "DATA31_279","DATA1_127","DATA31_225","DATA26_21","DATA1_121",
                                        "DATA33_21","DATA33_211","DATA33_217","DATA33_223","DATA33_29",
                                        "DATA29_220","DATA29_215","DATA25_213","DATA25_222","DATA25_216",
                                        "DATA1_118","DATA26_229","DATA31_231","DATA29_21","DATA29_22",
                                        "DATA29_25","DATA29_26","DATA29_27","DATA29_28","DATA29_29",
                                        "DATA29_210","DATA29_211","DATA29_212","DATA29_213","DATA29_214",
                                        "DATA29_216","DATA29_256","DATA29_217","DATA29_219","DATA29_221",
                                        "DATA29_226","DATA29_227","DATA29_232","DATA29_233","DATA29_234",
                                        "DATA29_240","DATA29_241","DATA29_243","DATA29_244","DATA29_245",
                                        "DATA29_247","DATA29_252","DATA29_254","DATA29_257","DATA25_25",
                                        "DATA25_210","DATA40_51"))

colnames(data_2012)[7:133] <- c("farms","totacres","avgfarmsize","avg_farmval","equipval",
                               "irrac","totval","cropval","cornac_grain","corn_grain",
                               "cornac_silage","corn_silage","wheatac","wheat","oatsac",
                               "oats","barleyac","barley","soybeanac_bean","soybean_bean",
                               "cottonac","cotton","tobaccoac","tobacco","riceac",
                               "rice","canesugac_sugar","cansug_sugar","farmfert","chemval",
                               "farmrepair","farmwages","farmwages_contract","farm_custom","machine_hire_sub",
                               "horse","mule","buckwheatac","buckwheat","flaxac",
                               "flax","ryeac","rye","irpotatac","swpotatac",
                               "tract_num","combine_num","baler_num","chemac_insects1","chemac_weeds",
                               "chemac_nematodes","chemac_diseases","peasac","beansac","cloverac1",
                               "cloverac2","cloverac3","cloverac4","sorghumgac","peanutac",
                               "appleac","apricotac","cherryac1","cherryac2","peachac",
                               "pearac","plumac","almondac","bananaac","figac",
                               "guavaac","lemonac","limeac","oliveac","orangeac",
                               "pecanac","pineappleac","pomeloac","alfalfac","SBEETAC",
                               "BLACKBAC","curranac","RASPBAC","STRAWBAC","CRANBAC",
                               "escaroleac1","escaroleac2","emmerspletac","milletac","mustardac",
                               "sunflowerac","vetchac","hazelnultac","artichokeac","asparagusac",
                               "beetac","broccoliac","brusselac","cabbageac1","cabbageac2",
                               "cabbageac3","cantaloupac","carrotac","cauliflowerac","celeryac",
                               "collardac","watercressac","cucumberac","eggplantac","garlicac",
                               "kaleac","lettuceromaineac","okraac","oniondac","oniongac",
                               "pepperac1","pepperac2","pumpkinac","radishac","rhubarbac",
                               "squashac","tomatoac","turnipac","watermelonac","canolaac",
                               "limaac","cottonstripper")


data_2012 <- setDT(data_2012)
data_2012 <- data_2012[,`:=`(farmval=farms*avg_farmval,
                             totval=totval*1000,
                             cropval=cropval*1000,
                             farmfert=farmfert*1000,
                             chemval=chemval*1000,
                             farmwages=farmwages*1000,
                             farmwages_contract=farmwages_contract*1000,
                             farmrepair=farmrepair*1000,
                             machine_hire_sub=machine_hire_sub*1000)]

data_2012 <- data_2012[,ID:=.I]
data_2012 <- data_2012[,chemac_insects:=max(na.rm=T,.SD),.SDcols=c("chemac_insects1","chemac_nematodes",
                                                                   "chemac_diseases","chemac_weeds"),by=ID]
data_2012 <- data_2012[,cloverac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cloverac1","cloverac2",
                                                                    "cloverac3","cloverac4")]
data_2012 <- data_2012[,horsemu:=rowSums(.SD,na.rm=TRUE),.SDcols=c("horse","mule")]
data_2012 <- data_2012[,escaroleac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("escaroleac1","escaroleac2")]
data_2012 <- data_2012[,cabbageac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cabbageac1","cabbageac2","cabbageac3")]
data_2012 <- data_2012[,beansac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("beansac","limaac")]
data_2012 <- data_2012[,cherryac:=rowSums(.SD,na.rm=TRUE),.SDcols=c("cherryac1","cherryac2")]

# Remove columns and change names to lower
colstoremove <- c("avg_farmval",
                  "chemac_insects1","chemac_nematodes",
                  "chemac_diseases","cloverac1","cloverac2",
                  "cloverac3","cloverac4","chemac_weeds",
                  "cherryac1","cherryac2",
                  "horse","mule","cabbageac3",
                  "escaroleac1","escaroleac2",
                  "cabbageac1","cabbageac2","beansac","limaac","ID")
data_2012 <- data_2012[,(colstoremove):=NULL]
names(data_2012) <- tolower(names(data_2012))

# match to common geo identifier
data_2012 <- data_2012 %>% mutate(fips=str_pad(as.character(fips),5,pad="0",side="left"))
data_2012$year <- 2012

# Keep County-Level Observations
data_2012 <- data_2012[data_2012$level==1,]
names(data_2012) <- tolower(names(data_2012))

# Match to 1997 crosswalk
# We have one row per subset of county from 2007
crosswalkuse <- semi_join(crosswalk,data_2012,by=c("beginning_FIPS"="fips","year"="year"))
data_2012 <- setDT(left_join(crosswalkuse,data_2012,by=c("beginning_FIPS"="fips","year"="year"))) ; rm(crosswalkuse)
data_2012 <- data_2012[which(data_2012$year==2012),]

# Var which we want to aggregate to 1997 counties - stocks
colstoavg <- as.list(names(data_2012)[!names(data_2012)%in%c("beginning_FIPS","base_FIPS","base_NAME",
                                                             "weight_agarea1","weight_agarea2","state","county","fips",
                                                             "level","region1","region2","year","avgfarmsize",
                                                             "statefip","counfip")])
data_2012 <- data_2012[is.nan(weight_agarea1),weight_agarea1:=0]
data_2012 <- data_2012[is.nan(weight_agarea2),weight_agarea2:=0]

data_2012_part1 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_2012",weightname="weight_agarea1"))

data_2012_part1 <- data_2012_part1 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

# Var which we want to aggregate to 1997 counties - average
colstoavg <- c("avgfarmsize")
data_2012_part2 <- lapply(colstoavg,
                          FUN=function(x) myweighted_sum(colname=x,dataframe="data_2012",weightname="weight_agarea2"))
data_2012_part2 <- data_2012_part2 %>% purrr::reduce(left_join,by = c("base_FIPS","year"))

data_2012 <- left_join(data_2012_part1,data_2012_part2,by=c("base_FIPS","year"))
rm(data_2012_part1,data_2012_part2)
names(data_2012) <- tolower(names(data_2012))



######### *** FINAL STEPS  *** #######

# Removing horse and mules from 1964 - weird values
data_1964 <- setDT(data_1964)
data_1964 <- data_1964[,horsemu:=NA]

# Which states to remove: non-contiguous US (American Samoa, Guam, Northern Mariana Islands, Puerto Rico, Virgin Islands)
datafinal <- bind_rows(data_1840,data_1850,data_1860,data_1870,data_1880,data_1890,data_1900,
                       data_1910,data_1920,data_1925,data_1930,data_1935,data_1940,data_1945,
                       data_1950,data_1954,data_1959,data_1964,data_1969,data_1974,data_1978,
                       data_1982,data_1987,data_1992,data_1997,data_2002,data_2007,data_2012)
datafinal <- setDT(datafinal)
datafinal <- datafinal[!substr(base_fips,1,2)%in%c("60","66","69","72","78"),]

# Harmonize the var names -
names(datafinal) <- tolower(names(datafinal))
union <- union(c("peasac1","beans1","beans2","peas1","peas2","peas3","beans3","beans4",
                 "pepperac1","pepperac2","pepperac3","equipval2","beansac2","beans_snap",
                 "statefip.y","statefip.x","clover1","cloverac1","peasac2","scotbal1",
                 "counfip","county","fips","state","statefip","level","scbalewt","balewt"),
               grep(names(datafinal),patter="[[:digit:]]",value=T))
colstoremove <- intersect(names(datafinal),union)
datafinal <- datafinal[,(colstoremove):=NULL]
names(datafinal) <- tolower(names(datafinal))

# Tree Acreage
pecandensity <- 17.4
lemondensity <- 87
limedensity <- 87
pineappledensity <- 30000
appledensity <- datafinal[year<1980&appleac>10&appltree>1000,.(mean=quantile(appltree/appleac,na.rm=T,probs=c(.75)))][1]
apricotdensity <- datafinal[year<1980&apricotac>10&apritree>1000,.(mean=quantile(apritree/apricotac,na.rm=T,probs=c(.75)))][1]
cherrydensity <- 200  # https://extension.oregonstate.edu/crop-production/fruit-trees/growers-adjusting-methods-high-density-cherries
peachdensity <- datafinal[year<1980&peachac>10&peactree>1000,.(mean=quantile(peactree/peachac,na.rm=T,probs=c(.75)))][1]
peardensity <- datafinal[year<1980&pearac>10&peartree>1000,.(mean=quantile(peartree/pearac,na.rm=T,probs=c(.75)))][1]
plumensity <- datafinal[year<1980&plumac>10&plumtree>1000,.(mean=quantile(plumtree/plumac,na.rm=T,probs=c(.75)))][1]
almonddensity <- datafinal[year<1980&almondac>10&almondtree>1000,.(mean=quantile(almondtree/almondac,na.rm=T,probs=c(.75)))][1]
figdensity <- datafinal[year<1980&figac>10&figtree>1000,.(mean=quantile(figtree/figac,na.rm=T,probs=c(.75)))][1]
olivedensity <- datafinal[year<1980&oliveac>10&olivetree>1000,.(mean=quantile(olivetree/oliveac,na.rm=T,probs=c(.75)))][1]
orangedensity <- datafinal[year<1980&orangeac>10&orangetree>1000,.(mean=quantile(orangetree/orangeac,na.rm=T,probs=c(.75)))][1]
pomelodensity <- datafinal[year<1980&pomeloac>10&pomelotree>1000,.(mean=quantile(pomelotree/pomeloac,na.rm=T,probs=c(.75)))][1]
hazelnutdensity <- 108

datafinal <- datafinal[(is.na(appleac)|appleac==0)&!is.na(appltree),appleac:=appltree*73.92761] #appledensity
datafinal <- datafinal[is.na(apricotac)&!is.na(apritree),apricotac:=apritree*83.86121] #apricotdensity
datafinal <- datafinal[is.na(cherryac)&!is.na(chertree),cherryac:=chertree*200] #cherrydensity - 94.11603
datafinal <- datafinal[is.na(peachac)&!is.na(peactree),peachac:=peactree*98.5464] #peachdensity - https://www.mafes.msstate.edu/publications/archived-bulletins/b0951.pdf
datafinal <- datafinal[is.na(pearac)&!is.na(peartree),pearac:=peartree*99.20314] #peardensity
datafinal <- datafinal[is.na(plumac)&!is.na(plumtree),plumac:=plumtree*98.65283] #plumensity
datafinal <- datafinal[is.na(almondac)&!is.na(almondtree),almondac:=almondtree*72.88609] #almonddensity
datafinal <- datafinal[is.na(bananaac)&!is.na(bananatree),bananaac:=bananatree*500] #bananadensity - https://www.hort.purdue.edu/newcrop/morton/banana.html
datafinal <- datafinal[is.na(figac)&!is.na(figtree),figac:=figtree*96.21429] #figdensity
datafinal <- datafinal[is.na(lemonac)&!is.na(lemontree),lemonac:=lemontree*87] #lemondensity
datafinal <- datafinal[is.na(limeac)&!is.na(limetree),limeac:=limetree*87] #limedensity
datafinal <- datafinal[is.na(oliveac)&!is.na(olivetree),oliveac:=olivetree*72.359] #olivedensity
datafinal <- datafinal[is.na(orangeac)&!is.na(orangetree),orangeac:=orangetree*93.31918] #orangedensity
datafinal <- datafinal[is.na(pecanac)&!is.na(pecantree),pecanac:=pecantree*17.4] #pecandensity
datafinal <- datafinal[is.na(pineappleac)&!is.na(pineappletree),pineappleac:=pineappletree*30000] #pineappledensity
datafinal <- datafinal[is.na(pomeloac)&!is.na(pomelotree),pomeloac:=pomelotree*91.05327] #pomelodensity

# Output
crop_output <- c("wheat","barley","oats","rye","buckwheat","corn","rice","hops",
                    "irpotato","swpotato","hay","hemp","flax","tobacco",
                    "cotton","canesug","peasbean","clovseed","grasseed","peanuts",
                    "apples","apricots","cherries","peaches","pears","sorghum","peas",
                    "beans","plums","almonds","bananas","figs","guavas","lemons",
                    "limes","pecans","pineapples","pomeloes","grapes","alfalfa",
                    "sbeetwt","blackber","currants","raspber","strawber","cranber",
                    "clover","canesug_sir","soybean","sorghumg","chicory","emmersplet",
                    "millet","mustard","sunflower","vetch","hazelnults","canola","cansug",
                    "hazelnuts","olives")
crop_output2 <- unlist(lapply(crop_output,FUN=function(x) paste0(x,"_output")))
for(rank in seq_along(crop_output)){
  setnames(datafinal, crop_output[rank], crop_output2[rank])
}
# Acreage
crop_acreage <- grep(x=names(datafinal),pattern="ac$",value=T)
crop_acreage <- grep(x=crop_acreage,pattern="irrac",invert=T,value=T)
crop_acreage2 <- unlist(lapply(crop_acreage,FUN=function(x) paste0(x,"reage")))
for(rank in seq_along(crop_acreage)){
  setnames(datafinal, crop_acreage[rank], crop_acreage2[rank])
}
names(datafinal) <- gsub('acreage', '_acreage', names(datafinal))
# Trees
names(datafinal) <- gsub('tree', '_tree', names(datafinal))
# Other
setnames(datafinal, "canesugac_sir", "canesug_sir_acreage")
setnames(datafinal, "cornac_all", "corn_all_acreage")
setnames(datafinal, "corn_grain", "corn_grain_output")
setnames(datafinal, "cornac_grain", "corn_grain_acreage")
setnames(datafinal, "cornac_silage", "corn_silage_acreage")
setnames(datafinal, "corn_silage", "corn_silage_output")
setnames(datafinal, "soybeanac_other", "soybean_other_acreage")
setnames(datafinal, "soyebeanac_bean", "soybean_other_output")
setnames(datafinal, "grapetab", "grapestable_output")

# Remove tree variables
colstoremove <- grep(names(datafinal),pattern="_tree",value=T)
datafinal <- datafinal[,(colstoremove):=NULL]

# Corn & Sorghum & others
colscorn <- grep(names(datafinal),pattern="corn",value=T)
colssorgh <- grep(names(datafinal),pattern="sorgh",value=T)
datafinal <- datafinal[is.na(corn_acreage),corn_acreage:=rowSums(.SD,na.rm=TRUE),.SDcols=c("corn_grain_acreage","corn_silage_acreage","cornac_other")]
datafinal <- datafinal[is.na(corn_acreage),corn_acreage:=rowSums(.SD,na.rm=TRUE),.SDcols=c("corn_all_acreage")]
datafinal <- datafinal[is.na(sorgh_acreage),sorgh_acreage:=rowSums(.SD,na.rm=TRUE),.SDcols=c("sorghumg_acreage")]
datafinal <- datafinal[is.na(soybean_acreage),soybean_acreage:=rowSums(.SD,na.rm=TRUE),
                       .SDcols=c("soybean_other_acreage","soybeanac_bean")]
# Modif here Sept 14 - used to be if is.na(onion_acreage), but was not found in dataset
datafinal <- datafinal[,onion_acreage:=rowSums(.SD,na.rm=TRUE),
                       .SDcols=c("oniond_acreage","oniong_acreage")]
datafinal <- datafinal[!is.na(endive_acreage),escarole_acreage:=rowSums(.SD,na.rm=TRUE),
                       .SDcols=c("endive_acreage","escarole_acreage")]
datafinal <- datafinal[!is.na(lettuce_acreage)|!is.na(romaine_acreage),lettuceromaine_acreage:=rowSums(.SD,na.rm=TRUE),
                       .SDcols=c("lettuce_acreage","romaine_acreage")]
datafinal <- datafinal[is.na(canesug_acreage),canesug_acreage:=rowSums(.SD,na.rm=TRUE),
                       .SDcols=c("canesug_sir_acreage")]
datafinal <- datafinal[,cabbage_acreage:=rowSums(.SD,na.rm=TRUE),
                       .SDcols=c("cabbage_acreage","ccabbage_acreage")]

colstoremove <- c("corn_grain_acreage","corn_silage_acreage","cornac_other","corn_all_acreage",
                  "sorghumg_acreage","lima_acreage","soybean_other_acreage","soybeanac_bean",
                  "oniond_acreage","oniong_acreage","endive_acreage","lettuce_acreage","romaine_acreage",
                  "canesug_sir_acreage","cabbage_acreage","ccabbage_acreage")
datafinal <- datafinal[,(colstoremove):=NULL]

# Removal of some years with strange data values - based on timeseries in diversity_time_series.R
datafinal <- datafinal[year==1935,irpotat_acreage:=NA]
datafinal <- datafinal[year==1935,swpotat_acreage:=NA]
datafinal <- datafinal[year==1935,tomato_acreage:=NA]
datafinal <- datafinal[year==1969,watermelon_acreage:=NA]
datafinal <- datafinal[year==1925,oats_acreage:=NA]

# Finally we remove output for now
colstoremove <- grep(names(datafinal),pattern="output|oranges|soybean_bean",value=T)
datafinal <- datafinal[,(colstoremove):=NULL]


saveRDS(datafinal, build_file("haines_census", "finaldataset_continuous.rds"))
})
