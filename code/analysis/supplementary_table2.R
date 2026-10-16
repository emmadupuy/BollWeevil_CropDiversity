# Supplementary Table 2: descriptive statistics.
# Open replication.Rproj, then run this file from top to bottom (or click Source).
# Paths are relative to code/; this script does not change the working directory.

if (!dir.exists("../data/intermediate"))
  stop("Open replication.Rproj or set the working directory to code/ first.")

# 1. Packages and output folders ----
library(data.table)
library(dplyr)
library(stringr)
library(sf)
library(fixest)
library(R.utils)

dir.create("../output/tables", recursive = TRUE, showWarnings = FALSE)
set.seed(123)

# 2. Data and analysis ----
datacrop <- fread("../data/intermediate/panels/datacrop.csv.gz")
datacrop[, statefips_code := substr(str_pad(base_fips, 5, "left", "0"), 1, 2)]

# load alternative data for comparison
datacrop2 <- fread("../data/intermediate/panels/datacrop_altxwalk.csv.gz")
datacrop2 <- datacrop2[,c("base_fips","year","ginisimpson"),with=F]
setnames(datacrop2,"ginisimpson","ginisimpson_alt")
datacrop <- setDT(left_join(datacrop,datacrop2,by=c("base_fips","year")))

## Add the supplied county coordinates and areas.
# Built by clean/county_coordinates.R from the 2016 Census boundaries.
counties <- readRDS("../data/intermediate/panels/county_coordinates.rds")
datacrop <- left_join(datacrop, counties)

datacrop <- datacrop[order(base_fips, year)]
datacrop[year %in%c(1920, 1925, 1930), distance_to_rail := mean(distance_to_rail, na.rm= T), by = "base_fips"]

# initial cotton
datacrop[year==1880,initial_cotton:=cotton]
datacrop[,initial_cotton:=max(initial_cotton,na.rm=T),by=list(base_fips)]
datacrop[year==1880,initial_richness:=richness]
datacrop[,initial_richness:=max(initial_richness,na.rm=T),by=list(base_fips)]
datacrop[year==1880,initial_pielou:=pielou]
datacrop[,initial_pielou:=max(initial_pielou,na.rm=T),by=list(base_fips)]
datacrop[year==1880,initial_totacres:=totacres]
datacrop[,initial_totacres:=max(initial_totacres,na.rm=T),by=list(base_fips)]

## 1. Add Weather ----
tp <- readRDS("../data/intermediate/weather/prism_tp_county.rds")
tp[,FIPS:=as.numeric(FIPS)]
tp[,year:=as.numeric(year)]
tmin <- readRDS("../data/intermediate/weather/prism_tmin_county.rds")
tmin[,FIPS:=as.numeric(FIPS)]
tmin[,year:=as.numeric(year)]
tmax <- readRDS("../data/intermediate/weather/prism_tmax_county.rds")
tmax[,FIPS:=as.numeric(FIPS)]
tmax[,year:=as.numeric(year)]
datacrop <- setDT(left_join(datacrop,tp,by=c("base_fips"="FIPS","year"='year')))
datacrop <- setDT(left_join(datacrop,tmin,by=c("base_fips"="FIPS","year"='year')))
datacrop <- setDT(left_join(datacrop,tmax,by=c("base_fips"="FIPS","year"='year')))
datacrop[,`:=`(mean_tp=mean(tp,na.rm=T),
               mean_tmin=mean(tmin,na.rm=T),
               mean_tmax=mean(tmax,na.rm=T)),by=list(base_fips)]

## BW Data
data_9030 <- datacrop[year>=1890&year<=1930,] # 1930
data_9030 <- data_9030[cotton > 0]
data_9030[,low_initial:=ifelse(initial_cotton<quantile(data_9030$initial_cotton,probs=c(.5),na.rm=T),1,0)]
data_9030[,tmean:=(tmin+tmax)/2]
# clean
data_9030[BWpresence%in%c(1910)&year>=1910,BWpresence:=1]
data_9030[BWpresence%in%c(1910)&year<1910,BWpresence:=0]
data_9030[BWpresence%in%c(1920)&year>=1920,BWpresence:=1]
data_9030[BWpresence%in%c(1920)&year<1920,BWpresence:=0]
data_9030[BWpresence%in%c(1930)&year>=1930,BWpresence:=1]
data_9030[BWpresence%in%c(1930)&year<1930,BWpresence:=0]

# BW sample
data_9030[BWpresence==1,yearBW:=min(year),by=list(base_fips)]
temp <- data_9030[, .(yearBW = min(yearBW, na.rm = T)), by = "base_fips"]
data_9030[, yearBW:=NULL]
data_9030 <- merge(data_9030, temp, by = c("base_fips"))
data_9030_drop_contaminated <- data_9030[year<=yearBW|is.na(yearBW),]
data_9030_drop_neigh <- data_9030_drop_contaminated[BWneighboringpresence_lag==1]


datasim <- data_9030[,c('ginisimpson','ruggedness','BWpresence','distance_to_rail','tmin','tmax','tp','totacres'),with=F]


# Display rail distance in kilometres, as in the manuscript.
display_data <- copy(datasim)
display_data[, distance_to_rail := distance_to_rail / 1000]
descriptives <- lapply(display_data, function(x) {
  x <- x[!is.na(x)]
  c(length(x), mean(x), sd(x), min(x), max(x))
})

# Write the manuscript table.
# Formatting used by this table.
format_value <- function(value, digits = 3, commas = FALSE) {
  if (is.na(value)) return("")
  formatC(value, format = "f", digits = digits, big.mark = if (commas) "," else "")
}
table_row <- function(label, values) {
  paste0("  ", if (nzchar(label)) paste0(label, " ") else "", "& ", paste(values, collapse = " & "), " \\\\")
}
table_tex <- c(
  "",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lccccc} ",
  "\\\\[-1.8ex]\\hline ",
  "\\hline \\\\[-1.8ex] ",
  "Statistic & \\multicolumn{1}{c}{N} & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{St. Dev.} & \\multicolumn{1}{c}{Min} & \\multicolumn{1}{c}{Max} \\\\ ",
  "\\hline \\\\[-1.8ex] ",
  table_row("Gini-Simpson", c(format_value(descriptives$ginisimpson[1], 0, commas = TRUE), format_value(descriptives$ginisimpson[2], 2), format_value(descriptives$ginisimpson[3], 2), format_value(descriptives$ginisimpson[4], 2), format_value(descriptives$ginisimpson[5], 2))),
  table_row("Ruggedness", c(format_value(descriptives$ruggedness[1], 0, commas = TRUE), format_value(descriptives$ruggedness[2], 2), format_value(descriptives$ruggedness[3], 2), format_value(descriptives$ruggedness[4], 2), format_value(descriptives$ruggedness[5], 2))),
  table_row("Boll Weevil Presence", c(format_value(descriptives$BWpresence[1], 0, commas = TRUE), format_value(descriptives$BWpresence[2], 2), format_value(descriptives$BWpresence[3], 2), format_value(descriptives$BWpresence[4], 0), format_value(descriptives$BWpresence[5], 0))),
  table_row("Distance to Rail", c(format_value(descriptives$distance_to_rail[1], 0, commas = TRUE), format_value(descriptives$distance_to_rail[2], 0), format_value(descriptives$distance_to_rail[3], 0), format_value(descriptives$distance_to_rail[4], 0), format_value(descriptives$distance_to_rail[5], 0))),
  table_row("Minimum Temperature", c(format_value(descriptives$tmin[1], 0, commas = TRUE), format_value(descriptives$tmin[2], 2), format_value(descriptives$tmin[3], 2), format_value(descriptives$tmin[4], 2), format_value(descriptives$tmin[5], 2))),
  table_row("Maximum Temperature", c(format_value(descriptives$tmax[1], 0, commas = TRUE), format_value(descriptives$tmax[2], 2), format_value(descriptives$tmax[3], 2), format_value(descriptives$tmax[4], 2), format_value(descriptives$tmax[5], 2))),
  table_row("Rainfall", c(format_value(descriptives$tp[1], 0, commas = TRUE), format_value(descriptives$tp[2], 0, commas = TRUE), format_value(descriptives$tp[3], 0), format_value(descriptives$tp[4], 0), format_value(descriptives$tp[5], 0, commas = TRUE))),
  table_row("County Agricultural Area", c(format_value(descriptives$totacres[1], 0, commas = TRUE), format_value(descriptives$totacres[2], 0, commas = TRUE), format_value(descriptives$totacres[3], 0, commas = TRUE), format_value(descriptives$totacres[4], 0), format_value(descriptives$totacres[5], 0, commas = TRUE))),
  "\\hline \\\\[-1.8ex] ",
  "\\end{tabular} ",
  ""
)
writeLines(table_tex, "../output/tables/TableS2.tex")
