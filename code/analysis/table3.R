# Main Table 3: alternative samples.
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


# Baseline, through first contamination, contamination boundary, ever-cotton counties.
# Determine first contamination BEFORE filtering on contemporaneous cotton acreage.
t3_period <- copy(datacrop[year >= 1890 & year <= 1930])
t3_period[BWpresence %in% c(1910, 1920, 1930),
          BWpresence := as.integer(year >= BWpresence)]
t3_period[, t3_first := if (any(BWpresence == 1, na.rm = TRUE))
  as.numeric(min(year[which(BWpresence == 1)])) else Inf, by = base_fips]
t3_period[, t3_eligible := any(cotton > 0, na.rm = TRUE), by = base_fips]
t3_baseline <- t3_period[cotton > 0]
t3_through <- t3_baseline[year <= t3_first]
t3_samples <- list(t3_baseline, t3_through,
                   t3_through[BWneighboringpresence_lag == 1],
                   t3_period[t3_eligible == TRUE])
t3_models <- lapply(t3_samples, function(d) feols(
  BWpresence ~ mean_tp + mean_tmax + mean_tmin + initial_cotton + initial_totacres |
    STATEFP^year | ginisimpson ~ ruggedness, data = d, cluster = ~base_fips))
# Use actual model rows and the model's own first stage for summary statistics.
t3_stats <- rbindlist(lapply(seq_along(t3_models), function(i) {
  m <- t3_models[[i]]; d <- t3_samples[[i]][fixest::obs(m)]
  fs <- summary(m, stage = 1)
  data.table(column = i, estimate = coef(m)['fit_ginisimpson'],
    se = se(m)['fit_ginisimpson'], p = pvalue(m)['fit_ginisimpson'],
    N = nobs(m), counties = uniqueN(d$base_fips),
    r2 = as.numeric(fitstat(m, 'r2')$r2),
    mean_y = mean(d$BWpresence), sd_y = sd(d$BWpresence),
    mean_fitted = mean(fitted(fs)), sd_fitted = sd(fitted(fs)),
    fs = coef(fs)['ruggedness'], fs_se = se(fs)['ruggedness'],
    mean_ruggedness = mean(d$ruggedness), sd_ruggedness = sd(d$ruggedness),
    F = as.numeric(fitstat(m, 'ivf1')$ivf1$stat))
}))
t3_num <- function(x, digits = 3L) formatC(x, format = 'f', digits = digits)
t3_row <- function(label, x) paste0('   ', label, ' & ', paste(x, collapse = ' & '), ' \\\\')
t3_stars <- ifelse(t3_stats$p < .01, '***', ifelse(t3_stats$p < .05, '**', ifelse(t3_stats$p < .1, '*', '')))
t3_tex <- c('\\begingroup', '\\centering', '\\setlength{\\tabcolsep}{5pt}',
  '\\begin{tabular}{lcccc}', '   \\toprule \\toprule',
  '   Dependent Variable: & \\multicolumn{4}{c}{Boll Weevil Presence} \\\\',
  t3_row('Model:', paste0('(', 1:4, ')')), t3_row('', rep('IV', 4)), '   \\midrule',
  t3_row('Crop Diversity (Gini-Simpson)', paste0(t3_num(t3_stats$estimate), '$^{', t3_stars, '}$')),
  t3_row('', paste0('(', t3_num(t3_stats$se), ')')),
  t3_row('State $\\times$ Decade FE', rep('Yes', 4)),
  t3_row('', rep('', 4)),
  t3_row('Sample', c('Baseline', 'Through first', 'Contamination', 'Ever cotton')),
  t3_row('', c('', 'contamination', 'boundary', '1890--1930')), '   \\midrule',
  t3_row('Observations', formatC(t3_stats$N, format='d', big.mark=',')),
  t3_row('Number of Clusters', formatC(t3_stats$counties, format='d', big.mark=',')),
  t3_row('R$^2$', t3_num(t3_stats$r2, 2)), t3_row('', rep('', 4)),
  t3_row('Mean Boll Weevil', t3_num(t3_stats$mean_y, 2)),
  t3_row('SD Boll Weevil', t3_num(t3_stats$sd_y, 2)),
  t3_row('Mean Gini-Simpson', t3_num(t3_stats$mean_fitted)),
  t3_row('SD Gini-Simpson', t3_num(t3_stats$sd_fitted)),
  t3_row('', rep('', 4)),
  '   \\midrule',
  t3_row('First Stage', paste0(t3_num(t3_stats$fs), ' (', t3_num(t3_stats$fs_se), ')')),
  t3_row('Mean Ruggedness', t3_num(t3_stats$mean_ruggedness, 2)),
  t3_row('SD Ruggedness', t3_num(t3_stats$sd_ruggedness, 2)),
  t3_row('$F$-statistic', t3_num(t3_stats$F, 1)),
  '   \\bottomrule \\bottomrule', '\\end{tabular}', '\\par\\endgroup')
writeLines(t3_tex, '../output/tables/Table3.tex')
