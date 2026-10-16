# Main Table 1: baseline OLS and IV models.
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


ols1 <- feols(BWpresence ~ 1 + ginisimpson + mean_tp + mean_tmax + mean_tmin + initial_cotton + initial_totacres | STATEFP^year ,
              data = data_9030, cluster = ~base_fips)

iv1 <- feols(BWpresence ~1 +  mean_tp + mean_tmax + mean_tmin + initial_cotton + initial_totacres | STATEFP^year | ginisimpson ~ ruggedness,
             data = data_9030, cluster = ~base_fips)

ols2 <- feols(BWpresence ~ 1 + ginisimpson + mean_tp + mean_tmax + mean_tmin + initial_cotton + initial_totacres + initial_richness | STATEFP^year ,
              data = data_9030, cluster = ~base_fips)

iv2 <- feols(BWpresence ~1 +  mean_tp + mean_tmax + mean_tmin + initial_cotton + initial_totacres + initial_richness  | STATEFP^year | ginisimpson ~ ruggedness,
             data = data_9030, cluster = ~base_fips)


# First stages and reported summary statistics.
#### iv1
est_data_iv1 <- data_9030[setdiff(seq_len(nrow(data_9030)), iv1$obs_selection$obsRemoved), ]
first_stage_iv1 <- feols(ginisimpson ~ ruggedness + mean_tp + mean_tmax + mean_tmin + initial_cotton + initial_totacres | STATEFP^year,
                         data = est_data_iv1, cluster = ~base_fips)
#### iv2
est_data_iv2 <- data_9030[setdiff(seq_len(nrow(data_9030)), iv2$obs_selection$obsRemoved), ]
first_stage_iv2 <- feols(ginisimpson ~ ruggedness + mean_tp + mean_tmax + mean_tmin + initial_cotton + initial_totacres | STATEFP^year,
                         data = est_data_iv2, cluster = ~base_fips)

### Compute mean of main regressors in the various regressions
mean_gini_ols1 <- mean(data_9030[setdiff(seq_len(nrow(data_9030)), -ols1$obs_selection$obsRemoved), ginisimpson])
mean_gini_ols2 <- mean(data_9030[setdiff(seq_len(nrow(data_9030)), -ols2$obs_selection$obsRemoved), ginisimpson])
mean_gini_iv1 <- mean(predict(first_stage_iv1), na.rm = TRUE)
mean_gini_iv2 <- mean(predict(first_stage_iv2), na.rm = TRUE)

### Compute standard deviation of main regressors in the various regressions
sd_gini_ols1 <- sd(data_9030[setdiff(seq_len(nrow(data_9030)), -ols1$obs_selection$obsRemoved), ginisimpson])
sd_gini_ols2 <- sd(data_9030[setdiff(seq_len(nrow(data_9030)), -ols2$obs_selection$obsRemoved), ginisimpson])
sd_gini_iv1 <- sd(predict(first_stage_iv1), na.rm = TRUE)
sd_gini_iv2 <- sd(predict(first_stage_iv2), na.rm = TRUE)


### Compute mean of dependent variable
mean_BW_ols1 <- mean(data_9030[setdiff(seq_len(nrow(data_9030)), -ols1$obs_selection$obsRemoved), BWpresence])
mean_BW_ols2 <- mean(data_9030[setdiff(seq_len(nrow(data_9030)), -ols2$obs_selection$obsRemoved), BWpresence])
mean_BW_iv1 <- mean(data_9030[setdiff(seq_len(nrow(data_9030)), -iv1$obs_selection$obsRemoved), BWpresence])
mean_BW_iv2 <- mean(data_9030[setdiff(seq_len(nrow(data_9030)), -iv2$obs_selection$obsRemoved), BWpresence])

### Compute mean of dependent variable
sd_BW_ols1 <- sd(data_9030[setdiff(seq_len(nrow(data_9030)), -ols1$obs_selection$obsRemoved), BWpresence])
sd_BW_ols2 <- sd(data_9030[setdiff(seq_len(nrow(data_9030)), -ols2$obs_selection$obsRemoved), BWpresence])
sd_BW_iv1 <-  sd(data_9030[setdiff(seq_len(nrow(data_9030)), -iv1$obs_selection$obsRemoved), BWpresence])
sd_BW_iv2 <-  sd(data_9030[setdiff(seq_len(nrow(data_9030)), -iv2$obs_selection$obsRemoved), BWpresence])

## compute mean and sd ruggedness
mean_rugged_iv1 <-  mean(data_9030[setdiff(seq_len(nrow(data_9030)), -iv1$obs_selection$obsRemoved), ruggedness])
mean_rugged_iv2 <-  mean(data_9030[setdiff(seq_len(nrow(data_9030)), -iv2$obs_selection$obsRemoved), ruggedness])

sd_rugged_iv1 <-  sd(data_9030[setdiff(seq_len(nrow(data_9030)), -iv1$obs_selection$obsRemoved), ruggedness])
sd_rugged_iv2 <-  sd(data_9030[setdiff(seq_len(nrow(data_9030)), -iv2$obs_selection$obsRemoved), ruggedness])


# 3. Collect the results for this table ----
table_models <- list(ols1, iv1, ols2, iv2)

table_statistics <- list(Clusters = c(attr(summary(ols1)$cov.scaled, "G"), attr(summary(iv1)$cov.scaled, "G"), attr(summary(ols2)$cov.scaled, 
    "G"), attr(summary(iv2)$cov.scaled, "G")), `Mean Gini` = round(c(mean_gini_ols1, mean_gini_iv1, mean_gini_ols2, 
    mean_gini_iv2), 2), `Sd. Gini` = round(c(sd_gini_ols1, sd_gini_iv1, sd_gini_ols2, sd_gini_iv2), 2), 
    `Mean Boll Weevil` = round(c(mean_BW_ols1, mean_BW_iv1, mean_BW_ols2, mean_BW_iv2), 2), `Sd. Boll Weevil` = round(c(sd_BW_ols1, 
        sd_BW_iv1, sd_BW_ols2, sd_BW_iv2), 2), `Mean Ruggedness` = round(c(NA, mean_rugged_iv1, NA, mean_rugged_iv2), 
        2), `Sd. Ruggedness` = round(c(NA, sd_rugged_iv1, NA, sd_rugged_iv2), 2), `First Stage Estimate` = c(NA, 
        first_stage_iv1$coefficients["ruggedness"], NA, first_stage_iv2$coefficients["ruggedness"]), 
    `First Stage Estimate` = c(NA, first_stage_iv1$coeftable[1, 2], NA, first_stage_iv2$coeftable[1, 
        2]))

first_stage_models <- list(NULL, first_stage_iv1, NULL, first_stage_iv2)

raw_table <- fixest::etable(ols1, iv1, ols2, iv2, cluster = ~base_fips, keep = "ginisimpson", fitstat = ~n + r2 + 
    ivf1, extralines = table_statistics, tex = TRUE)

# Save the numerical values alongside the finished table.
table_coefficients <- rbindlist(lapply(seq_along(table_models), function(i) {
  m <- table_models[[i]]
  ct <- as.data.frame(fixest::coeftable(m))
  data.frame(model = i, term = rownames(ct), estimate = ct[[1]],
             standard_error = ct[[2]], statistic = ct[[3]], p_value = ct[[4]],
             observations = stats::nobs(m), row.names = NULL)
}))
table_extra <- rbindlist(lapply(seq_along(table_statistics), function(j) {
  values <- table_statistics[[j]]
  data.table(label = names(table_statistics)[j], model = seq_along(values), value = as.character(values))
}))
first_coef <- vapply(first_stage_models, function(m) if (is.null(m)) NA_real_ else
  unname(coef(m)["ruggedness"]), numeric(1))
first_se <- vapply(first_stage_models, function(m) if (is.null(m)) NA_real_ else
  unname(fixest::coeftable(m)["ruggedness", "Std. Error"]), numeric(1))
coefficients <- table_coefficients[term %in% c("ginisimpson", "fit_ginisimpson",
  "ginisimpson_alt", "fit_ginisimpson_alt", "pielou", "fit_pielou")][order(model)]
stars <- ifelse(coefficients$p_value < .01, "***",
  ifelse(coefficients$p_value < .05, "**", ifelse(coefficients$p_value < .1, "*", "")))

# 4. Write the manuscript table ----
# Formatting used by this table.
format_value <- function(value, digits = 3, commas = FALSE) {
  if (is.na(value)) return("")
  formatC(value, format = "f", digits = digits, big.mark = if (commas) "," else "")
}
table_row <- function(label, values) {
  paste0("  ", if (nzchar(label)) paste0(label, " ") else "", "& ", paste(values, collapse = " & "), " \\\\")
}
latex_numbers <- function(x) {
  x <- gsub(',', '', x, fixed = TRUE)
  x <- gsub('$-$', '-', x, fixed = TRUE)
  x <- gsub('\\$\\^\\{[^}]*\\}\\$', '', x)
  regmatches(x, gregexpr('[-+]?[0-9]+(?:\\.[0-9]+)?', x, perl = TRUE))[[1L]]
}

split_tex_row <- function(line) {
  cells <- strsplit(line, '&', fixed = TRUE)[[1L]]
  cells <- trimws(cells)
  cells[length(cells)] <- trimws(sub('\\\\\\\\.*$', '', cells[length(cells)]))
  cells
}

extra <- function(label, occurrence = 1L) {
  vapply(seq_along(table_models), function(j) {
    x <- table_extra$value[table_extra$label == label & table_extra$model == j]
    if (length(x) < occurrence) NA_real_ else as.numeric(x[occurrence])
  }, numeric(1))
}
raw_stat <- function(label, prefix = FALSE) {
  rows <- lapply(raw_table[grepl("&", raw_table, fixed = TRUE)], split_tex_row)
  hit <- Filter(function(r) if (prefix) startsWith(r[1L], label) else r[1L] == label, rows)
  stopifnot(length(hit) == 1L)
  vapply(hit[[1L]][-1L], function(x) {
    values <- latex_numbers(x)
    if (length(values)) as.numeric(values[1L]) else NA_real_
  }, numeric(1))
}
table_tex <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lcccc}",
  "   \\toprule \\toprule",
  "   Dependent Variable: & \\multicolumn{4}{c}{Boll Weevil Presence} \\\\",
  "   Model: & (1) & (2) & (3) & (4) \\\\",
  "          & OLS  & IV & OLS & IV \\\\",
  "   \\midrule",
  table_row("Crop Diversity (Gini-Simpson)", c(
    paste0(format_value(coefficients$estimate[1], 3), if (nzchar(stars[1])) paste0("$^{", stars[1], "}$") else ""),
    paste0(format_value(coefficients$estimate[2], 3), if (nzchar(stars[2])) paste0("$^{", stars[2], "}$") else ""),
    paste0(format_value(coefficients$estimate[3], 3), if (nzchar(stars[3])) paste0("$^{", stars[3], "}$") else ""),
    paste0(format_value(coefficients$estimate[4], 3), if (nzchar(stars[4])) paste0("$^{", stars[4], "}$") else ""))),
  table_row("", c(
    paste0("(", format_value(coefficients$standard_error[1], 3), ")"),
    paste0("(", format_value(coefficients$standard_error[2], 3), ")"),
    paste0("(", format_value(coefficients$standard_error[3], 3), ")"),
    paste0("(", format_value(coefficients$standard_error[4], 3), ")"))),
  "   & & & & \\\\",
  "   State $\\times$ Decade FE        & Yes & Yes & Yes & Yes \\\\",
  "   Baseline Crop Diversity         & No   & No & Yes & Yes \\\\",
  "   \\midrule",
  table_row("Observations", c(
    format_value(coefficients$observations[1], 0, commas = TRUE),
    format_value(coefficients$observations[2], 0, commas = TRUE),
    format_value(coefficients$observations[3], 0, commas = TRUE),
    format_value(coefficients$observations[4], 0, commas = TRUE))),
  table_row("Number of Clusters", c(
    format_value(extra("Clusters")[1], 0, commas = TRUE),
    format_value(extra("Clusters")[2], 0, commas = TRUE),
    format_value(extra("Clusters")[3], 0, commas = TRUE),
    format_value(extra("Clusters")[4], 0, commas = TRUE))),
  table_row("R$^2$", c(
    format_value(raw_stat("R$^2$")[1], 2),
    format_value(raw_stat("R$^2$")[2], 2),
    format_value(raw_stat("R$^2$")[3], 2),
    format_value(raw_stat("R$^2$")[4], 2))),
  "   & & & & \\\\",
  "",
  table_row("Mean Boll Weevil", c(
    format_value(extra("Mean Boll Weevil")[1], 2),
    format_value(extra("Mean Boll Weevil")[2], 2),
    format_value(extra("Mean Boll Weevil")[3], 2),
    format_value(extra("Mean Boll Weevil")[4], 2))),
  table_row("SD Boll Weevil", c(
    format_value(extra("Sd. Boll Weevil")[1], 2),
    format_value(extra("Sd. Boll Weevil")[2], 2),
    format_value(extra("Sd. Boll Weevil")[3], 2),
    format_value(extra("Sd. Boll Weevil")[4], 2))),
  table_row("Mean Gini-Simpson", c(
    format_value(extra("Mean Gini")[1], 2),
    format_value(extra("Mean Gini")[2], 2),
    format_value(extra("Mean Gini")[3], 2),
    format_value(extra("Mean Gini")[4], 2))),
  table_row("SD Gini-Simpson", c(
    format_value(extra("Sd. Gini")[1], 2),
    format_value(extra("Sd. Gini")[2], 2),
    format_value(extra("Sd. Gini")[3], 2),
    format_value(extra("Sd. Gini")[4], 2))),
  "   ",
  "   \\midrule",
  table_row("First Stage", c(
    "",
    paste0(format_value(first_coef[2], 3), " (", format_value(first_se[2], 3), ")"),
    "",
    paste0(format_value(first_coef[4], 3), " (", format_value(first_se[4], 3), ")"))),
  table_row("Mean Ruggedness", c(
    "",
    format_value(extra("Mean Ruggedness")[2], 2),
    "",
    format_value(extra("Mean Ruggedness")[4], 2))),
  table_row("SD Ruggedness", c(
    "",
    format_value(extra("Sd. Ruggedness")[2], 2),
    "",
    format_value(extra("Sd. Ruggedness")[4], 2))),
  table_row("F-statistic", c(
    "",
    format_value(raw_stat("F-test (1st stage)", prefix = TRUE)[2], 1),
    "",
    format_value(raw_stat("F-test (1st stage)", prefix = TRUE)[4], 1))),
  "   \\bottomrule \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)
writeLines(table_tex, "../output/tables/Table1.tex")
