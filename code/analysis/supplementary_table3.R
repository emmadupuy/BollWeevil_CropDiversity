# Supplementary Table 3: linear IV and control-function models.
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


# --- 1. Baseline IV LPM (same spec as Table 1, col (2)) ---

iv_lpm <- feols(
  BWpresence ~ 1 + mean_tp + mean_tmax + mean_tmin + initial_cotton + initial_totacres |
    STATEFP^year | ginisimpson ~ ruggedness,
  data    = data_9030,
  cluster = ~ base_fips
)

# --- 2. First-stage for control-function logit/probit ---

# Preserve the original first-stage sample rule; see the replication audit.
est_data_cf <- data_9030[setdiff(seq_len(nrow(data_9030)), iv_lpm$obs_selection$obsRemoved), ]

fs_cf_lpm <- feols(
  ginisimpson ~ ruggedness + mean_tp + mean_tmax + mean_tmin +
    initial_cotton + initial_totacres | STATEFP^year,
  data    = est_data_cf,
  cluster = ~ base_fips
)

# Residuals from first stage (control function)
est_data_cf[, resid_fs := ginisimpson - predict(fs_cf_lpm, newdata = est_data_cf)]

# --- 3. Logit + control function ---

logit_cf <- feglm(
  BWpresence ~ ginisimpson + resid_fs + mean_tp + mean_tmax + mean_tmin +
    initial_cotton + initial_totacres | STATEFP^year,
  data    = est_data_cf,
  family  = binomial(link = "logit"),
  cluster = ~ base_fips
)

# --- 4. Probit + control function ---

probit_cf <- feglm(
  BWpresence ~ ginisimpson + resid_fs + mean_tp + mean_tmax + mean_tmin +
    initial_cotton + initial_totacres | STATEFP^year,
  data    = est_data_cf,
  family  = binomial(link = "probit"),
  cluster = ~ base_fips
)

# --- 5. Helper for rows used in each model ---

rows_used <- function(model, data){
  setdiff(seq_len(nrow(data)), model$obs_selection$obsRemoved)
}

# --- 6. Estimation samples for each model ---

idx_lpm   <- rows_used(iv_lpm, data_9030)
data_lpm  <- data_9030[idx_lpm]

idx_logit  <- rows_used(logit_cf, est_data_cf)
data_logit <- est_data_cf[idx_logit]

idx_probit  <- rows_used(probit_cf, est_data_cf)
data_probit <- est_data_cf[idx_probit]

# --- 7. Means and SDs of *predicted* Gini and of BWpresence ---

# First-stage predictions (fitted values) in each sample
ghat_lpm    <- predict(fs_cf_lpm,  newdata = data_lpm)

# Logit FS
fs_cf_logit <- feols(
  ginisimpson ~ ruggedness + mean_tp + mean_tmax + mean_tmin +
    initial_cotton + initial_totacres | STATEFP^year,
  data    = data_logit,
  cluster = ~ base_fips
)
ghat_logit  <- predict(fs_cf_logit, newdata = data_logit)

# Probit FS
fs_cf_probit <- feols(
  ginisimpson ~ ruggedness + mean_tp + mean_tmax + mean_tmin +
    initial_cotton + initial_totacres | STATEFP^year,
  data    = data_probit,
  cluster = ~ base_fips
)
ghat_probit <- predict(fs_cf_probit, newdata = data_probit)

# Means and SDs of *predicted* Gini-Simpson
mean_gini_lpm    <- mean(ghat_lpm,    na.rm = TRUE)
sd_gini_lpm      <- sd(ghat_lpm,      na.rm = TRUE)

mean_gini_logit  <- mean(ghat_logit,  na.rm = TRUE)
sd_gini_logit    <- sd(ghat_logit,    na.rm = TRUE)

mean_gini_probit <- mean(ghat_probit, na.rm = TRUE)
sd_gini_probit   <- sd(ghat_probit,   na.rm = TRUE)

# Means and SDs of the dependent variable (raw BWpresence)
mean_BW_lpm      <- mean(data_lpm$BWpresence,    na.rm = TRUE)
sd_BW_lpm        <- sd(data_lpm$BWpresence,      na.rm = TRUE)

mean_BW_logit    <- mean(data_logit$BWpresence,  na.rm = TRUE)
sd_BW_logit      <- sd(data_logit$BWpresence,    na.rm = TRUE)

mean_BW_probit   <- mean(data_probit$BWpresence, na.rm = TRUE)
sd_BW_probit     <- sd(data_probit$BWpresence,   na.rm = TRUE)

# --- 8. First-stage coefficients and SEs on ruggedness ---

fs_coef_lpm    <- fs_cf_lpm$coefficients["ruggedness"]
fs_se_lpm      <- fs_cf_lpm$coeftable["ruggedness", "Std. Error"]

fs_coef_logit  <- fs_cf_logit$coefficients["ruggedness"]
fs_se_logit    <- fs_cf_logit$coeftable["ruggedness", "Std. Error"]

fs_coef_probit <- fs_cf_probit$coefficients["ruggedness"]
fs_se_probit   <- fs_cf_probit$coeftable["ruggedness", "Std. Error"]

# These rows previously inherited values from Supplementary Table 5.
# Recompute that same dependency explicitly so this table runs independently.
iv1 <- feols(BWpresence ~1 +  mean_tp + mean_tmax + mean_tmin + initial_cotton + initial_totacres | STATEFP^year | ginisimpson ~ ruggedness,
             data = data_9030, cluster = ~base_fips)

iv2 <- feols(BWpresence ~1 +  mean_tp + mean_tmax + mean_tmin + initial_cotton + initial_totacres | STATEFP + year | ginisimpson ~ ruggedness,
             data = data_9030, cluster = ~base_fips)

iv3 <- feols(BWpresence ~1 +  mean_tp + mean_tmax + mean_tmin + initial_cotton + initial_totacres + year*STATEFP | STATEFP + year | ginisimpson ~ ruggedness,
             data = data_9030, cluster = ~base_fips)
rugged_models <- list(iv1, iv2, iv3)
rugged_means <- vapply(rugged_models, function(m)
  mean(data_9030[setdiff(seq_len(nrow(data_9030)), m$obs_selection$obsRemoved), ruggedness]), numeric(1))
rugged_sds <- vapply(rugged_models, function(m)
  sd(data_9030[setdiff(seq_len(nrow(data_9030)), m$obs_selection$obsRemoved), ruggedness]), numeric(1))

# --- 9. Marginal effects for a 1 s.d. increase in predicted Gini ---

sd_gini_pred <- sd(ghat_lpm, na.rm = TRUE)   # SD of predicted Gini in IV sample

## LPM-IV: marginal effect = beta * sd_pred
me_lpm <- coef(iv_lpm)["fit_ginisimpson"] * sd_gini_pred

## Logit CF: Δp from 1 s.d. shift
p0_logit <- mean(predict(logit_cf, type = "response"), na.rm = TRUE)
newdata  <- copy(est_data_cf)
newdata$ginisimpson <- newdata$ginisimpson + sd_gini_pred
newdata$resid_fs    <- newdata$resid_fs       # keep residual fixed
p1_logit <- mean(predict(logit_cf, newdata = newdata, type = "response"), na.rm = TRUE)
me_logit <- p1_logit - p0_logit   # ~ -0.20

## Probit CF: Δp from 1 s.d. shift
p0_probit <- mean(predict(probit_cf, type = "response"), na.rm = TRUE)
newdata   <- copy(est_data_cf)
newdata$ginisimpson <- newdata$ginisimpson + sd_gini_pred
newdata$resid_fs    <- newdata$resid_fs
p1_probit <- mean(predict(probit_cf, newdata = newdata, type = "response"), na.rm = TRUE)
me_probit <- p1_probit - p0_probit   # ~ -0.22

# --- 10. Build comparison table with extra lines incl. SDs and first stage ---


# 3. Collect the results for this table ----
table_models <- list(iv_lpm, logit_cf, probit_cf)

table_statistics <- list(Model = c("LPM IV (2SLS)", "Logit + CF", "Probit + CF"), Clusters = c(attr(summary(iv_lpm)$cov.scaled, 
    "G"), attr(summary(logit_cf)$cov.scaled, "G"), attr(summary(probit_cf)$cov.scaled, "G")), `Mean Gini-Simpson (pred.)` = round(c(mean_gini_lpm, 
    mean_gini_logit, mean_gini_probit), 3), `Sd. Gini-Simpson (pred.)` = round(c(sd_gini_lpm, sd_gini_logit, 
    sd_gini_probit), 3), `Mean Boll Weevil` = round(c(mean_BW_lpm, mean_BW_logit, mean_BW_probit), 3), 
    `Sd. Boll Weevil` = round(c(sd_BW_lpm, sd_BW_logit, sd_BW_probit), 3), `First Stage (Ruggedness)` = round(c(fs_coef_lpm, 
        fs_coef_logit, fs_coef_probit), 4), `First Stage SE` = round(c(fs_se_lpm, fs_se_logit, fs_se_probit), 
        4), `Mean Ruggedness` = round(rugged_means, 2), `Sd. Ruggedness` = round(rugged_sds, 2), `Marginal effect (1 s.d.)` = round(c(me_lpm, 
        me_logit, me_probit), 3))

first_stage_models <- list(fs_cf_lpm, fs_cf_logit, fs_cf_probit)

raw_table <- fixest::etable(iv_lpm, logit_cf, probit_cf, cluster = ~base_fips, keep = "ginisimpson", fitstat = ~n + 
    r2 + ivf1, extralines = table_statistics, tex = TRUE)

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
  "\\begin{tabular}{lccc}",
  " \\midrule \\midrule",
  "   Dependent Variable: & \\multicolumn{3}{c}{Boll Weevil Presence}\\\\",
  "   Model:                    & (1)            & (2)            & (3)\\\\  ",
  "                             &  IV           & Logit + CF     & Probit + CF\\\\  ",
  "   \\midrule",
  table_row("Crop Diversity (Gini-Simpson)", c(
    paste0(format_value(coefficients$estimate[1], 3), if (nzchar(stars[1])) paste0("$^{", stars[1], "}$") else ""),
    paste0(format_value(coefficients$estimate[2], 2), if (nzchar(stars[2])) paste0("$^{", stars[2], "}$") else ""),
    paste0(format_value(coefficients$estimate[3], 2), if (nzchar(stars[3])) paste0("$^{", stars[3], "}$") else ""))),
  table_row("", c(
    paste0("(", format_value(coefficients$standard_error[1], 3), ")"),
    paste0("(", format_value(coefficients$standard_error[2], 2), ")"),
    paste0("(", format_value(coefficients$standard_error[3], 2), ")"))),
  "      &\\\\",
  "   State $\\times$ Decade FE                      & Yes            & Yes            & Yes\\\\  ",
  " \t\\midrule",
  table_row("Observations", c(
    format_value(coefficients$observations[1], 0, commas = TRUE),
    format_value(coefficients$observations[2], 0, commas = TRUE),
    format_value(coefficients$observations[3], 0, commas = TRUE))),
  table_row("Number of Clusters", c(
    format_value(extra("Clusters")[1], 0, commas = TRUE),
    format_value(extra("Clusters")[2], 0),
    format_value(extra("Clusters")[3], 0))),
  "    &\\\\",
  table_row("Marginal effect (1 s.d.)", c(
    format_value(extra("Marginal effect (1 s.d.)")[1], 3),
    format_value(extra("Marginal effect (1 s.d.)")[2], 3),
    format_value(extra("Marginal effect (1 s.d.)")[3], 3))),
  "",
  "   &\\\\  ",
  table_row("Mean Boll Weevil", c(
    format_value(extra("Mean Boll Weevil")[1], 2),
    format_value(extra("Mean Boll Weevil")[2], 2),
    format_value(extra("Mean Boll Weevil")[3], 2))),
  table_row("SD Boll Weevil", c(
    format_value(extra("Sd. Boll Weevil")[1], 2),
    format_value(extra("Sd. Boll Weevil")[2], 2),
    format_value(extra("Sd. Boll Weevil")[3], 2))),
  table_row("Mean Gini-Simpson", c(
    format_value(extra("Mean Gini-Simpson (pred.)")[1], 2),
    format_value(extra("Mean Gini-Simpson (pred.)")[2], 2),
    format_value(extra("Mean Gini-Simpson (pred.)")[3], 2))),
  table_row("SD Gini-Simpson", c(
    format_value(extra("Sd. Gini-Simpson (pred.)")[1], 2),
    format_value(extra("Sd. Gini-Simpson (pred.)")[2], 2),
    format_value(extra("Sd. Gini-Simpson (pred.)")[3], 2))),
  "   \\midrule",
  table_row("First Stage", c(
    paste0(format_value(first_coef[1], 3), " (", format_value(first_se[1], 3), ")"),
    paste0(format_value(first_coef[2], 3), " (", format_value(first_se[2], 3), ")"),
    paste0(format_value(first_coef[3], 3), " (", format_value(first_se[3], 3), ")"))),
  table_row("Mean Ruggedness", c(
    format_value(extra("Mean Ruggedness")[1], 2),
    format_value(extra("Mean Ruggedness")[2], 2),
    format_value(extra("Mean Ruggedness")[3], 2))),
  table_row("SD Ruggedness", c(
    format_value(extra("Sd. Ruggedness")[1], 2),
    format_value(extra("Sd. Ruggedness")[2], 2),
    format_value(extra("Sd. Ruggedness")[3], 2))),
  "   \\midrule \\midrule",
  "   ",
  "\\end{tabular}",
  "",
  ""
)
writeLines(table_tex, "../output/tables/TableS3.tex")
