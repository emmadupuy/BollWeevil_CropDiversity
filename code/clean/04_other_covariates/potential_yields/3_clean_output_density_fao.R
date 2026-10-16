rm(list = ls())
source("clean/paths.R")
library(data.table)
library(ggplot2)
library(usmap)
library(stringr)

## get crosswalk crop abbr
meta <- fread(raw_file(
  "optional_covariates", "fao", "Suitability and Attainable Yield_download_file.csv"
))
meta[, Name := gsub(".*_(.*)", "\\1", Name)]
meta <- unique(meta[, c("Name", "Crop", "Data Units")])


## get output density historical
output_density_hist <- fread(
  build_file("optional_covariates", "fao", "output_density_6190.csv")
)


output_density_hist[, fips := str_pad(CTFIPS, 5, "left", "0")]

colnms <- colnames(output_density_hist)[grepl(".tif",colnames(output_density_hist))]
output_density_hist[, (colnms):= lapply(.SD, function(x){gsub("\\{'mean': ", "", x)}), .SDcols = colnms]
output_density_hist[, (colnms):= lapply(.SD, function(x){gsub("\\}", "", x)}), .SDcols = colnms]
output_density_hist[, (colnms):= lapply(.SD, function(x){as.numeric(x)}), .SDcols = colnms]
colnames(output_density_hist)[grepl(".tif",colnames(output_density_hist))] <- 
  gsub(".tif", "", colnames(output_density_hist)[grepl(".tif",colnames(output_density_hist))])
colnames(output_density_hist)[grepl("avg",colnames(output_density_hist))] <- 
  gsub("avg_", "", colnames(output_density_hist)[grepl("avg",colnames(output_density_hist))])


colnms <- colnames(output_density_hist)[grepl("density", colnames(output_density_hist))]
output_density_hist <- melt(output_density_hist, id.vars = c("fips"),
                            measure.vars = colnms)

colnames(output_density_hist) <- c("fips","Name", "output_density")
output_density_hist[, Name := gsub("output_density_", "", Name)]

output_density_hist[, `Time Period` := "1961-1990"]
output_density_hist[, RCP := "Historical"]
output_density_hist[, `Climate Model` := "CRUTS32"]
output_density_hist[, `Water Supply` := "Rainfed"]
output_density_hist[, `CO2 Fertilization` := "With CO2 Fertilization"]
output_density_hist[, `Input Level` := "High"]
output_density_hist[, Name := gsub(".*_(.*)", "\\1", Name)]


output_density_hist <- merge(
  output_density_hist, 
  meta, 
  by= "Name"
)

## correct units
output_density_hist[grepl("10", `Data Units`), output_density := output_density * 10]
output_density_hist[grepl("10", `Data Units`), `Data Units` := "kg DW/ha"]


## Save
write.csv(output_density_hist, 
          build_file("optional_covariates", "fao", "output_density_6190_clean.csv"),
          row.names = F)


