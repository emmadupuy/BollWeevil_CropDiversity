# Supplementary Table 1: saved 2021 prediction validation.
# Open replication.Rproj, then run this file from top to bottom (or click Source).
# Paths are relative to code/; this script does not change the working directory.

if (!dir.exists("../data/intermediate"))
  stop("Open replication.Rproj or set the working directory to code/ first.")

# 1. Packages and output folders ----
library(data.table)
library(R.utils)

dir.create("../output/tables", recursive = TRUE, showWarnings = FALSE)
set.seed(123)

# 2. Data and analysis ----
# Reproduce the original validation calculation from the saved 2021 predictions.
# No model retraining or mlr installation is required for this step.
e <- new.env()
load("../data/intermediate/supplementary/model2_results2021.Rdata", envir = e)
results <- data.table::as.data.table(e$xgb_save[[1]]$data)
stopifnot(all(c("truth", "response") %in% names(results)))
# Preserve the original sequential bin assignments, including boundary treatment.
bin <- function(x) {
  category <- rep(NA_integer_, length(x))
  category[which(x >= 0 & x <= .25)] <- 1L
  category[which(x >= .25 & x <= .55)] <- 2L
  category[which(x >= .5 & x <= .75)] <- 3L
  category[which(x >= .75 & x <= 1)] <- 4L
  category
}
results[, category := bin(truth)]
results[, category_predict := bin(response)]
matrix <- table(factor(results$category, levels = 1:4),
                factor(results$category_predict, levels = 1:4))
labels <- c("0--25%", "25--50%", "50--75%", "75--100%")
dimnames(matrix) <- list(observed = labels, predicted = labels)
metrics <- data.frame(
  metric = c("observations", "binned_observations", "rmse", "r_squared"),
  value = c(nrow(results), sum(matrix),
            sqrt(mean((results$truth - results$response)^2, na.rm = TRUE)),
            1 - sum((results$truth - results$response)^2) /
              sum((results$truth - mean(results$truth))^2)))

# Write the manuscript table.
# Formatting used by this table.
table_row <- function(label, values) {
  paste0("  ", if (nzchar(label)) paste0(label, " ") else "", "& ", paste(values, collapse = " & "), " \\\\")
}
table_tex <- c(
  "\\begin{tabular}{r|rrrr}",
  "\\hline",
  " & 0 - 25\\% & 25\\% - 50\\% & 50\\% - 75\\% & 75\\% - 100\\% \\\\ ",
  "  \\hline",
  table_row("0 - 25\\%", matrix[1, ]),
  table_row("25\\% - 50\\%", matrix[2, ]),
  table_row("50\\% - 75\\%", matrix[3, ]),
  table_row("75\\% - 100\\%", matrix[4, ]),
  " \\hline",
  "\\end{tabular}",
  ""
)
writeLines(table_tex, "../output/tables/TableS1.tex")
