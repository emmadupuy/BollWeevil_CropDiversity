# Shared path definitions for the raw-to-intermediate construction scripts.
# Open code/replication.Rproj before running a script in code/clean/.

if (!file.exists("replication.Rproj") || !dir.exists("../data/raw")) {
  stop("Open code/replication.Rproj so the working directory is code/.")
}

clean_project_root <- normalizePath("..", winslash = "/", mustWork = TRUE)

raw_file <- function(...) {
  file.path(clean_project_root, "data", "raw", ...)
}

build_file <- function(...) {
  path <- file.path(clean_project_root, "data", "intermediate", ".build", ...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}

build_dir <- function(...) {
  path <- file.path(clean_project_root, "data", "intermediate", ".build", ...)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

intermediate_file <- function(...) {
  path <- file.path(clean_project_root, "data", "intermediate", ...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}

clean_output_file <- function(...) {
  path <- file.path(clean_project_root, "output", ...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}

scratch_file <- function(...) {
  path <- build_file("scratch", ...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}
