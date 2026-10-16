# Optional historical covariates

The scripts under `code/clean/04_other_covariates/` also preserve historical
work for crop prices, GAEZ potential yields, price instruments, revenue
instruments, and non-boll-weevil pest records. None of these inputs is used by a
retained paper regression or by the required panel build.

The original sources were USDA NASS Quick Stats crop prices, FAO/IIASA GAEZ v4
potential-yield rasters, the U.S. CPI series used to deflate prices, and the
authors' compiled APHIS/state quarantine records for other pests. Anyone
reconstructing these optional branches should place the downloaded archives in
this folder using the relative paths named in the corresponding scripts and
record the release, download date, checksum, and terms. These third-party files
are not redistributed.
