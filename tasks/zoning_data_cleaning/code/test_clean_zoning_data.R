source("../../setup_environment/code/packages.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

run_rscript <- function(args) {
  out <- suppressWarnings(system2("Rscript", args, stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status") %||% 0L
  list(status = status, output = out)
}

out_far <- tempfile(fileext = ".csv")
out_gpkg <- tempfile(fileext = ".gpkg")

res <- run_rscript(c(
  "clean_zoning_data.R",
  "--in-zoning-geojson", "smoke/raw_zoning_data.geojson",
  "--in-zoning-lookup-csv", "smoke/zoning-code-summary-district-types.csv",
  "--in-old-zoning-csv", "smoke/old_zoning_bulk_density_1957_2004.csv",
  "--out-zoning-gpkg", out_gpkg,
  "--out-far-lookup-csv", out_far
))
if (res$status != 0L) {
  stop(sprintf("zoning cleaning smoke run failed: %s", paste(res$output, collapse = "\n")))
}

if (!file.exists(out_far)) {
  stop("Expected FAR lookup CSV was not written")
}
if (!file.exists(out_gpkg)) {
  stop("Expected zoning GPKG was not written")
}

far_lookup <- readr::read_csv(out_far, show_col_types = FALSE)
if (!any(far_lookup$zone_code == "B3-2" & far_lookup$zoning_code_version == "post_2004")) {
  stop("Expected post-2004 B3-2 lookup row")
}
if (!any(far_lookup$zone_code == "R5" & far_lookup$zoning_code_version == "pre_2004" & far_lookup$floor_area_ratio == 1.5)) {
  stop("Expected pre-2004 R5 lookup row from old zoning input")
}

zones <- sf::st_read(out_gpkg, quiet = TRUE)
if (any(stringr::str_detect(zones$zone_code, "^PD"))) {
  stop("PD rows should be filtered out from cleaned zoning geometries")
}

cat("zoning_data_cleaning tests passed\n")
