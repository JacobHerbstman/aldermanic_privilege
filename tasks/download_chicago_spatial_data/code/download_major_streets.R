# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/download_chicago_spatial_data/code")

source("../../setup_environment/code/packages.R")

tmp_dir <- tempfile("major_streets_", tmpdir = "../temp")
dir.create(tmp_dir)
on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

zip_path <- file.path(tmp_dir, "Major_Streets.zip")
download.file(
  "https://data.cityofchicago.org/api/views/ueqs-5wr6/files/hodPkYBNwoIaudrmiVOpTA7MEz9BCDcC6rEAFCrtbSs?download=true&filename=Major%20Streets.zip",
  zip_path,
  mode = "wb",
  quiet = TRUE
)

unzip(zip_path, exdir = tmp_dir)
major_streets <- st_read(file.path(tmp_dir, "Major_Streets.shp"), quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  st_transform(4326)

if (nrow(major_streets) < 1000) {
  stop("Downloaded Major Streets file has too few features.")
}

if (!all(c("STREET_NAM", "CLASS") %in% names(major_streets))) {
  stop("Downloaded Major Streets file is missing expected STREET_NAM or CLASS columns.")
}

st_write(
  major_streets,
  "../output/major_streets.geojson",
  delete_dsn = TRUE,
  quiet = TRUE
)
