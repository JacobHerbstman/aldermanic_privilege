# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/download_zillow_zori/code")

source("../../setup_environment/code/packages.R")

zillow_zori <- readr::read_csv(
  "https://files.zillowstatic.com/research/public_csvs/zori/City_zori_uc_sfrcondomfr_sm_month.csv",
  show_col_types = FALSE
)

if (!all(c("RegionName", "State") %in% names(zillow_zori))) {
  stop("Zillow ZORI city file is missing RegionName or State.", call. = FALSE)
}

readr::write_csv(zillow_zori, "../output/zillow_zori_city.csv")
