# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/spatial_rd_multifamily_threshold/code")
# min_units <- 3
# pd_screen <- "allzoning"

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(min_units, pd_screen)
}

if (length(cli_args) != 2) {
  stop("FATAL: Script requires args: <min_units> <pd_screen>", call. = FALSE)
}

min_units <- as.integer(cli_args[1])
pd_screen <- cli_args[2]
if (!is.finite(min_units) || min_units < 2) {
  stop("min_units must be an integer >= 2.", call. = FALSE)
}
if (!pd_screen %in% c("allzoning", "nopd")) {
  stop("pd_screen must be one of: allzoning, nopd.", call. = FALSE)
}

output_suffix <- if_else(pd_screen == "nopd", "_nopd", "")

read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  filter(
    unitscount >= min_units,
    pd_screen == "allzoning" | !str_starts(str_to_upper(zone_code), "PD")
  ) %>%
  write_csv(sprintf("../temp/parcels_with_ward_distances_minunits%d%s.csv", min_units, output_suffix))
