#this code assigns wards + distance to nearest ward border for all parcels in the assessor panel 

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# ---- Load + narrow to RD window (0.1 mi) ----
land_values <- sfarrow::st_read_parquet("../input/land_values_aug.parquet", show_col_types = FALSE) %>%
  dplyr::filter(dist_to_boundary_ft <= 528)

DT <- data.table::as.data.table(sf::st_drop_geometry(land_values))
rm(land_values); gc()

# Keep only what we need
DT <- DT[, .(pin10, tax_year, land_sum, bldg_sum, n_units,
             land_share_pin10, ward_map_year, ward, ward_pair,
             dist_to_boundary_ft, alderman, latitude, longitude)]

# -------------------------------------------------------------------
# 1) Stable side definition within each ward-pair
#    (orientation = higher ward number side = 1; lower = 0)
# -------------------------------------------------------------------
DT[, c("wp_a", "wp_b") := tstrsplit(gsub("_", "-", ward_pair), "-", fixed = FALSE)]
DT[, `:=`(wp_a = as.integer(wp_a), wp_b = as.integer(wp_b))]
DT[, `:=`(ward_lo = pmin(wp_a, wp_b), ward_hi = pmax(wp_a, wp_b))]
DT[, side_hi := as.integer(ward == ward_hi)]
DT[, dist_abs := as.numeric(dist_to_boundary_ft)]

# -------------------------------------------------------------------
# 2) Alderman per side-year (modal) + "both sides present" flag
# -------------------------------------------------------------------
counts <- DT[, .N, by = .(ward_pair, tax_year, side_hi, alderman)]
counts <- counts[order(ward_pair, tax_year, side_hi, -N, alderman)]
counts <- counts[, .SD[1], by = .(ward_pair, tax_year, side_hi)]  # modal alderman

aldpairs <- tidyr::pivot_wider(
  as.data.frame(counts),
  id_cols   = c("ward_pair", "tax_year"),
  names_from = "side_hi",
  values_from = "alderman",
  names_prefix = "ald_side_"
) |>
  dplyr::rename(ald_lo = ald_side_0, ald_hi = ald_side_1) |>
  data.table::as.data.table()

# "Both sides present" that year on that border
aldpairs[, both_sides := !is.na(ald_lo) & !is.na(ald_hi)]

# Attach ward_map_year at the year-border level (for optional episode breaks)
map_by_year <- DT[, .(ward_map_year = data.table::first(ward_map_year)), by = .(ward_pair, tax_year)]
aldpairs <- map_by_year[aldpairs, on = .(ward_pair, tax_year)]

# Merge back to parcels (so each row knows that year's pair + both_sides)
DT <- aldpairs[DT, on = .(ward_pair, tax_year)]

# Pair key (ordered lo|hi, consistent across time)
DT[, ald_pair_key := fifelse(both_sides, paste0(ald_lo, " | ", ald_hi), NA_character_)]

# -------------------------------------------------------------------
# 3) Episode definition on the YEAR-BORDER table (only when both sides present)
#    -> avoids fragmenting episodes due to NA years
# -------------------------------------------------------------------
years_tbl <- aldpairs[both_sides == TRUE,
                      .(ward_pair, tax_year, ward_map_year, ald_lo, ald_hi)]
setorder(years_tbl, ward_pair, tax_year)

# define episodes ONLY by alderman pair (contiguous runs)
years_tbl[, episode_seq := data.table::rleid(ald_lo, ald_hi), by = ward_pair]
years_tbl[, episode_id  := paste0(ward_pair, "_", sprintf("%03d", episode_seq))]

# episode metadata (note: an episode can span multiple ward_map_years)
episode_meta <- years_tbl[, .(
  year_start      = min(tax_year),
  year_end        = max(tax_year),
  ald_lo          = data.table::first(ald_lo),
  ald_hi          = data.table::first(ald_hi),
  ward_pair       = data.table::first(ward_pair),
  ward_map_years  = paste(sort(unique(ward_map_year)), collapse = ";"),
  mixed_map_year  = data.table::uniqueN(ward_map_year) > 1
), by = episode_id]

# attach episode_id to parcels; episodes exist only in years with both sides present
DT <- years_tbl[, .(ward_pair, tax_year, episode_id)][DT, on = .(ward_pair, tax_year)]

# handy ID that already pools noncontiguous same-pair runs across time if you ever want it
DT[, pair_id := paste(ward_pair, ald_lo, ald_hi, sep = " | ")]

DT[, signed_distance := data.table::fifelse(side_hi == 1L, dist_abs, -dist_abs)]

write_csv(DT, "../output/land_values_rd_data.csv")
