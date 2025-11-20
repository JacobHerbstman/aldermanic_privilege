# border_pair_FE_volume_interaction.R
# 1. Interaction Model: Tests for "Jump at Zero" using linear interaction
# 2. Volume Model: Tests if strictness reduces TOTAL quantity (SqFt/Units)

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- PARAMETERS ---
# =======================================================================================
bw_ft           <- 750  # 0.2 miles (Use a wider bandwidth for interaction models to get slope)
output_filename <- "../output/fe_table_volume_interaction.tex"
# =======================================================================================

# ── 1) LOAD DATA ───────────────────────────────────────────────────────────────
# We keep ALL new construction (unitscount > 0) to capture the full market effect.
parcels <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  mutate(
    strictness_own_std = strictness_own / sd(strictness_own, na.rm = TRUE),
    abs_dist = abs(dist_to_boundary) # Absolute distance for the interaction term
  ) %>%
  filter(unitscount > 0) %>% 
  filter(dist_to_boundary <= bw_ft)

# Helper: Keep modal zone that exists on both sides (Optional - comment out to run on full sample)
restrict_to_modal_zone <- function(df, bw) {
  df_bw <- df %>% filter(dist_to_boundary <= bw, !is.na(zone_code))
  group_keys <- intersect(c("boundary_year", "ward_pair"), names(df_bw))
  
  zone_counts <- if ("signed_distance" %in% names(df_bw)) {
    df_bw %>%
      group_by(across(all_of(c(group_keys, "zone_code")))) %>%
      summarise(n = n(), n_sides = n_distinct(sign(signed_distance)), .groups = "drop") %>%
      filter(n_sides == 2)
  } else {
    df_bw %>%
      group_by(across(all_of(c(group_keys, "zone_code")))) %>%
      summarise(n = n(), .groups = "drop")
  }
  modal_zone <- zone_counts %>%
    arrange(across(all_of(group_keys)), desc(n), zone_code) %>%
    group_by(across(all_of(group_keys))) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(all_of(group_keys), zone_code) %>%
    rename(modal_zone_code = zone_code)
  
  df_bw %>%
    inner_join(modal_zone, by = group_keys) %>%
    filter(zone_code == modal_zone_code) %>%
    select(-modal_zone_code)
}

# Apply Restriction (You can skip this line if you want to test the "Legislative" channel)
parcels_fe <- parcels
# parcels_fe <- restrict_to_modal_zone(parcels_fe, bw_ft)


# ── 2) MODEL 1: INTERACTION (Jump at Zero) ─────────────────────────────────────
# We interact strictness with distance.
# The coefficient on `strictness_own_std` is the effect at dist=0.
# The coefficient on `strictness:abs_dist` allows the density gradient to differ.

m_interact_dupac <- feols(
  log(density_dupac) ~ strictness_own_std * abs_dist + factor(construction_quality) | construction_year^ward_pair,
  data = parcels_fe,
  cluster = ~ward_pair
)

m_interact_far <- feols(
  log(density_far) ~ strictness_own_std * abs_dist + factor(construction_quality) | construction_year^ward_pair,
  data = parcels_fe,
  cluster = ~ward_pair
)


# ── 3) MODEL 2: AGGREGATE VOLUME (Total Supply) ────────────────────────────────
# Collapse data to the Border-Pair-Year-Side level.
# "Does the strict side produce less TOTAL housing?"

df_volume <- parcels_fe %>%
  # Group by the "Experiment" unit: Pair-Year-Side
  group_by(ward_pair, boundary_year, construction_year, strictness_own_std) %>%
  summarise(
    total_units = sum(unitscount, na.rm = TRUE),
    total_sqft  = sum(areabuilding, na.rm = TRUE),
    n_projects  = n(), 
    .groups = "drop"
  ) %>%
  mutate(
    log_total_units = log(total_units), # Use log(x) since volume > 0 by definition of sample
    log_total_sqft  = log(total_sqft)
  )

m_vol_sqft <- feols(
  log_total_sqft ~ strictness_own_std | construction_year^ward_pair,
  data = df_volume,
  cluster = ~ward_pair
)

m_vol_units <- feols(
  log_total_units ~ strictness_own_std | construction_year^ward_pair,
  data = df_volume,
  cluster = ~ward_pair
)


# ── 4) OUTPUT ──────────────────────────────────────────────────────────────────

# Create a nice table combining both approaches
models <- list(
  "Interaction: DUPAC" = m_interact_dupac,
  "Interaction: FAR"   = m_interact_far,
  "Volume: Total SqFt" = m_vol_sqft,
  "Volume: Total Units"= m_vol_units
)

# Custom fitstat to show N obs
n_ward_pairs <- function(x) length(unique(x$cluster[[1]]))
fitstat_register("nwp", n_ward_pairs, alias = "Ward Pairs")

etable(models,
       keep         = c("Strictness Score" = "strictness_own_std"), # Keep only the main effect
       # order        = c("Strictness", "Distance"), 
       fitstat      = ~ n + r2 + nwp,
       style.tex    = style.tex("aer", model.format = ""),
       digits       = 3,
       signif.code  = c("***"=0.01, "**"=0.05, "*"=0.1),
       fixef.group  = list("Ward-pair × Year FE" = "construction_year\\^ward_pair"),
       title        = "Robustness: Interaction & Volume Models",
       # file         = output_filename,
       replace      = TRUE)

cat("✓ Table saved to:", output_filename, "\n")

# Print to console for immediate check
etable(models, keep = "strictness_own_std", digits=3)