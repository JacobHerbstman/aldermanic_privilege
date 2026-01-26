# identifying_obs_diagnostic.R
# Compute diagnostic table showing identifying variation under different FE specs

source("../../setup_environment/code/packages.R")

# ── DATA ──────────────────────────────────────────────────────────────────────
parcels <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  mutate(strictness_own = strictness_own / sd(strictness_own, na.rm = TRUE)) %>%
  filter(arealotsf > 1) %>%
  filter(areabuilding > 1) %>%
  filter(unitscount > 1 & unitscount <= 100) %>%
  filter(construction_year >= 2006)

message(sprintf("Total parcels after base filters: %d", nrow(parcels)))

# ── PARAMETERS ────────────────────────────────────────────────────────────────
bandwidths <- c(250, 500, 1000)

# FE specs: name -> list with group_vars and fe_formula
fe_specs <- list(
  zone_x_pair_year = list(
    group_vars = c("zone_code", "ward_pair"),
    fe_formula = "zone_code^ward_pair + construction_year"
  ),
  zone_pair_x_year = list(
    group_vars = c("ward_pair", "construction_year"),
    fe_formula = "zone_code + ward_pair^construction_year"
  ),
  triple = list(
    group_vars = c("zone_code", "ward_pair", "construction_year"),
    fe_formula = "zone_code^ward_pair^construction_year"
  ),
  pair_year_only = list(
    group_vars = c("ward_pair"),
    fe_formula = "ward_pair + construction_year"
  ),
  pair_x_year = list(
    group_vars = c("ward_pair", "construction_year"),
    fe_formula = "ward_pair^construction_year"
  )
)

# ── COMPUTE IDENTIFYING OBSERVATIONS ──────────────────────────────────────────
results <- list()

for (bw in bandwidths) {
  df <- parcels %>% filter(dist_to_boundary <= bw)
  n_total <- nrow(df)
  
  for (fe_name in names(fe_specs)) {
    spec <- fe_specs[[fe_name]]
    group_vars <- spec$group_vars
    fe_formula <- spec$fe_formula
    
    # Create FE group ID by pasting together group variables
    df_grouped <- df %>%
      mutate(fe_group_id = do.call(paste, c(across(all_of(group_vars)), sep = "_")))
    
    # Find groups with identifying variation
    identifying <- df_grouped %>%
      group_by(fe_group_id) %>%
      summarize(
        n_obs = n(),
        sd_strictness = sd(strictness_own, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(n_obs > 1 & !is.na(sd_strictness) & sd_strictness > 0)
    
    n_groups_identifying <- nrow(identifying)
    n_obs_identifying <- sum(identifying$n_obs)
    pct_identifying <- 100 * n_obs_identifying / n_total
    
    # Run actual regression to get table N
    fml_txt <- sprintf(
      "log(density_far) ~ strictness_own + share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own + avg_rent_own | %s",
      fe_formula
    )
    m <- tryCatch(
      feols(as.formula(fml_txt), data = df, cluster = ~ward_pair),
      error = function(e) NULL
    )
    n_regression <- if (!is.null(m)) nobs(m) else NA_integer_
    
    results[[length(results) + 1]] <- tibble(
      bandwidth = bw,
      fe_spec = fe_name,
      n_total = n_total,
      n_groups_identifying = n_groups_identifying,
      n_obs_identifying = n_obs_identifying,
      pct_identifying = round(pct_identifying, 1),
      n_regression = n_regression
    )
  }
}

results_df <- bind_rows(results)

# ── OUTPUT ────────────────────────────────────────────────────────────────────
write_csv(results_df, "../output/identifying_obs_diagnostic.csv")
message("\nSaved: ../output/identifying_obs_diagnostic.csv")

# Print formatted table
message("\n=== Identifying Observations Diagnostic ===\n")
print(knitr::kable(results_df, format = "pipe"))
