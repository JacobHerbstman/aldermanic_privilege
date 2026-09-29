# setwd("tasks/explore_land_prices/code")
# Exploratory: did land get cheaper where the 2015 ward map moved blocks to a stricter alderman? The permit
# event study's blocks (tasks/create_event_study_permit_data): blocks within 500 ft of a 2015 ward boundary, each
# either moved to a new ward or not, with the direction of the change in stringency for moved blocks (+1 to a
# stricter alderman, -1 to a more lenient one, by the 2006-2014 processing-time index) and the stable-incumbent
# sample of the paper. Vacant-lot and teardown sales of 2010-2020 (build_land_sales.R) are placed in their 2010
# census block. Log price per square foot is compared before and after 2015 between moved and unmoved blocks of the
# same ward pair: post x direction with ward-pair-side, ward-pair-by-year, zoning-group and kind-of-sale fixed
# effects, clustered by ward pair. Moved blocks are also compared by direction separately.
bandwidth_m <- 152.4
first_year <- 2010L
last_year <- 2020L
remap_year <- 2015L

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
sf_use_s2(FALSE)

blocks <- arrow::read_parquet("../input/permit_block_year_panel_2015.parquet",
  col_select = c(block_id, ward_pair_id, ward_pair_side, strictness_change_frozen, stable_both, dist_m)) |>
  distinct() |>
  filter(dist_m <= bandwidth_m, stable_both, !is.na(strictness_change_frozen), !is.na(ward_pair_id), ward_pair_id != "") |>
  mutate(direction = sign(strictness_change_frozen))
stopifnot(!anyDuplicated(blocks$block_id))
block_shapes <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE,
  col_select = c(the_geom, GEOID10), col_types = cols(GEOID10 = "c")) |>
  filter(GEOID10 %in% blocks$block_id) |>
  distinct(GEOID10, .keep_all = TRUE) |>
  st_as_sf(wkt = "the_geom", crs = 4326) |>
  st_transform(3435)

sales <- read_csv("../output/land_sales.csv", show_col_types = FALSE) |>
  filter(year >= first_year, year <= last_year, !is.na(zone_group), is.finite(log_price_per_sqft)) |>
  st_as_sf(coords = c("x_3435", "y_3435"), crs = 3435) |>
  st_join(select(block_shapes, block_id = GEOID10), join = st_within) |>
  st_drop_geometry() |>
  inner_join(blocks, by = "block_id", relationship = "many-to-one") |>
  mutate(post = as.numeric(year >= remap_year), post_direction = post * direction,
    post_stricter = post * as.numeric(direction == 1), post_more_lenient = post * as.numeric(direction == -1))

fit_remap <- function(formula, term) {
  fit <- fixest::feols(formula, data = sales, cluster = ~ward_pair_id, notes = FALSE, warn = FALSE)
  used <- sales[fixest::obs(fit), ]
  tibble(term, estimate = coef(fit)[[term]], std_error = fixest::se(fit)[[term]], p_value = fixest::pvalue(fit)[[term]],
    observations = nrow(used), moved_block_sales = sum(used$direction != 0),
    ward_pairs = n_distinct(used$ward_pair_id))
}
fixed_effects <- "| ward_pair_side + ward_pair_id^year + zone_group + sale_kind"
results <- bind_rows(
  fit_remap(stats::as.formula(paste("log_price_per_sqft ~ post_direction", fixed_effects)), "post_direction"),
  fit_remap(stats::as.formula(paste("log_price_per_sqft ~ post_stricter + post_more_lenient", fixed_effects)),
    "post_stricter"),
  fit_remap(stats::as.formula(paste("log_price_per_sqft ~ post_stricter + post_more_lenient", fixed_effects)),
    "post_more_lenient")
)
counts <- sales |> count(sale_kind, direction, post, name = "sales")
SaveData(results, "term", "../output/land_remap_estimates.csv")
SaveData(counts, c("sale_kind", "direction", "post"), "../output/land_remap_sales_counts.csv")
