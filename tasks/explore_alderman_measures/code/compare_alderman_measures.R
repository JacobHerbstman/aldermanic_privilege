# setwd("tasks/explore_alderman_measures/code")
# Exploratory: how the alderman-level measures relate to one another, to aldermen's characteristics and stated
# positions, and to their wards. Every measure is oriented so that higher means stricter:
#   processing-time index (2006-2022 and 2006-2014 permits; tasks/create_alderman_uncertainty_index),
#   fewer high- or low-discretion permit applications (minus the alderman's permit effect,
#     tasks/estimate_alderman_permit_effects, all-changes design),
#   stall rate, days to passage and own downzonings per year (tasks/estimate_alderman_zoning_measures).
# Shrunk estimates are used throughout.
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

index_2022 <- read_csv("../input/alderman_uncertainty_index_through2022.csv", show_col_types = FALSE)
index_2014 <- read_csv("../input/alderman_uncertainty_index_through2014.csv", show_col_types = FALSE)
permits <- read_csv("../input/alderman_permit_effects.csv", show_col_types = FALSE) |>
  filter(design == "all_changes") |>
  transmute(alderman, measure = paste0("fewer_", outcome, "_permits"), value = -shrunk_estimate) |>
  tidyr::pivot_wider(names_from = measure, values_from = value)
zoning <- read_csv("../input/alderman_zoning_measures.csv", show_col_types = FALSE)
aldermen <- zoning |>
  transmute(alderman, stall_rate = stall_shrunk, days_to_passage = days_shrunk, own_downzonings = downzoning_rate_shrunk,
    applications_decided) |>
  full_join(transmute(index_2022, alderman, processing_time_index = uncertainty_index), by = "alderman",
    relationship = "one-to-one") |>
  full_join(transmute(index_2014, alderman, processing_time_index_2014 = uncertainty_index), by = "alderman",
    relationship = "one-to-one") |>
  full_join(permits, by = "alderman", relationship = "one-to-one")
measures <- c("processing_time_index", "processing_time_index_2014", "fewer_high_discretion_permits",
  "fewer_low_discretion_permits", "stall_rate", "days_to_passage", "own_downzonings")

# 1. Rank correlations between measures, with the number of aldermen behind each.
correlations <- tidyr::expand_grid(measure_a = measures, measure_b = measures) |>
  filter(match(measure_a, measures) < match(measure_b, measures)) |>
  mutate(result = purrr::map2(measure_a, measure_b, function(a, b) {
    x <- aldermen[is.finite(aldermen[[a]]) & is.finite(aldermen[[b]]), ]
    test <- suppressWarnings(stats::cor.test(x[[a]], x[[b]], method = "spearman"))
    tibble(aldermen = nrow(x), spearman = unname(test$estimate), p_value = test$p.value)
  })) |>
  tidyr::unnest(result)
SaveData(correlations, c("measure_a", "measure_b"), "../output/measure_correlations.csv")

# 2. Characteristics, stated positions on prerogative and community zoning processes.
characteristics <- read_csv("../input/alderman_characteristics.csv", show_col_types = FALSE, na = "NA")
positions <- read_csv("../input/prerogative_positions.csv", show_col_types = FALSE) |>
  summarise(stated_keep_prerogative = any(position == "keep"),
    stated_reform_or_abolish = any(position %in% c("reform_or_limit", "abolish")), .by = alderman)
processes <- read_csv("../input/community_zoning_processes.csv", show_col_types = FALSE) |>
  filter(process_type != "other") |>
  distinct(alderman) |>
  mutate(community_zoning_process = TRUE)
traits <- characteristics |>
  transmute(alderman, female = gender == "female", black = race_ethnicity %in% "Black",
    hispanic = race_ethnicity %in% "Hispanic", lawyer, real_estate_or_property_tax = property_tax_or_real_estate_work,
    appointed = entry_route == "appointed", prior_elected_office, family_political_ties, progressive_caucus,
    indicted_or_convicted, entered_before_1999 = first_year_in_council < 1999,
    government_or_political_staff = occupation_category %in% "government_political_staff") |>
  left_join(positions, by = "alderman", relationship = "one-to-one") |>
  left_join(processes, by = "alderman", relationship = "one-to-one") |>
  mutate(community_zoning_process = coalesce(community_zoning_process, FALSE))
traits_long <- traits |> tidyr::pivot_longer(-alderman, names_to = "characteristic", values_to = "has")
by_characteristic <- tidyr::expand_grid(measure = measures, characteristic = unique(traits_long$characteristic)) |>
  mutate(result = purrr::map2(measure, characteristic, function(m, k) {
    x <- traits_long |> filter(characteristic == k, !is.na(has)) |>
      inner_join(select(aldermen, alderman, value = all_of(m)), by = "alderman", relationship = "one-to-one") |>
      filter(is.finite(value)) |>
      mutate(value = (value - mean(value)) / sd(value))
    if (sum(x$has) < 3 || sum(!x$has) < 3) return(tibble(with = sum(x$has), without = sum(!x$has),
      difference_sd = NA_real_, std_error = NA_real_, p_value = NA_real_))
    test <- stats::t.test(x$value[x$has], x$value[!x$has])
    tibble(with = sum(x$has), without = sum(!x$has), difference_sd = unname(diff(rev(test$estimate))),
      std_error = test$stderr, p_value = test$p.value)
  })) |>
  tidyr::unnest(result)
SaveData(by_characteristic, c("measure", "characteristic"), "../output/measures_by_characteristic.csv")

# 3. Ward demographics behind each alderman: permit-weighted averages of the index's first-stage ward controls.
wards <- read_csv("../input/permits_for_uncertainty_index.csv", show_col_types = FALSE,
  col_select = c(alderman, share_black, share_hisp, share_white, median_hh_income, homeownership_rate, dist_cbd_km)) |>
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)), .by = alderman)
ward_correlations <- tidyr::expand_grid(measure = measures,
    ward_characteristic = c("share_black", "share_hisp", "share_white", "median_hh_income", "homeownership_rate",
      "dist_cbd_km")) |>
  mutate(result = purrr::map2(measure, ward_characteristic, function(m, w) {
    x <- inner_join(select(aldermen, alderman, value = all_of(m)), wards, by = "alderman", relationship = "one-to-one")
    x <- x[is.finite(x$value) & is.finite(x[[w]]), ]
    test <- suppressWarnings(stats::cor.test(x$value, x[[w]], method = "spearman"))
    tibble(aldermen = nrow(x), spearman = unname(test$estimate), p_value = test$p.value)
  })) |>
  tidyr::unnest(result)
SaveData(ward_correlations, c("measure", "ward_characteristic"), "../output/measures_by_ward_characteristics.csv")

# 4. Is there a common factor? First principal components of the standardized measures, for aldermen with all of
# the 2006-2022 measures.
complete <- aldermen |>
  select(alderman, all_of(setdiff(measures, "processing_time_index_2014"))) |>
  filter(if_all(-alderman, is.finite))
components <- stats::prcomp(select(complete, -alderman), scale. = TRUE)
loadings <- as_tibble(components$rotation[, 1:3], rownames = "measure") |>
  mutate(aldermen = nrow(complete))
variance_share <- tibble(measure = "share_of_variance", PC1 = summary(components)$importance[2, 1],
  PC2 = summary(components)$importance[2, 2], PC3 = summary(components)$importance[2, 3], aldermen = nrow(complete))
SaveData(bind_rows(loadings, variance_share), "measure", "../output/measure_principal_components.csv")

SaveData(aldermen, "alderman", "../output/alderman_measures.csv")
