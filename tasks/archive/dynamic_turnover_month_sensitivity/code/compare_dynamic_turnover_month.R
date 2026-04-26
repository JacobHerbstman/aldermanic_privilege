source("../../setup_environment/code/packages.R")

analysis_cutoff_year <- 2023L

june_pre <- read_csv("../input/june_parcels_pre_scores.csv", show_col_types = FALSE) %>%
  mutate(pin = as.character(pin))

dynamic_pre <- read_csv("../output/parcels_pre_scores.csv", show_col_types = FALSE) %>%
  mutate(pin = as.character(pin))

june_merged <- read_csv("../input/june_parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  mutate(pin = as.character(pin))

dynamic_merged <- read_csv("../output/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  mutate(pin = as.character(pin))

diagnostics <- read_csv("../output/synthetic_month_choice_diagnostics.csv", show_col_types = FALSE) %>%
  mutate(pin = as.character(pin))

assignment_compare <- june_pre %>%
  transmute(
    pin,
    construction_year,
    construction_date_june = construction_date,
    boundary_year_june = boundary_year,
    ward_pair_june = ward_pair,
    alderman_own_june = alderman_own,
    alderman_neighbor_june = alderman_neighbor
  ) %>%
  inner_join(
    dynamic_pre %>%
      transmute(
        pin,
        construction_year,
        construction_date_dynamic = construction_date,
        boundary_year_dynamic = boundary_year,
        ward_pair_dynamic = ward_pair,
        alderman_own_dynamic = alderman_own,
        alderman_neighbor_dynamic = alderman_neighbor
      ),
    by = c("pin", "construction_year")
  ) %>%
  left_join(
    diagnostics %>%
      select(
        pin,
        construction_year,
        selected_month_num,
        changed_any,
        changed_boundary_year,
        changed_assigned_ward,
        changed_other_ward,
        changed_alderman_own,
        changed_alderman_neighbor,
        selection_reason,
        selected_boundary_year,
        boundary_year_june,
        selected_ward_pair,
        ward_pair_june,
        selected_alderman_own,
        alderman_own_june,
        selected_alderman_neighbor,
        alderman_neighbor_june
      ),
    by = c("pin", "construction_year", "boundary_year_june", "ward_pair_june", "alderman_own_june", "alderman_neighbor_june")
  ) %>%
  mutate(
    changed_month = selected_month_num != 6L,
    changed_boundary_year = coalesce(changed_boundary_year, boundary_year_dynamic != boundary_year_june),
    changed_own_alderman = coalesce(changed_alderman_own, alderman_own_dynamic != alderman_own_june),
    changed_neighbor_alderman = coalesce(changed_alderman_neighbor, alderman_neighbor_dynamic != alderman_neighbor_june),
    changed_ward_pair = ward_pair_dynamic != ward_pair_june
  ) %>%
  filter(construction_year <= analysis_cutoff_year)

main_sample_counts <- bind_rows(
  list(
    list(bw_ft = 250L, sample_filter = "all"),
    list(bw_ft = 500L, sample_filter = "multifamily")
  ) %>%
    purrr::map_dfr(function(spec) {
      june_sample <- june_merged %>%
        filter(
          construction_year >= 2006,
          construction_year <= analysis_cutoff_year,
          abs(signed_distance) <= spec$bw_ft,
          if (spec$sample_filter == "all") unitscount > 0 else unitscount > 1,
          is.finite(density_far), density_far > 0,
          is.finite(density_dupac), density_dupac > 0,
          !is.na(zone_code),
          !is.na(segment_id),
          segment_id != ""
        ) %>%
        distinct(pin, construction_year)

      dynamic_sample <- dynamic_merged %>%
        filter(
          construction_year >= 2006,
          construction_year <= analysis_cutoff_year,
          abs(signed_distance) <= spec$bw_ft,
          if (spec$sample_filter == "all") unitscount > 0 else unitscount > 1,
          is.finite(density_far), density_far > 0,
          is.finite(density_dupac), density_dupac > 0,
          !is.na(zone_code),
          !is.na(segment_id),
          segment_id != ""
        ) %>%
        distinct(pin, construction_year)

      overlap <- june_sample %>%
        inner_join(dynamic_sample, by = c("pin", "construction_year")) %>%
        inner_join(
          assignment_compare %>%
            select(
              pin,
              construction_year,
              changed_month,
              changed_boundary_year,
              changed_own_alderman,
              changed_neighbor_alderman
            ),
          by = c("pin", "construction_year")
        )

      tibble(
        metric = c(
          sprintf("sample_%d_%s_june_n", spec$bw_ft, spec$sample_filter),
          sprintf("sample_%d_%s_dynamic_n", spec$bw_ft, spec$sample_filter),
          sprintf("sample_%d_%s_overlap_n", spec$bw_ft, spec$sample_filter),
          sprintf("sample_%d_%s_changed_month_overlap", spec$bw_ft, spec$sample_filter),
          sprintf("sample_%d_%s_changed_boundary_overlap", spec$bw_ft, spec$sample_filter),
          sprintf("sample_%d_%s_changed_own_alderman_overlap", spec$bw_ft, spec$sample_filter),
          sprintf("sample_%d_%s_changed_neighbor_alderman_overlap", spec$bw_ft, spec$sample_filter)
        ),
        value = c(
          nrow(june_sample),
          nrow(dynamic_sample),
          nrow(overlap),
          sum(overlap$changed_month, na.rm = TRUE),
          sum(overlap$changed_boundary_year, na.rm = TRUE),
          sum(overlap$changed_own_alderman, na.rm = TRUE),
          sum(overlap$changed_neighbor_alderman, na.rm = TRUE)
        )
      )
    })
)

assignment_summary <- bind_rows(
  tibble(
    metric = c(
      "n_parcels",
      "n_changed_month",
      "n_changed_own_alderman",
      "n_changed_neighbor_alderman",
      "n_changed_boundary_year",
      "n_changed_ward_pair"
    ),
    value = c(
      nrow(assignment_compare),
      sum(assignment_compare$changed_month, na.rm = TRUE),
      sum(assignment_compare$changed_own_alderman, na.rm = TRUE),
      sum(assignment_compare$changed_neighbor_alderman, na.rm = TRUE),
      sum(assignment_compare$changed_boundary_year, na.rm = TRUE),
      sum(assignment_compare$changed_ward_pair, na.rm = TRUE)
    )
  ),
  main_sample_counts
)

write_csv(assignment_summary, "../output/dynamic_turnover_assignment_summary.csv")

assignment_by_year <- assignment_compare %>%
  summarise(
    n_parcels = n(),
    n_changed_month = sum(changed_month, na.rm = TRUE),
    n_changed_own_alderman = sum(changed_own_alderman, na.rm = TRUE),
    n_changed_neighbor_alderman = sum(changed_neighbor_alderman, na.rm = TRUE),
    n_changed_boundary_year = sum(changed_boundary_year, na.rm = TRUE),
    n_changed_ward_pair = sum(changed_ward_pair, na.rm = TRUE),
    .by = construction_year
  ) %>%
  arrange(construction_year)

write_csv(assignment_by_year, "../output/dynamic_turnover_assignment_by_year.csv")

rd_results <- list.files("../output", pattern = "^rd_summary_(june|dynamic)_.*\\.csv$", full.names = TRUE) %>%
  sort() %>%
  purrr::map_dfr(function(path) {
    read_csv(path, show_col_types = FALSE) %>%
      mutate(
        scenario = if_else(str_detect(basename(path), "^rd_summary_june_"), "june", "dynamic"),
        outcome = case_when(
          str_detect(yvar, "far") ~ "FAR",
          str_detect(yvar, "dupac") ~ "DUPAC",
          TRUE ~ yvar
        )
      ) %>%
      select(method, scenario, outcome, bw_ft, sample_filter, estimate, se, p_value, n_obs)
  })

fe_results <- list.files("../output", pattern = "^fe_summary_(june|dynamic)_.*\\.csv$", full.names = TRUE) %>%
  sort() %>%
  purrr::map_dfr(function(path) {
    read_csv(path, show_col_types = FALSE) %>%
      mutate(
        method = "segment_zonegroup_year_fe",
        scenario = if_else(str_detect(basename(path), "^fe_summary_june_"), "june", "dynamic"),
        outcome = case_when(
          str_detect(yvar, "far") ~ "FAR",
          str_detect(yvar, "dupac") ~ "DUPAC",
          TRUE ~ yvar
        )
      ) %>%
      select(method, scenario, outcome, bw_ft, sample_filter, estimate, se, p_value, n_obs)
  })

main_density_comparison <- bind_rows(rd_results, fe_results) %>%
  pivot_wider(
    names_from = scenario,
    values_from = c(estimate, se, p_value, n_obs)
  ) %>%
  mutate(delta_estimate = estimate_dynamic - estimate_june) %>%
  arrange(method, bw_ft, sample_filter, outcome)

write_csv(main_density_comparison, "../output/dynamic_turnover_main_density_comparison.csv")

regular_turnover_years <- c(2007L, 2011L, 2015L, 2019L, 2023L)

spotcheck_examples <- bind_rows(
  diagnostics %>%
    filter(changed_any, construction_year <= analysis_cutoff_year, construction_year %in% regular_turnover_years) %>%
    mutate(case_group = "regular_turnover") %>%
    slice_head(n = 10),
  diagnostics %>%
    filter(changed_any, construction_year <= analysis_cutoff_year, selected_month_num > 6, !construction_year %in% regular_turnover_years) %>%
    mutate(case_group = "late_special_turnover") %>%
    slice_head(n = 10),
  diagnostics %>%
    filter(construction_year <= analysis_cutoff_year, !changed_any, construction_year == 2017) %>%
    mutate(case_group = "stable_year") %>%
    slice_head(n = 10)
) %>%
  select(
    case_group,
    pin,
    construction_year,
    selected_month_num,
    selection_reason,
    changed_any,
    changed_boundary_year,
    changed_assigned_ward,
    changed_other_ward,
    changed_alderman_own,
    changed_alderman_neighbor,
    construction_date_june,
    selected_construction_date,
    boundary_year_june,
    selected_boundary_year,
    ward_pair_june,
    selected_ward_pair,
    alderman_own_june,
    selected_alderman_own,
    alderman_neighbor_june,
    selected_alderman_neighbor
  )

write_csv(spotcheck_examples, "../output/dynamic_turnover_spotcheck_examples.csv")
