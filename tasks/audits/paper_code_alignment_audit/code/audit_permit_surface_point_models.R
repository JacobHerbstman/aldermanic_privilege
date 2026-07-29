# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

sf_use_s2(FALSE)

spatial <- read_csv(
  "../output/permit_spatial_block_assignments.csv",
  show_col_types = FALSE,
  col_types = cols(block_id = col_character())
)

panel <- read_parquet("../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet") %>%
  as_tibble() %>%
  mutate(block_id = as.character(block_id)) %>%
  filter(relative_year >= -5L, relative_year <= 5L) %>%
  inner_join(
    spatial %>%
      select(
        block_id, event_pair_id, surface_event_pair_id,
        centroid_500ft_sample, surface_500ft_sample
      ),
    by = "block_id",
    relationship = "many-to-one"
  )

missing_surface <- spatial %>%
  filter(surface_500ft_sample %in% TRUE, !block_id %in% panel$block_id)

if (nrow(missing_surface) > 0) {
  blocks <- read_csv(
    "../../../download_chicago_spatial_data/output/census_blocks_2010.csv",
    show_col_types = FALSE,
    col_types = cols(GEOID10 = col_character())
  ) %>%
    filter(GEOID10 %in% missing_surface$block_id) %>%
    rename(geometry = the_geom, block_id = GEOID10) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_make_valid() %>%
    st_transform(3435)

  permits <- st_read(
    "../../../clean_building_permits/output/building_permits_clean.gpkg",
    query = paste(
      "SELECT id, permit_type, high_discretion, application_start_date_ym, geom",
      "FROM building_permits_clean",
      "WHERE permit_status NOT IN ('CANCELLED', 'REVOKED', 'SUSPENDED') OR permit_status IS NULL"
    ),
    quiet = TRUE
  ) %>%
    st_transform(3435) %>%
    mutate(
      year = as.integer(format(as.Date(application_start_date_ym), "%Y")),
      high = high_discretion == 1,
      low = high_discretion == 0 & permit_type != "PERMIT - SIGNS"
    ) %>%
    filter(year >= 2010L, year <= 2020L)

  missing_counts <- st_join(
    permits,
    blocks %>% select(block_id),
    join = st_within,
    left = FALSE
  ) %>%
    st_drop_geometry() %>%
    summarise(
      n_high_discretion_application = sum(high, na.rm = TRUE),
      n_low_discretion_nosigns_application = sum(low, na.rm = TRUE),
      .by = c(block_id, year)
    )

  aldermen_2014 <- read_csv(
    "../../../create_alderman_data/output/chicago_alderman_panel.csv",
    show_col_types = FALSE
  ) %>%
    filter(month == "Jun 2014") %>%
    select(ward, alderman)

  scores_2014 <- read_csv(
    "../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2014.csv",
    show_col_types = FALSE
  ) %>%
    select(alderman, score = uncertainty_index)

  missing_panel <- missing_surface %>%
    select(
      block_id, ward_origin, ward_dest, event_pair_id, surface_event_pair_id,
      centroid_500ft_sample, surface_500ft_sample
    ) %>%
    left_join(
      aldermen_2014 %>% rename(ward_origin = ward, alderman_origin = alderman),
      by = "ward_origin",
      relationship = "many-to-one"
    ) %>%
    left_join(
      aldermen_2014 %>% rename(ward_dest = ward, alderman_dest = alderman),
      by = "ward_dest",
      relationship = "many-to-one"
    ) %>%
    left_join(
      scores_2014 %>% rename(alderman_origin = alderman, strictness_origin_frozen = score),
      by = "alderman_origin",
      relationship = "many-to-one"
    ) %>%
    left_join(
      scores_2014 %>% rename(alderman_dest = alderman, strictness_dest_frozen = score),
      by = "alderman_dest",
      relationship = "many-to-one"
    ) %>%
    mutate(strictness_change_frozen = strictness_dest_frozen - strictness_origin_frozen) %>%
    tidyr::crossing(year = 2010:2020) %>%
    mutate(relative_year = year - 2015L) %>%
    left_join(missing_counts, by = c("block_id", "year"), relationship = "one-to-one") %>%
    mutate(
      n_high_discretion_application = coalesce(n_high_discretion_application, 0L),
      n_low_discretion_nosigns_application = coalesce(n_low_discretion_nosigns_application, 0L)
    )

  if (anyNA(missing_panel$strictness_change_frozen)) {
    stop("Alternative point sample has unresolved frozen stringency scores.", call. = FALSE)
  }

  panel <- bind_rows(panel, missing_panel)
}

model_rows <- list()
for (point_rule in c("centroid", "point_on_surface")) {
  sample_flag <- if (point_rule == "centroid") "centroid_500ft_sample" else "surface_500ft_sample"
  pair_var <- if (point_rule == "centroid") "event_pair_id" else "surface_event_pair_id"

  for (outcome_var in c("n_high_discretion_application", "n_low_discretion_nosigns_application")) {
    model_data <- panel %>%
      filter(.data[[sample_flag]] %in% TRUE) %>%
      mutate(
        outcome = .data[[outcome_var]],
        fit_pair = .data[[pair_var]],
        strictness_change = strictness_change_frozen
      )

    pre_controls <- model_data %>%
      filter(relative_year < 0) %>%
      summarise(
        pre_period_permit_volume = sum(outcome, na.rm = TRUE),
        .by = block_id
      ) %>%
      mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))

    model_data <- model_data %>%
      left_join(pre_controls, by = "block_id", relationship = "many-to-one") %>%
      mutate(post_treat = as.integer(relative_year >= 0) * strictness_change)

    model <- fepois(
      outcome ~ post_treat +
        pre_period_permit_volume:factor(year) +
        no_pre_period_permits:factor(year) |
        block_id + fit_pair^year,
      data = model_data,
      cluster = ~fit_pair,
      notes = FALSE
    )

    model_rows[[length(model_rows) + 1]] <- tibble(
      point_rule,
      outcome = outcome_var,
      estimate = coef(model)[["post_treat"]],
      std_error = se(model)[["post_treat"]],
      p_value = pvalue(model)[["post_treat"]],
      percent_effect = 100 * expm1(coef(model)[["post_treat"]]),
      nobs = nobs(model),
      design_blocks = n_distinct(model_data$block_id),
      design_pairs = n_distinct(model_data$fit_pair),
      reconstructed_blocks = nrow(missing_surface)
    )
  }
}

write_csv(bind_rows(model_rows), "../output/permit_surface_point_model_sensitivity.csv")
