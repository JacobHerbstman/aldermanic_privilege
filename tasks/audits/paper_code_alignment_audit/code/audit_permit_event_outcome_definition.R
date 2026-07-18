# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

panel <- read_parquet("../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet") %>%
  filter(relative_year >= -5L, relative_year <= 5L, dist_m <= 152.4) %>%
  select(
    block_id, year, relative_year, ward_pair_id, strictness_change_frozen,
    n_high_discretion_application, n_high_discretion_issue,
    n_low_discretion_nosigns_application, n_low_discretion_nosigns_issue
  )

if (anyDuplicated(panel[c("block_id", "year")]) > 0) {
  stop("Production permit panel is not unique by block-year.", call. = FALSE)
}

blocks <- read_csv("../../../download_chicago_spatial_data/output/census_blocks_2010.csv", show_col_types = FALSE) %>%
  rename(geometry = the_geom, block_id = GEOID10) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  mutate(block_id = as.character(block_id)) %>%
  distinct(block_id, .keep_all = TRUE) %>%
  select(block_id)

permits <- st_read(
  "../../../clean_building_permits/output/building_permits_clean.gpkg",
  query = paste(
    "SELECT id, permit_status, permit_type, high_discretion,",
    "application_start_date_ym, issue_date_ym, geom",
    "FROM building_permits_clean"
  ),
  quiet = TRUE
) %>%
  mutate(
    id = as.character(id),
    permit_status = str_to_upper(trimws(permit_status)),
    application_year = year(as.Date(application_start_date_ym)),
    issue_year = year(as.Date(issue_date_ym)),
    high_discretion = as.integer(high_discretion),
    production_status = is.na(permit_status) |
      !permit_status %in% c("CANCELLED", "REVOKED", "SUSPENDED"),
    issued_status = permit_status %in% c("COMPLETE", "ACTIVE", "PHASED PERMITTING"),
    permit_group = case_when(
      high_discretion == 1L ~ "High-discretion",
      high_discretion == 0L & permit_type != "PERMIT - SIGNS" ~ "Low-discretion",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(permit_group),
    (application_year >= 2010L & application_year <= 2020L) |
      (issue_year >= 2010L & issue_year <= 2020L)
  )

if (st_crs(permits) != st_crs(blocks)) {
  permits <- st_transform(permits, st_crs(blocks))
}

permit_blocks <- st_join(permits, blocks, join = st_within, left = TRUE) %>%
  st_drop_geometry()

if (anyDuplicated(permit_blocks$id) > 0) {
  stop("Permit-to-block spatial join produced duplicate permit IDs.", call. = FALSE)
}

manual_assignments <- read_csv(
  "../../../create_event_study_permit_data/input/manual_permit_block_assignments.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  filter(block_vintage == "2010") %>%
  transmute(id = as.character(id), manual_block_id = na_if(reviewed_block_id, ""))

permit_blocks <- permit_blocks %>%
  left_join(manual_assignments, by = "id", relationship = "many-to-one") %>%
  mutate(block_id = coalesce(block_id, manual_block_id)) %>%
  select(-manual_block_id) %>%
  filter(block_id %in% unique(panel$block_id))

status_composition <- permit_blocks %>%
  filter(application_year >= 2010L, application_year <= 2020L) %>%
  count(
    permit_group,
    permit_status,
    production_status,
    issued_status,
    name = "permits"
  ) %>%
  arrange(permit_group, desc(permits))
write_csv(status_composition, "../output/permit_event_outcome_status_composition.csv")

portal_coverage <- permit_blocks %>%
  summarise(
    permits_in_500ft_design_blocks = n(),
    missing_application_year = sum(is.na(application_year)),
    missing_issue_year = sum(is.na(issue_year)),
    production_status_permits = sum(production_status),
    cancelled_revoked_suspended = sum(!production_status),
    issued_status_permits = sum(issued_status),
    expired_status_permits = sum(permit_status == "EXPIRED", na.rm = TRUE),
    missing_status_permits = sum(is.na(permit_status) | permit_status == "")
  )
write_csv(portal_coverage, "../output/permit_event_outcome_portal_coverage.csv")

count_specs <- tribble(
  ~outcome_family, ~date_rule, ~status_rule, ~date_var,
  "High-discretion", "application", "production", "application_year",
  "High-discretion", "issue", "production", "issue_year",
  "High-discretion", "application", "all_clean_statuses", "application_year",
  "High-discretion", "application", "issued_status_only", "application_year",
  "Low-discretion", "application", "production", "application_year",
  "Low-discretion", "issue", "production", "issue_year",
  "Low-discretion", "application", "all_clean_statuses", "application_year",
  "Low-discretion", "application", "issued_status_only", "application_year"
)

model_rows <- list()
for (i in seq_len(nrow(count_specs))) {
  outcome_family <- count_specs$outcome_family[i]
  date_rule <- count_specs$date_rule[i]
  status_rule <- count_specs$status_rule[i]
  date_var <- count_specs$date_var[i]

  permits_i <- permit_blocks %>%
    filter(permit_group == outcome_family)
  if (status_rule == "production") {
    permits_i <- permits_i %>% filter(production_status)
  } else if (status_rule == "issued_status_only") {
    permits_i <- permits_i %>% filter(issued_status)
  }

  counts_i <- permits_i %>%
    filter(.data[[date_var]] >= 2010L, .data[[date_var]] <= 2020L) %>%
    count(block_id, year = .data[[date_var]], name = "outcome")

  data_i <- panel %>%
    select(block_id, year, relative_year, ward_pair_id, strictness_change_frozen) %>%
    left_join(counts_i, by = c("block_id", "year"), relationship = "one-to-one") %>%
    mutate(outcome = replace_na(outcome, 0L))

  pre_controls <- data_i %>%
    filter(relative_year < 0) %>%
    group_by(block_id) %>%
    summarise(pre_period_permit_volume = sum(outcome), .groups = "drop") %>%
    mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))

  data_i <- data_i %>%
    left_join(pre_controls, by = "block_id", relationship = "many-to-one") %>%
    mutate(post_treat = as.integer(relative_year >= 0) * strictness_change_frozen)

  model_i <- fepois(
    outcome ~ post_treat +
      pre_period_permit_volume:factor(year) +
      no_pre_period_permits:factor(year) |
      block_id + ward_pair_id^year,
    data = data_i,
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  model_rows[[i]] <- tibble(
    outcome_family = outcome_family,
    date_rule = date_rule,
    status_rule = status_rule,
    coefficient = coef(model_i)[["post_treat"]],
    standard_error = se(model_i)[["post_treat"]],
    p_value = pvalue(model_i)[["post_treat"]],
    percent_effect = 100 * expm1(coef(model_i)[["post_treat"]]),
    observations = nobs(model_i),
    blocks = n_distinct(data_i$block_id[obs(model_i)]),
    ward_pairs = n_distinct(data_i$ward_pair_id[obs(model_i)]),
    counted_permits = sum(counts_i$outcome)
  )
}

write_csv(bind_rows(model_rows), "../output/permit_event_date_status_sensitivity.csv")
