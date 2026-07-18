# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

connection <- DBI::dbConnect(
  RSQLite::SQLite(),
  "../../../clean_building_permits/output/building_permits_clean.gpkg"
)

permits <- DBI::dbGetQuery(
  connection,
  paste(
    "SELECT id, permit, permit_status, permit_milestone, permit_type, review_type,",
    "application_start_date, issue_date, processing_time, reported_cost, total_fee,",
    "permit_issued, corporate_applicant, street_number, street_direction, street_name,",
    "work_type, work_description",
    "FROM building_permits_clean",
    "WHERE high_discretion = 1",
    "AND application_start_date < '2023-01-01'"
  )
)
DBI::dbDisconnect(connection)

permits <- permits %>%
  mutate(
    id = as.character(id),
    application_start_date = as.Date(application_start_date),
    issue_date = as.Date(issue_date),
    year = year(application_start_date),
    day_group = if_else(processing_time == 0, "zero_day", "positive_day"),
    permit_status = str_to_upper(str_squish(coalesce(permit_status, ""))),
    permit_milestone = str_to_upper(str_squish(coalesce(permit_milestone, ""))),
    review_type = str_to_upper(str_squish(coalesce(review_type, ""))),
    description = str_squish(coalesce(work_description, "")),
    description_upper = str_to_upper(description),
    address = str_squish(paste(street_number, street_direction, street_name)),
    positive_cost = if_else(reported_cost > 0, reported_cost, NA_real_)
  )

if (
  anyDuplicated(permits$id) > 0 ||
  any(permits$processing_time < 0) ||
  any(
    permits$day_group == "zero_day" &
      permits$application_start_date != permits$issue_date
  )
) {
  stop("High-discretion permit audit inputs violate expected record or date structure.", call. = FALSE)
}

permit_summary <- bind_rows(
  permits,
  permits %>% mutate(permit_type = "ALL HIGH-DISCRETION")
) %>%
  group_by(permit_type, day_group) %>%
  summarise(
    permits = n(),
    share_complete = mean(permit_status == "COMPLETE"),
    share_issued_status = mean(permit_issued == 1, na.rm = TRUE),
    share_self_cert = mean(review_type == "SELF CERT"),
    share_standard_plan_review = mean(review_type == "STANDARD PLAN REVIEW"),
    share_developer_services = mean(str_detect(review_type, "DEVELOPER SERVICES")),
    share_easy_permit_review = mean(review_type == "EASY PERMIT"),
    share_positive_reported_cost = mean(reported_cost > 0),
    median_reported_cost_positive = median(positive_cost, na.rm = TRUE),
    p75_reported_cost_positive = quantile(positive_cost, 0.75, na.rm = TRUE),
    p90_reported_cost_positive = quantile(positive_cost, 0.90, na.rm = TRUE),
    share_cost_at_least_100000 = mean(reported_cost >= 100000, na.rm = TRUE),
    share_cost_at_least_1000000 = mean(reported_cost >= 1000000, na.rm = TRUE),
    share_with_description = mean(description != ""),
    share_revision_description = mean(str_detect(description_upper, "REVISION|REVISE")),
    share_interior_description = mean(str_detect(description_upper, "INTERIOR")),
    share_units_description = mean(str_detect(description_upper, "[0-9]+[ -]?(UNIT|DU)")),
    share_foundation_description = mean(str_detect(description_upper, "FOUNDATION")),
    share_certificate_of_occupancy = mean(
      str_detect(permit_milestone, "CERTIFICATE OF OCCUPANCY")
    ),
    .groups = "drop"
  ) %>%
  arrange(permit_type, day_group)

write_csv(permit_summary, "../output/permit_zero_day_substantiveness.csv")

year_profile <- permits %>%
  count(year, day_group, name = "permits") %>%
  pivot_wider(names_from = day_group, values_from = permits, values_fill = 0) %>%
  mutate(
    all_high_discretion_permits = zero_day + positive_day,
    zero_day_share = zero_day / all_high_discretion_permits
  ) %>%
  arrange(year)

write_csv(year_profile, "../output/permit_zero_day_year_profile.csv")

high_cost_examples <- permits %>%
  group_by(day_group, permit_type) %>%
  arrange(desc(reported_cost), id, .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  mutate(example_group = "highest_reported_cost")

typical_cost_examples <- permits %>%
  filter(reported_cost > 0) %>%
  group_by(day_group, permit_type) %>%
  mutate(
    permit_type_median_cost = median(reported_cost),
    distance_from_median_log_cost = abs(
      log(reported_cost) - log(permit_type_median_cost)
    )
  ) %>%
  arrange(distance_from_median_log_cost, id, .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  mutate(example_group = "near_type_median_cost")

bind_rows(high_cost_examples, typical_cost_examples) %>%
  select(
    example_group, day_group, id, permit, permit_type, review_type, permit_status,
    permit_milestone, application_start_date, issue_date, processing_time,
    reported_cost, total_fee, address, work_type, description
  ) %>%
  arrange(permit_type, day_group, example_group, desc(reported_cost), id) %>%
  write_csv("../output/permit_zero_day_examples.csv")
