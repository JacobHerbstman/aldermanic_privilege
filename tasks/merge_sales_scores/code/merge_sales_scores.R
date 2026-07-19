# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/merge_sales_scores/code")

source("../../setup_environment/code/packages.R")

scores <- read_csv("../input/aldermen_uncertainty_scores_through2022.csv", show_col_types = FALSE) %>%
  select(alderman, score = uncertainty_index) %>%
  filter(!is.na(alderman))
if (anyDuplicated(scores$alderman) > 0 || any(!is.finite(scores$score))) {
  stop("Stringency scores must be finite and unique by alderman.", call. = FALSE)
}

sales_pre <- read_csv(
  "../input/sales_pre_scores.csv",
  col_types = cols(pin = col_character(), .default = col_guess()),
  show_col_types = FALSE
) %>%
  mutate(
    pin = gsub("[^0-9]", "", trimws(pin)),
    pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin)
  )
if (!all(c("alderman_own", "alderman_neighbor", "dist_m") %in% names(sales_pre))) {
  stop("Sales input must include alderman_own, alderman_neighbor, and dist_m.", call. = FALSE)
}
if (any(nchar(sales_pre$pin) != 14L)) {
  stop("Sales input contains an invalid full PIN.", call. = FALSE)
}

sales <- sales_pre %>%
  left_join(scores, by = c("alderman_own" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_own = score) %>%
  left_join(scores, by = c("alderman_neighbor" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_neighbor = score) %>%
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_dist_m = dist_m * sign,
    dist_ft = dist_m / 0.3048,
    signed_dist = signed_dist_m / 0.3048
  )

if (nrow(sales) != nrow(sales_pre)) {
  stop("Row count changed while attaching sales scores.", call. = FALSE)
}

sales <- sales %>%
  filter(!is.na(sign)) %>%
  select(
    any_of(c("row_id", "sale_document_num")),
    pin, year, sale_date, sale_price,
    any_of(c(
      "sale_price_nominal",
      "sale_price_real_2022_raw",
      "sale_price_cpi_chi_all_items",
      "sale_price_deflator_to_2022"
    )),
    class,
    latitude, longitude, any_of(c("coordinate_source", "coordinate_year")),
    ward, neighbor_ward, ward_pair_id, any_of("segment_id"),
    dist_m, signed_dist_m, sign,
    alderman_own, alderman_neighbor,
    strictness_own, strictness_neighbor
  )

write_csv(sales, "../output/sales_with_ward_distances_through2022.csv")
