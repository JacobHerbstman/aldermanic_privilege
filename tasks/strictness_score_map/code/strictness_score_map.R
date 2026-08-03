# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/strictness_score_map/code")
# early_date <- "2014-01"
# late_date <- "2022-01"
# uncertainty_spec <- "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(early_date, late_date, uncertainty_spec)
}

if (length(cli_args) != 3) {
  stop(
    "Script requires 3 arguments: <early_date> <late_date> <uncertainty_spec>.",
    call. = FALSE
  )
}

early_date <- cli_args[1]
late_date <- cli_args[2]
uncertainty_spec <- cli_args[3]

early_month <- as.Date(paste0(early_date, "-01"))
late_month <- as.Date(paste0(late_date, "-01"))
early_year <- as.integer(format(early_month, "%Y"))
late_year <- as.integer(format(late_month, "%Y"))

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
early_wards <- ward_panel %>% filter(year == early_year)
late_wards <- ward_panel %>% filter(year == late_year)
if (anyDuplicated(st_drop_geometry(early_wards)$ward) > 0 ||
    anyDuplicated(st_drop_geometry(late_wards)$ward) > 0) {
  stop("Ward panel must be unique by ward and map year.", call. = FALSE)
}

scores <- read_csv(
  sprintf("../input/alderman_uncertainty_index_%s.csv", uncertainty_spec),
  show_col_types = FALSE
) %>%
  mutate(alderman = str_squish(str_to_lower(alderman)))
if (!"uncertainty_index" %in% names(scores)) {
  stop("Score file must contain uncertainty_index.", call. = FALSE)
}
scores <- scores %>%
  transmute(alderman, score = as.numeric(uncertainty_index))
if (anyDuplicated(scores$alderman) > 0) {
  stop("Scores must be unique by alderman.", call. = FALSE)
}

alderman_panel <- read_csv(
  "../input/chicago_alderman_panel.csv",
  show_col_types = FALSE
) %>%
  mutate(month = as.yearmon(month)) %>%
  mutate(alderman = str_squish(str_to_lower(alderman)))
early_aldermen <- alderman_panel %>%
  filter(month == as.yearmon(early_month)) %>%
  transmute(ward, alderman)
late_aldermen <- alderman_panel %>%
  filter(month == as.yearmon(late_month)) %>%
  transmute(ward, alderman)
if (anyDuplicated(early_aldermen$ward) > 0 ||
    anyDuplicated(late_aldermen$ward) > 0) {
  stop("Alderman panel must be unique by ward and selected month.", call. = FALSE)
}

early_map <- early_wards %>%
  left_join(early_aldermen, by = "ward", relationship = "one-to-one") %>%
  left_join(scores, by = "alderman", relationship = "many-to-one") %>%
  mutate(vintage = "Panel A: January 2014")
late_map <- late_wards %>%
  left_join(late_aldermen, by = "ward", relationship = "one-to-one") %>%
  left_join(scores, by = "alderman", relationship = "many-to-one") %>%
  mutate(vintage = "Panel B: January 2022")
if (any(is.na(early_map$score)) || any(is.na(late_map$score))) {
  stop("Every mapped ward must have an alderman and score.", call. = FALSE)
}

map_data <- bind_rows(early_map, late_map) %>%
  mutate(
    vintage = factor(
      vintage,
      levels = c(
        "Panel A: January 2014",
        "Panel B: January 2022"
      )
    )
  )
score_limit <- max(abs(map_data$score))

p <- ggplot(map_data) +
  geom_sf(aes(fill = score), color = "grey20", linewidth = 0.2) +
  facet_wrap(~vintage, nrow = 1) +
  scale_fill_gradient2(
    low = "#2c7bb6",
    mid = "#ffffbf",
    high = "#d7191c",
    midpoint = 0,
    limits = c(-score_limit, score_limit),
    name = "Regulatory stringency"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = grid::unit(0.5, "cm")
  )

ggsave(
  "../output/uncertainty_score_map_comparison.pdf",
  plot = p,
  width = 11,
  height = 6.5,
  dpi = 300
)
