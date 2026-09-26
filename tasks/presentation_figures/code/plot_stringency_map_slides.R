# setwd("tasks/presentation_figures/code")
# Slide map of alderman stringency scores in three months on one color scale; the paper's two-month version is
# tasks/strictness_score_map.
map_months <- as.Date(c("2007-01-01", "2014-01-01", "2022-01-01"))

source("../../setup_environment/code/packages.R")

scores <- read_csv("../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv",
  show_col_types = FALSE) |>
  transmute(alderman = str_squish(str_to_lower(alderman)), score = uncertainty_index)
stopifnot(!anyDuplicated(scores$alderman))
aldermen <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) |>
  mutate(month = as.yearmon(month), alderman = str_squish(str_to_lower(alderman)))
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)

map_data <- bind_rows(lapply(map_months, function(map_month) {
  wards <- filter(ward_panel, year == as.integer(format(map_month, "%Y")))
  serving <- aldermen |> filter(month == as.yearmon(map_month)) |> transmute(ward, alderman)
  stopifnot(nrow(wards) == 50, !anyDuplicated(wards$ward), !anyDuplicated(serving$ward))
  wards |>
    left_join(serving, by = "ward", relationship = "one-to-one") |>
    left_join(scores, by = "alderman", relationship = "many-to-one") |>
    mutate(vintage = format(map_month, "%B %Y"))
})) |>
  mutate(vintage = factor(vintage, levels = format(map_months, "%B %Y")))
stopifnot(!anyNA(map_data$score))
score_limit <- max(abs(map_data$score))

map <- ggplot(map_data) +
  geom_sf(aes(fill = score), color = "grey20", linewidth = 0.2) +
  facet_wrap(~vintage, nrow = 1) +
  coord_sf(expand = FALSE) +
  scale_fill_gradient2(low = "#2c7bb6", mid = "#ffffbf", high = "#d7191c", midpoint = 0,
    limits = c(-score_limit, score_limit), name = "Stringency\nscore (SD)") +
  theme_void() +
  theme(legend.position = "right", legend.key.height = grid::unit(1.2, "cm"),
    strip.text = element_text(face = "bold", size = 13, margin = margin(b = 4)),
    panel.spacing = grid::unit(0.6, "cm"), plot.margin = margin(0, 0, 0, 0))
ggsave("../output/stringency_map_2007_2014_2022_slides.pdf", map, width = 9.3, height = 4.4, bg = "white")
