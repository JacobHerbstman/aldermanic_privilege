source("../../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/within_ward_strictness_audit/code")
# residualized_wide_input <- "../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# ward_panel_input <- "../input/ward_panel.gpkg"
# alderman_panel_input <- "../input/chicago_alderman_panel.csv"
# map_year <- "2022"
# output_map_csv <- "../output/residualized_high_low_current_map_data_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022_2022.csv"
# output_summary_csv <- "../output/residualized_high_low_current_map_summary_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022_2022.csv"
# output_pdf <- "../output/residualized_high_low_current_map_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022_2022.pdf"
# output_png <- "../output/residualized_high_low_current_map_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022_2022.png"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    residualized_wide_input,
    ward_panel_input,
    alderman_panel_input,
    map_year,
    output_map_csv,
    output_summary_csv,
    output_pdf,
    output_png
  )
}

if (length(args) != 8) {
  stop(
    paste(
      "FATAL: Script requires 8 args:",
      "<residualized_wide_input> <ward_panel_input> <alderman_panel_input>",
      "<map_year> <output_map_csv> <output_summary_csv>",
      "<output_pdf> <output_png>"
    ),
    call. = FALSE
  )
}

residualized_wide_input <- args[1]
ward_panel_input <- args[2]
alderman_panel_input <- args[3]
map_year <- as.integer(args[4])
output_map_csv <- args[5]
output_summary_csv <- args[6]
output_pdf <- args[7]
output_png <- args[8]

message("=== Current-Alderman Residualized High-vs-Low Map ===")
message("Map year: ", map_year)

alderman_wide <- read_csv(residualized_wide_input, show_col_types = FALSE)
if (anyDuplicated(alderman_wide$alderman) > 0) {
  stop("Residualized high-low input must be unique by alderman.", call. = FALSE)
}
ward_panel <- st_read(ward_panel_input, quiet = TRUE)
if (anyDuplicated(st_drop_geometry(ward_panel %>% filter(year == map_year))$ward) > 0) {
  stop("Ward panel must be unique by ward for the selected map year.", call. = FALSE)
}
alderman_panel <- read_csv(alderman_panel_input, show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month))
if (anyDuplicated(alderman_panel[c("ward", "month")]) > 0) {
  stop("Alderman panel must be unique by ward-month.", call. = FALSE)
}

ward_labels <- alderman_panel %>%
  filter(year(as.Date(month)) == map_year) %>%
  group_by(ward) %>%
  slice_max(month, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ward, alderman)
if (anyDuplicated(ward_labels$ward) > 0) {
  stop("Current ward labels must be unique by ward.", call. = FALSE)
}

map_data <- ward_panel %>%
  filter(year == map_year) %>%
  left_join(ward_labels, by = "ward", relationship = "one-to-one") %>%
  left_join(alderman_wide, by = "alderman", relationship = "many-to-one")

st_drop_geometry(map_data) %>%
  write_csv(output_map_csv)

cor_sample <- st_drop_geometry(map_data) %>%
  filter(!is.na(mean_resid_high), !is.na(mean_resid_low))

summary_out <- tibble(
  map_year = map_year,
  n_wards_matched = nrow(cor_sample),
  pearson_resid = if (nrow(cor_sample) >= 2) cor(cor_sample$mean_resid_high, cor_sample$mean_resid_low) else NA_real_,
  spearman_resid = if (nrow(cor_sample) >= 2) cor(cor_sample$mean_resid_high, cor_sample$mean_resid_low, method = "spearman") else NA_real_,
  mean_n_high = mean(cor_sample$n_high, na.rm = TRUE),
  mean_n_low = mean(cor_sample$n_low, na.rm = TRUE)
)

write_csv(summary_out, output_summary_csv)

map_long <- map_data %>%
  transmute(ward, alderman, mean_resid_high, mean_resid_low) %>%
  pivot_longer(
    cols = c(mean_resid_high, mean_resid_low),
    names_to = "group",
    values_to = "mean_resid"
  ) %>%
  mutate(
    group = recode(
      group,
      mean_resid_high = "High-discretion permits",
      mean_resid_low = "Low-discretion permits"
    )
  )

fill_limit <- max(abs(map_long$mean_resid), na.rm = TRUE)

p_map <- ggplot(map_long) +
  geom_sf(aes(fill = mean_resid), color = "white", linewidth = 0.2) +
  geom_sf_text(aes(label = ward), color = "grey15", size = 2.2) +
  facet_wrap(~group, nrow = 1) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-fill_limit, fill_limit),
    oob = scales::squish,
    na.value = "grey90",
    name = "Mean residual\nlog days"
  ) +
  labs(
    title = paste0("Residualized Permit Processing Times on the ", map_year, " Ward Map"),
    subtitle = "Each ward is colored by its current alderman's full through-2022 mean residualized processing time",
    caption = paste0(
      map_year, " aldermen mapped: ", summary_out$n_wards_matched,
      " | Pearson r across mapped wards: ", formatC(summary_out$pearson_resid, digits = 3, format = "f")
    )
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 0),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(output_pdf, p_map, width = 12, height = 6.75)
ggsave(output_png, p_map, width = 12, height = 6.75, dpi = 220)

message("Saved: ", output_map_csv)
message("Saved: ", output_summary_csv)
message("Saved: ", output_pdf)
message("Saved: ", output_png)
