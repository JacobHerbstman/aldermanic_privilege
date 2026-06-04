source("../../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/within_ward_strictness_audit/code")
# residualized_wide_input <- "../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# ward_panel_input <- "../input/ward_panel.gpkg"
# alderman_panel_input <- "../input/chicago_alderman_panel.csv"
# end_year <- "2022"
# output_series_csv <- "../output/residualized_high_low_map_year_series_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# output_period_csv <- "../output/residualized_high_low_map_period_summary_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# output_pdf <- "../output/residualized_high_low_map_year_series_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.pdf"
# output_png <- "../output/residualized_high_low_map_year_series_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.png"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    residualized_wide_input,
    ward_panel_input,
    alderman_panel_input,
    end_year,
    output_series_csv,
    output_period_csv,
    output_pdf,
    output_png
  )
}

if (length(args) != 8) {
  stop(
    paste(
      "FATAL: Script requires 8 args:",
      "<residualized_wide_input> <ward_panel_input> <alderman_panel_input>",
      "<end_year> <output_series_csv> <output_period_csv>",
      "<output_pdf> <output_png>"
    ),
    call. = FALSE
  )
}

residualized_wide_input <- args[1]
ward_panel_input <- args[2]
alderman_panel_input <- args[3]
end_year <- as.integer(args[4])
output_series_csv <- args[5]
output_period_csv <- args[6]
output_pdf <- args[7]
output_png <- args[8]

message("=== Residualized High-vs-Low Map-Year Series ===")
message("End year: ", end_year)

alderman_wide <- read_csv(residualized_wide_input, show_col_types = FALSE)
if (anyDuplicated(alderman_wide$alderman) > 0) {
  stop("Residualized high-low input must be unique by alderman.", call. = FALSE)
}
ward_panel <- st_read(ward_panel_input, quiet = TRUE)
if (anyDuplicated(st_drop_geometry(ward_panel)[c("ward", "year")]) > 0) {
  stop("Ward panel must be unique by ward-year.", call. = FALSE)
}
alderman_panel <- read_csv(alderman_panel_input, show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month)) %>%
  mutate(map_year = year(as.Date(month)))
if (anyDuplicated(alderman_panel[c("ward", "month")]) > 0) {
  stop("Alderman panel must be unique by ward-month.", call. = FALSE)
}

map_years <- ward_panel %>%
  st_drop_geometry() %>%
  filter(year <= end_year) %>%
  distinct(year) %>%
  pull(year) %>%
  sort()

series <- map_dfr(map_years, function(map_year) {
  ward_labels <- alderman_panel %>%
    filter(map_year == !!map_year) %>%
    group_by(ward) %>%
    slice_max(month, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(ward, alderman)
  if (anyDuplicated(ward_labels$ward) > 0) {
    stop("Current ward labels must be unique by ward.", call. = FALSE)
  }

  cor_sample <- ward_panel %>%
    filter(year == map_year) %>%
    st_drop_geometry() %>%
    select(ward) %>%
    left_join(ward_labels, by = "ward", relationship = "one-to-one") %>%
    left_join(alderman_wide, by = "alderman", relationship = "many-to-one") %>%
    filter(!is.na(mean_resid_high), !is.na(mean_resid_low))

  tibble(
    map_year = map_year,
    n_wards_matched = nrow(cor_sample),
    pearson_resid = if (nrow(cor_sample) >= 2) cor(cor_sample$mean_resid_high, cor_sample$mean_resid_low) else NA_real_,
    spearman_resid = if (nrow(cor_sample) >= 2) cor(cor_sample$mean_resid_high, cor_sample$mean_resid_low, method = "spearman") else NA_real_,
    mean_gap = if (nrow(cor_sample) >= 1) mean(cor_sample$mean_resid_high - cor_sample$mean_resid_low, na.rm = TRUE) else NA_real_
  )
})

write_csv(series, output_series_csv)

period_summary <- series %>%
  mutate(period = if_else(map_year <= 2014, "pre_2015", "post_2015")) %>%
  group_by(period) %>%
  summarise(
    start_year = min(map_year),
    end_year = max(map_year),
    n_years = n(),
    mean_pearson = mean(pearson_resid, na.rm = TRUE),
    median_pearson = median(pearson_resid, na.rm = TRUE),
    min_pearson = min(pearson_resid, na.rm = TRUE),
    max_pearson = max(pearson_resid, na.rm = TRUE),
    share_years_negative = mean(pearson_resid < 0, na.rm = TRUE),
    mean_wards_matched = mean(n_wards_matched, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(period_summary, output_period_csv)

p_series <- ggplot(series, aes(x = map_year, y = pearson_resid)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 2015, color = "#B2182B", linewidth = 0.6, linetype = "dotted") +
  geom_line(color = "#2C7FB8", linewidth = 0.9) +
  geom_point(color = "#2C7FB8", size = 2) +
  scale_x_continuous(breaks = pretty(series$map_year, n = 10)) +
  labs(
    title = "Correlation Between Residualized High- and Low-Discretion Processing Times",
    subtitle = "Cross-sectional correlation across wards on each year's ward map using current officeholders",
    x = "Ward-map year",
    y = "Pearson correlation",
    caption = paste0(
      "Mean annual correlation, pre-2015: ",
      formatC(period_summary$mean_pearson[period_summary$period == "pre_2015"], digits = 3, format = "f"),
      " | post-2015: ",
      formatC(period_summary$mean_pearson[period_summary$period == "post_2015"], digits = 3, format = "f")
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(output_pdf, p_series, width = 8.5, height = 4.75)
ggsave(output_png, p_series, width = 8.5, height = 4.75, dpi = 220)

message("Saved: ", output_series_csv)
message("Saved: ", output_period_csv)
message("Saved: ", output_pdf)
message("Saved: ", output_png)
