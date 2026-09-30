# setwd("tasks/audits/zoning_stalls_and_delays/code")
# Stalls and delays of zoning map amendment applications by year of introduction, 2000--2025, from the City Council
# Journals and eLMS (summarize_by_year.R). Left: the share stalled, lapsing with a council term, with 95 percent
# intervals, and (dashed) the share stalled for good, no refiling having passed, through 2022. Middle: the share not
# passed within 90 days, through 2025. Right: median days from introduction to passage, with the
# interquartile range, and (dashed) the mean, for applications with a year of follow-up. Dotted lines mark the starts
# of council terms.
# The last year of introduction whose applications all have 90 days of follow-up.
last_full_year <- 2025
term_starts <- as.Date(c("2003-05-05", "2007-05-21", "2011-05-16", "2015-05-18", "2019-05-20", "2023-05-15"))

source("../../../setup_environment/code/packages.R")

by_year <- read_csv("../output/stalls_and_delays_by_year.csv", show_col_types = FALSE) |>
  mutate(source = factor(if_else(source == "journals", "City Council Journals", "eLMS"),
    c("City Council Journals", "eLMS")))
source_colors <- c("City Council Journals" = "#2478B5", "eLMS" = "#D92D27")
term_lines <- as.numeric(format(term_starts, "%Y")) + 0.37
style <- list(
  geom_vline(xintercept = term_lines, linetype = "dotted", color = "gray35", linewidth = 0.4),
  scale_color_manual(values = source_colors, name = NULL),
  scale_fill_manual(values = source_colors, name = NULL),
  scale_x_continuous(breaks = seq(2000, 2024, 4), limits = c(2000, 2025)),
  theme_bw(base_size = 10),
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()))

stalls <- ggplot(filter(by_year, !is.na(stall_rate)), aes(x = introduction_year, color = source, fill = source)) +
  geom_ribbon(aes(ymin = stall_ci_low, ymax = stall_ci_high), alpha = 0.16, color = NA) +
  geom_line(aes(y = stall_rate), linewidth = 0.65) +
  geom_point(aes(y = stall_rate), size = 2) +
  geom_line(aes(y = stalled_for_good_rate), linewidth = 0.5, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(title = "Applications that stalled", subtitle = "Dashed: stalled for good (no refiling passed)",
    x = "Year of introduction", y = NULL) +
  style
window <- ggplot(filter(by_year, !is.na(not_passed_in_window_rate), introduction_year <= last_full_year),
  aes(x = introduction_year, color = source, fill = source)) +
  geom_ribbon(aes(ymin = window_ci_low, ymax = window_ci_high), alpha = 0.16, color = NA) +
  geom_line(aes(y = not_passed_in_window_rate), linewidth = 0.65) +
  geom_point(aes(y = not_passed_in_window_rate), size = 2) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(title = "Not passed within 90 days", subtitle = "All applications with 90 days of follow-up",
    x = "Year of introduction", y = NULL) +
  style +
  guides(color = "none", fill = "none")
days <- ggplot(filter(by_year, !is.na(median_days)), aes(x = introduction_year, color = source, fill = source)) +
  geom_ribbon(aes(ymin = days_q25, ymax = days_q75), alpha = 0.16, color = NA) +
  geom_line(aes(y = median_days), linewidth = 0.65) +
  geom_point(aes(y = median_days), size = 2) +
  geom_line(aes(y = mean_days), linewidth = 0.5, linetype = "dashed") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Days to passage, applications passed",
    subtitle = "Median and interquartile range; dashed: mean", x = "Year of introduction", y = NULL) +
  style +
  guides(color = "none", fill = "none")

figure <- stalls + window + days + patchwork::plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")
ggsave("../output/stalls_and_delays_by_year.png", figure, width = 16, height = 5.5, bg = "white")
