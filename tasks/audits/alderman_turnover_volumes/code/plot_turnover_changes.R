# setwd("tasks/audits/alderman_turnover_volumes/code")
# measure <- "new_units"
# New construction across the elections of 2007, 2011 and 2019 (test_turnover_changes.R), one dot per ward and
# election, for one measure: new construction permits (new_construction) or the dwelling units of new residential
# buildings (new_units). Each dot plots what the ward would have had in the term after if it kept its share of its
# side of the city (its count before and after times the side's share after) against how far the actual count departs
# from that, in standard deviations of counting noise (z_side). Without ward-specific change about 95 percent of dots
# lie between -2 and 2 (dashed lines); the test's statistic is the mean of z squared in each panel. Left: wards whose
# alderman stayed; right: wards with a new alderman, the largest departures labeled.
labeled_departures <- 5
# A surname is the last word, with a particle before it ("La Spata").
surname <- "(?:(?:La|De|Van|Da) )?\\S+$"
titles <- c(new_construction = "New construction permits", new_units = "Dwelling units in new residential buildings")

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) cli_args <- measure
stopifnot(length(cli_args) == 1, cli_args[1] %in% names(titles))
measure_plotted <- cli_args[1]

changes <- read_csv("../output/ward_election_changes.csv", show_col_types = FALSE) |>
  filter(measure == measure_plotted, is.finite(z_side)) |>
  mutate(expected = (count_before + count_after) * share_after_side,
    panel = factor(if_else(turnover, "New alderman", "Alderman stayed"), c("Alderman stayed", "New alderman")),
    label = paste0("Ward ", ward, ", ", format(election, "%Y"), ": ", str_extract(alderman_before, surname), " to ",
      str_extract(alderman_after, surname)))
means <- changes |>
  summarise(mean_z2 = mean(z_side^2), wards = n(), .by = panel) |>
  mutate(text = sprintf("Mean z² = %.1f (%d ward-elections)", mean_z2, wards))
labels <- changes |>
  filter(turnover) |>
  slice_max(abs(z_side), n = labeled_departures) |>
  arrange(z_side) |>
  # Labels of neighboring departures are set above and below their dots in turn.
  mutate(hjust = if_else(expected > quantile(changes$expected, 0.8), 1.08, -0.08),
    vjust = if_else(abs(z_side - lag(z_side, default = -Inf)) < 0.5 & row_number() %% 2 == 0, -0.8, 0.4))
panel_colors <- c("Alderman stayed" = "#2478B5", "New alderman" = "#D92D27")

figure <- ggplot(changes, aes(x = expected, y = z_side, color = panel)) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "gray40", linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray40", linewidth = 0.4) +
  geom_point(size = 2, alpha = 0.8) +
  geom_text(data = labels, aes(label = label, hjust = hjust, vjust = vjust), size = 2.7, show.legend = FALSE) +
  geom_text(data = means, aes(label = text), x = -Inf, y = Inf, hjust = -0.05, vjust = 1.5, size = 3.4,
    color = "black", inherit.aes = FALSE) +
  facet_wrap(~panel) +
  scale_color_manual(values = panel_colors, guide = "none") +
  scale_x_log10() +
  labs(title = paste(titles[[measure_plotted]], "after an election, against the ward's share of its side of the city"),
    subtitle = "Elections of 2007, 2011 and 2019, when wards kept their boundaries",
    x = "Expected in the term after (log scale)",
    y = "Departure from expected (standard deviations of counting noise)") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11), panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"))
ggsave(sprintf("../output/turnover_%s.png", measure_plotted), figure, width = 12, height = 6, bg = "white")
