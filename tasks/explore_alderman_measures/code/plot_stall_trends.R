# setwd("tasks/explore_alderman_measures/code")
# Exploratory: the share of zoning map amendment applications (not filed by an alderman) that stalled, by year of
# introduction, 2010 to the start of the current council term, for all applications and for those introduced more
# than six months before their term ended (applications introduced late in a term lapse with it). Dashed lines mark
# the starts of council terms. 95 percent intervals are binomial.
current_term_start <- as.Date("2023-05-15")
term_starts <- as.Date(c("2011-05-16", "2015-05-18", "2019-05-20", "2023-05-15"))
term_end_months <- 6

source("../../setup_environment/code/packages.R")

applications <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE) |>
  filter(!filed_by_alderman, outcome != "pending", introduction_date < current_term_start) |>
  mutate(year = as.integer(format(introduction_date, "%Y")), stalled = outcome == "stalled",
    next_term_start = term_starts[findInterval(introduction_date, term_starts) + 1L],
    late_in_term = as.numeric(next_term_start - introduction_date) / 30.44 <= term_end_months)
rates <- bind_rows(
  applications |> mutate(series = "All applications"),
  applications |> filter(!late_in_term) |> mutate(series = "Introduced more than six months before the term ended")
) |>
  summarise(applications = n(), stall_rate = mean(stalled), .by = c(series, year)) |>
  mutate(std_error = sqrt(stall_rate * (1 - stall_rate) / applications),
    ci_low = pmax(stall_rate - 1.96 * std_error, 0), ci_high = stall_rate + 1.96 * std_error)
series_colors <- c("All applications" = "#D92D27", "Introduced more than six months before the term ended" = "#2478B5")

figure <- ggplot(rates, aes(x = year, y = stall_rate, color = series, fill = series)) +
  geom_vline(xintercept = as.numeric(format(term_starts, "%Y")) + 0.37, linetype = "dashed", color = "gray35",
    linewidth = 0.4) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.16, color = NA) +
  geom_line(linewidth = 0.65) +
  geom_point(size = 2.3) +
  scale_color_manual(values = series_colors, name = NULL) +
  scale_fill_manual(values = series_colors, guide = "none") +
  scale_x_continuous(breaks = 2010:2023) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Share of zoning map amendment applications that stalled, by year of introduction",
    subtitle = "Dashed lines: council terms begin (May 2011, 2015, 2019, 2023)",
    x = "Year of introduction", y = "Stalled") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 12, face = "bold"), axis.title = element_text(size = 9),
    axis.text = element_text(size = 8), panel.grid.minor = element_blank())
ggsave("../output/stall_rate_by_year.png", figure, width = 10, height = 6, bg = "white")
