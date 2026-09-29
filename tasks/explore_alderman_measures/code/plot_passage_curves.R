# setwd("tasks/explore_alderman_measures/code")
# Exploratory: share of zoning map amendment applications passed by months since introduction, by council term
# (passage_curves.csv from explore_application_dynamics.R). The curves end at the share that ever passed; the rest
# stalled when their term ended.
term_colors <- c("2011-2015" = "gray45", "2015-2019" = "#2478B5", "2019-2023" = "#D92D27")

source("../../setup_environment/code/packages.R")

curves <- read_csv("../output/passage_curves.csv", show_col_types = FALSE) |>
  mutate(term = sprintf("%s (%s applications)", term, format(applications, big.mark = ",")))
names(term_colors) <- sort(unique(curves$term))
figure <- ggplot(curves, aes(x = months, y = share_passed, color = term)) +
  geom_step(linewidth = 0.7) +
  scale_color_manual(values = term_colors, name = NULL) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 6, 12, 24, 36, 48)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Share of zoning map amendment applications passed, by months since introduction",
    subtitle = "Applications of each council term, followed until the term ended",
    x = "Months since introduction", y = "Passed") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 12, face = "bold"), axis.title = element_text(size = 9),
    axis.text = element_text(size = 8), panel.grid.minor = element_blank())
ggsave("../output/passage_curves.png", figure, width = 10, height = 6, bg = "white")
