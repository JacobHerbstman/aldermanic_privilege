library(tidyverse)
library(ggplot2)

# Load minimal controls results
alderman_index <- read_csv('../output/alderman_uncertainty_index_MINIMAL_CONTROLS.csv', show_col_types = FALSE)

# Create plot
plot_data <- alderman_index %>%
  filter(!is.na(uncertainty_index)) %>%
  arrange(uncertainty_index) %>%
  mutate(alderman = factor(alderman, levels = alderman))

p <- ggplot(plot_data, aes(x = uncertainty_index, y = alderman, fill = uncertainty_index)) +
  geom_col() +
  scale_fill_distiller(
    palette = "RdYlBu",
    direction = -1,
    name = "Index"
  ) +
  labs(
    title = "Alderman Uncertainty Index (Minimal Controls)",
    subtitle = "Controls: Ward demographics + Month FE + Review Type FE",
    x = "Uncertainty Index (standardized)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7)

ggsave("../output/uncertainty_index_MINIMAL_CONTROLS.pdf", p, width = 8, height = 14)
message("Saved: ../output/uncertainty_index_MINIMAL_CONTROLS.pdf")
