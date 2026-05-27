# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/within_ward_strictness/code")
# spec <- "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022"

source("../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(spec)
}
if (length(args) != 1) {
  stop("FATAL: Script requires 1 arg: <uncertainty_spec>", call. = FALSE)
}

spec <- args[1]
score_name <- "Regulatory Stringency Index"

scores <- read_csv(
  sprintf("../input/alderman_uncertainty_index_%s.csv", spec),
  show_col_types = FALSE
) %>%
  mutate(stringency_index = suppressWarnings(as.numeric(uncertainty_index))) %>%
  filter(!is.na(stringency_index))
if (anyDuplicated(scores$alderman) > 0) {
  stop("Scores must be unique by alderman.", call. = FALSE)
}

panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month))
if (anyDuplicated(panel[c("ward", "month")]) > 0) {
  stop("Alderman panel must be unique by ward-month.", call. = FALSE)
}

alderman_sequence <- panel %>%
  filter(month >= as.yearmon("2006-01")) %>%
  group_by(ward, alderman) %>%
  summarise(first_month = min(month), .groups = "drop") %>%
  arrange(ward, first_month)

turnover_pairs <- alderman_sequence %>%
  group_by(ward) %>%
  arrange(first_month) %>%
  mutate(
    successor = lead(alderman),
    predecessor = alderman
  ) %>%
  filter(!is.na(successor)) %>%
  select(ward, predecessor, successor) %>%
  ungroup() %>%
  inner_join(
    scores %>%
      select(alderman, stringency_index) %>%
      rename(predecessor_score = stringency_index),
    by = c("predecessor" = "alderman"),
    relationship = "many-to-one"
  ) %>%
  inner_join(
    scores %>%
      select(alderman, stringency_index) %>%
      rename(successor_score = stringency_index),
    by = c("successor" = "alderman"),
    relationship = "many-to-one"
  )

if (nrow(turnover_pairs) < 3) {
  stop("Not enough predecessor-successor pairs to plot.", call. = FALSE)
}

message(sprintf("Created %d predecessor-successor pairs with both scores", nrow(turnover_pairs)))
message(sprintf(
  "Predecessor-successor correlation: %.3f",
  cor(turnover_pairs$predecessor_score, turnover_pairs$successor_score)
))

plot <- ggplot(turnover_pairs, aes(x = predecessor_score, y = successor_score)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_smooth(method = "lm", se = TRUE, color = "#2171B5", alpha = 0.2) +
  geom_point(size = 3, alpha = 0.7, color = "#E41A1C") +
  labs(
    x = paste0("Predecessor ", score_name),
    y = paste0("Successor ", score_name),
    title = paste0(score_name, " Persistence Across Alderman Turnovers"),
    subtitle = NULL
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_blank()
  )

ggsave(
  sprintf("../output/predecessor_successor_scatter_uncertainty_%s.pdf", spec),
  plot,
  width = 6,
  height = 6
)
