source("../../setup_environment/code/packages.R")

parse_mixed_date <- function(x) {
  chars <- as.character(x)
  chars <- trimws(chars)
  chars[chars %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      chars,
      orders = c("ymd", "mdy", "dmy"),
      quiet = TRUE
    )
  )
  as.Date(parsed)
}

name_aliases <- c(
  "Felix Cardona Jr" = "Felix Cardona Jr.",
  "Michael Scott Jr" = "Michael Scott Jr.",
  "Walter Burnett, Jr" = "Walter Burnett, Jr."
)

rezoning <- read_csv(
  "../input/rezoning_dataset_usable_19990101_20260212.csv",
  show_col_types = FALSE
)

required_cols <- c("assigned_alderman", "far_change", "matter_intro_date", "matter_passed_date")
missing_cols <- setdiff(required_cols, names(rezoning))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  transmute(alderman = str_squish(as.character(alderman))) %>%
  filter(!is.na(alderman), alderman != "") %>%
  distinct()

all_aldermen <- alderman_panel %>%
  arrange(alderman) %>%
  pull(alderman)

rezoning <- rezoning %>%
  mutate(
    assigned_alderman = str_squish(as.character(assigned_alderman)),
    alderman = recode(assigned_alderman, !!!name_aliases),
    alderman = if_else(alderman %in% all_aldermen, alderman, NA_character_),
    far_change = suppressWarnings(as.numeric(far_change)),
    intro_date = parse_mixed_date(matter_intro_date),
    passed_date = parse_mixed_date(matter_passed_date)
  )

unmatched_assigned <- rezoning %>%
  filter(!is.na(assigned_alderman), assigned_alderman != "", is.na(alderman)) %>%
  count(assigned_alderman, sort = TRUE)

if (nrow(unmatched_assigned) > 0) {
  stop(
    "Assigned alderman names missing from panel after alias normalization: ",
    paste(unmatched_assigned$assigned_alderman, collapse = ", ")
  )
}

scope_all <- rezoning %>%
  filter(!is.na(alderman), !is.na(far_change))

scope_approved <- rezoning %>%
  filter(!is.na(alderman), !is.na(far_change), !is.na(passed_date))

all_stats <- scope_all %>%
  group_by(alderman) %>%
  summarise(
    n_rezonings = n(),
    n_downzones = sum(far_change < 0, na.rm = TRUE),
    downzone_share = n_downzones / n_rezonings,
    mean_far_change_raw = mean(far_change, na.rm = TRUE),
    .groups = "drop"
  )

if (nrow(all_stats) == 0) {
  stop("No all-status rezonings with non-missing alderman and far_change.")
}

approved_stats <- scope_approved %>%
  group_by(alderman) %>%
  summarise(
    n_rezonings_all_approved = n(),
    downzone_share_all_approved = mean(far_change < 0, na.rm = TRUE),
    mean_far_change_raw_all_approved = mean(far_change, na.rm = TRUE),
    .groups = "drop"
  )

city_downzone_share <- sum(all_stats$n_downzones, na.rm = TRUE) /
  sum(all_stats$n_rezonings, na.rm = TRUE)

all_stats <- all_stats %>%
  mutate(
    p_tilde = (n_downzones + 0.5) / (n_rezonings + 1),
    se2 = p_tilde * (1 - p_tilde) / (n_rezonings + 1),
    alderman_se = sqrt(se2)
  )

signal_var <- var(all_stats$downzone_share, na.rm = TRUE) -
  mean(all_stats$se2, na.rm = TRUE)
if (!is.finite(signal_var) || signal_var <= 0) {
  signal_var <- 1e-8
}

all_stats <- all_stats %>%
  mutate(
    shrinkage_B = signal_var / (signal_var + se2),
    downzone_share_shrunk = shrinkage_B * downzone_share +
      (1 - shrinkage_B) * city_downzone_share,
    strictness_index = downzone_share_shrunk
  )

output <- tibble(alderman = all_aldermen) %>%
  left_join(all_stats, by = "alderman") %>%
  left_join(approved_stats, by = "alderman") %>%
  mutate(
    sample_scope = "all_rezonings",
    n_rezonings = replace_na(n_rezonings, 0L),
    n_rezonings_all_approved = replace_na(n_rezonings_all_approved, 0L),
    alderman_fe_raw = downzone_share,
    alderman_fe_shrunk = downzone_share_shrunk
  ) %>%
  select(
    alderman,
    sample_scope,
    n_rezonings,
    downzone_share,
    mean_far_change_raw,
    alderman_fe_raw,
    alderman_se,
    shrinkage_B,
    alderman_fe_shrunk,
    strictness_index,
    n_rezonings_all_approved,
    downzone_share_all_approved,
    mean_far_change_raw_all_approved
  ) %>%
  arrange(desc(strictness_index), alderman)

write_csv(output, "../output/alderman_rezoning_strictness_scores.csv")

plot_df <- output %>%
  filter(!is.na(strictness_index)) %>%
  arrange(strictness_index) %>%
  mutate(alderman = factor(alderman, levels = alderman))

score_plot <- ggplot(plot_df, aes(x = strictness_index, y = alderman, fill = strictness_index)) +
  geom_col() +
  scale_fill_distiller(palette = "RdYlBu", direction = -1, name = "Strictness") +
  geom_vline(xintercept = city_downzone_share, linetype = "dashed", alpha = 0.7) +
  labs(
    title = "Alderman Strictness Index (Downzone Share, EB-Weighted)",
    subtitle = "Higher values indicate stricter behavior",
    x = "Strictness Index (= EB-shrunk downzone share)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave("../output/rezoning_strictness_index.pdf", plot = score_plot, width = 9, height = 13, device = "pdf", bg = "white")

coverage <- mean(!is.na(output$strictness_index))
ranked <- output %>%
  filter(!is.na(strictness_index)) %>%
  arrange(desc(strictness_index)) %>%
  mutate(rank = row_number())

top_five <- ranked %>% slice_head(n = 5) %>% mutate(rank_group = "top")
bottom_five <- ranked %>% slice_tail(n = 5) %>% mutate(rank_group = "bottom")

qc_scalar <- tibble(
  check = c(
    "scope_all_rows",
    "scope_approved_rows",
    "unmatched_assigned_names",
    "panel_aldermen",
    "scored_aldermen",
    "strictness_non_missing_coverage",
    "city_downzone_share_all"
  ),
  value_numeric = c(
    nrow(scope_all),
    nrow(scope_approved),
    nrow(unmatched_assigned),
    length(all_aldermen),
    sum(!is.na(output$strictness_index)),
    coverage,
    city_downzone_share
  ),
  value_text = c(
    "rows with alderman and far_change",
    "rows with alderman, far_change, and passed_date",
    "assigned_alderman names unmatched to panel after alias mapping",
    "unique aldermen in panel",
    "aldermen with strictness score",
    "share of panel aldermen with non-missing strictness",
    "citywide downzone share in all-status scope"
  ),
  alderman = NA_character_,
  n_rezonings = NA_real_,
  strictness_index = NA_real_,
  rank = NA_real_,
  rank_group = NA_character_
)

qc_rank <- bind_rows(top_five, bottom_five) %>%
  transmute(
    check = "strictness_rank",
    value_numeric = as.numeric(rank),
    value_text = paste0(rank_group, "_", rank),
    alderman,
    n_rezonings,
    strictness_index,
    rank = as.numeric(rank),
    rank_group
  )

qc <- bind_rows(qc_scalar, qc_rank)
write_csv(qc, "../output/strictness_qc.csv")

qc_lines <- c(
  "strictness qc summary",
  paste0("scope_all_rows: ", nrow(scope_all)),
  paste0("scope_approved_rows: ", nrow(scope_approved)),
  paste0("unmatched_assigned_names: ", nrow(unmatched_assigned)),
  paste0("panel_aldermen: ", length(all_aldermen)),
  paste0("scored_aldermen: ", sum(!is.na(output$strictness_index))),
  paste0("strictness_non_missing_coverage: ", round(coverage, 4)),
  paste0("city_downzone_share_all: ", round(city_downzone_share, 4)),
  "top_5_strictness:",
  paste0("  ", top_five$rank, ". ", top_five$alderman, " | n=", top_five$n_rezonings, " | s=", round(top_five$strictness_index, 4)),
  "bottom_5_strictness:",
  paste0("  ", bottom_five$rank, ". ", bottom_five$alderman, " | n=", bottom_five$n_rezonings, " | s=", round(bottom_five$strictness_index, 4))
)
writeLines(qc_lines, "../output/strictness_qc.txt")

message("Wrote ../output/alderman_rezoning_strictness_scores.csv")
message("Wrote ../output/rezoning_strictness_index.pdf")
message("Wrote ../output/strictness_qc.csv")
message("Wrote ../output/strictness_qc.txt")
