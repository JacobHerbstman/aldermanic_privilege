source("../../setup_environment/code/packages.R")
library(optparse)

# =======================================================================================
# Interactive diagnostics block (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_rezoning_strictness_scores/code")
# opt <- list(
#   input_rezoning = "../input/rezoning_dataset_usable_19990101_20260212.csv",
#   input_alderman_panel = "../input/chicago_alderman_panel.csv",
#   output_csv = "../output/alderman_rezoning_strictness_scores.csv",
#   output_plot = "../output/rezoning_strictness_index.pdf",
#   use_all_rezonings = "TRUE"
# )
# =======================================================================================

option_list <- list(
  make_option(
    "--input_rezoning",
    type = "character",
    default = "../input/rezoning_dataset_usable_19990101_20260212.csv"
  ),
  make_option(
    "--input_alderman_panel",
    type = "character",
    default = "../input/chicago_alderman_panel.csv"
  ),
  make_option(
    "--output_csv",
    type = "character",
    default = "../output/alderman_rezoning_strictness_scores.csv"
  ),
  make_option(
    "--output_plot",
    type = "character",
    default = "../output/rezoning_strictness_index.pdf"
  ),
  make_option(
    "--use_all_rezonings",
    type = "character",
    default = "TRUE"
  )
)
if (!exists("opt")) {
  opt <- parse_args(OptionParser(option_list = option_list))
}

parse_flag <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(default)
  }
  x_chr <- tolower(trimws(as.character(x[[1]])))
  if (x_chr %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (x_chr %in% c("false", "f", "0", "no", "n")) return(FALSE)
  default
}

parse_mixed_date_mdy_first <- function(x) {
  chars <- as.character(x)
  chars <- trimws(chars)
  chars[chars %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      chars,
      orders = c("mdy", "ymd", "dmy"),
      quiet = TRUE
    )
  )
  as.Date(parsed)
}

parse_mixed_date_ymd_first <- function(x) {
  chars <- as.character(x)
  chars <- trimws(chars)
  chars[chars %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  out <- suppressWarnings(as.Date(lubridate::ymd(chars, quiet = TRUE)))
  idx <- is.na(out) & !is.na(chars)
  if (any(idx)) {
    out[idx] <- suppressWarnings(as.Date(lubridate::mdy(chars[idx], quiet = TRUE)))
  }
  idx <- is.na(out) & !is.na(chars)
  if (any(idx)) {
    out[idx] <- suppressWarnings(as.Date(lubridate::dmy(chars[idx], quiet = TRUE)))
  }
  out
}

message("=== Build rezoning strictness scores (simple downzone-share EB) ===")

rezoning_raw <- read_csv(opt$input_rezoning, show_col_types = FALSE)

required_rezoning_cols <- c("assigned_alderman", "far_change", "matter_passed_date", "matter_intro_date")
missing_rez_cols <- setdiff(required_rezoning_cols, names(rezoning_raw))
if (length(missing_rez_cols) > 0) {
  stop("Missing required rezoning columns: ", paste(missing_rez_cols, collapse = ", "))
}

alderman_panel <- read_csv(opt$input_alderman_panel, show_col_types = FALSE) %>%
  mutate(
    alderman = str_squish(as.character(alderman))
  ) %>%
  filter(!is.na(alderman), alderman != "")

all_aldermen <- alderman_panel %>%
  distinct(alderman) %>%
  arrange(alderman) %>%
  pull(alderman)

name_aliases <- c(
  "Felix Cardona Jr" = "Felix Cardona Jr.",
  "Michael Scott Jr" = "Michael Scott Jr.",
  "Walter Burnett, Jr" = "Walter Burnett, Jr."
)

rezoning <- rezoning_raw %>%
  mutate(
    assigned_alderman = str_squish(as.character(assigned_alderman)),
    alderman = recode(assigned_alderman, !!!name_aliases),
    alderman = if_else(alderman %in% all_aldermen, alderman, NA_character_),
    far_change = suppressWarnings(as.numeric(far_change)),
    intro_date = parse_mixed_date_mdy_first(matter_intro_date),
    passed_date_summary = parse_mixed_date_ymd_first(matter_passed_date)
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

use_all_rezonings <- parse_flag(opt$use_all_rezonings, default = TRUE)
sample_scope <- if (use_all_rezonings) "all_rezonings" else "approved_only"
in_scope <- if (use_all_rezonings) {
  rep(TRUE, nrow(rezoning))
} else {
  !is.na(rezoning$passed_date_summary)
}
message("Score sample scope: ", sample_scope)

scope_stats <- rezoning %>%
  filter(in_scope, !is.na(alderman), !is.na(far_change)) %>%
  group_by(alderman) %>%
  summarise(
    n_rezonings = n(),
    n_downzones = sum(far_change < 0, na.rm = TRUE),
    downzone_share = n_downzones / n_rezonings,
    mean_far_change_raw = mean(far_change, na.rm = TRUE),
    .groups = "drop"
  )

if (nrow(scope_stats) == 0) {
  stop("No in-scope rezonings with non-missing alderman and far_change.")
}

global_downzone_share <- sum(scope_stats$n_downzones, na.rm = TRUE) / sum(scope_stats$n_rezonings, na.rm = TRUE)

# Jeffreys-style variance proxy avoids zero variance at 0% or 100% downzone share.
scope_stats <- scope_stats %>%
  mutate(
    p_tilde = (n_downzones + 0.5) / (n_rezonings + 1),
    se2 = p_tilde * (1 - p_tilde) / (n_rezonings + 1),
    alderman_se = sqrt(se2)
  )

signal_var <- var(scope_stats$downzone_share, na.rm = TRUE) - mean(scope_stats$se2, na.rm = TRUE)
if (!is.finite(signal_var) || signal_var <= 0) {
  signal_var <- 1e-8
}

scope_stats <- scope_stats %>%
  mutate(
    shrinkage_B = signal_var / (signal_var + se2),
    downzone_share_shrunk = shrinkage_B * downzone_share + (1 - shrinkage_B) * global_downzone_share,
    strictness_index = downzone_share_shrunk
  )

output <- tibble(alderman = all_aldermen) %>%
  left_join(scope_stats, by = "alderman") %>%
  mutate(
    sample_scope = sample_scope,
    n_rezonings = replace_na(n_rezonings, 0L),
    n_rezonings_all_approved = n_rezonings,
    downzone_share_all_approved = downzone_share,
    mean_far_change_raw_all_approved = mean_far_change_raw,
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

write_csv(output, opt$output_csv)

plot_df <- output %>%
  filter(!is.na(strictness_index)) %>%
  arrange(strictness_index) %>%
  mutate(alderman = factor(alderman, levels = alderman))

score_plot <- ggplot(plot_df, aes(x = strictness_index, y = alderman, fill = strictness_index)) +
  geom_col() +
  scale_fill_distiller(
    palette = "RdYlBu",
    direction = -1,
    name = "Strictness"
  ) +
  geom_vline(xintercept = global_downzone_share, linetype = "dashed", alpha = 0.7) +
  labs(
    title = "Alderman Strictness Index (Downzone Share, EB-Weighted)",
    subtitle = "Higher values indicate stricter behavior (higher downzone share), shrunk by rezoning count",
    x = "Strictness Index (= EB-shrunk downzone share)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40")
  )

ggsave(opt$output_plot, plot = score_plot, width = 9, height = 13, device = "pdf", bg = "white")

message("Citywide downzone share (scope): ", round(global_downzone_share, 4))
message("Signal variance for EB shrinkage: ", round(signal_var, 6))
message("Scores written: ", opt$output_csv)
message("Plot written: ", opt$output_plot)
message("=== Simple rezoning strictness score complete ===")
