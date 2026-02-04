source("../../setup_environment/code/packages.R")

meta_files <- list.files("../output", pattern = "_bins_meta\\.csv$", full.names = TRUE)
if (length(meta_files) == 0) {
  stop("No *_bins_meta.csv files found in ../output.", call. = FALSE)
}

meta <- bind_rows(lapply(meta_files, read_csv, show_col_types = FALSE)) %>%
  mutate(
    usable_visual = cutoff_bin_present &
      n_bins_estimated >= 8 &
      (is.na(drop_frac) | drop_frac <= 0.30),
    rd_sign = case_when(
      is.na(rd_estimate) ~ "na",
      rd_estimate > 0 ~ "positive",
      rd_estimate < 0 ~ "negative",
      TRUE ~ "zero"
    )
  ) %>%
  arrange(yvar, desc(use_log), bw_ft, bin_mode)

comparison_out <- "../output/nonparametric_spec_comparison.csv"
selection_out <- "../output/nonparametric_spec_selection.csv"
memo_out <- "../output/nonparametric_spec_memo.md"

write_csv(meta, comparison_out)

pick_spec <- function(df) {
  if (nrow(df) == 0) return(df)
  df %>%
    mutate(
      priority_usable = if_else(usable_visual, 0L, 1L),
      priority_fe = if_else(fe_used == "pair_x_year", 0L, 1L),
      priority_has_p = if_else(is.na(rd_p), 1L, 0L),
      abs_t = abs(rd_estimate / rd_se)
    ) %>%
    arrange(priority_usable, priority_fe, priority_has_p, rd_p, desc(abs_t)) %>%
    slice_head(n = 1)
}

sel_far_log <- meta %>%
  filter(yvar == "density_far", use_log) %>%
  group_by(bw_ft) %>%
  group_modify(~pick_spec(.x)) %>%
  ungroup() %>%
  mutate(target = "far_log")

sel_far_levels <- meta %>%
  filter(yvar == "density_far", !use_log) %>%
  group_by(bw_ft) %>%
  group_modify(~pick_spec(.x)) %>%
  ungroup() %>%
  mutate(target = "far_levels")

dupac_ref <- meta %>%
  filter(yvar == "density_dupac", use_log) %>%
  select(
    bw_ft,
    bin_mode,
    fe_join = fe_used,
    dupac_rd_estimate = rd_estimate,
    dupac_rd_p = rd_p,
    dupac_fe_used = fe_used
  )

selection <- bind_rows(sel_far_log, sel_far_levels) %>%
  mutate(fe_join = fe_used) %>%
  left_join(dupac_ref, by = c("bw_ft", "bin_mode", "fe_join")) %>%
  select(-fe_join) %>%
  mutate(
    dupac_sign_align = case_when(
      is.na(rd_estimate) | is.na(dupac_rd_estimate) ~ NA,
      sign(rd_estimate) == sign(dupac_rd_estimate) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  arrange(target, bw_ft)

write_csv(selection, selection_out)

far_log_stats <- meta %>%
  filter(yvar == "density_far", use_log) %>%
  summarise(
    n_specs = n(),
    share_pair_x_year_used = mean(fe_used == "pair_x_year", na.rm = TRUE),
    share_usable = mean(usable_visual, na.rm = TRUE)
  )

main_pref <- selection %>%
  filter(target == "far_log", bw_ft == 500) %>%
  slice_head(n = 1)
if (nrow(main_pref) == 0) {
  main_pref <- selection %>% filter(target == "far_log") %>% slice_head(n = 1)
}

memo_lines <- c(
  "# Nonparametric FAR Spec Memo",
  "",
  sprintf("- Meta specs evaluated: %d", nrow(meta)),
  sprintf("- FAR log specs: %d", sum(meta$yvar == "density_far" & meta$use_log)),
  sprintf("- FAR log share with FE used = pair_x_year: %.2f", far_log_stats$share_pair_x_year_used),
  sprintf("- FAR log share passing visual usability rule: %.2f", far_log_stats$share_usable),
  "",
  "## Selected FAR log specs by bandwidth",
  ""
)

if (nrow(sel_far_log) > 0) {
  sel_lines <- sel_far_log %>%
    transmute(
      line = sprintf(
        "- bw=%d: mode=%s, FE used=%s, fallback=%s, rd_est=%.3f, rd_p=%.3f, usable=%s",
        bw_ft, bin_mode, fe_used, fallback_used, rd_estimate, rd_p, usable_visual
      )
    ) %>%
    pull(line)
  memo_lines <- c(memo_lines, sel_lines, "")
}

if (nrow(main_pref) > 0) {
  memo_lines <- c(
    memo_lines,
    "## Preferred main FAR visual",
    sprintf(
      "- Use bw=%d, mode=%s, FE used=%s (fallback=%s).",
      main_pref$bw_ft,
      main_pref$bin_mode,
      main_pref$fe_used,
      main_pref$fallback_used
    ),
    sprintf("- Estimated cutoff jump (selected): %.3f (p=%.3f).", main_pref$rd_estimate, main_pref$rd_p),
    sprintf(
      "- DUPAC sign alignment at same bw/mode: %s (dupac rd=%.3f, p=%.3f).",
      main_pref$dupac_sign_align,
      main_pref$dupac_rd_estimate,
      main_pref$dupac_rd_p
    )
  )
}

writeLines(memo_lines, memo_out)

message("Saved:")
message(sprintf("  - %s", comparison_out))
message(sprintf("  - %s", selection_out))
message(sprintf("  - %s", memo_out))
