# nonparametric_RD.R
# Stacked nonparametric RD (Eq. 9) with boundary FE and binned distance.
# Produces: (1) CSV of bin coefficients (clustered + robust SEs)
#           (2) PNG plot mimicking Kulka (2022) nonparametric panels.

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")


# --- 1. ARGUMENT HANDLING ---
# =======================================================================================
# --- Interactive Test Block (uncomment to run in RStudio) ---
yvar            <- "density_dupac"
use_log         <- F
bw              <- 1056
bins            <- bw/10
# =======================================================================================
# --- Command-Line Arguments (uncomment for Makefile) ---
args <- commandArgs(trailingOnly = TRUE)
# if (length(args) != 3) {
#   stop("FATAL: need 3 args: <yvar> <use_log> <window_miles> <bin_miles>", call. = FALSE)
# }
# yvar            <- args[1]
# use_log         <- as.logical(args[2])
# bw              <- as.numeric(args[3])
# bins            <- bw / 10
# 
# bw_mi   <- bw   / 5280
# bins_mi <- bins / 5280
# cat(sprintf("→ Nonparametric stacked RD | y=%s | log=%s | bw=%.0fft (%.2f mi) | bin=%.1fft (%.2f mi)\n",
#             yvar, as.character(use_log), bw, bw_mi, bins, bins_mi))

# ------------------------- 2) LOAD -------------------------
cat("Loading data...\n")
df <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>% 
  mutate(homeownership_own = homeownership_own*100) %>% 
  filter(arealotsf > 1) %>% 
  # filter(unitscount > 1) %>% 
  filter(construction_year > 2006)

  
  # --- Sample restriction helper: keep modal zone that exists on both sides within the bw ---
  restrict_to_modal_zone <- function(df, bw) {
    # 1) limit to current bandwidth + non-missing zone
    df_bw <- df %>%
      filter(dist_to_boundary <= bw)
    
    # 2) grouping keys: use (boundary_year, ward_pair) when available, else ward_pair
    group_keys <- intersect(c("boundary_year", "ward_pair"), names(df_bw))
    
    # 3) count parcels by zone within each group, requiring presence on BOTH sides
    #    prefer signed distance if available; else fall back to ward on each side
    zone_counts <-
      if ("signed_distance" %in% names(df_bw)) {
        df_bw %>%
          group_by(across(all_of(c(group_keys, "zone_code")))) %>%
          summarise(n = n(),
                    n_sides = n_distinct(sign(signed_distance)),
                    .groups = "drop") %>%
          filter(n_sides == 2)
      } else if ("ward" %in% names(df_bw)) {
        df_bw %>%
          group_by(across(all_of(c(group_keys, "zone_code")))) %>%
          summarise(n = n(),
                    n_sides = n_distinct(ward),
                    .groups = "drop") %>%
          filter(n_sides == 2)
      } else {
        # if neither side indicator is present, just compute modal zone
        df_bw %>%
          group_by(across(all_of(c(group_keys, "zone_code")))) %>%
          summarise(n = n(), .groups = "drop")
      }
    
    # 4) pick the modal zone (tie-breaker: alphabetical zone_code)
    modal_zone <- zone_counts %>%
      arrange(across(all_of(group_keys)), desc(n), zone_code) %>%
      group_by(across(all_of(group_keys))) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      select(all_of(group_keys), zone_code) %>%
      rename(modal_zone_code = zone_code)
    
    # 5) keep parcels in the modal zone for each group
    df_bw %>%
      inner_join(modal_zone, by = group_keys) %>%
      filter(zone_code == modal_zone_code) %>%
      select(-modal_zone_code)
  }
  
df <- restrict_to_modal_zone(df, bw)

# ------------------------- 3) OUTCOME ----------------------
if (use_log) {
  # df <- df %>% filter(unitscount > 0)
  df <- df %>% mutate(outcome = log(.data[[yvar]]))
} else {
  # df <- df %>% filter(unitscount > 0)
  df <- df %>% mutate(outcome = .data[[yvar]])
}

df <- df %>% filter(!is.na(outcome))


# Pretty label
pretty_y <- function(v, is_log) {
  lab <- switch(v,
                "density_dupac" = "Dwelling Units Per Acre (DUPAC)",
                "density_far"  = "Floor-Area Ratio (FAR)",
                "density_lapu" = "Lot Area Per Unit (LAPU)",
                "density_bcr"  = "Building Coverage Ratio (BCR)",
                "density_lps"  = "Lot Size Per Story (LPS)",
                v
  )
  if (is_log) paste0("Log(", lab, ")") else lab
}
y_axis_label <- pretty_y(yvar, isTRUE(use_log))

# ------------------------- 4) RUNNING VAR (FEET) ----------
# Window in feet
df <- df %>% filter(abs(signed_distance) <= bw)

# Minimal columns
keep_cols <- c("outcome", "signed_distance", "ward_pair", "construction_year", "ward")
df <- df[, intersect(names(df), keep_cols)]

N_full <- nrow(df)
Ey     <- mean(df$outcome, na.rm = TRUE)

# ------------------------- 5) BINS (FEET) ------------------
df <- df %>%
  mutate(
    bindex      = floor(signed_distance / bins),
    bin_center_ft = (bindex + 0.5) * bins,
    bin_center_mi = bin_center_ft / 5280,
    on_right    = bindex >= 0,
    on_left     = bindex <  0
  )

# Reference bin = [-bins,0) (less-restrictive side nearest zero) ⇒ index -1
ref_bin <- -1L

# ------------------------- 6) FIXED EFFECTS ----------------
fe_part <- "ward_pair"
if ("construction_year" %in% names(df)) fe_part <- paste(fe_part, "construction_year", sep = "^")

# ------------------------- 7) ESTIMATION -------------------
fml <- as.formula(paste0("outcome ~ i(bindex, ref = ", ref_bin, ") | ", fe_part))

cat("Estimating (clustered by ward_pair)...\n")
est_clu <- feols(fml, data = df, cluster = ~ ward_pair)
etable(est_clu)

# cat("Estimating (heteroskedastic-robust)...\n")
est_rob <- feols(fml, data = df, vcov = "hetero")

tidy_extract <- function(est, se_label) {
  broom::tidy(est) %>%
    dplyr::filter(str_starts(term, "bindex::")) %>%
    dplyr::mutate(bin = suppressWarnings(as.integer(str_remove(term, "bindex::"))),
                  {{se_label}} := std.error) %>%
    dplyr::select(bin, estimate, {{se_label}})
}

bins_clu <- tidy_extract(est_clu, se_clu)
bins_rob <- tidy_extract(est_rob, se_rob)

bins_all <- bins_clu %>%
  dplyr::rename(coef_clu = estimate) %>%
  dplyr::full_join(bins_rob %>% dplyr::rename(coef_rob = estimate), by = "bin") %>%
  dplyr::mutate(
    bin_center_ft = (bin + 0.5) * bins,
    bin_center_mi = bin_center_ft / 5280,
    on_right = bin >= 0, on_left = bin < 0,
    lo95 = coef_clu - 1.96 * se_clu,
    hi95 = coef_clu + 1.96 * se_clu
  ) %>%
  dplyr::arrange(bin_center_ft)

# RD estimate = bin adjacent to zero on STRICT side (RIGHT) ⇒ bin == 0
rd_row   <- bins_all %>% dplyr::filter(bin == 0L) %>% dplyr::slice(1)
rd_tau   <- if (nrow(rd_row)) rd_row$coef_clu else NA_real_
rd_seclu <- if (nrow(rd_row)) rd_row$se_clu   else NA_real_
rd_serob <- if (nrow(rd_row)) rd_row$se_rob   else NA_real_

# --- exact p-value for the RD bin (clustered model) and stars ---
tt_clu <- broom::tidy(est_clu)
rd_p_exact <- tt_clu %>%
  dplyr::filter(term == "bindex::0") %>%
  dplyr::pull(p.value) %>%
  { if (length(.) == 0) NA_real_ else .[1] }

stars <- if (is.na(rd_p_exact)) "" else
  if (rd_p_exact <= 0.01) "***" else
    if (rd_p_exact <= 0.05) "**"  else
      if (rd_p_exact <= 0.10) "*"   else ""

# ------------------------- 8) SAVE COEFS -------------------
# coef_outfile <- paste0(outprefix, "_bins.csv")
# readr::write_csv(bins_all, coef_outfile)
# cat("✓ Bin coefficient table:", coef_outfile, "\n")

# ------------------------- 9) PLOT -------------------------
bins_L <- bins_all %>% dplyr::filter(on_left)
bins_R <- bins_all %>% dplyr::filter(on_right)

# Colors to match line & ribbon
col_left  <- "#1f77b4"  # blue
col_right <- "#d62728"  # red

subtitle_txt <- sprintf(
  "RD est (clustered) = %.3f%s (SE %.3f) |  N = %s, E(y) = %.3f |  bw=%.2f mi, bin=%.2f mi",
  rd_tau, stars, rd_seclu, N_full, Ey, bw/5280, bins/5280
)

p <- ggplot() +
  # ribbons (95% CI, colored to match the lines)
  geom_ribbon(data = bins_L, aes(x = bin_center_mi, ymin = lo95, ymax = hi95),
              fill = col_left, alpha = 0.25, color = NA) +
  geom_ribbon(data = bins_R, aes(x = bin_center_mi, ymin = lo95, ymax = hi95),
              fill = col_right, alpha = 0.25, color = NA) +
  # lines (clustered coefficients)
  geom_line(data = bins_L, aes(x = bin_center_mi, y = coef_clu),
            linewidth = 1, color = col_left) +
  geom_line(data = bins_R, aes(x = bin_center_mi, y = coef_clu),
            linewidth = 1, color = col_right) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title    = paste0("Nonparametric Stacked RD for ", y_axis_label),
    subtitle = subtitle_txt,
    x = "Distance to boundary (miles)",
    y = y_axis_label, 
    caption  = "Reference bin = [-bin, 0) (less-restrictive). Shaded = 95% CI (clustered by border)."
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.subtitle = element_text(size = 9)  # smaller subtitle
  )+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
p


### save plot 
log_prefix <- if (isTRUE(use_log)) "log_" else ""
outfile <- file.path("../output",
                     sprintf("nonparametric_rd_%s%s_bw%s.pdf", log_prefix, yvar, bw)
)

cowplot::save_plot(outfile, plot = p, base_width = 8.2, base_height = 6.0, dpi = 300, device = "pdf")
cat("✓ Plot saved to:", outfile, "\n")