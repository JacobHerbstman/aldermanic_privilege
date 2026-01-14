# bunching_border.R
# Bunching near ward borders (descriptive): mirrored histogram, counterfactual fits,
# excess/missing mass with shading, reallocation ratio, bootstrap CIs.
# Methodology mirrors Brueckner–Leather–Zerecero (2024) and standard bunching practice:
# fit smooth counterfactual outside donut; extrapolate into window; compute excess/missing mass. 
# Refs: JUE article + replication materials; World Bank bunching explainer. 
# (Brueckner et al. 2024; Chetty/Saez bunching). 

# ------------------------------
## run this line when editing code in Rstudio 
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# ------------------------------------------------------------------------------
# 1) Interactive arguments (comment out when using makefile)
# Usage: Rscript bunching_analysis.R <max_dist> <bin_width> <inner_window> <poly_degree> <boot_B> <plot_outfile>
# ------------------------------------------------------------------------------
# max_dist    <- 1500
# bin_width   <- 50
# inner_win   <- 200
# poly_degree <- 2
# boot_B      <- 250
# 
# set.seed(123)

# ------------------------------------------------------------------------------
# 1) ARGUMENTS (explicit, Makefile-style) comment out when runnign interactively. 
# Usage: Rscript bunching_analysis.R <max_dist> <bin_width> <inner_window> <poly_degree> <boot_B> <plot_outfile>
# ------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 6) {
  stop("Usage: Rscript bunching_analysis.R <max_dist> <bin_width> <inner_window> <poly_degree> <boot_B> <plot_outfile>", call. = FALSE)
}
max_dist    <- as.numeric(args[1])
bin_width   <- as.numeric(args[2])
inner_win   <- as.numeric(args[3])   # 0 => no donut
poly_degree <- as.integer(args[4])
boot_B      <- as.integer(args[5])
outfile     <- args[6]
set.seed(1234)


# ------------------------------
# 1) Load & prep
# ------------------------------
parcels <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>% 
  filter(arealotsf > 1) %>%
  filter(areabuilding > 1) %>%
  filter(unitscount > 1) %>% 
  filter(unitscount > 1 & unitscount < 10) 

req <- c("ward_pair","dist_to_boundary","strictness_own","strictness_neighbor","construction_year")
miss <- setdiff(req, names(parcels))
if (length(miss)) stop("Missing required columns: ", paste(miss, collapse=", "))

dat <- parcels %>%
  filter(!is.na(construction_year)) %>%
  mutate(
    is_lenient = strictness_own < strictness_neighbor,
    x_signed   = if_else(is_lenient, abs(dist_to_boundary), -abs(dist_to_boundary))
  ) %>%
  filter(between(x_signed, -max_dist, max_dist))

# ------------------------------
# 2) Binning 
# ------------------------------
breaks  <- seq(-max_dist, max_dist, by = bin_width)
centers <- head(breaks, -1) + bin_width/2

dat_binned <- dat %>%
  mutate(
    bin    = cut(x_signed, breaks = breaks, include.lowest = TRUE, right = FALSE),
    bin_id = as.integer(bin),
    x_c    = centers[bin_id]
  ) %>%
  filter(!is.na(bin_id))

binned <- dat_binned %>%
  count(is_lenient, bin_id, x_c, name = "n") %>%
  arrange(x_c, is_lenient)

bin_len <- binned %>% filter(is_lenient)
bin_str <- binned %>% filter(!is_lenient)

# ------------------------------
# 3) Counterfactual fit
# ------------------------------
fit_side <- function(df, deg, inner_win) {
  df_fit <- df %>% filter(abs(x_c) > inner_win)
  if (nrow(df_fit) < (deg + 2L)) return(NULL)
  lm(n ~ poly(x_c, deg, raw = TRUE), data = df_fit)
}

m_len <- fit_side(bin_len, poly_degree, inner_win)
m_str <- fit_side(bin_str, poly_degree, inner_win)

pred_len <- if (!is.null(m_len)) bin_len %>% mutate(nhat = as.numeric(predict(m_len, newdata = .))) else NULL
pred_str <- if (!is.null(m_str)) bin_str %>% mutate(nhat = as.numeric(predict(m_str, newdata = .))) else NULL

# mass-conservation scaling (outside the donut)
rescale_cf <- function(pred, inner_win) {
  out <- pred %>% filter(abs(x_c) > inner_win)
  s <- sum(out$n) / sum(pmax(out$nhat, 1e-8))
  pred %>% mutate(nhat = nhat * s)
}
pred_len <- rescale_cf(pred_len, inner_win)
pred_str <- rescale_cf(pred_str, inner_win)

# ------------------------------
# 4) Excess/missing in donut + x*
# ------------------------------
inside_len <- pred_len %>% filter(x_c >= 0, x_c <= inner_win)
inside_str <- pred_str %>% filter(x_c <= 0, x_c >= -inner_win)

excess_tbl  <- inside_len %>% transmute(x_c, contrib = pmax(n - nhat, 0))
missing_tbl <- inside_str %>% transmute(x_c, contrib = pmax(nhat - n, 0))

B_mass <- sum(excess_tbl$contrib)  * bin_width
M_mass <- sum(missing_tbl$contrib) * bin_width
rho    <- ifelse(M_mass > 0, B_mass / M_mass, NA_real_)

cum_excess  <- excess_tbl  %>% arrange(x_c)       %>% mutate(cA = cumsum(contrib) * bin_width)
cum_missing <- missing_tbl %>% arrange(desc(x_c)) %>% mutate(cA = cumsum(contrib) * bin_width) %>% arrange(x_c)

x_star <- NA_real_
if (nrow(cum_excess) > 0 && nrow(cum_missing) > 0) {
  K <- min(nrow(cum_excess), nrow(cum_missing))
  diffs <- abs(cum_excess$cA[seq_len(K)] - cum_missing$cA[seq_len(K)])
  x_star <- cum_excess$x_c[which.min(diffs)]
}

# ------------------------------
# 5) Block bootstrap (by ward_pair) — with same rescaling
# ------------------------------
panel <- dat %>%
  mutate(
    bin    = cut(x_signed, breaks = breaks, include.lowest = TRUE, right = FALSE),
    bin_id = as.integer(bin),
    x_c    = centers[bin_id]
  ) %>%
  filter(!is.na(bin_id)) %>%
  count(ward_pair, is_lenient, x_c, name = "n")

one_boot <- function() {
  sampled <- tibble(ward_pair = sample(unique(panel$ward_pair),
                                       size = length(unique(panel$ward_pair)),
                                       replace = TRUE)) %>%
    count(ward_pair, name = "w")
  agg <- panel %>%
    inner_join(sampled, by = "ward_pair") %>%
    mutate(n = n * w) %>%
    group_by(is_lenient, x_c) %>%
    summarise(n = sum(n), .groups="drop")
  
  L <- agg %>% filter(is_lenient)
  S <- agg %>% filter(!is_lenient)
  
  mL <- fit_side(L, poly_degree, inner_win)
  mS <- fit_side(S, poly_degree, inner_win)
  if (is.null(mL) || is.null(mS)) return(c(B=NA, M=NA, rho=NA, xstar=NA))
  
  Lp <- L %>% mutate(nhat = as.numeric(predict(mL, newdata = .))) %>% rescale_cf(inner_win)
  Sp <- S %>% mutate(nhat = as.numeric(predict(mS, newdata = .))) %>% rescale_cf(inner_win)
  
  inL <- Lp %>% filter(x_c >= 0, x_c <= inner_win)
  inS <- Sp %>% filter(x_c <= 0, x_c >= -inner_win)
  
  B  <- sum(pmax(inL$n - inL$nhat, 0)) * bin_width
  M  <- sum(pmax(inS$nhat - inS$n, 0)) * bin_width
  r  <- ifelse(M > 0, B/M, NA_real_)
  
  cEx <- inL %>% transmute(x_c, contrib = pmax(n - nhat, 0)) %>%
    arrange(x_c) %>% mutate(cA = cumsum(contrib) * bin_width)
  cMi <- inS %>% transmute(x_c, contrib = pmax(nhat - n, 0)) %>%
    arrange(desc(x_c)) %>% mutate(cA = cumsum(contrib) * bin_width) %>% arrange(x_c)
  
  xs <- NA_real_
  if (nrow(cEx) > 0 && nrow(cMi) > 0) {
    K  <- min(nrow(cEx), nrow(cMi))
    ii <- which.min(abs(cEx$cA[seq_len(K)] - cMi$cA[seq_len(K)]))
    xs <- cEx$x_c[ii]
  }
  c(B=B, M=M, rho=r, xstar=xs)
}

boot_mat <- replicate(boot_B, one_boot())
q2 <- function(v) quantile(v, c(0.025,0.975), na.rm = TRUE)
ci_B   <- q2(boot_mat["B",])
ci_M   <- q2(boot_mat["M",])
ci_rho <- q2(boot_mat["rho",])
ci_xs  <- q2(boot_mat["xstar",])

# ------------------------------
# 6) Plot
# ------------------------------
shade_B <- inside_len %>%
  mutate(ymin = pmin(n, nhat), ymax = pmax(n, nhat)) %>%
  transmute(xmin = x_c - bin_width/2, xmax = x_c + bin_width/2, ymin, ymax)

shade_M <- inside_str %>%
  mutate(ymin = pmin(n, nhat), ymax = pmax(n, nhat)) %>%
  transmute(xmin = x_c - bin_width/2, xmax = x_c + bin_width/2, ymin, ymax)

fmt <- function(x) formatC(x, digits = 2, format = "f")
cap <- paste0(
  "Excess Projects (B) = ", fmt(B_mass/bin_width), "  |  ",
  "Missing Projects (M) = ", fmt(M_mass/bin_width), "  |  ",
  "Reallocation rho (B/M) = ", fmt(rho)
)

lab_side <- c(`TRUE` = "Lenient side (+)", `FALSE` = "Strict side (−)")
col_side <- c(`TRUE`="#2b8cbe", `FALSE`="#cb181d")

p <- ggplot() +
  annotate("rect", xmin=-inner_win, xmax=inner_win, ymin=-Inf, ymax=Inf, alpha=0.06, fill="grey50") +
  geom_col(data = binned, aes(x = x_c, y = n, fill = is_lenient),
           width = bin_width, alpha = 0.85, color="white", size=0.1) +
  { if (!is.null(m_len)) geom_line(data = pred_len %>% filter(abs(x_c) > inner_win),
                                   aes(x = x_c, y = nhat, color = is_lenient), linewidth = 1.0) } +
  { if (!is.null(m_str)) geom_line(data = pred_str %>% filter(abs(x_c) > inner_win),
                                   aes(x = x_c, y = nhat, color = is_lenient), linewidth = 1.0) } +
  { if (!is.null(m_len)) geom_line(data = pred_len %>% filter(abs(x_c) <= inner_win),
                                   aes(x = x_c, y = nhat, color = is_lenient), linewidth = 1.0, linetype="longdash") } +
  { if (!is.null(m_str)) geom_line(data = pred_str %>% filter(abs(x_c) <= inner_win),
                                   aes(x = x_c, y = nhat, color = is_lenient), linewidth = 1.0, linetype="longdash") } +
  geom_rect(data = shade_B, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#2b8cbe", alpha=0.20) +
  geom_rect(data = shade_M, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#cb181d", alpha=0.20) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7) +
  scale_fill_manual(values = col_side, labels = lab_side, name = NULL) +
  scale_color_manual(values = col_side, labels = lab_side, name = NULL) +
  scale_x_continuous(breaks = pretty(c(-max_dist, max_dist), n=7), limits = c(-max_dist, max_dist)) +
  labs(
    title    = "Bunching at Ward Borders",
    subtitle =  paste("Bandwidth=", max_dist, ", Bin Size=", bin_width, ", Degree=", poly_degree,
    if (inner_win > 0) paste0(" | Donut=", inner_win) else ""),
    x        = "Signed distance to border (feet; + = lenient side)",
    y        = "Project starts per 100-ft bin",
    caption  = cap
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face="bold"),
        plot.caption = element_text(size=9, color="grey40"), panel.grid= element_blank())

p


ggsave(outfile, plot = p, width = 12, height = 7.5, dpi = 320, bg = "white")

# ------------------------------
# 7) Save a tidy summary alongside the plot
# ------------------------------
sum_path <- sub("\\.png$", ".csv", outfile)
summary_out <- tibble(
  max_dist = max_dist, bin_width = bin_width, inner_window = inner_win, poly_degree = poly_degree, boot_B = boot_B,
  B_mass = B_mass, M_mass = M_mass, rho = rho, x_star = x_star,
  ci_B_l = as.numeric(ci_B[1]), ci_B_u = as.numeric(ci_B[2]),
  ci_M_l = as.numeric(ci_M[1]), ci_M_u = as.numeric(ci_M[2]),
  ci_rho_l = as.numeric(ci_rho[1]), ci_rho_u = as.numeric(ci_rho[2]),
  ci_xs_l = as.numeric(ci_xs[1]),  ci_xs_u = as.numeric(ci_xs[2]),
  n_obs = nrow(dat),
  n_bins = length(centers)
)
# write_csv(summary_out, sum_path)

cat("✓ Wrote plot: ", outfile, "\n", sep = "")
cat("✓ Wrote summary: ", sum_path, "\n", sep = "")
