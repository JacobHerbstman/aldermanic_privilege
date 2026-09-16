# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/score_robustness_review/code")
# start_year <- 2006
# cutoff_years <- c(2014, 2022)
# minimum_months <- 4
# stage1_controls <- "median_hh_income_10k + share_black + share_hisp + share_white + homeownership_rate + pop_total_10k + dist_cbd_km + dist_lake_km + n_rail_stations_800m + n_permits_wm_l1"
# stage1_fe <- "month + permit_type_clean + review_type_clean"
# raw_measures <- c("mean_days", "median_days", "mean_log_days")
library(data.table)
library(ggplot2)
source("../../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 6L)
  start_year <- as.integer(args[1])
  cutoff_years <- as.integer(strsplit(args[2], ",", fixed = TRUE)[[1]])
  minimum_months <- as.integer(args[3])
  stage1_controls <- args[4]
  stage1_fe <- args[5]
  raw_measures <- strsplit(args[6], ",", fixed = TRUE)[[1]]
}
stopifnot(start_year == 2006L, setequal(cutoff_years, c(2014L, 2022L)), minimum_months == 4L,
  all(raw_measures %in% c("mean_days", "median_days", "mean_log_days")))

source_permits <- fread("../input/permits_for_uncertainty_index.csv", colClasses = c(id = "character", pin = "character"))
stopifnot(!anyDuplicated(source_permits$id))
source_permits[, month := as.Date(paste("01", month), format = "%d %b %Y")]
local_pairs <- fread("../input/comparison_pairs.csv")
stopifnot(!anyDuplicated(local_pairs[, .(market, cutoff, pair)]))
covariates <- strsplit(stage1_controls, " + ", fixed = TRUE)[[1]]
fe_columns <- strsplit(stage1_fe, " + ", fixed = TRUE)[[1]]
aldermen <- list(); comparisons <- list(); correlations <- list()
for (cutoff_year in cutoff_years) {
  # Reproduce the score's permit eligibility, including the recorded workload filter.
  # These covariates select the SAME permits; raw averages do not adjust for them.
  permits <- copy(source_permits[year >= start_year & year <= cutoff_year])
  keep <- permits[, .(months = uniqueN(month)), by = alderman][months >= minimum_months, alderman]
  permits <- permits[alderman %in% keep]
  counts <- permits[, .(n_permits_wm = .N), by = .(ward, month)]
  workload <- merge(CJ(ward = sort(unique(permits$ward)), month = seq(min(permits$month), max(permits$month), by = "month")),
    counts, by = c("ward", "month"), all.x = TRUE, sort = TRUE)
  workload[is.na(n_permits_wm), n_permits_wm := 0L]
  workload[, n_permits_wm_l1 := shift(n_permits_wm), by = ward]
  stopifnot(!anyDuplicated(workload[, .(ward, month)]))
  permits <- merge(permits, workload, by = c("ward", "month"), all.x = TRUE, sort = FALSE)
  permits[, `:=`(median_hh_income_10k = median_hh_income / 10000, pop_total_10k = pop_total / 10000)]
  permits <- permits[Reduce(`&`, lapply(permits[, c("log_processing_time", covariates), with = FALSE], is.finite)) &
    complete.cases(permits[, ..fe_columns])]
  stopifnot(all(is.finite(permits$processing_time)), all(permits$processing_time > 0),
    max(abs(log(permits$processing_time) - permits$log_processing_time)) < 1e-8)
  setorder(permits, id)

  # Each permit receives equal weight; arithmetic means are the primary comparison.
  a <- permits[, .(n_permits = .N, months = uniqueN(month), first_year = min(year), last_year = max(year),
    wards = paste(sort(unique(ward)), collapse = "; "), mean_days = mean(processing_time),
    median_days = as.numeric(median(processing_time)), mean_log_days = mean(log_processing_time)), by = alderman]
  baseline <- fread(sprintf("../input/baseline_through%d.csv", cutoff_year))
  stopifnot(!anyDuplicated(baseline$alderman), setequal(a$alderman, baseline$alderman))
  index <- match(a$alderman, baseline$alderman)
  stopifnot(identical(a$n_permits, baseline$n_permits[index]), identical(a$months, baseline$months[index]))
  a[, `:=`(cutoff = cutoff_year, score = baseline$score[index], geometric_mean_days = exp(mean_log_days))]
  aldermen[[as.character(cutoff_year)]] <- a
  cat("Matched score sample through", cutoff_year, ":", sum(a$n_permits), "permits;", nrow(a), "aldermen.\n")

  # Keep every possible citywide comparison separate from the actual local pairs.
  combinations <- combn(sort(a$alderman), 2)
  citywide <- data.table(alderman_a = combinations[1, ], alderman_b = combinations[2, ])
  citywide[, `:=`(pair = paste(alderman_a, alderman_b, sep = " / "), market = "all_citywide_pairs", observations = 1L, cutoff = cutoff_year)]
  p <- rbind(local_pairs[cutoff == cutoff_year], citywide, use.names = TRUE)
  stopifnot(!anyDuplicated(p[, .(market, pair)]))
  ia <- match(p$alderman_a, a$alderman); ib <- match(p$alderman_b, a$alderman)
  stopifnot(!anyNA(ia), !anyNA(ib))
  p[, `:=`(score_a = a$score[ia], score_b = a$score[ib], permits_a = a$n_permits[ia], permits_b = a$n_permits[ib])]
  stopifnot(all(p$score_a != p$score_b))
  p[, score_slower := fifelse(score_a > score_b, alderman_a, alderman_b)]
  for (metric in raw_measures) {
    q <- copy(p)
    q[, `:=`(measure = metric, raw_a = a[[metric]][ia], raw_b = a[[metric]][ib])]
    q[, agreement := fcase(raw_a == raw_b, "tie", (raw_a - raw_b) * (score_a - score_b) > 0, "agree", default = "reverse")]
    comparisons[[length(comparisons) + 1L]] <- q
    correlations[[length(correlations) + 1L]] <- data.table(cutoff = cutoff_year, measure = metric, aldermen = nrow(a), permits = sum(a$n_permits),
      pearson = cor(a[[metric]], a$score), spearman = cor(a[[metric]], a$score, method = "spearman"))
  }
}
aldermen <- rbindlist(aldermen)
comparisons <- rbindlist(comparisons)
agreement <- comparisons[, .(pairs = .N, agree_pairs = sum(agreement == "agree"), reverse_pairs = sum(agreement == "reverse"),
  tied_pairs = sum(agreement == "tie"), pair_agreement = mean(agreement == "agree"), observations = sum(observations),
  agree_observations = sum(observations[agreement == "agree"]), reverse_observations = sum(observations[agreement == "reverse"]),
  tied_observations = sum(observations[agreement == "tie"]), observation_agreement = weighted.mean(agreement == "agree", observations)),
  by = .(cutoff, market, measure)]
stopifnot(all(agreement$pairs == agreement$agree_pairs + agreement$reverse_pairs + agreement$tied_pairs),
  all(agreement$observations == agreement$agree_observations + agreement$reverse_observations + agreement$tied_observations))
SaveData(aldermen, c("cutoff", "alderman"), "../output/raw_processing_by_alderman.csv")
SaveData(comparisons, c("cutoff", "market", "measure", "pair"), "../output/raw_processing_pairs.csv")
SaveData(agreement, c("cutoff", "market", "measure"), "../output/raw_processing_agreement.csv")
SaveData(rbindlist(correlations), c("cutoff", "measure"), "../output/raw_processing_correlations.csv")

aldermen[, period := paste0(start_year, "–", cutoff)]
plot <- ggplot(aldermen, aes(mean_days, score)) + geom_point(color = "#23617d", alpha = .75, size = 1.8) +
  facet_wrap(~period, ncol = 2) + labs(title = "Average raw processing time and the adjusted stringency score",
    subtitle = "One point per alderman; the two measures use exactly the same eligible permits.",
    x = "Average application-to-issuance time (days)", y = "Stringency score (standard deviations)",
    caption = "Raw means weight every permit equally. This comparison describes what the adjustment changes; it is not independent validation of the score.") +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), plot.caption = element_text(size = 8))
ggsave("../output/raw_processing_comparison.pdf", plot, width = 11, height = 4.5, bg = "white")
ggsave("../output/raw_processing_comparison.png", plot, width = 11, height = 4.5, dpi = 150, bg = "white")
