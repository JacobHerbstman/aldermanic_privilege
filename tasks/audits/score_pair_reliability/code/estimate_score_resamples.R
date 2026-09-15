# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/score_pair_reliability/code")
# cutoff_year <- 2022
# start_year <- 2006
# bootstrap_draws <- 499
# split_draws <- 50
# seed <- 9142026
# workers <- 4
# stage1_controls <- "median_hh_income_10k + share_black + share_hisp + share_white + homeownership_rate + pop_total_10k + dist_cbd_km + dist_lake_km + n_rail_stations_800m + n_permits_wm_l1"
# stage1_fe <- "month + permit_type_clean + review_type_clean"
# stage2_control <- "n_permits_wm_l1"
# minimum_months <- 4

library(data.table)
library(fixest)
source("../../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 10L)
  cutoff_year <- as.integer(args[1])
  start_year <- as.integer(args[2])
  bootstrap_draws <- as.integer(args[3])
  split_draws <- as.integer(args[4])
  seed <- as.integer(args[5])
  workers <- as.integer(args[6])
  stage1_controls <- args[7]
  stage1_fe <- args[8]
  stage2_control <- args[9]
  minimum_months <- as.integer(args[10])
}
stopifnot(cutoff_year %in% c(2014L, 2022L), start_year == 2006L,
  bootstrap_draws > 1, split_draws > 1, workers >= 1, minimum_months == 4L)
setFixest_nthreads(1)
setDTthreads(1)

# Reconstruct the paper's observed workload before making samples smaller.
permits <- fread("../input/permits_for_uncertainty_index.csv", colClasses = c(id = "character", pin = "character"))
stopifnot(!anyDuplicated(permits$id))
permits[, month := as.Date(paste("01", month), format = "%d %b %Y")]
permits <- permits[year >= start_year & year <= cutoff_year]
keep <- permits[, .(months = uniqueN(month)), by = alderman][months >= minimum_months, alderman]
permits <- permits[alderman %in% keep]
counts <- permits[, .(n_permits_wm = .N), by = .(ward, month)]
workload <- merge(CJ(ward = sort(unique(permits$ward)),
  month = seq(min(permits$month), max(permits$month), by = "month")), counts,
  by = c("ward", "month"), all.x = TRUE, sort = TRUE)
workload[is.na(n_permits_wm), n_permits_wm := 0L]
workload[, n_permits_wm_l1 := shift(n_permits_wm), by = ward]
stopifnot(!anyDuplicated(workload[, .(ward, month)]))
permits <- merge(permits, workload, by = c("ward", "month"), all.x = TRUE, sort = FALSE)
permits[, `:=`(median_hh_income_10k = median_hh_income / 10000, pop_total_10k = pop_total / 10000)]
covariates <- strsplit(stage1_controls, " + ", fixed = TRUE)[[1]]
fe_columns <- strsplit(stage1_fe, " + ", fixed = TRUE)[[1]]
permits <- permits[Reduce(`&`, lapply(permits[, c("log_processing_time", covariates), with = FALSE], is.finite)) &
  complete.cases(permits[, ..fe_columns])]
setorder(permits, id)
permits[, quarter := paste0(year, "Q", (as.integer(format(month, "%m")) - 1L) %/% 3L + 1L)]
# This source records ten-digit parcel roots, sometimes several separated by " | ".
# Connect permits sharing ANY recorded parcel, including multi-parcel projects.
permits[, has_pin := !is.na(pin) & grepl("^[0-9]{10}( [|] [0-9]{10})*$", pin)]
parcel_lists <- strsplit(permits[has_pin == TRUE, pin], " | ", fixed = TRUE)
edges <- data.table(permit = rep(paste0("permit_", permits[has_pin == TRUE, id]), lengths(parcel_lists)),
  parcel = paste0("parcel_", unlist(parcel_lists)))
groups <- igraph::components(igraph::graph_from_data_frame(edges, directed = FALSE))$membership
permits[, property := paste0("permit_", id)]
permits[has_pin == TRUE, property := paste0("group_", groups[paste0("permit_", id)])]
stopifnot(!anyNA(permits$property))
stage1_formula <- as.formula(paste("log_processing_time ~", stage1_controls, "|", stage1_fe))
stage2_formula <- as.formula(paste("mean_resid_wm ~ 0 + i(alderman) +", stage2_control))

# This same estimator is used for the baseline, every reweighting, and both halves.
# All-one weights must reproduce both published scores before any experiment runs.
fit_score <- function(d, weights) {
  d <- copy(d)
  d[, draw_weight := weights]
  first <- feols(stage1_formula, d, weights = ~draw_weight, notes = FALSE, warn = FALSE)
  stopifnot(nobs(first) == nrow(d))
  d[, residual := resid(first)]
  wm <- d[, .(mean_resid_wm = weighted.mean(residual, draw_weight),
    weight_sum = sum(draw_weight), n_permits_wm_l1 = first(n_permits_wm_l1)),
    by = .(ward, month, alderman)]
  second <- feols(stage2_formula, wm, weights = ~weight_sum, vcov = "hetero", notes = FALSE, warn = FALSE)
  terms <- grep("^alderman::", names(coef(second)), value = TRUE)
  stopifnot(length(terms) == uniqueN(d$alderman))
  effects <- unname(coef(second)[terms])
  center <- diag(length(terms)) - 1 / length(terms)
  variance <- center %*% vcov(second)[terms, terms] %*% center
  effects <- effects - mean(effects)
  squared_se <- pmax(diag(variance), 0)
  tau2 <- max(0, var(effects) - mean(squared_se))
  shrunk <- effects * tau2 / (tau2 + squared_se)
  stopifnot(all(is.finite(shrunk)))
  score <- if (sd(shrunk) > 0) as.numeric(scale(shrunk)) else rep(0, length(shrunk))
  result <- data.table(alderman = sub("^alderman::", "", terms), score,
    raw_effect = effects, shrinkage = tau2 / (tau2 + squared_se))
  support <- d[, .(n_permits = .N, months = uniqueN(month), properties = uniqueN(property),
    years = uniqueN(year), pin_share = mean(has_pin)), by = alderman]
  merge(result, support, by = "alderman", sort = TRUE)
}

baseline <- fit_score(permits, rep(1, nrow(permits)))
published <- if (cutoff_year == 2022L) fread("../input/paper_scores.csv") else fread("../input/early_scores.csv")
stopifnot(!anyDuplicated(published$alderman), setequal(baseline$alderman, published$alderman))
baseline[, published_score := published$uncertainty_index[match(alderman, published$alderman)]]
stopifnot(max(abs(baseline$score - baseline$published_score)) < 1e-8)
cat("Baseline matches through", cutoff_year, ":", nrow(permits), "permits;", nrow(baseline), "aldermen.\n")

# Positive cluster weights preserve every alderman in every bootstrap draw.
# A city's whole quarter/year receives one common weight, preserving covariance across wards.
resamples <- list()
for (method in c("property", "quarter", "year")) {
  cluster <- match(permits[[method]], sort(unique(permits[[method]])))
  cluster_count <- max(cluster)
  for (first_draw in seq(1L, bootstrap_draws, by = 25L)) {
    draws <- first_draw:min(first_draw + 24L, bootstrap_draws)
    results <- parallel::mclapply(draws, function(draw) {
      set.seed(seed + cutoff_year * 1000L + match(method, c("property", "quarter", "year")) * 100000L + draw)
      weights <- rexp(cluster_count)[cluster]
      scores <- fit_score(permits, weights / mean(weights))
      scores[, `:=`(method = method, draw = draw, cluster_count = cluster_count)]
      scores[, .(method, draw, alderman, score, raw_effect, shrinkage, cluster_count)]
    }, mc.cores = workers, mc.set.seed = FALSE)
    stopifnot(all(vapply(results, is.data.table, logical(1))))
    resamples[[length(resamples) + 1L]] <- rbindlist(results)
    cat(cutoff_year, method, "draws completed:", max(draws), "\n")
  }
}
SaveData(rbindlist(resamples), c("method", "draw", "alderman"),
  sprintf("../output/resamples_through%d.parquet", cutoff_year))

# Disjoint observations are assigned once per split; both regressions are learned separately.
# Random quarters put two of each year's four quarters in each half citywide.
split_scores <- list()
for (method in c("random_properties", "random_quarters", "odd_even_years", "early_late")) {
  repetitions <- if (method %in% c("random_properties", "random_quarters")) split_draws else 1L
  for (first_draw in seq(1L, repetitions, by = 10L)) {
    draws <- first_draw:min(first_draw + 9L, repetitions)
    results <- parallel::mclapply(draws, function(draw) {
      set.seed(seed + cutoff_year * 1000L + match(method,
        c("random_properties", "random_quarters", "odd_even_years", "early_late")) * 10000L + draw)
      if (method == "random_properties") {
        properties <- sort(unique(permits$property))
        half <- sample(rep(1:2, length.out = length(properties)))[match(permits$property, properties)]
      }
      if (method == "random_quarters") {
        quarters <- unique(permits[, .(year, quarter)])[order(year, quarter)]
        quarters[, half := sample(rep(1:2, each = 2L)), by = year]
        half <- quarters$half[match(permits$quarter, quarters$quarter)]
      }
      if (method == "odd_even_years") half <- 1L + permits$year %% 2L
      if (method == "early_late") half <- 1L + as.integer(permits$year > floor((start_year + cutoff_year) / 2))
      stopifnot(length(half) == nrow(permits), all(half %in% 1:2))
      halves <- lapply(1:2, function(h) {
        d <- permits[half == h]
        eligible <- d[, .(months = uniqueN(month)), by = alderman][months >= minimum_months, alderman]
        d <- d[alderman %in% eligible]
        scores <- fit_score(d, rep(1, nrow(d)))
        scores[, `:=`(method = method, draw = draw, half = h)]
        scores
      })
      rbindlist(halves)
    }, mc.cores = workers, mc.set.seed = FALSE)
    stopifnot(all(vapply(results, is.data.table, logical(1))))
    split_scores[[length(split_scores) + 1L]] <- rbindlist(results)
    cat(cutoff_year, method, "splits completed:", max(draws), "\n")
  }
}
SaveData(rbindlist(split_scores), c("method", "draw", "half", "alderman"),
  sprintf("../output/splits_through%d.parquet", cutoff_year))
SaveData(baseline, "alderman", sprintf("../output/baseline_through%d.csv", cutoff_year))
