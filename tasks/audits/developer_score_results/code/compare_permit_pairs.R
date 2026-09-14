# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/developer_score_results/code")
# permit_fe <- "block_id + ward_pair_id^year"
# permit_cluster <- "ward_pair_id"
library(data.table)
library(fixest)
source("../../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 2L)
  permit_fe <- args[1]
  permit_cluster <- args[2]
}
setFixest_nthreads(1)

# Read exactly the panel used above; sample and donation decisions have one owner.
data <- as.data.table(arrow::read_parquet("../output/permit_comparison_panel.parquet"))
scores <- fread("../output/developer_scores.csv")[period == "pre_remap"]
benchmark <- fread("../output/developer_estimates.csv")[market == "permits" & specification == "signed"]
stopifnot(!anyDuplicated(data[, .(block_id, year)]), !anyDuplicated(scores$alderman))
# A boundary's other ward can have no eligible blocks. Read its incumbent from the
# full preserved source rather than infer names from the regression sample.
ward_sources <- as.data.table(arrow::read_parquet("../input/permit_block_year_panel_2015.parquet",
  col_select = c("ward_origin", "ward_dest", "alderman_origin_2014", "alderman_dest_2014",
    "strictness_origin_frozen", "strictness_dest_frozen")))
wards <- unique(rbind(
  ward_sources[, .(ward = ward_origin, alderman = alderman_origin_2014, permit_score = strictness_origin_frozen)],
  ward_sources[, .(ward = ward_dest, alderman = alderman_dest_2014, permit_score = strictness_dest_frozen)]))
wards <- wards[!is.na(ward)]
stopifnot(!anyDuplicated(wards$ward))
wards[, `:=`(developer_share = scores$developer_share[match(alderman, scores$alderman)],
  developer_dollars = scores$developer_dollars[match(alderman, scores$alderman)],
  eligible_dollars = scores$eligible_dollars[match(alderman, scores$alderman)])]

# One row per boundary pair, including pairs excluded for tied or missing donations.
blocks <- unique(data[, .(block_id, ward_pair_id, ward_origin, ward_dest, paper_sign, developer_sign, pair_status)])
stopifnot(!anyDuplicated(blocks$block_id), all((blocks$paper_sign != 0) == (blocks$ward_origin != blocks$ward_dest)))
pairs <- blocks[, .(status = unique(pair_status), blocks = .N,
  switched_blocks = sum(paper_sign != 0), unchanged_blocks = sum(paper_sign == 0)), by = ward_pair_id]
pairs[, c("ward_a", "ward_b") := tstrsplit(ward_pair_id, "-", fixed = TRUE)]
pairs[, `:=`(ward_a = as.integer(ward_a), ward_b = as.integer(ward_b))]
for (side in c("a", "b")) {
  index <- match(pairs[[paste0("ward_", side)]], wards$ward)
  stopifnot(!anyNA(index))
  for (column in c("alderman", "permit_score", "developer_share", "developer_dollars", "eligible_dollars")) {
    set(pairs, j = paste0(column, "_", side), value = wards[[column]][index])
  }
}
pairs[, `:=`(paper_higher = fifelse(permit_score_a > permit_score_b, alderman_a, alderman_b),
  developer_higher = fifelse(developer_share_a > developer_share_b, alderman_a, alderman_b))]
pairs[status != "common", developer_higher := NA_character_]
pairs[, ordering := fcase(status != "common", status,
  sign(permit_score_a - permit_score_b) == sign(developer_share_a - developer_share_b), "same", default = "reversed")]
data[, ordering := pairs$ordering[match(ward_pair_id, pairs$ward_pair_id)]]
stopifnot(all(data[pair_status == "common" & paper_sign != 0,
  (paper_sign == developer_sign) == (ordering == "same")]))
stopifnot(all(data[paper_sign != 0, (pmin(ward_origin, ward_dest) == as.numeric(sub("-.*", "", ward_pair_id))) &
  (pmax(ward_origin, ward_dest) == as.numeric(sub(".*-", "", ward_pair_id)))]))

# These raw before/after counts describe actual reassignments; they are not adjusted effects.
flows <- data[, .(blocks = uniqueN(block_id), pre_block_years = sum(post == 0), post_block_years = sum(post == 1),
  permits_pre = sum(outcome[post == 0]), permits_post = sum(outcome[post == 1]),
  permits_per_block_year_pre = mean(outcome[post == 0]), permits_per_block_year_post = mean(outcome[post == 1])),
  by = .(ward_pair_id, ward_origin, ward_dest, alderman_origin_2014, alderman_dest_2014,
    pair_status, ordering, paper_sign, developer_sign)]

# Fit separate slopes for pairs that agree and disagree. Each pair keeps its own controls.
# The two ranking versions must fit identical observations and give opposite slopes only
# for reversed pairs. This checks the sign interpretation directly.
common <- copy(data[pair_status == "common"])
group_results <- list()
pair_results <- list()
for (ranking in c("paper", "developer")) {
  common[, post_signed := post * if (ranking == "paper") paper_sign else developer_sign]
  common[, `:=`(post_same = post_signed * as.integer(ordering == "same"),
    post_reversed = post_signed * as.integer(ordering == "reversed"))]
  pooled <- fepois(as.formula(paste("outcome ~ post_signed |", permit_fe)), data = common,
    cluster = as.formula(paste("~", permit_cluster)), notes = FALSE)
  expected <- benchmark[version == paste0(ranking, "_common")]
  stopifnot(nobs(pooled) == expected$n, abs(coef(pooled)["post_signed"] - expected$estimate) < 1e-10)
  if (ranking == "paper") fitted_rows <- obs(pooled)
  if (ranking == "developer") stopifnot(identical(obs(pooled), fitted_rows))
  group_model <- fepois(as.formula(paste("outcome ~ post_same + post_reversed |", permit_fe)), data = common,
    cluster = as.formula(paste("~", permit_cluster)), notes = FALSE)
  stopifnot(identical(obs(pooled), obs(group_model)))
  tab <- coeftable(group_model)
  for (group in c("same", "reversed")) {
    term <- paste0("post_", group)
    beta <- tab[term, 1]
    se <- tab[term, 2]
    df <- degrees_freedom(group_model, type = "t")
    count <- pairs[status == "common" & ordering == group]
    group_results[[length(group_results) + 1L]] <- data.table(ranking, ordering = group,
      estimate = beta, std_error = se, p_value = 2 * pt(-abs(beta / se), df),
      percent_effect = 100 * expm1(beta), percent_low = 100 * expm1(beta - qt(.975, df) * se),
      percent_high = 100 * expm1(beta + qt(.975, df) * se),
      pairs_with_switches = sum(count$switched_blocks > 0), switched_blocks = sum(count$switched_blocks),
      fitted_block_years = sum(common$ordering[fitted_rows] == group))
  }
  # Omit each informative pair in turn, retaining all other pairs and their controls.
  # These changes measure sensitivity; they are not additive contributions to Poisson.
  for (pair in pairs[status == "common" & switched_blocks > 0, ward_pair_id]) {
    d <- common[ward_pair_id != pair]
    omitted <- fepois(as.formula(paste("outcome ~ post_signed |", permit_fe)), data = d,
      cluster = as.formula(paste("~", permit_cluster)), notes = FALSE)
    stopifnot(nobs(omitted) == nobs(pooled) - sum(common$ward_pair_id[obs(pooled)] == pair))
    beta <- coef(omitted)["post_signed"]
    se <- fixest::se(omitted)["post_signed"]
    df <- degrees_freedom(omitted, type = "t")
    pair_results[[length(pair_results) + 1L]] <- data.table(ward_pair_id = pair, ranking,
      estimate_without = beta, std_error_without = se, p_value_without = 2 * pt(-abs(beta / se), df),
      percent_without = 100 * expm1(beta),
      change_percentage_points = 100 * (expm1(beta) - expm1(coef(pooled)["post_signed"])), n_without = nobs(omitted))
  }
}
results <- rbindlist(pair_results)
group_results <- rbindlist(group_results)
stopifnot(abs(group_results[ranking == "paper" & ordering == "same", estimate] -
  group_results[ranking == "developer" & ordering == "same", estimate]) < 1e-10)
stopifnot(abs(group_results[ranking == "paper" & ordering == "reversed", estimate] +
  group_results[ranking == "developer" & ordering == "reversed", estimate]) < 1e-10)
stopifnot(!anyDuplicated(results[, .(ward_pair_id, ranking)]))
wide <- dcast(results, ward_pair_id ~ ranking,
  value.var = c("estimate_without", "std_error_without", "p_value_without", "percent_without", "change_percentage_points", "n_without"))
stopifnot(!anyDuplicated(wide$ward_pair_id))
pairs <- merge(pairs, wide, by = "ward_pair_id", all.x = TRUE, sort = FALSE)
fitted_counts <- common[fitted_rows, .(fitted_blocks = uniqueN(block_id),
  fitted_switched_blocks = uniqueN(block_id[paper_sign != 0]), fitted_block_years = .N), by = ward_pair_id]
stopifnot(!anyDuplicated(fitted_counts$ward_pair_id))
pairs <- merge(pairs, fitted_counts, by = "ward_pair_id", all.x = TRUE, sort = FALSE)
SaveData(pairs, "ward_pair_id", "../output/permit_pair_comparisons.csv")
SaveData(group_results, c("ranking", "ordering"), "../output/permit_ordering_results.csv")
SaveData(flows, c("ward_pair_id", "ward_origin", "ward_dest"), "../output/permit_reassignment_flows.csv")
