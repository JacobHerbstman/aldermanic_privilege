source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_rental_guangbin/code")
# mf_path <- "../output/event_study_summary_guangbin_quarterly_mf.csv"
# full_path <- "../output/event_study_summary_guangbin_quarterly_full.csv"
# out_path <- "../output/event_study_summary_guangbin_quarterly_comparison.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(mf_path, full_path, out_path)
}

if (length(args) != 3) {
  stop("FATAL: Script requires args: <mf_path> <full_path> <out_path>", call. = FALSE)
}
mf_path <- args[1]
full_path <- args[2]
out_path <- args[3]

out <- bind_rows(
  read_csv(mf_path, show_col_types = FALSE),
  read_csv(full_path, show_col_types = FALSE)
) %>%
  arrange(match(sample_filter, c("multifamily_only", "full_sample")))

write_csv(out, out_path)
