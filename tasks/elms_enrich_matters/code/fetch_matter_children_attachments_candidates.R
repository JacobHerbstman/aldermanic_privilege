## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/alderman_data/tasks/elms_enrich_matters/code")
#
## interactive argument examples (mirror Makefile inputs)
# fetch_matter_children_attachments_candidates.R --date-tag 20101201_20260212

parse_args <- function(args) {
  out <- list(date_tag = "20101201_20260212")
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (startsWith(arg, "--date-tag=")) {
      out$date_tag <- sub("^--date-tag=", "", arg)
    } else if (arg == "--date-tag" && i < length(args)) {
      i <- i + 1
      out$date_tag <- args[[i]]
    }
    i <- i + 1
  }
  out
}

safe_write <- function(df, path) {
  if ("child_table" %in% names(df)) {
    df$child_table <- NULL
  }
  write.csv(df, path, row.names = FALSE, na = "")
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))
  tag <- opts$date_tag
  long_path <- paste0("../temp/matter_children_long_", tag, ".csv")
  if (!file.exists(long_path)) stop("Missing long-form child table: ", long_path)

  long_df <- read.csv(long_path, stringsAsFactors = FALSE)
  dir.create("../output", recursive = TRUE, showWarnings = FALSE)
  safe_write(long_df[long_df$child_table == "attachments", , drop = FALSE], paste0("../output/matter_attachments_", tag, ".csv"))
  safe_write(long_df[long_df$child_table == "candidates", , drop = FALSE], paste0("../output/candidate_final_ids_", tag, ".csv"))
}

main()
