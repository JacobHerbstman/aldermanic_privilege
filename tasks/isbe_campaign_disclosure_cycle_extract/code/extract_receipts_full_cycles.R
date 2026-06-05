# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/isbe_campaign_disclosure_cycle_extract/code")

source("../../setup_environment/code/packages.R")

cycle_windows <- data.table::data.table(
  cycle_year = c(2003L, 2007L, 2011L, 2015L, 2019L, 2023L),
  cycle_start_date = as.Date(c(
    "1999-06-01", "2003-06-01", "2007-06-01",
    "2011-06-01", "2015-06-01", "2019-06-01"
  )),
  cycle_end_date = as.Date(c(
    "2003-05-31", "2007-05-31", "2011-05-31",
    "2015-05-31", "2019-05-31", "2023-05-31"
  ))
)

receipt_columns <- names(data.table::fread(
  "../temp/Receipts.txt",
  sep = "\t",
  quote = "",
  encoding = "UTF-8",
  na.strings = "",
  colClasses = "character",
  nrows = 0
))

receipts <- data.table::fread(
  "../temp/Receipts.txt",
  sep = "\t",
  quote = "",
  fill = Inf,
  encoding = "UTF-8",
  na.strings = "",
  colClasses = "character",
  showProgress = TRUE
)

if (!("RcvDate" %in% names(receipts))) {
  stop("ISBE receipts file is missing RcvDate.", call. = FALSE)
}
extra_columns <- setdiff(names(receipts), receipt_columns)
if (length(extra_columns) > 0) {
  bad_archived <- which(
    !is.na(receipts$Archived) &
      nzchar(receipts$Archived) &
      !(tolower(receipts$Archived) %in% c("true", "false"))
  )
  if (length(bad_archived) > 0) {
    all_columns <- names(receipts)
    for (row_i in bad_archived) {
      row_values <- as.character(unlist(receipts[row_i, ..all_columns], use.names = FALSE))
      row_values <- row_values[!is.na(row_values)]
      boolean_values <- row_values[tolower(row_values) %in% c("true", "false")]
      if (length(boolean_values) >= 2) {
        data.table::set(receipts, row_i, "Archived", boolean_values[length(boolean_values) - 1L])
        data.table::set(receipts, row_i, "RedactionRequested", boolean_values[length(boolean_values)])
      }
    }
  }
  receipts[, (extra_columns) := NULL]
}

rows_read <- nrow(receipts)
receipts[, received_date := as.Date(substr(RcvDate, 1, 10))]
bad_date_rows <- sum(is.na(receipts$received_date))

receipts[, cycle_year := data.table::fcase(
  received_date >= as.Date("1999-06-01") & received_date <= as.Date("2003-05-31"), 2003L,
  received_date >= as.Date("2003-06-01") & received_date <= as.Date("2007-05-31"), 2007L,
  received_date >= as.Date("2007-06-01") & received_date <= as.Date("2011-05-31"), 2011L,
  received_date >= as.Date("2011-06-01") & received_date <= as.Date("2015-05-31"), 2015L,
  received_date >= as.Date("2015-06-01") & received_date <= as.Date("2019-05-31"), 2019L,
  received_date >= as.Date("2019-06-01") & received_date <= as.Date("2023-05-31"), 2023L,
  default = NA_integer_
)]

receipts <- receipts[!is.na(cycle_year)]
receipts <- merge(receipts, cycle_windows, by = "cycle_year", all.x = TRUE, sort = FALSE)
data.table::setcolorder(
  receipts,
  c("cycle_year", "cycle_start_date", "cycle_end_date", setdiff(names(receipts), c(
    "cycle_year", "cycle_start_date", "cycle_end_date", "received_date"
  )))
)
receipts[, received_date := NULL]

data.table::fwrite(
  receipts,
  "../output/isbe_receipts_full_cycles_raw.tsv.tmp",
  sep = "\t",
  na = ""
)
if (file.exists("../output/isbe_receipts_full_cycles_raw.tsv")) {
  file.remove("../output/isbe_receipts_full_cycles_raw.tsv")
}
file.rename(
  "../output/isbe_receipts_full_cycles_raw.tsv.tmp",
  "../output/isbe_receipts_full_cycles_raw.tsv"
)

message(sprintf(
  "kept %s of %s rows; skipped %s bad-date rows and rows outside aldermanic cycles",
  format(nrow(receipts), big.mark = ","),
  format(rows_read, big.mark = ","),
  format(bad_date_rows, big.mark = ",")
))
