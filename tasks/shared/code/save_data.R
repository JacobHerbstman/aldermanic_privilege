# Save a dataset and describe the saved file. Preserve its row order and format.
report_program <- sub("save_data[.]R$", "report.py", sys.frame(1)$ofile)

ReportData <- function(outfile, key = character()) {
  status <- system2("python3", c(
    shQuote(report_program), shQuote(outfile),
    shQuote(paste0("../report/", basename(outfile), ".log")), shQuote(key)))
  if (status != 0L) stop("Could not report saved data: ", outfile, call. = FALSE)
  invisible(outfile)
}

SaveData <- function(df, key = character(), outfile, ...) {
  # GeoPackage assigns its own feature ID when the data have no fid column.
  columns <- if (tools::file_ext(outfile) == "gpkg" && !"fid" %in% names(df)) {
    setdiff(key, "fid")
  } else key
  if (length(columns)) {
    stopifnot(all(columns %in% names(df)))
    identifiers <- as.data.frame(df)[columns]
    stopifnot(!anyNA(identifiers), !anyDuplicated(identifiers))
  }
  switch(tools::file_ext(outfile),
    csv = readr::write_csv(df, outfile, ...),
    parquet = arrow::write_parquet(df, outfile, ...),
    gpkg = sf::st_write(df, outfile, ...),
    stop("SaveData supports CSV, Parquet and GeoPackage files.", call. = FALSE))
  ReportData(outfile, key)
  invisible(df)
}
