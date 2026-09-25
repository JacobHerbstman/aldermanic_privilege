# Save a dataset and describe the saved file. Preserve its row order and format.
report_program <- sub("save_data[.]R$", "report.py", sys.frame(1)$ofile)
# Reports run in the Python environment built by tasks/setup_environment.
report_python <- file.path(dirname(report_program), "../../setup_environment/output/python-env/bin/python")

ReportData <- function(outfile, key = character()) {
  if (!file.exists(report_python)) stop("Run make in tasks/setup_environment/code to build the report environment.", call. = FALSE)
  status <- system2(report_python, c(
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
