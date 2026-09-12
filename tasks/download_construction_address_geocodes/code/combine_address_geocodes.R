# setwd("tasks/download_construction_address_geocodes/code")
# provider <- "census"

source("../../shared/code/save_data.R")

library(dplyr)
library(readr)

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(provider)
stopifnot(length(args) == 1L, args[1] %in% c("census", "chicago"))
provider <- args[1]
original <- read_csv(paste0("../input/address_geocodes_", provider, "_2026-09-07.csv"),
  col_types = cols(.default = col_character()))
additional <- read_csv(paste0("../input/address_geocodes_", provider, "_additional_2026-09-07.csv"),
  col_types = cols(.default = col_character()))
responses <- bind_rows(original, additional) |> arrange(selected_address)
stopifnot(!anyNA(responses), !anyDuplicated(responses$selected_address))
write_csv(responses, paste0("../output/address_geocodes_", provider, ".csv"))

ReportData(paste0("../output/address_geocodes_", provider, ".csv"), "selected_address")
