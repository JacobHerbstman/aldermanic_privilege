# House number, direction and street name of a Chicago address, without the street type or unit:
# "1234 N MILWAUKEE AVE 2F" and "1234 N MILWAUKEE AV" both give "1234 N MILWAUKEE". Requires normalize_chicago_address.R.
street_key <- function(x) {
  x <- normalize_address(x) |> str_replace_all("\\bPKY\\b", "PKWY") |> str_replace_all("\\bAV\\b", "AVE") |>
    str_replace_all("\\bSAINT\\b", "ST") |>
    str_replace_all("\\b(?:DR )?(?:MARTIN L(?:UTHER)?|M L) KING(?: JR)?\\b", "MARTIN LUTHER KING")
  coalesce(str_match(x, "^([0-9]+ [NSEW] .+?) (?:AVE|ST|RD|BLVD|DR|PL|CT|PKWY|TER|HWY|LN|WAY|SQ|CIR)\\b")[, 2],
    str_extract(x, "^[0-9]+ [NSEW] [A-Z]+(?: [A-Z]+)*"))
}

# House number and first street-name word, with or without a direction: "4143 S CALUMET AVE" and the Assessor's
# "4143 CALUMET" both give 4143 and "CALUMET". For comparing nearby addresses, where the direction is not needed.
address_parts <- function(x) {
  x <- normalize_address(x) |> str_replace_all("\\b(?:DR )?(?:MARTIN L(?:UTHER)?|M L) KING(?: JR)?\\b", "KING")
  parts <- str_match(x, "^0*([0-9]+) (?:[NSEW] )?([A-Z0-9]+)")
  tibble(number = as.integer(parts[, 2]), street = parts[, 3])
}
