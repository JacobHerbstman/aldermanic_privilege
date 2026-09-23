# House number, direction and street name of a Chicago address, without the street type or unit:
# "1234 N MILWAUKEE AVE 2F" and "1234 N MILWAUKEE AV" both give "1234 N MILWAUKEE". Requires normalize_chicago_address.R.
street_key <- function(x) {
  x <- normalize_address(x) |> str_replace_all("\\bPKY\\b", "PKWY") |> str_replace_all("\\bAV\\b", "AVE") |>
    str_replace_all("\\bSAINT\\b", "ST") |>
    str_replace_all("\\b(?:DR )?(?:MARTIN L(?:UTHER)?|M L) KING(?: JR)?\\b", "MARTIN LUTHER KING")
  coalesce(str_match(x, "^([0-9]+ [NSEW] .+?) (?:AVE|ST|RD|BLVD|DR|PL|CT|PKWY|TER|HWY|LN|WAY|SQ|CIR)\\b")[, 2],
    str_extract(x, "^[0-9]+ [NSEW] [A-Z]+(?: [A-Z]+)*"))
}
