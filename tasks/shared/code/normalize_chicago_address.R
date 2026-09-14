normalize_address <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("[.,#]", " ") %>%
    str_replace_all("\\bNORTH\\b", "N") %>%
    str_replace_all("\\bSOUTH\\b", "S") %>%
    str_replace_all("\\bEAST\\b", "E") %>%
    str_replace_all("\\bWEST\\b", "W") %>%
    str_replace_all("\\bAVENUE\\b", "AVE") %>%
    str_replace_all("\\bSTREET\\b", "ST") %>%
    str_replace_all("\\bROAD\\b", "RD") %>%
    str_replace_all("\\bBOULEVARD\\b", "BLVD") %>%
    str_replace_all("\\bDRIVE\\b", "DR") %>%
    str_replace_all("\\bPLACE\\b", "PL") %>%
    str_replace_all("\\bCOURT\\b", "CT") %>%
    str_replace_all("\\bPARKWAY\\b", "PKWY") %>%
    str_replace_all("\\bTERRACE\\b", "TER") %>%
    str_replace_all("\\bHIGHWAY\\b", "HWY") %>%
    str_replace_all("\\bLANE\\b", "LN") %>%
    str_squish() %>%
    na_if("")
}

# Building-level geocoding may omit a trailing unit, but never a direction or street type.
geocode_street_address <- function(x) {
  address <- normalize_address(stringr::str_remove(x, ",.*$"))
  parts <- stringr::str_match(address,
    "^([0-9]+ [NSEW] .+? (?:AVE|ST|RD|BLVD|DR|PL|CT|PKWY|TER|HWY|LN))(?: (?:APT |UNIT |STE )?[A-Z0-9]+(?:[-/][A-Z0-9]+)?)?$")
  parts[, 2]
}
