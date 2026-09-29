# Street segments of Chicago addresses, for matching amendments of the same site (tasks/link_zoning_refilings,
# tasks/follow_journal_zoning_amendments). Addresses list segments separated by commas, "and", slashes or semicolons
# ("158-182 N Green St, 833-857 W Lake St"); an abbreviated upper number ("5689-93") takes the lower number's leading
# digits, and a range written high to low is read as the same range. A segment whose end numbers are both odd or both
# even lies on one side of the street (Chicago numbers the two sides odd and even); one with an odd and an even end
# spans both. Requires normalize_address (normalize_chicago_address.R).
segment_street_types <- "AVE|AV|ST|RD|BLVD|DR|PL|CT|PKWY|PKY|TER|HWY|LN|WAY|SQ|CIR|BROADWAY"
address_segments <- function(id, address) {
  tibble(id, address) |>
    filter(!is.na(address)) |>
    mutate(address = normalize_address(address) |>
      str_remove(" - .*$") |>
      str_replace_all("\\b(?:DR )?(?:MARTIN L(?:UTHER)?|M L) KING(?: JR)?\\b", "KING") |>
      str_replace_all(paste0("\\b(", segment_street_types, ") (?=[0-9])"), "\\1; ")) |>
    mutate(segment = strsplit(address, "\\s*(?:;|/| AND | AMD )\\s*")) |>
    tidyr::unnest_longer(segment) |>
    mutate(parts = str_match(segment, paste0("^0*([0-9]+)(?:\\s*-\\s*([0-9]+))?\\s+(?:([NSEW])\\s+)?(.+?)",
      "(?:\\s+(?:", segment_street_types, "))?$"))) |>
    transmute(id, low = as.integer(parts[, 2]), high_written = parts[, 3], direction = parts[, 4],
      street = parts[, 5]) |>
    filter(!is.na(low), !is.na(street)) |>
    mutate(high_written = coalesce(high_written, as.character(low)),
      high = as.integer(if_else(nchar(high_written) < nchar(low),
        paste0(substr(low, 1, nchar(low) - nchar(high_written)), high_written), high_written)),
      low_number = pmin(low, high), high = pmax(low, high), low = low_number,
      side = case_when(low %% 2 != high %% 2 ~ "both", low %% 2 == 1 ~ "odd", TRUE ~ "even")) |>
    select(id, low, high, direction, street, side)
}
# Pairs of ids (from earlier and later segments) with a shared segment: the same street (and direction, where both
# give one), overlapping house numbers and the same side of the street. Later segments are grouped by street, so each
# earlier segment meets only those of its own street.
overlapping_segments <- function(earlier, later) {
  earlier |>
    inner_join(later |> rename(later_id = id, later_low = low, later_high = high, later_direction = direction,
      later_side = side) |> tidyr::nest(later = -street), by = "street", relationship = "many-to-one") |>
    tidyr::unnest(later) |>
    filter(id != later_id, low <= later_high, later_low <= high,
      is.na(direction) | is.na(later_direction) | direction == later_direction,
      side == "both" | later_side == "both" | side == later_side) |>
    distinct(id, later_id)
}
