# A house number placed on the City's street centerlines (tasks/download_chicago_gis_layers), for addresses a geocoder
# misplaces or that no geocoder read (tasks/assign_zoning_amendment_wards, tasks/place_journal_zoning_amendments). The
# address lies on the segment of its direction and street (the name without spaces) whose house-number range on the
# side of the number's parity (Chicago numbers the two sides of a street odd and even) includes it, at the number's
# position along that range, offset_feet toward that side. An address on no such segment, or on several, is not
# placed. centerlines are the shapefile's segments in feet (EPSG:3435); addresses a tibble of id, number, direction and
# street; the result has id, x and y in the same coordinates.
centerline_address_points <- function(centerlines, addresses, offset_feet) {
  segments <- centerlines |>
    transmute(direction = PRE_DIR, street = str_remove_all(STREET_NAM, "[^A-Z0-9]"),
      left_low = pmin(L_F_ADD, L_T_ADD), left_high = pmax(L_F_ADD, L_T_ADD), right_low = pmin(R_F_ADD, R_T_ADD),
      right_high = pmax(R_F_ADD, R_T_ADD), L_F_ADD, L_T_ADD, R_F_ADD, R_T_ADD)
  addresses <- addresses |> mutate(street = str_remove_all(street, "[^A-Z0-9]"))
  on <- bind_rows(lapply(seq_len(nrow(addresses)), function(i) {
    filter(segments, direction == addresses$direction[i], street == addresses$street[i]) |>
      mutate(id = addresses$id[i], number = addresses$number[i])
  }))
  if (nrow(on) == 0) return(tibble(id = addresses$id[0], x = numeric(), y = numeric()))
  on <- on |>
    mutate(side = case_when(number %% 2 == L_F_ADD %% 2 & number >= left_low & number <= left_high ~ "left",
      number %% 2 == R_F_ADD %% 2 & number >= right_low & number <= right_high ~ "right")) |>
    filter(!is.na(side)) |>
    add_count(id) |>
    filter(n == 1)
  if (nrow(on) == 0) return(tibble(id = addresses$id[0], x = numeric(), y = numeric()))
  along <- with(on, if_else(side == "left", (number - L_F_ADD) / (L_T_ADD - L_F_ADD),
    (number - R_F_ADD) / (R_T_ADD - R_F_ADD)))
  along <- coalesce(if_else(is.finite(along), along, 0.5), 0.5)
  lines <- st_geometry(on)
  at <- st_coordinates(st_line_interpolate(lines, along, normalized = TRUE))
  ahead <- st_coordinates(st_line_interpolate(lines, pmin(along + 0.01, 1), normalized = TRUE)) -
    st_coordinates(st_line_interpolate(lines, pmax(along - 0.01, 0), normalized = TRUE))
  toward <- if_else(on$side == "left", 1, -1) * offset_feet / sqrt(rowSums(ahead^2))
  tibble(id = on$id, x = at[, 1] - ahead[, 2] * toward, y = at[, 2] + ahead[, 1] * toward)
}
