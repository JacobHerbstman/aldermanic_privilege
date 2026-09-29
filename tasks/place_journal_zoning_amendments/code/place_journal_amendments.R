# setwd("tasks/place_journal_zoning_amendments/code")
# The place, ward and alderman of each zoning map amendment introduced in the City Council Journals
# (tasks/parse_journal_zoning_amendments). The Journals print an amendment's boundary and, before July 2008, no
# address, so every amendment is placed from its boundary, the same way in every year:
#   1. Streets. Each street the boundary names, as a side ("South Halsted Street") or as the street it measures from
#      ("a line 131.20 feet north of and parallel to West Cermak Road"), is matched to the City's 2013 street
#      centerlines (tasks/download_chicago_gis_layers) by direction, name and kind of street, taking the longest run
#      of words after the direction that names a centerline street ("South St. Louis Avenue", "South Dr. Martin Luther
#      King, Jr. Drive"), first with the type printed. A street keeps its name as it changes type along its length
#      (West Diversey Avenue becomes Diversey Parkway east of Western; West Fulton Street becomes Fulton Boulevard),
#      and the Journals misprint types ("South Sangamon Avenue" for Sangamon Street), so an amendment not placed with
#      the printed types is tried again with the through-street types (street_types_through: street, avenue,
#      boulevard, parkway, road, highway, expressway) taken as one kind (street_kinds records which placed it). A
#      drive, place, court, terrace, lane, way, square or plaza remains a street of its own (West 61st Place lies
#      between 61st and 62nd Streets; South Peoria Drive is not Peoria Street). Spaces within a name are ignored
#      ("LaSalle" for La Salle). An ordinal's OCR-garbled suffix is dropped ("West 35"" Street" is West 35th Street),
#      and a name the centerlines lack is matched to the one street of the same direction and kind whose name differs
#      by one letter ("Westem" for Western). OCR marks after a number are dropped ("West 22™ Street").
#   2. Corners. The corners are the centerline nodes where two of the named streets meet, less any more than
#      corner_cluster_feet from most of the others (a long street crossing another again elsewhere). An amendment with
#      three or more corners bounds a block and is placed at their mean. If no group of corners stands out (South
#      Peoria and South Sangamon Streets meet each other far from where either meets West 63rd Street), it is not
#      placed.
#   3. Side. An amendment with one or two corners is placed off them, since streets are often ward boundaries. For each
#      street the boundary measures from, it gives the parcel's side ("north of ... West Cermak Road", "the alley next
#      west of ... South Halsted Street") and distances, and the point moves to that side by half a street's width
#      (half_street_feet) plus the mean of the distances (with 0 where the street is itself a side), or plus half a lot
#      (half_lot_feet) where it gives only an alley.
#   4. Amendments not placed so. One with a common address (printed from July 2008) is placed at it on the
#      centerlines, by house-number range (place_source common_address; tasks/shared/code/centerline_address.R). Else
#      one street's printed direction is taken as each of the other three ("North Halsted Street" at West 33rd Street
#      is South Halsted), all its mentions at once, and the amendment is placed if exactly one such repair places it
#      (street_kinds direction_repaired; direction_repair records it).
# An amendment is not placed if its boundary names fewer than two centerline streets or streets that do not meet, or if
# its corners lie apart, after these steps.
# The ward is the one containing the point on the ward map in force at introduction (the wards redrawn in 1998, until
# the council term that began on May 5, 2003, then the 2003 map, until May 17, 2015), unless the point lies where two
# of the map's ward polygons overlap; an alderman's amendment that is not placed in a ward takes the filing ward of its
# heading ("BY ALDERMAN BURNETT (27th Ward)"). The alderman is the one serving the ward that day
# (tasks/create_alderman_data). The Council adopted the 2003 map on December 19, 2001 (wards_redrawn), and from about
# May 2002 aldermen filed amendments in the wards they would represent, so for introductions between its adoption and
# its taking effect, redrawn_ward and redrawn_alderman give the ward on the 2003 map, found the same way, and the
# alderman then serving that ward's number; otherwise they are the ward and alderman.
journal_years <- 2000:2011
corner_cluster_feet <- 1500
half_street_feet <- 33
half_lot_feet <- 62
wards_redrawn <- as.Date("2001-12-19")
ward_map_2003_start <- as.Date("2003-05-05")
ward_map_2003_end <- as.Date("2015-05-17")
street_types <- c(Street = "ST", Avenue = "AVE", Road = "RD", Boulevard = "BLVD", Drive = "DR", Place = "PL",
  Court = "CT", Parkway = "PKWY", Terrace = "TER", Lane = "LN", Way = "WAY", Highway = "HWY", Plaza = "PLZ",
  Square = "SQ", Expressway = "EXPY")
street_types_through <- c("ST", "AVE", "BLVD", "PKWY", "RD", "HWY", "EXPY")
compass <- list(north = c(0, 1), south = c(0, -1), east = c(1, 0), west = c(-1, 0), northeast = c(1, 1) / sqrt(2),
  northwest = c(-1, 1) / sqrt(2), southeast = c(1, -1) / sqrt(2), southwest = c(-1, -1) / sqrt(2))

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/normalize_chicago_address.R")
source("../../shared/code/address_segments.R")
source("../../shared/code/centerline_address.R")

introductions <- bind_rows(lapply(journal_years, function(year) read_csv(
  sprintf("../input/journal_introductions_%d.csv", year), col_types = cols(.default = col_character())))) |>
  mutate(introduction_date = as.Date(meeting_date))
stopifnot(!anyDuplicated(introductions[c("file", "position")]),
  all(introductions$introduction_date <= ward_map_2003_end))

# A street is its direction, its name without spaces and its kind: its type, or, taking the through-street types as
# one kind, blank for a through street.
street_key <- function(direction, name, type, kinds) {
  kind <- if_else(kinds == "through" & type %in% street_types_through, "", type)
  str_squish(paste(direction, str_remove_all(name, " "), kind))
}
typed_street <- paste0("\\s(?:", paste(street_types, collapse = "|"), ")$")
# Centerline streets, without ramps and without the suffix of a divided road's two roadways ("S LAKE SHORE DR NB").
centerlines <- st_read("/vsizip/../input/street_centerlines_2013.zip/Transportation.shp", quiet = TRUE) |>
  st_transform(3435) |>
  filter(!STREET_TYP %in% c("ER", "XR", "SR", "RL")) |>
  mutate(printed = street_key(coalesce(PRE_DIR, ""), STREET_NAM, coalesce(STREET_TYP, ""), "printed"),
    through = street_key(coalesce(PRE_DIR, ""), STREET_NAM, coalesce(STREET_TYP, ""), "through"))
centerline_streets <- list(printed = unique(centerlines$printed), through = unique(centerlines$through))
segment_ends <- st_coordinates(centerlines) |>
  as_tibble() |>
  summarise(from_x = first(X), from_y = first(Y), to_x = last(X), to_y = last(Y), .by = L1)
stopifnot(nrow(segment_ends) == nrow(centerlines))
nodes <- lapply(c(printed = "printed", through = "through"), function(kinds) bind_rows(
  tibble(node = centerlines$FNODE_ID, street = centerlines[[kinds]], x = segment_ends$from_x, y = segment_ends$from_y),
  tibble(node = centerlines$TNODE_ID, street = centerlines[[kinds]], x = segment_ends$to_x, y = segment_ends$to_y)) |>
  distinct(node, street, .keep_all = TRUE))

# 1. Streets named in a text, as centerline streets, in order of first mention.
ordinal <- function(n) {
  paste0(n, if_else(n %% 100 %in% 11:13, "TH", c("TH", "ST", "ND", "RD", rep("TH", 6))[n %% 10 + 1]))
}
name_word <- function(word) {
  word <- toupper(str_remove_all(word, "[.,]|[^\\x01-\\x7F]"))
  number <- str_match(word, "^([0-9]{1,3})[^A-Z0-9]*(?:ST|ND|RD|TH)?[^A-Z0-9]*$")[, 2]
  if_else(is.na(number), word, ordinal(as.integer(number)))
}
street_names <- function(text, kinds) {
  known <- centerline_streets[[kinds]]
  words <- str_split(str_squish(str_replace_all(text, "[;:()]", " ; ")), " ")[[1]]
  starts <- which(words %in% c("North", "South", "East", "West") & seq_along(words) < length(words))
  found <- vapply(starts, function(s) {
    following <- words[(s + 1):min(s + 7, length(words))]
    following <- head(following, match(";", following, nomatch = length(following) + 1) - 1)
    if (length(following) == 0) return(NA_character_)
    for (n in rev(seq_along(following))) {
      last <- str_remove_all(following[n], "[.,]")
      type <- if (last %in% names(street_types)) street_types[[last]] else ""
      name <- paste(name_word(following[seq_len(n - (type != ""))]), collapse = " ")
      if (name == "") next
      street <- street_key(substr(words[s], 1, 1), name, type, kinds)
      if (street %in% known) return(street)
      if (type != "" && nchar(name) >= 5) {
        kind <- if_else(kinds == "through" & type %in% street_types_through, "", type)
        near <- known[str_starts(known, paste0(substr(words[s], 1, 1), " ")) &
          (if (kind == "") !str_detect(known, typed_street) else str_ends(known, paste0(" ", kind)))]
        near_names <- str_squish(str_remove(str_remove(near, "^[NSEW] "), paste0(" ", kind, "$")))
        close <- near[utils::adist(str_remove_all(name, " "), near_names)[1, ] == 1]
        if (length(close) == 1) return(close)
      }
    }
    NA_character_
  }, character(1))
  unique(na.omit(found))
}

# 3. The side of each street the boundary measures from, and its distances.
side_measures <- function(boundary, sides, kinds) {
  text <- str_replace_all(boundary, "(?<=[0-9]),(?=[0-9])", "")
  found <- str_match_all(text, regex(paste0(
    "(?:([0-9]+(?:\\.[0-9]+)?)\\s+feet\\s+|alley\\s+(?:next\\s+|immediately\\s+)?)",
    "(north|south|east|west)(east|west)?(?:erly)?\\s+(?:of|from)\\s+(?:and\\s+parallel\\s+(?:to|with)\\s+)?",
    "(?:the\\s+(?:\\w+\\s+){0,2}line\\s+of\\s+|the\\s+intersection\\s+of\\s+)?",
    "((?:North|South|East|West)\\s[^;()]{2,60})"),
    ignore_case = TRUE))[[1]]
  if (nrow(found) == 0) return(tibble(street = character(), direction = character(), offset = numeric()))
  tibble(street = vapply(found[, 5], function(s) c(street_names(s, kinds), NA_character_)[1], character(1)),
    direction = tolower(paste0(found[, 3], coalesce(found[, 4], ""))), feet = as.numeric(found[, 2])) |>
    filter(!is.na(street)) |>
    summarise(offset = half_street_feet + if (all(is.na(feet))) half_lot_feet else
      mean(c(feet[!is.na(feet)], if (first(street) %in% sides) 0)), .by = c(street, direction))
}

# 2. Corners and the placed point.
place <- function(boundary, kinds) {
  streets <- street_names(boundary, kinds)
  empty <- tibble(streets = paste(streets, collapse = "; "), corners = 0L, x = NA_real_, y = NA_real_,
    place_source = NA_character_, unplaced = NA_character_)
  if (length(streets) < 2) return(mutate(empty, unplaced = "fewer_than_two_streets"))
  corners <- nodes[[kinds]] |> filter(street %in% streets) |> add_count(node) |> filter(n >= 2) |> distinct(node, x, y)
  if (nrow(corners) == 0) return(mutate(empty, unplaced = "streets_do_not_meet"))
  near <- rowSums(as.matrix(dist(corners[c("x", "y")])) < corner_cluster_feet)
  corners <- corners[near == max(near), ]
  if (nrow(corners) > 1 && max(dist(corners[c("x", "y")])) > corner_cluster_feet) {
    return(mutate(empty, unplaced = "corners_apart"))
  }
  x <- mean(corners$x)
  y <- mean(corners$y)
  source <- "block"
  if (nrow(corners) < 3) {
    segments <- str_squish(str_remove(str_split(boundary, ";")[[1]], "^\\W*(?:and\\s+)?"))
    sides <- unlist(lapply(segments[str_detect(segments, "^(?:North|South|East|West)\\s")], street_names, kinds))
    measures <- side_measures(boundary, sides, kinds)
    shift <- Reduce(`+`, Map(function(direction, offset) compass[[direction]] * offset, measures$direction,
      measures$offset), c(0, 0))
    x <- x + shift[1]
    y <- y + shift[2]
    source <- if (nrow(measures) > 0) "corner_and_side" else "corner"
  }
  tibble(streets = paste(streets, collapse = "; "), corners = nrow(corners), x, y, place_source = source,
    unplaced = NA_character_)
}
boundaries <- coalesce(introductions$boundary, "")
located <- bind_rows(lapply(boundaries, place, kinds = "printed")) |>
  mutate(street_kinds = if_else(is.na(unplaced), "printed", NA_character_))
retried <- which(!is.na(located$unplaced))
again <- bind_rows(lapply(boundaries[retried], place, kinds = "through")) |>
  mutate(street_kinds = "through")
located[retried[is.na(again$unplaced)], ] <- again[is.na(again$unplaced), ]
located$direction_repair <- NA_character_

# 4. Amendments still not placed: at the common address the Journals print from July 2008, on the centerlines
# (tasks/shared/code/centerline_address.R), else with one street's direction repaired.
first_addresses <- address_segments(seq_along(boundaries), printed_address(introductions$common_address)) |>
  slice_head(n = 1, by = id) |>
  filter(id %in% which(!is.na(located$unplaced)), !is.na(direction)) |>
  transmute(id, number = low, direction,
    street = if_else(str_detect(street, "^[0-9]+$"), ordinal(as.integer(str_extract(street, "^[0-9]+$"))), street))
at_address <- centerline_address_points(centerlines, first_addresses, half_street_feet)
located[at_address$id, c("x", "y")] <- at_address[c("x", "y")]
located[at_address$id, c("place_source", "street_kinds", "unplaced")] <- list("common_address", NA_character_,
  NA_character_)
repaired_place <- function(boundary) {
  mentions <- str_match_all(boundary, "\\b(North|South|East|West)\\s+([A-Z0-9][A-Za-z0-9'-]*)")[[1]]
  tries <- bind_rows(lapply(unique(mentions[, 3]), function(word) {
    printed <- unique(mentions[mentions[, 3] == word, 2])
    if (length(printed) != 1) return(NULL)
    bind_rows(lapply(setdiff(c("North", "South", "East", "West"), printed), function(direction) {
      variant <- str_replace_all(boundary, paste0("\\b", printed, "(?=\\s+", str_escape(word), "\\b)"), direction)
      mutate(place(variant, "through"), direction_repair = paste(printed, word, "as", direction))
    }))
  }))
  if (nrow(tries) == 0) return(NULL)
  placed <- filter(tries, is.na(unplaced))
  if (nrow(placed) == 1) mutate(placed, street_kinds = "direction_repaired") else NULL
}
unrepaired <- which(!is.na(located$unplaced) & boundaries != "")
repairs <- lapply(boundaries[unrepaired], repaired_place)
repaired <- unrepaired[!vapply(repairs, is.null, logical(1))]
located[repaired, ] <- bind_rows(repairs)[names(located)]
places <- bind_cols(select(introductions, file, position, introduction_date, filer, filing_ward = ward), located)

# Wards and aldermen.
ward_maps <- bind_rows(
  st_read("../input/Chicago_Wards_1998.geojson", quiet = TRUE) |> st_transform(3435) |>
    transmute(map = "1998", placed_ward = as.integer(WARD)),
  st_read("../input/Wards_2014.geojson", quiet = TRUE) |> filter(ward != "OUT") |> st_transform(3435) |>
    transmute(map = "2003", placed_ward = as.integer(ward))) |>
  st_make_valid()
placed <- places |>
  filter(!is.na(x)) |>
  mutate(map = if_else(introduction_date < ward_map_2003_start, "1998", "2003")) |>
  st_as_sf(coords = c("x", "y"), crs = 3435, remove = FALSE)
placed <- bind_rows(lapply(c("1998", "2003"), function(m) {
  st_join(filter(placed, map == m), filter(ward_maps, map == m) |> select(placed_ward), join = st_within)
})) |>
  st_drop_geometry() |>
  add_count(file, position, name = "wards") |>
  filter(wards == 1)
redrawn <- places |>
  filter(!is.na(x), introduction_date >= wards_redrawn, introduction_date < ward_map_2003_start) |>
  st_as_sf(coords = c("x", "y"), crs = 3435, remove = FALSE) |>
  st_join(filter(ward_maps, map == "2003") |> select(redrawn_placed_ward = placed_ward), join = st_within) |>
  st_drop_geometry() |>
  add_count(file, position, name = "wards") |>
  filter(wards == 1)
terms <- read_csv("../input/alderman_terms.csv", show_col_types = FALSE)
alderman_serving <- function(ward, date) {
  tibble(ward, date) |>
    left_join(terms, by = join_by(ward, between(date, start_date, end_date)), relationship = "many-to-one") |>
    pull(alderman)
}
places <- places |>
  left_join(select(placed, file, position, placed_ward), by = c("file", "position"), relationship = "one-to-one") |>
  left_join(select(redrawn, file, position, redrawn_placed_ward), by = c("file", "position"),
    relationship = "one-to-one") |>
  mutate(filing_ward = as.integer(filing_ward), ward = coalesce(placed_ward, filing_ward),
    ward_source = case_when(!is.na(placed_ward) ~ "boundary", !is.na(filing_ward) ~ "filing_ward",
      !is.na(x) ~ "placed_on_ward_overlap_or_outside", TRUE ~ unplaced),
    redrawn_ward = if_else(introduction_date >= wards_redrawn & introduction_date < ward_map_2003_start,
      coalesce(redrawn_placed_ward, filing_ward), ward),
    alderman = alderman_serving(ward, introduction_date),
    redrawn_alderman = alderman_serving(redrawn_ward, introduction_date))
coordinates <- places |>
  filter(!is.na(x)) |>
  st_as_sf(coords = c("x", "y"), crs = 3435) |>
  st_transform(4326)
places <- places |>
  left_join(tibble(file = coordinates$file, position = coordinates$position,
    longitude = st_coordinates(coordinates)[, 1], latitude = st_coordinates(coordinates)[, 2]),
    by = c("file", "position"), relationship = "one-to-one") |>
  select(file, position, introduction_date, filer, streets, corners, place_source, street_kinds, direction_repair,
    unplaced, longitude, latitude, filing_ward, placed_ward, ward, ward_source, alderman, redrawn_ward,
    redrawn_alderman)
SaveData(places, c("file", "position"), "../output/journal_amendment_places.csv")
