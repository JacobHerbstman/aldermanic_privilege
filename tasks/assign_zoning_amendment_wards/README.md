# Ward and alderman of each zoning map amendment

Run `make` in `code/`.

- `build_address_queries.R` takes the first address after "at" in each amendment's title
  (`tasks/clean_zoning_map_amendments`) and reduces an address range to its first number, in
  `zoning_amendment_address_queries.csv`. The unique addresses were geocoded with the U.S. Census Bureau's batch
  geocoder (public, no key; Public_AR_Current benchmark), in batches of 2,000, on September 27--28, 2026. The responses
  are preserved as received in `sources/census_geocoder_responses_20260928.csv.gz`, verified against
  `code/geocoder_snapshot.sha256` and restored to `output/census_geocoder_responses.csv` (uncompressed SHA-256
  `391eb5c3b062c0e7d8a5436ef80f92902fc6828ecd1447ffba0d11a073cd20b5`), so ordinary builds do not query the live
  geocoder. The ward step matches responses to queries by the address sent and stops if a query has none, as new
  titles would; `make refresh-geocodes` then sends the current queries (`geocode_amendment_addresses.R`) into
  `temp/` for comparison, and adopting the refresh means preserving it in `sources/` with a new checksum. Before
  September 30, 2026 the geocoder ran as part of the build and any change to the cleaned amendments re-queried it.
- `assign_amendment_wards.R` places each matched point on the ward map in force on the amendment's introduction date
  (the 2003 map until May 18, 2015, the 2015 map until May 15, 2023, then the 2023 map; `data_raw/`) and assigns the
  alderman serving that ward that day, from the hand-built terms in `tasks/create_alderman_data/adjudication`. The
  geocoded location takes priority; an amendment filed by an alderman whose address is not located takes the filing
  ward. `ward_source` records which, or why there is no ward.
- The geocoder sometimes returns the house number on the opposite side of the city from the title's direction
  ("5145 N CALIFORNIA AVE" as 5145 S California Ave, in the 14th Ward rather than the 40th) or in Chicago Heights
  ("1601 W DIVISION ST"). Such a match is rejected (`geocode_rejected`: `opposite_side` or `outside_chicago`), and
  the title's address is placed instead on the City's 2013 street centerlines (`tasks/download_chicago_gis_layers`),
  on the segment of the title's direction, street and number range, 30 feet to the side of the number's parity
  (`location_source`). A match that changes a direction the street cannot have ("11231 W WESTERN AVE" as S Western)
  or matches another address the title gives ("... AKA 1611 W IRVING PARK RD") is kept. 25 matches are rejected: 23
  are placed on the centerlines, one takes the filing ward and one (125 E Van Buren St, past the street's last
  segment) has no ward. Their correction changed the ward and alderman of 22 amendments.
- An amendment still unplaced (no match, a rejected match the centerlines cannot place, or a title from which no
  geocoder query was formed) is read again from its title and placed on the centerlines (`title_on_street_centerlines`):
  every address the title lists ("400-410 N Green St/401-411 N Peoria St"), house numbers listed before one street
  ("5531, 5533, 5535 and 5537 S Oakley Ave"), ranges written with "to", directions spelled out ("1846 North Bissell
  St"), a parenthesis dropped and an ordinal's suffix restored ("75t St"); an address printed without a direction is
  kept if exactly one direction places it, and the first address placed is used. Added September 30, 2026, when a
  review found that 8 percent of large projects (100 or more units or 150 feet) had no ward against 3 percent of other
  applications, among them the Old Town Canvas planned development (1600 N LaSalle St). Tested on 800 amendments the
  geocoder places, the reread places 755, a median of 52 feet from the geocoder's point (96 percent within 300 feet,
  none beyond 1,000), in the same ward for 742; nearly all of the 13 others lie on a street that bounds two wards.

Of 6,568 amendments, 6,236 are placed by their geocoded address, 23 on the centerlines as corrected matches, 182 on
the centerlines from their titles and 100 by the filing ward; 27 have no ward, 9 whose titles' addresses no source
places and 18 whose titles give no street address (intersections, boundary streets or a planned development's name
only). For 1,221 amendments filed by an alderman with a located address, the located ward equals the filing ward for
93.2 percent. Of those that differed before the titles were read again (76 of 1,167), 23 (30 percent) lie within 500
feet of the filing ward; others are far from it, so the filing office is not always the property's ward. 6,538
amendments have an alderman (terms recorded through September 27, 2026); the others have no ward, apart from three
introduced during vacancies (ward 4 in February 2016, ward 11 in March 2022).
