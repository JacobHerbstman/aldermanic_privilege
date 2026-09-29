# Ward and alderman of each zoning map amendment

Run `make` in `code/`.

- `geocode_amendment_addresses.R` takes the first address after "at" in each amendment's title
  (`tasks/clean_zoning_map_amendments`), reduces an address range to its first number, and geocodes the unique
  addresses with the U.S. Census Bureau's batch geocoder (public, no key; Public_AR_Current benchmark), in batches of
  2,000. `census_geocoder_responses.csv` keeps the responses as received (retrieved September 27, 2026; the source is
  live, so a rerun is a deliberate refresh) and `zoning_amendment_address_queries.csv` records each amendment's query.
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

Of 6,568 amendments, 6,236 are placed by their geocoded address, 23 on the centerlines and 151 by the filing ward;
96 addresses are not matched by the geocoder, 61 titles give no street address (intersections or map numbers only)
and 1 rejected match has no centerline segment. For 1,167 amendments filed by an alderman with a located address, the
located ward equals the filing ward for 93.5 percent. Of the 76 that differ, 23 (30 percent) lie within 500 feet of
the filing ward; others are far from it, so the filing office is not always the property's ward. 6,407 amendments
have an alderman (terms recorded through September 27, 2026); the others have no ward, apart from three introduced
during vacancies (ward 4 in February 2016, ward 11 in March 2022).
