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
  geocoded location takes priority; an amendment filed by an alderman whose address is not geocoded takes the filing
  ward. `ward_source` records which, or why there is no ward.

Of 6,716 amendments, 6,396 are placed by their geocoded address and 156 by the filing ward; 97 addresses are not
matched by the geocoder, 63 titles give no street address (intersections or map numbers only) and 4 points fall
outside the ward maps. For 1,189 amendments filed by an alderman with a geocoded address, the geocoded ward equals
the filing ward for 93.4 percent. Of the 80 that differ, about 30 percent lie within 500 feet of the filing ward;
others are far from it, so the filing office is not always the property's ward. 6,549 amendments have an alderman
(terms recorded through September 27, 2026); the other 167 have no ward, apart from three introduced during
vacancies (ward 4 in February 2016, ward 11 in March 2022).
