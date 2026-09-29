# Place, ward and alderman of the Journals' zoning map amendments

`place_journal_amendments.R` places each zoning map amendment introduced in the City Council Journals of 2000--2011
(`tasks/parse_journal_zoning_amendments`) and assigns its ward and alderman, in
`output/journal_amendment_places.csv` (one row per introduction). Run `make` in `code/`; the years are
`JOURNAL_YEARS` in the Makefile and `journal_years` in the script.

The Journals print each amendment's boundary but, before July 9, 2008, no address (and aldermen's amendments
rarely one after), so every amendment is placed from its boundary, the same way in every year. The streets the
boundary names are matched to the City's 2013 street centerlines (`tasks/download_chicago_gis_layers`); the corners
are where two of them meet. Three or more corners bound a block, and the amendment is placed at their mean. Otherwise
it is placed off its corners on the parcel's side, which the boundary gives ("a line 131.20 feet north of and parallel
to West Cermak Road", "the alley next west of and parallel to South Halsted Street"), since streets are often ward
boundaries. The script's header gives the rules. The ward is the one containing the point on the ward map in force
at introduction: the wards redrawn in 1998 (`data_raw/Chicago_Wards_1998.geojson`) until the council term that began
on May 5, 2003, then the 2003 map (`data_raw/Wards_2014.geojson`) until May 2015. An alderman's amendment not placed
takes the filing ward of its heading. The alderman is the one serving the ward that day
(`tasks/create_alderman_data/adjudication/alderman_terms.csv`).

Columns: the streets matched, the number of corners, how the amendment was placed (`block`, `corner_and_side`, or
`corner` where the boundary gives no side) or why not (`unplaced`: `fewer_than_two_streets`, `streets_do_not_meet`,
`corners_apart`), longitude and latitude, the filing ward (aldermen's amendments), the placed ward, the ward and its
source, and the alderman.

## Counts (September 29, 2026)

Of 6,924 introductions, 6,434 are placed (4,124 of 4,475 applications, 2,310 of 2,449 aldermen's amendments): 773 as
blocks, 5,461 off a corner on the parcel's side and 200 at a corner. 490 are not placed: 226 name fewer than two
centerline streets (metes and bounds, railroad rights of way, OCR-garbled names), 194 name streets that do not meet
and 70 have corners far apart. 140 aldermen's amendments take their filing ward. 359 applications have no ward: 351
not placed and 8 placed where two of the map's ward polygons overlap. 3 applications fall in wards without an alderman
that day (the 26th in June 2009 and the 1st in February 2010).

## Validation

- Against the parcels themselves (`tasks/audits/zoning_record_validation`): for 1,684 passed ordinances whose
  application number, passage date and district class match a district on the City's zoning map (almost all from 2004
  on; the map carries few amendments' numbers before), the placed point is inside the parcel for 1,205 and within 100
  feet for 1,563; 99 percent are within 476 feet, and 3 are farther than 1,500 feet. The ward agrees for 1,444 of the
  1,448 parcels that lie in one ward, and for 213 of the 232 that span two.
- Aldermen's amendments are filed under the alderman's ward: the placed ward is the filing ward for 2,181 of 2,307
  placed: 95 to 99 percent a year in 2000, 2001 and 2004--2011, but 85 percent in 2002 and 91 in 2003. The Council
  redrew the wards on December 19, 2001, for the council term that began on May 5, 2003, and from May 2002 aldermen
  filed amendments in the wards they would represent: where the two maps differ, the filing ward is the 1998 map's for
  30 of 33 placed amendments introduced from September 2001 to March 2002 and the 2003 map's for 36 of 46 from May
  2002 to April 2003. The ward and alderman here follow the map in force. For eLMS amendments of 2010--2026 whose
  title address is geocoded, the geocoded ward is the filing ward for 93.4 percent
  (`tasks/assign_zoning_amendment_wards`).

Known gaps: 8 percent of applications have no ward; in the year before May 2003 an application's alderman is the
sitting one, though aldermen were already filing for the wards they would represent; a long or irregular site is
placed at its main group of corners, which may lie at one end of it. Consumer: none yet.
