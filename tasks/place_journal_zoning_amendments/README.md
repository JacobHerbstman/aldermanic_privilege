# Place, ward and alderman of the Journals' zoning map amendments

`place_journal_amendments.R` places each zoning map amendment introduced in the City Council Journals of 2000--2011
(`tasks/parse_journal_zoning_amendments`) and assigns its ward and alderman, in
`output/journal_amendment_places.csv` (one row per introduction). Run `make` in `code/`; the years are
`JOURNAL_YEARS` in the Makefile and `journal_years` in the script.

The Journals print each amendment's boundary but, before July 9, 2008, no address (and aldermen's amendments rarely
one after), so every amendment is placed from its boundary, the same way in every year. The streets the boundary names
are matched to the City's 2013 street centerlines (`tasks/download_chicago_gis_layers`), first with the street types
printed; an amendment not placed that way is tried again with the through-street types (street, avenue, boulevard,
parkway, road, highway, expressway) taken as one, since a street changes type along its length (Diversey Avenue
becomes Diversey Parkway east of Western) and the Journals misprint types ("South Sangamon Avenue" for Sangamon
Street). The corners are where two of the streets meet. An amendment still not placed is placed at its common address
(printed from July 2008) on the centerlines, by house-number range, or else with one street's printed direction
repaired, if exactly one repair makes the named streets meet ("North Halsted Street" at West 33rd Street is South
Halsted). Three or more corners bound a block, and the amendment is placed at their mean. Otherwise it is placed off
its corners on the parcel's side, which the boundary gives ("a line 131.20 feet north of and parallel to West Cermak
Road", "the alley next west of and parallel to South Halsted Street"), since streets are often ward boundaries. The
script's header gives the rules. The ward is the one containing the point on the ward map in force at introduction:
the wards redrawn in 1998 (`data_raw/Chicago_Wards_1998.geojson`) until the council term that began on May 5, 2003,
then the 2003 map (`data_raw/Wards_2014.geojson`) until May 2015. An alderman's amendment not placed takes the filing
ward of its heading. The alderman is the one serving the ward that day
(`tasks/create_alderman_data/adjudication/alderman_terms.csv`).

Columns: the streets matched, the number of corners, how the amendment was placed (`block`, `corner_and_side`, or
`corner` where the boundary gives no side) or why not (`unplaced`: `fewer_than_two_streets`, `streets_do_not_meet`,
`corners_apart`), longitude and latitude, the filing ward (aldermen's amendments), the placed ward, the ward and its
source, and the alderman; `street_kinds` records whether the printed types (`printed`), the through-street types taken
as one (`through`) or a repaired direction (`direction_repaired`, with `direction_repair`) placed the amendment;
`place_source` is `common_address` for one placed at its address. `redrawn_ward` and `redrawn_alderman` assign the
introductions of December 19, 2001, when the Council adopted the 2003 map, to May 4, 2003, before it took effect, by
the 2003 map instead (the ward containing the point, or an alderman's filing ward, and the alderman then serving that
ward's number), since aldermen filed amendments in their new wards from about May 2002 (see Validation); for all other
introductions they are the ward and alderman. In that window 107 of 411 applications and 67 of 349 aldermen's
amendments have a different ward by the 2003 map, 23 of the applications moving from Alderman Ocasio to Alderman
Granato; 19 percent of the 106 applications whose alderman changes stalled, and 17 percent of the others.

## Counts (September 29, 2026)

Of 6,924 introductions, 6,687 are placed (4,322 of 4,475 applications, 2,365 of 2,449 aldermen's amendments): 782 as
blocks, 5,655 off a corner on the parcel's side, 203 at a corner and 47 at their common address; 129 only once the
through-street types are taken as one and 50 with a direction repaired. 237 are not placed: 105 name fewer than two
centerline streets (metes and bounds, railroad rights of way, lot descriptions, OCR-garbled names), 72 name streets
that do not meet and 60 have corners far apart. 87 aldermen's amendments take their filing ward. 163 applications have
no ward: 153 not placed and 10 placed where two of the map's ward polygons overlap. 3 applications fall in wards
without an alderman that day (the 26th in June 2009 and the 1st in February 2010).

## Validation

- Against the parcels themselves (`tasks/audits/zoning_record_validation`): for 1,763 passed ordinances whose
  application number, passage date and district class match a district on the City's zoning map (almost all from 2004
  on; the map carries few amendments' numbers before), the placed point is inside the parcel for 1,261 and within 100
  feet for 1,633; 99 percent are within 469 feet, and 3 are farther than 1,500 feet. The ward agrees for 1,516 of the
  1,520 parcels that lie in one ward, and for 218 of the 237 that span two. Of those placed only with the
  through-street types taken as one, all 35 lie within 240 feet of the parcel and the ward agrees for 30 of 31 in one
  ward; of those with a direction repaired, all 15 lie within 330 feet and agree in ward; of those placed at their
  common address, all 22 lie within 230 feet and the ward agrees for 20 of 21.
- Aldermen's amendments are filed under the alderman's ward: the placed ward is the filing ward for 2,230 of 2,362
  placed: 95 to 99 percent a year in 2000, 2001 and 2004--2011, but 84 percent in 2002 and 91 in 2003. The Council
  redrew the wards on December 19, 2001, for the council term that began on May 5, 2003, and from May 2002 aldermen
  filed amendments in the wards they would represent: where the two maps differ, the filing ward is the 1998 map's for
  30 of 33 placed amendments introduced from September 2001 to March 2002 and the 2003 map's for 36 of 46 from May
  2002 to April 2003. The ward and alderman here follow the map in force. For eLMS amendments of 2010--2026 whose
  title address is geocoded, the geocoded ward is the filing ward for 93.4 percent
  (`tasks/assign_zoning_amendment_wards`).

Known gaps: 4 percent of applications have no ward: boundaries that name fewer than two centerline streets (metes and
bounds, railroad lines, OCR-garbled names) or streets that do not meet as printed, some with a direction misprinted
("North Halsted Street" at West 33rd Street); in the year before May 2003 an application's alderman is the sitting
one, though aldermen were already filing for the wards they would represent; a long or irregular site is placed at its
main group of corners, which may lie at one end of it. Consumer: none yet.
