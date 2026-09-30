# Validation of the Journal and application-form parsers (audit)

Eight checks of `tasks/parse_journal_zoning_amendments`, `tasks/link_journal_zoning_outcomes`,
`tasks/place_journal_zoning_amendments` and `tasks/extract_zoning_application_forms`, run September 28 and 29, 2026.
Run `make` in `code/`. The hand reads in `adjudication/` were made by readers who saw only page images, drawn with
fixed seeds by the scripts here (the stall search's sample is kept as drawn, section 7); the reads of 14 Journal
entries (on 6 pages) and of 6 forms were read again against the images, and all agreed.

## 1. Journal actions against eLMS

`check_introductions_against_elms.R`, `check_passages_against_elms.R` and `check_withdrawals_against_elms.R`. From
November 2010 through 2011, 377 of the Journals' 381 introductions match an eLMS amendment by record number, and 377
of eLMS's 399 amendments introduced then are found in the Journals; dates agree for all 377, application numbers for
254 of 255, map sheets for 336 of 340, districts before for 283 of 314 and after for 278 of 305. Of the 22 eLMS
amendments not found, 10 are older matters eLMS dates at their passage, and most others are aldermen's amendments
introduced directly in committee, two text amendments, a pedestrian-street designation and duplicate or renumbered
records. Every eLMS passage of the period is found among the Journals' passed ordinances: 301 match (299 by record
number, then by application number, then by meeting date and map sheet where that pair is unique on both sides), and
the other two eLMS records duplicate matched ones. Dates agree for 291 of 301 (eLMS dates the other 10, substitutes
and aldermen's amendments, a day after the meeting), map sheets for 267 of 269, districts before for 242 of 249 and
after for 262 of 271. Of the Journals' 8 withdrawals, 7 match an eLMS amendment and eLMS records all 7 as withdrawn;
the eighth is noted in a report without its ordinance. eLMS has one more withdrawal in the period, a text amendment
its cleaning counts as a map amendment. 37 passed ordinances have no eLMS match, most of them on introductions
before November 2010 that eLMS lacks.

The first runs of these checks (September 28, 2026) found faults since fixed: the City Clerk's listing dates three
Journals wrongly (`tasks/extract_council_journal_text` now takes each Journal's date from its pages, which brought
the introduction dates into full agreement from 337 of 372); ordinances the committee reports on their own, printed
under the committee's title rather than the usual heading, were not read (1, 12, 10 and 15 in 2008--2011); the parser
took every ordinance for a passage, though the reports also defer and withdraw them; and an ordinance whose heading
leaves out "On", introductions whose clause reads "to 'classify as", and record numbers set after a stray mark or
closed with ")}" were not read. The passages were first matched by application number and by meeting date and map
sheet only, which left 57 unmatched on each side, nearly all aldermen's amendments.

## 2. Random samples of Journal pages (`draw_journal_sample.R`, `score_journal_sample.R`)

Pages are drawn by their OCR text rather than by the parser's output. The first sample has 8 introduction pages and 8
ordinance pages of 2008 and of 2009: 54 introductions and 30 ordinances begin on them. The parser finds every one and
no others. Of the ordinances it reads the application number of all 30, the map sheet of 29 (the other's two readings
conflict), and the districts before and after of all 29 whose districts show. Of the introductions it reads the filer,
ward, application number and district before of all 54, the map sheet and district after of 53, the record number of
53, the common address of 50 (4 with an OCR digit or ordinal misread) and the name of 52 (two with a misread letter).

The first reading of the sample found, besides these, two ordinances whose second step is to a planned development
printed without a number, one ordinance not read at all, two aldermen's amendments credited to the alderman above
because the heading prints "O’Connor" with a curly apostrophe, and applicants' names beginning with the previous
entry's record number where OCR spaced it or set it after dot leaders; each cause is fixed. The 2008--2009 readers
were not asked to mark codes of the 1957 ordinance, and one ordinance of 2009 printed with "an R2 Single-Family
Residence District", which the parser now codes 1957:R2, scores as differing.

The second sample, drawn after the parser was extended to 2000--2007 with its own seed, has 4 introduction pages and 4
ordinance pages of each of those years; its readers coded each district as of the 1957 or the 2004 ordinance by the
name printed with it, or by the date where none is. 111 introductions and 55 ordinances begin on its pages, and the
parser finds every one and no others. Of the introductions it reads the filer, ward and districts before and after of
all 111, the map sheet of 110 (OCR reads the other's J as "]", which stands for I as often, so it is left blank) and
the name of 107 (three with a misread letter or mark, and one begun with the last line of the previous boundary, which
OCR set after its period). Of the ordinances it reads the application number and map sheet of all 55, the district
after of all 54 whose district shows and the district before of 53 (OCR reads the other's "R4" as "R¢"). Two readers
left four aldermen's amendments without their alderman where the heading was on the page before; the alderman was read
from that page in adjudication, as noted in the reads. The first scoring of this sample found 11 other disagreements,
all general parser faults since fixed: a period after a lettered street ("South Avenue M.") taken for an initial's, so
that the name began in the previous boundary (2); a stray mark after an applicant's dash (1); "Map Number11-L" without
a space (2); an article run into the code ("aC1-1", 2); and an ordinance change written "symbol and indications on Map
Number", "indications shown on", ending "District, SECTION 2" or, in a second step, "symbols in the area described in
Section 1 to an Institutional Planned Development" (4).

## 3. A holdout sample of application forms (`draw_form_holdout.R`, `score_form_holdout.R`)

40 applications not among the 60 the parser was written against, 3 from each year of introduction. Readers were
given the parser's documented reading rules. The clear flag is not reliable here: one reader marked 5 of 20 forms
unclear and the other 16 of 20, counting caps such as "will not exceed 58 feet".

| Field | Form states a value | Parser agrees | Parser blank | Parser differs | Parser reads a value the form does not state |
| --- | ---: | ---: | ---: | ---: | ---: |
| Lot size | 37 | 34 | 3 | 0 | 0 |
| Dwelling units | 28 | 19 | 6 | 3 | 0 |
| Parking spaces | 34 | 22 | 4 | 8 | 0 |
| Commercial space | 19 | 10 | 8 | 1 | 0 |
| Height | 23 | 17 | 5 | 1 | 0 |
| Stories | 29 | 24 | 4 | 1 | 2 |

One commercial area, given as a range, is not scored. The table is as of September 30, 2026, after the corrections of
the parser that day (the first scoring had 17 agreeing unit counts and 5 differing, and 2 lot sizes read from
irregular lots whose forms state none). Of the 16 differences, including values the form does not state, 4 are general
parser errors: bicycle spaces counted as parking (2), a space "including one handicapped space" added to the total
(1), and a height printed with a degree sign for the foot mark (1). The other 12 are counts per unit or per building
the parser does not multiply ("each home will receive two spaces"), figures for several buildings or given in separate
sentences, before-and-after figures in one answer, an OCR misreading ("2 4 -story" for 2 1/2-story), a live/work unit,
and two existing buildings that remain, which the reading rules count and the readers did not.

## 4. eLMS coverage before November 2010 (`check_elms_coverage.R`)

The eLMS index holds 0 to 74 matters of any kind a month from January to October 2010 and 938 to 2,513 a month
afterwards. None of the 185 record numbers the Journals print for introductions in January--October 2010 is in the
index, as a record number or a legacy record number; from November 2010, 377 of 380 are. Queried directly on
September 28, 2026, the eLMS API returned no matter for PO2010-1693, O2010-1693, PO2009-6084 or O2009-6084 under
either number, and only one matter without an introduction date, so none are hidden by the month-by-month download.
eLMS itself lacks the City's zoning amendments before November 2010.

## 5. Links between ordinances and introductions (`check_journal_links.R`, `check_outcomes_against_elms.R`)

The links of `tasks/link_journal_zoning_outcomes`, checked three ways. Of 515 ordinances linked by record or
application number (all from 2009 on), the best boundary candidate is the same introduction for 490 and another for 7:
two where the Journal printed the record numbers of two aldermen's amendments of one meeting the other way round, two
where one amendment was introduced twice and the boundary rule takes the later introduction, and three where the same
site had been introduced years before (in 2001, 2005 and 2006) and the boundary rule takes the earlier introduction;
18 have no boundary candidate. Applications are numbered at filing, and each meeting's take the next run of numbers,
so a linked application's number should lie between the median numbers of the meetings before and after its
introduction's: 3,647 of 3,682 do. 16 of the other 35 lie around the meeting of November 12, 2003, which introduced a
single numbered application, and most others are sites introduced twice and linked to the earlier introduction or
misread numbers. Aldermen's "A-" numbers are less strictly ordered; 56 of 1,645 lie out of order. Against eLMS, the
outcomes as of December 14, 2011 of the 377 introductions of November 2010 through 2011 matched by record number agree
for 375, and the dates for 268 of 275 decided (eLMS dates five substitutes a day later and two withdrawals at the
committee); the two disagreements are an aldermen's amendment introduced directly in November 2011 (A-7742) and linked
to an introduction of the same site in May 2011, and an application that passed under a new record number, which eLMS
keeps as a separate matter.

While the linking rules were written, a run with every introduction's printed application and record number hidden
linked 421 of the 433 applicants' ordinances whose introductions the printed numbers identify, all correctly, and
left the other 12 unlinked; the rules were not changed after it.

## 6. Places of the Journals' amendments against the zoning map (`check_journal_places.R`)

The City's zoning map, as in force now and in November 2012 (`tasks/download_chicago_gis_layers`), records for each
district set since about 2002 the application number and passage date of the amendment that set it. A passed Journal
ordinance whose number matches a district passed within 3 days of its meeting, in a class the ordinance creates, has
that district as its parcel: 1,777 ordinances whose introductions are placed. The class must agree because the map
files the odd district under another amendment's number (map number 16849 of June 30, 2009 is a B3-5 district in the
5th Ward; the Journal's ordinance 16849 of that day rezones a parcel on map sheet 11-G to RT-4). The placed point is
inside the parcel for 1,274, within 100 feet for 1,646 and within 468 feet for 99 percent; 3 are farther than 1,500
feet. The placed ward, on the ward map in force at introduction, is the parcel's for 1,528 of the 1,532 parcels in one
ward and for 220 of the 239 that span two.

While the placement was written, it was also compared with the common addresses the Journals print from July 2008,
geocoded from the centerlines' address ranges: the median distance was 126 feet, but a geocoded address lies on the
street's centerline, often a ward boundary, so the addresses settle wards less well than the parcels do.

## 7. A blind search for what became of stalled applications (`draw_stall_search.R`, `score_stall_search.R`)

Whether an application recorded as stalled in fact passed was tested without the parser or the links. 40 applications
of 2000--2008 then recorded as stalled were drawn at random, with 10 recorded as passed as controls
(`adjudication/stall_search_sample.csv`, kept as drawn on September 29, 2026). For each, every later page of the
Journals through 2011 was searched in its own text for the application's measured distances, streets, map sheet and
applicant's name, and the four best-matching pages, with the page before each, were given to ten readers who saw only
these images, the introduction's own pages and the applicant's name, not which applications were controls
(`adjudication/stall_search_reads.csv`; two reads marked as ordinances whose readers' notes say a different site are
corrected). The readers found the passage of 8 of the 10 controls; of the other two, the page found was deep in a
planned development's exhibit, and the passed ordinance was amended with another street name (Drake for Monticello,
the same distances).

Of the 40 recorded as stalled, 31 had no later passage, and 2 passed only through a later introduction that the
records hold as their refiling. 5 had passed and were wrongly recorded: two whose ordinances OCR printed "area-bounded
by" or "bounded. by", so that no boundary was read to link them; one on a page tesseract split into two columns,
breaking its ordinance heading; and two amendments of the Public Building Commission printed twice at one meeting,
once as its application and once as the alderman's, whose ordinance was linked to the alderman's printing. Each cause
was fixed in general (`tasks/parse_journal_zoning_amendments`, `tasks/ocr_council_journal_zoning_pages`, the twin rule
of `tasks/link_journal_zoning_outcomes`), which turned 32 introductions of 2000--2011 from stalled to passed; the five
are now recorded as passed. Two remain open: an application whose project passed later on an enlarged site, which the
refiling rules count as a different project, and one whose boundary a later ordinance only partly repeats. The five
were found in this sample and fixed from it, so the sample does not measure the rate that remains; a fresh draw would.

## 8. A holdout sample of the forms attached from 2023 (`draw_attached_form_holdout.R`, `score_form_holdout.R`)

From 2023 eLMS attaches each application's form as a separate scanned file, read by
`tasks/extract_zoning_application_forms/code/ocr_attached_applications.R`. 20 of these forms, 5 from each year of
2023--2026 and none among the 100 read before, were read from their page images by two readers who saw only the
images and the parser's reading rules (`adjudication/attached_form_holdout_reads.csv`); they marked 11 of the 20
clear. The first scoring found that most attached forms had been left unread: their item 14 mentions "the Type 1
narrative", which marked the page as a narrative page, and the parser now reads such a page as the form (the fix also
added values for 28 earlier records). Against the 20 reads:

| Field | Form states a value | Parser agrees | Parser blank | Parser differs | Parser reads a value the form does not state |
| --- | ---: | ---: | ---: | ---: | ---: |
| Lot size | 20 | 17 | 3 | 0 | 0 |
| Dwelling units | 12 | 11 | 0 | 1 | 0 |
| Parking spaces | 16 | 11 | 3 | 2 | 0 |
| Commercial space | 11 | 3 | 7 | 1 | 0 |
| Height | 12 | 8 | 4 | 0 | 1 |
| Stories | 10 | 9 | 1 | 0 | 0 |

A dwelling-unit cap ("up to 140") and a building of "partial two-story and partial one-story" are not scored. Four of
the five differences are on forms the readers marked unclear: a subdivision of "eight 2,500.0 sq. ft. slab on-grade
single family homes" read as one unit (the corrected parser reads a number of single-family homes, but not with a size
between), parking given for alternative build-outs and a garage counted with bicycle spaces, and commercial space for
one of two subareas of a planned development. The fifth is a height the parser took from a narrative for a
construction yard whose form states none.

## 9. eLMS districts against the zoning map (`check_elms_districts.R`)

The City's current zoning map names, for districts set since 2011, the ordinance record that set it. Of the passed
eLMS amendments, 4,316 have a record on the map (a quarter of those passed in 2011, 62 to 95 percent in each year from
2014), and 4,105 of them a district after or a change to a planned development read from their legislation text
(`tasks/clean_zoning_map_amendments`). The map has that district, or a planned development, for 98 percent
(`output/elms_districts_vs_zoning_map.csv`): 97.8 percent of the 3,042 applications to a district and 99.2 percent of
the 499 to a planned development, and 98.3 and 93.3 percent of aldermen's 534 and 30. A district may have been rezoned
again since, so these are lower bounds.

## 10. Introduced and substitute ordinances (`draw_substitute_holdout.R`, `score_substitute_holdout.R`)

A check of `tasks/compare_substitute_ordinances`. On September 30, 2026, 30 passed applications whose introduced and
substitute versions are compared on the project's floor-area ratio, dwelling units or height were drawn at random, 20
with a change and 10 without (kept in `adjudication/substitute_holdout_sample.csv`). Two readers who saw only the page
images of the versions' narratives or bulk tables and the reading rules
(`adjudication/substitute_holdout_reading_rules.md`) read each version's three values, 15 applications each, 6 of each
reader's also read by the other (`adjudication/substitute_holdout_reads.csv`); on those 12 they agreed on 62 of 63
values, the other a height printed both as 46'9" in a table and as a maximum of 47 feet in the text, where the rules'
first listed value was kept. Against the reads (`output/substitute_holdout_scores.csv`):

| Field | Values read and parsed alike | Parsed differently | Parser blank | Changes confirmed | Unchanged confirmed |
| --- | ---: | ---: | ---: | ---: | ---: |
| Floor-area ratio | 43 | 1 | 11 | 7 of 8 | 10 of 10 |
| Dwelling units | 41 | 0 | 12 | 8 of 8 | 10 of 10 |
| Height | 38 | 5 | 12 | 10 of 10 | 6 of 8 |

The floor-area ratio read differently comes from a planned development's statements (a permitted maximum of 18)
rather than its table (14.71), which made the pair a false decrease. Four of the heights read differently are inches
the parser does not read ("49 Feet 11’4 Inches", OCR for 11 1/2) or an overall height listed after a height to the
underside of the roof; two of these hide a decrease of a foot or two. The table is as of September 30, 2026, after
corrections the first scoring prompted: a number running on into further digits was read in part ("FAR 127 dwelling
units" as a ratio of 12, "2.4145" as 2.414), and "a total 19 dwelling units" was missed. The first scoring confirmed
26 of the 29 changes found and 26 of the 28 pairs found unchanged.
