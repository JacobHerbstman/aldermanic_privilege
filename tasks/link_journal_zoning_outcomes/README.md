# Outcomes of zoning map amendments in the City Council Journals

`link_journal_outcomes.R` links each zoning map amendment ordinance printed in a report of the Committee on Zoning in
the Journals of 2000--2011 (`tasks/parse_journal_zoning_amendments`) to the earlier introduction it resolves, and
gives each introduction its outcome. Run `make` in `code/`; the years are `JOURNAL_YEARS` in the Makefile and
`journal_years` in the script.

- `output/journal_ordinance_links.csv`: one row per ordinance, with the introduction it resolves and how it was
  linked (`link_basis`), the introductions its record number and application number point to, and its best boundary
  candidate (`boundary_introduction`, `boundary_similarity`) whatever its link, so that the boundary rule can be
  compared with the numbers.
- `output/journal_zoning_outcomes.csv`: one row per introduction, with its outcome (passed, withdrawn, placed on
  file or failed; else stalled, or pending if introduced within a year of the last meeting read), the date, the
  days from introduction to passage, the ordinance that decided it and how that ordinance was linked, the number of
  ordinances linked to it, and its application number, printed with the introduction or taken from a linked
  ordinance (`application_source`).

An ordinance is linked, in order, by record number (printed with both from November 2010), by application number
(printed with applications from April 22, 2009), by boundary (the share of words the two boundaries have in common,
at least 0.4, among introductions by the same kind of filer whose districts before share a district with the
ordinance's; an introduction whose districts before differ only if its boundary is nearly the same), and, for an
applicant's ordinance still unlinked, by application number range. Applications are numbered in order and each
meeting's take the next run of numbers, so the ordinance's number places it between two meetings, and it is linked to
an introduction of those meetings with no known number, the same map sheet, a district before in common and a boundary
at least 0.2 alike. An introduction resolves one ordinance per meeting, and ordinances of different meetings only if
they carry the same application number (a deferral and the later passage). A report's note of a withdrawal, placing on
file or deferral is linked by application number. The script's header gives the rules in full.

Matters did not lapse with the council term that began on May 16, 2011: amendments introduced in 2009 and 2010 passed
after it, as did 48 introduced from January to May 2011. An undecided introduction is therefore called stalled only if
it was introduced at least a year before the last meeting read (December 14, 2011); in eLMS, 2.6 percent of
applications that pass take longer than a year.

## Counts (September 29, 2026)

Of 5,806 ordinances, 273 are linked by record number, 242 by application number, 4,784 by boundary and 47 by
application number range; 460 are not linked. Of those, 151 were passed in 2000, most of them on introductions of
1999, before the Journals read. Of the other 309, 190 are aldermen's amendments, many introduced directly in committee
and passed at the same meeting (the Journal's index marks them "Direct Introduction"), which have no introduction to
link; from 2001 to 2011, 1 to 19 applications a year are not linked. 15 of 35 report notes link; the 11 notes of
withdrawals of 2004 and 2007 name applications whose numbers the Journals did not yet print with introductions.

| Introduced | Applications: passed, stalled, withdrawn or failed, pending | Aldermen's: passed, stalled, withdrawn, pending |
| --- | --- | --- |
| 2000 | 260, 53, 0, 0 | 171, 80, 0, 0 |
| 2001 | 262, 57, 0, 0 | 210, 74, 0, 0 |
| 2002 | 276, 59, 1, 0 | 177, 66, 0, 0 |
| 2003 | 252, 51, 1, 0 | 237, 109, 0, 0 |
| 2004 | 463, 77, 2, 0 | 119, 100, 0, 0 |
| 2005 | 528, 98, 0, 0 | 135, 67, 0, 0 |
| 2006 | 521, 128, 2, 0 | 151, 99, 0, 0 |
| 2007 | 388, 90, 0, 0 | 150, 69, 0, 0 |
| 2008 | 235, 70, 0, 0 | 88, 50, 0, 0 |
| 2009 | 161, 43, 0, 0 | 83, 32, 4, 0 |
| 2010 | 155, 25, 2, 0 | 42, 25, 1, 0 |
| 2011 | 155, 0, 0, 60 | 69, 0, 6, 35 |

From 2000 to 2010, 14 to 23 percent of applications and 26 to 46 percent of aldermen's amendments stall. The
applications not linked could lower the applications' share by at most about 4 points a year.

## Validation (`tasks/audits/zoning_record_validation`)

- Of 515 ordinances linked by number, the best boundary candidate is the same introduction for 490 and another for 7:
  two where the Journal printed the record numbers of two aldermen's amendments of one meeting the other way round,
  two where one amendment was introduced twice and the boundary rule takes the later introduction, and three where the
  same site had been introduced years before (in 2001, 2005 and 2006) and the boundary rule takes the earlier
  introduction, whose districts before match the ordinance's. The other 18 have no boundary candidate.
- Of the 3,666 linked applications with a number, 36 lie outside the numbers of the meetings before and after their
  introductions'; applications are numbered at filing, and 16 of the 36 lie around the meeting of November 12, 2003,
  which introduced a single numbered application. Most others are sites introduced twice, linked to the earlier
  introduction (16426, passed in July 2008, to an introduction of March 2005), or misread numbers (1341 for 13411).
  Aldermen's "A-" numbers are less strictly ordered; 56 of 1,638 linked ones lie out of order.
- Against eLMS, for the 377 introductions of November 2010 through 2011 that match an eLMS amendment by record number,
  the outcome as of December 14, 2011 agrees for 375, and the date for 268 of 275 decided (eLMS dates five substitutes
  a day later and two withdrawals at the committee). Of the two disagreements, one application passed under a new
  record number that eLMS keeps separately, and one is a wrong link: an aldermen's amendment introduced directly in
  November 2011 (A-7742) linked to an introduction of the same site in May 2011.

Known gaps: where only the boundary links them (before 2009), a site introduced again after an earlier
introduction stalled can be linked to the earlier one, as for three of the 515 ordinances linked by number, and
withdrawals noted only by an alderman's "A-" number, or by the number of an application of 2008 or early
2009 that no ordinance supplies, are not linked, and those introductions count as stalled; passages after December
2011 of introductions of 2010 are not seen. Consumer: none yet.
