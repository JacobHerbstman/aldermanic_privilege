# Follow-up and refilings of the Journals' zoning map amendments

`follow_journal_amendments.R` follows each zoning map amendment introduced in the City Council Journals of 2000--2011
(`tasks/link_journal_zoning_outcomes`) past the last Journal read, December 14, 2011, and links each one that did not
pass to its refiling, in `output/journal_amendment_follow_up.csv` (one row per introduction). Run `make` in `code/`.

- Follow-up. eLMS (`tasks/clean_zoning_map_amendments`) holds every amendment introduced from November 2010 and the
  passage of some older ones. An introduction undecided in the Journals takes the outcome of the eLMS matter with its
  record number, else of the one eLMS matter with its application number (`follow_up_source`).
- Refilings. An amendment that did not pass is refiled by the first later amendment of the same site by the same kind
  of filer within four years (one council term, as for eLMS in `tasks/link_zoning_refilings`), whose districts
  before share a district with its own. The site is the same if a later Journal introduction's boundary shares at
  least 70 percent of its words, or at least 50 percent and the two placed points
  (`tasks/place_journal_zoning_amendments`) lie within 300 feet, or, for an eLMS matter introduced after 2011, if its
  located title address (`tasks/assign_zoning_amendment_wards`) lies within 300 feet. A later amendment also refiles
  one with the same application number, or one whose common address (printed from July 2008) shares a street segment
  with its own common address or eLMS title address (`tasks/shared/code/address_segments.R`, as for eLMS). The
  script's header gives the rules.

Every refiling chosen has been read by hand in `adjudication/journal_refiling_reviews.csv` (one row per pair, with a
decision and a reason), and the build stops if one has not; a pair judged to be different projects is not a refiling,
and the next candidate is considered. The 180 pairs of the first build were read by two readers independently, from
both amendments' names, dates, maps, districts, boundaries and addresses; they agreed on 170. The 10 they disagreed
on, one they agreed on that the addresses contradict, and the 16 pairs later builds added were decided in adjudication
from the same fields ("Adjudicated" in the reason). A later amendment covering none of the earlier site (an adjoining
strip, the other side of the street), a much larger or smaller area by another applicant, or the opposite change is a
different project. 160 of the 195 pairs reviewed are the same project (one reviewed pair ceased to be a candidate when
a placement moved, and its row was removed). Reviewed September 29, 2026.

## Counts (September 29, 2026)

Of 6,924 introductions, 1,617 were undecided in December 2011; eLMS gives 103 of them a later outcome (100 by record
number, 3 by application number): 68 passed and 35 never did. All 95 still pending then match eLMS. Of the 1,568 that
never passed, 153 were refiled (73 applications and 80 aldermen's amendments), a median of 462 days later for an
application and 224 for an aldermen's amendment; 112 of the refilings passed. 141 refilings were found by boundary or
place alone, 12 by address (9 of them also by boundary or place).

| Introduced | Applications stalled | Net of refilings that passed | If refiled as often as in eLMS |
| --- | ---: | ---: | ---: |
| 2000 | 16.9% | 16.6% | 12.9% |
| 2001 | 17.9% | 15.4% | 13.6% |
| 2002 | 17.6% | 15.5% | 13.4% |
| 2003 | 16.8% | 14.5% | 12.8% |
| 2004 | 14.2% | 13.8% | 10.8% |
| 2005 | 15.7% | 14.9% | 12.0% |
| 2006 | 19.7% | 18.4% | 15.0% |
| 2007 | 18.8% | 18.2% | 14.3% |
| 2008 | 23.0% | 22.0% | 17.5% |
| 2009 | 19.6% | 17.6% | 14.9% |
| 2010 | 13.2% | 12.1% | 10.1% |
| 2011 | 8.4% | 7.0% | 6.4% |

A stalled application is one never passed, by December 2011 or, where eLMS follows it, since. The last column assumes
that 24 percent of stalled applications were refiled and passed, as in eLMS (79 of 332 stalled applications of
2010--2022, `tasks/link_zoning_refilings`). For 2011 the Journals and eLMS agree: 18 of the Journals' 215
applications never passed, 8.4 percent, the eLMS rate for applications introduced in 2011.

## Validation

For the 6 eLMS refilings of the Journals' amendments of November 2010 to 2011 (address-based and read by hand in
`tasks/link_zoning_refilings`), this task finds the same refiling for the 4 whose amendment stalled in the Journals;
the other 2 the Journals record as passing, linked to the earlier introduction. Before the addresses were matched it
found only 1 of the 4: the three it missed are large sites whose boundaries and placed points differ more than a small
site's would. Before July 2008 the Journals print no address, so refilings of large sites are likely missed then; the
last column above bounds what those misses could do to the stall rate.

Consumer: none yet.
