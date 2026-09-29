# Zoning map amendments in the City Council Journals

`parse_journal_zoning.R` reads a year's Journal pages into three tables. Pages come from the fresh OCR of the zoning
pages (`tasks/ocr_council_journal_zoning_pages`) and from the text layer (`tasks/extract_council_journal_text`) for any
other page; every entry of 2000--2011 is on a freshly read page (`text_source`). Each page keeps its meeting's date as
the Journal prints it. Run `make` in `code/`; the years are `JOURNAL_YEARS` in the Makefile.

- `output/journal_introductions_<year>.csv`: amendments introduced and referred to the Committee on Zoning, from the
  sections headed "Referred - Zoning Reclassifications Of Particular Areas": applications, by applicant, and
  aldermen's amendments, by alderman and ward. Each has its map sheet, districts before and after and boundary; the
  common address from July 9, 2008, the Clerk's record number from December 17, 2008, and an application's number
  from April 22, 2009.
  Aldermen's amendments are never introduced with their "A-" application numbers.
- `output/journal_ordinances_<year>.csv`: every map amendment ordinance printed in a report of the Committee on
  Zoning, with the Council's action on the report: passed, deferred (and published, to be taken up at a later
  meeting under Unfinished Business), withdrawn, re-referred, placed on file or failed. Each has its application
  number (or CPC number for the Plan Commission's), record number from late 2009, whether amended, map sheet,
  districts before and after, boundary, the report's title and the ordinance's text. The map is printed in the
  heading (or title) and in the first change, and OCR misreads either, so both readings are kept (`heading_map`,
  `change_map`); `map_number` is their common reading or the legible one, and is blank where they conflict.
- `output/journal_report_notes_<year>.csv`: applications a report notes as withdrawn, deferred or placed on file
  ("Please let the record reflect that Application Number A-7371 was withdrawn by the applicant"), once per meeting.

The committee's reports follow one another, each under a title in capitals, then "The Committee on Zoning submitted
the following report", the report, the Council's motion and the ordinances, either under their own headings
("Reclassification Of Area Shown On Map Number 1-G.") or, for a report of one ordinance, under the report's title.
A report runs to the next report's title or the next section of the Journal, and a report interrupted by an exhibit
("(Continued on page 55345)") is joined to its continuation. The action is read from the report's opening
("Deferred and ordered published"), its motion ("... were Passed by yeas and nays"), the sentence that introduces
its ordinances ("The following is said withdrawn ordinance") or its title ("Withdrawn --"); the parse stops if a
report has none of them. Text amendments, appointments and other business of the committee are left out.

Page headers are removed before pages are joined. An introduction's boundary follows "bounded by:" or its printed
variants ("bounded", "bounded:", "bound by", "bounded © by:"), or failing those "as follows:" or "described as:"; 21
of 6,924 print none. The anchors of each entry allow for the misreadings of both OCRs, listed in the script ("ZONING
-RECLASSIFICATIONS _ OF PARTICULAR AREAS" on the degraded scans of 2002). District codes are read as in the zoning
ordinance with OCR confusions repaired (l or I for 1 or a doubled 1, 8 for B, S for 5, "RS General Residence" for R5,
7 for T); a planned development is coded PD. Journals before November 1, 2004 use the zoning ordinance of 1957, and
its codes are marked "1957:" (1957:R4, 1957:B4-2); Journals of 2004 and 2005 mix the two ordinances, and the script's
header gives the rule that tells them apart by date and printed district name. Map sheets are numbered 1 to 20 and 22
to 32 in even numbers, with further sheets in column B numbered past 100 (153-B), so another reading is not a sheet.
Where an amendment has several clauses, the districts before are those not produced by an earlier clause and the
districts after those not changed by a later one.

## Hand checks

`adjudication/journal_parse_checks.csv` records 54 entries as read from the printed pages, and the parser stops if
its output disagrees with any of them: map sheet, districts before and after, and the common address of an
introduction or the Council's action on an ordinance and the kind of a report's note. The first 24 (12
introductions and 12 ordinances) were drawn at random from 2008; the first reading of the text layer got 7 wrong,
each for a general cause since fixed. The next 4 are ordinances the first reading of the fresh OCR got wrong. The last
15 check actions: deferrals and their later passage, withdrawals, a report that breaks off for an exhibit, a report
printed without its opening sentence, and notes of withdrawals and deferrals. 11 more come from the random sample of
2000--2007 pages (below): districts of the 1957 ordinance, before and after the 2004 ordinance took effect, and eight
entries the first reading of those years got wrong, each for a general cause since fixed.

## Validation (`tasks/audits/zoning_record_validation`)

- A random sample of 32 pages of 2008 and 2009, read by hand: the parser finds all 54 introductions and 30
  ordinances that begin on them and no others. It reads the map and districts of 53 to 54 introductions, the map of
  29 of the 30 ordinances (the other's two readings conflict) and the districts of all 29 whose districts show.
- A second random sample, of 64 pages of 2000--2007 (4 introduction and 4 ordinance pages a year): the parser finds
  all 111 introductions and 55 ordinances that begin on them and no others. It reads the filer, ward and districts of
  all 111 introductions, the map of 110 (OCR reads the other's J as "]") and the name of 107 (four with a misread
  letter); the map of all 55 ordinances, the district after of all 54 whose district shows and the district before of
  53 (OCR reads the other's "R4" as "R¢").
- eLMS, the City's legislative database, holds few matters of any kind before November 2010 and none of the
  Journals' record numbers from those months, so the Journals are the only complete source through October 2010.
- From November 2010 through 2011, 377 of the Journals' 381 introductions match an eLMS amendment by record number,
  and 377 of eLMS's 399 amendments introduced then are found in the Journals. The dates of all 377 agree; the
  application numbers of 254 of 255, the map sheets of 336 of 340, the districts before of 237 of 267 and after of 238
  of 265. Most district disagreements are not misreadings: the eLMS cleaning records a planned development
  separately, and a request can change between its introduction, which the Journal prints, and its ordinance. Of the
  22 eLMS amendments not found, 10 are older matters eLMS dates at their passage; most others are aldermen's
  amendments introduced directly in committee, two text amendments, a pedestrian-street designation and duplicate or
  renumbered records, none of which the Journals list as map amendments introduced.
- Every eLMS passage of that period is found in the Journals: 301 match, 299 of them by record number, and the other
  two eLMS records duplicate matched ones. The dates agree for 291 (eLMS dates the other 10 a day later), the map
  sheets for 267 of 269, the districts before for 187 of 194 and after for 202 of 214. Of the Journals' 8 withdrawals,
  7 match an eLMS amendment, and eLMS records all 7 as withdrawn.

## Counts (September 29, 2026)

| Year | Introductions (applications, aldermen's) | With map, before, after | Ordinances by action | With change, map |
| --- | --- | --- | --- | --- |
| 2000 | 564 (313, 251) | 98%, 99%, 100% | 482: 474 passed, 8 deferred | 97%, 98% |
| 2001 | 603 (319, 284) | 99%, 99%, 100% | 484: 472 passed, 12 deferred | 99%, 98% |
| 2002 | 579 (336, 243) | 99%, 99%, 98% | 512: 505 passed, 6 deferred, 1 failed | 99%, 98% |
| 2003 | 650 (304, 346) | 98%, 99%, 99% | 444: 438 passed, 5 deferred, 1 failed | 97%, 98% |
| 2004 | 761 (542, 219) | 98%, 99%, 98% | 597: 592 passed, 3 deferred, 2 failed | 97%, 99% |
| 2005 | 828 (626, 202) | 98%, 99%, 99% | 710: 706 passed, 4 deferred | 97%, 99% |
| 2006 | 901 (651, 250) | 98%, 98%, 98% | 729: 723 passed, 4 deferred, 1 failed, 1 re-referred | 98%, 99% |
| 2007 | 697 (478, 219) | 97%, 99%, 99% | 618: 613 passed, 2 deferred, 2 failed, 1 re-referred | 97%, 100% |
| 2008 | 443 (305, 138) | 100%, 99%, 99% | 415: 413 passed, 2 deferred | 98%, 100% |
| 2009 | 323 (204, 119) | 98%, 99%, 99% | 301: 297 passed, 2 deferred, 2 withdrawn | 98%, 99% |
| 2010 | 250 (182, 68) | 97%, 96%, 97% | 217: 212 passed, 5 withdrawn | 97%, 100% |
| 2011 | 325 (215, 110) | 99%, 99%, 99% | 297: 289 passed, 7 withdrawn, 1 deferred | 99%, 99% |

The Journals of 2000--2008 print no withdrawn ordinance; a withdrawal then appears only in a report's note (17
notes of withdrawals in 2000--2008, none before 2004).

Known gaps: an introduction that amends a planned development without a "to classify as" clause is not read as its own
entry; districts the ordinance no longer has (RT4.5, R2), that the Journal misprints (B7-6, M-1) or that OCR leaves
illegible ("B3-?") are left uncoded; a misread application number (77385 for A-7385) is kept as read; a record number
that OCR sets after the next section's heading is not read; and three corrections of earlier Journals printed under
the same heading (two in 2008, one in the Rules Committee's report of September 9, 2009) are not ordinances of the
Committee on Zoning and are left out.
