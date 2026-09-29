# Fresh OCR of the Journals' zoning pages

`ocr_journal_zoning_pages.R` reads again, with tesseract, the pages of a year's City Council Journals
(`tasks/download_council_journals`) that concern zoning map amendments. The PDFs' text layers
(`tasks/extract_council_journal_text`) are the City Clerk's OCR, which misreads district codes and words ("Cl-2" for
C1-2, "82-3" for B3-2, "RMS" for RM5, "Applicafion"). A page is selected if its text layer mentions a zoning
amendment ("to classify as", "Zoning Reclassification", "Reclassification Of Area", "Committee on Zoning", "changing
all"), read without spaces so that a letter-spaced text layer ("to classify a s", in 2001) counts, together with the
page after it, where entries run on. Each selected page is rendered at 300 dpi (pdftoppm)
and read by tesseract, one page per worker. Run `make` in `code/`; the years are `JOURNAL_YEARS` in the Makefile.
Output: `output/journal_zoning_ocr_<year>.parquet`, one row per page (file, page, meeting date, whether the page
itself mentions an amendment, and the text). Requires poppler and tesseract (run with tesseract 5.5.1 and pdftoppm
26.04.0).

| Year | Pages | Mentioning an amendment | Following | Meetings | Under 200 characters |
| --- | ---: | ---: | ---: | ---: | ---: |
| 2000 | 812 | 634 | 178 | 14 | 4 |
| 2001 | 763 | 597 | 166 | 15 | 0 |
| 2002 | 783 | 602 | 181 | 14 | 0 |
| 2003 | 743 | 592 | 151 | 14 | 1 |
| 2004 | 930 | 738 | 192 | 14 | 1 |
| 2005 | 1,037 | 818 | 219 | 14 | 0 |
| 2006 | 1,060 | 850 | 210 | 14 | 0 |
| 2007 | 874 | 688 | 186 | 15 | 2 |
| 2008 | 622 | 471 | 151 | 14 | 10 |
| 2009 | 567 | 425 | 142 | 14 | 13 |
| 2010 | 542 | 381 | 161 | 15 | 11 |
| 2011 | 907 | 635 | 272 | 15 | 2 |

The pages under 200 characters are maps, aerial photographs and site plans attached to ordinances, the cover pages of
the Journals' Appendix B code conversion table, and a few legislative-index and continuation pages. On 2008's zoning
entries, tesseract misreads less than the text layer but differently ("ail" for "all", "|" or "!" for the map letter
I, "Reciassification"); the parser allows for both (`tasks/parse_journal_zoning_amendments`, its consumer).
