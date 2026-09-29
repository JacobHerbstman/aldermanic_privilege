# Text of the City Council Journals

`extract_journal_text.R` reads every page of a year's Journals (`tasks/download_council_journals`) from the PDFs'
text layers with pdftotext, once in reading order (`text`, for the proceedings) and once keeping the physical layout
(`layout_text`, for the two-column legislative indexes), in `output/journal_pages_<year>.parquet`, one row per page.
Run `make` in `code/`; the years are `JOURNAL_YEARS` in the Makefile. Requires poppler (`pdftotext`, `pdfinfo`).

Each page carries its meeting's date as the Journal prints it at the head of its pages ("7/28/2011 REPORTS OF
COMMITTEES"), or on the cover of a short special meeting printed without running heads. The City Clerk's listing
labels three Journals with another date (7/08/2011 for July 28, 2011; 11/07/2010 for November 17, 2010; 10/21/2011
for October 12, 2011); `label_date` keeps the listing's. The printed dates agree with eLMS's passage dates
(`tasks/audits/zoning_record_validation`).

Pages: 26,715 in 2000, 28,212 in 2001, 24,703 in 2002, 22,851 in 2003, 24,076 in 2004, 27,033 in 2005, 28,471 in 2006,
28,855 in 2007, 34,295 in 2008, 32,053 in 2009, 27,753 in 2010 and 28,726 in 2011, one row for each page of a PDF's
declared page count (pdfinfo). The text layers are the Clerk's OCR and carry its errors ("Applicafion", "Cl-2" for
C1-2), so the zoning pages are read again by `tasks/ocr_council_journal_zoning_pages`. Consumers: that task, which
selects the zoning pages from this text, and `tasks/parse_journal_zoning_amendments`, which reads the text layer for
any page not read again.
