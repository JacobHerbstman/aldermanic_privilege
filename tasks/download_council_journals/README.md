# City Council Journals of the Proceedings

`download_council_journals.R` downloads, for one year, the Journal of the Proceedings of the Chicago City Council:
one PDF per meeting, regular and special, as listed on the City Clerk's Journals page
(https://www.chicityclerk.com/legislation-records/journals-and-reports/journals-proceedings). Run `make` in
`code/`; the years are `JOURNAL_YEARS` in the Makefile. PDFs go to `output/journals_<year>/`, named as at the source
with spaces replaced by underscores, and `output/council_journal_files_<year>.csv` records each meeting's date and
label, URL, file, bytes, SHA-256 and retrieval date. A file is kept only if it downloads completely and starts with
the PDF signature. The source is live, so a rerun is a deliberate refresh.

The listing shows 25 Journals a page, and all its pages are read. A Journal's date is its label's, or for the older
Journals labeled only by their file names, the file name's ("112801.pdf", "1985_12_11_VI_VII.pdf", "21093.pdf"); the
Journals' own printed dates are read from their pages downstream. The listing has years from 1977, but those before
1981 list no Journals, so `JOURNAL_YEARS` starts in 1981.

Downloaded September 28, 2026: 698 Journals of 1981--2011 (91 of them special meetings), 11.4 GB, 14 to 44 a year.
2008: 17 meetings (3 special), 594 MB; 2009: 15 (1), 519 MB; 2010: 19 (5), 456 MB; 2011: 17 (1), 491 MB. The PDFs
carry text layers, produced by OCR for the scanned pages. Consumers: `tasks/extract_council_journal_text` and
`tasks/ocr_council_journal_zoning_pages`, which read 2008--2011 so far.
