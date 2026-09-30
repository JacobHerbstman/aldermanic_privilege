# Project size from zoning map amendment applications

The size of the project each application describes, from its application form. Run `make` in `code/`.

- `ocr_application_forms.R` selects, from the text layer of every eLMS zoning matter's files
  (`tasks/extract_zoning_legislation_text`, files from `tasks/download_elms_matters`), the application form's pages
  ("Present Zoning", "Lot Size", "Be Specific", ...), the page after each, where the proposed use runs over, and any
  narrative or data-table page ("Narrative", "Data Table", "Bulk Regulations"). The text layers are the City Clerk's
  OCR; each selected page is rendered at 300 dpi and read again by tesseract. Output:
  `output/application_form_pages.parquet`, 20,787 pages (13,272 form pages, 7,515 data pages) of 4,413 matters, with
  both texts. 375 pages read as nearly empty (under 100 characters); nearly all are blank in the text layer too, and
  6 have more than 500 characters of text layer.
- `ocr_attached_applications.R` reads the application files eLMS attaches separately from 2023
  (`tasks/download_elms_matters`): each application's form ("Application.pdf") and its narrative and plans. They are
  scans without a text layer, so every page of a form, and the first three pages of a narrative, are read by tesseract
  at 300 dpi, and the pages kept are selected from that text by the same markers. Output:
  `output/attached_application_pages.parquet`, 4,392 pages (1,777 form pages) of 2,212 files of 1,032 matters.
- `parse_application_forms.R` reads, from both page files, for each record, the form's item 10 (lot size) and item 13
  (the proposed use: dwelling units, parking spaces, commercial square footage and building height, and stories where
  given), and for a Type 1 application whose form omits one, the same field from its narrative ("Lot Area 4,960 Square
  Feet", "Building Height 27 Feet"). Output: `output/application_form_fields.csv`, one row per record, with each
  value's source and the text it was read from. The script's header gives the reading rules: several counts keep the
  largest, parking listed in parts in one sentence is added up unless the sentence gives the total, "no commercial
  space" is 0, heights are whole feet as printed, an existing building that remains is the building after the
  rezoning, and a lot under 500 square feet is an OCR misreading left blank (`form_unreadable`).

Of the 5,402 records, 5,287 have form pages. Values are read for 84 percent (lot size), 63 (dwelling units), 65
(parking), 30 (commercial space), 51 (height) and 51 (stories); a blank means the form does not state the field or
the parser could not read it. 1,234 records name a Type 1 application. Planned developments' bulk tables are not
read. For applications of 2024--2026, read from the attached forms, lot size is read for 77--83 percent a year and
dwelling units for 68--73 percent, as in earlier years.

Two readings were corrected on September 30, 2026, when the attached forms were added. A form page that also mentions
a narrative ("more detail noted within the Type 1 narrative", in the forms from 2023) had been taken for a narrative
page and left out of the form; it is now read as the form, which adds lot sizes for 28 earlier records, dwelling units
for 20, heights for 14 and stories for 18, and changes one (O2017-7022, 6 units on the form rather than 12 from its
narrative). And the forms from 2023 add an item 14 on the relief a Type 1 application may include, which names a Type
1 application on every form; item 13 now ends at it, and it is left out when finding Type 1 applications.

The same day an audit of implausible values (unit counts equal to the property's house number, densities above 600
units an acre, lots above 2 million square feet, heights inconsistent with stories, and others; 195 records flagged,
each read against its text) found nine kinds of misreading, now corrected by the rules in the script's header: a
four-digit area with three decimals read in the millions ("2485.625 square feet" as 2,485,625, from the repair of a
period read for a comma), a part of the lot in parentheses taken for the lot, the first two sides of an irregular lot
multiplied, areas and dimensions a thousandfold apart, the property's house number read as its lot or its units ("The
8237 unit will be a retail liquor store"), unit counts abbreviated or misread ("4 D.U.", "dwetting units"), a number
of single-family homes read as one, a narrative's lot area per unit read as a count ("Dwelling Units: 550 sq. ft. per
DU"), and a height whose inch mark OCR read as a foot mark ("47'-2'" as 2). The corrections changed or added lot sizes
for 107 records (42 left blank as unreadable), dwelling units for 82, heights for 3 and one commercial area; the audit
flags 174 records after them, most of them large planned developments whose values are right. The per-building
products the rules do not form ("fourteen (14) six (6) unit residential buildings") remain.

## Hand checks

`adjudication/application_form_reads.csv` records 60 applications filed by applicants, 5 drawn from each year of
introduction and 60 of those at random, as read from images of their items 10 and 13 by readers who saw only the
images; 8 were read again against the images, and all agreed. `clear` marks the 36 whose reading is unambiguous
(the other 24 describe several buildings or lots, caps such as "not more than 400 units", or uses whose commercial
area is unclear). The parser stops if, on a clear record, any value it reads from the form differs from the hand
read, except where the OCR text itself misreads the printed value (`ocr_misread`: two lot sizes, 39,360 read as
"49,460" and 6,250 as "8,250").

Against all 60 reads:

| Field | Read by hand | Parser agrees | Parser blank | Parser differs | Parser reads a value the hand read leaves blank |
| --- | ---: | ---: | ---: | ---: | ---: |
| Lot size | 60 | 51 | 5 | 4 | 0 |
| Dwelling units | 45 | 39 | 5 | 1 | 0 |
| Parking spaces | 48 | 40 | 6 | 2 | 0 |
| Commercial space | 27 | 19 | 8 | 0 | 0 |
| Height | 34 | 27 | 6 | 1 | 2 |
| Stories | 27 | 22 | 4 | 1 | 1 |

The four lot sizes that differ are OCR misreadings of handwritten or underlined figures ("1®-883" for 16,883,
"291750" for 2,517.50). The other differences are on unclear records (several buildings or lots described together,
a new garage behind a taller existing building, townhomes each with a two-car garage), except one clear record's
height, read from its Type 1 narrative, which the readers did not see.

## Holdout

The parser was written against these 60 reads, so its agreement with them is optimistic. On 40 other applications
read by hand (`tasks/audits/zoning_record_validation`), where the form states the value, it reads the lot size
correctly for 34 of 37 (never a wrong figure, but it multiplies the first two sides of 2 irregular lots whose area
the form does not give), stories for 24 of 29, height for 17 of 23, parking for 22 of 34, dwelling units for 17 of
28 and commercial space for 10 of 19. Of its 20 differences from the hand reads, 8 are fixable errors (bicycle
spaces counted as parking, "9 single family homes" read as one unit, irregular lots, a space "including one
handicapped space" added to the total, a height marked with a degree sign); the rest are counts per unit or per
building, figures spread over several buildings or sentences, and before-and-after figures in one answer.
