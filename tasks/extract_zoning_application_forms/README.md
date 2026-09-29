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
- `parse_application_forms.R` reads, for each record, the form's item 10 (lot size) and item 13 (the proposed use:
  dwelling units, parking spaces, commercial square footage and building height, and stories where given), and for a
  Type 1 application whose form omits one, the same field from its narrative ("Lot Area 4,960 Square Feet",
  "Building Height 27 Feet"). Output: `output/application_form_fields.csv`, one row per record, with each value's
  source and the text it was read from. The script's header gives the reading rules: several counts keep the
  largest, parking listed in parts in one sentence is added up unless the sentence gives the total, "no commercial
  space" is 0, heights are whole feet as printed, an existing building that remains is the building after the
  rezoning, and a lot under 500 square feet is an OCR misreading left blank (`form_unreadable`).

Of the 4,413 records, 4,201 have form pages. Values are read for 84 percent (lot size), 60 (dwelling units), 62
(parking), 28 (commercial space), 48 (height) and 51 (stories); a blank means the form does not state the field or
the parser could not read it. 714 records name a Type 1 application. Planned developments' bulk tables are not read.

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
