# Text of zoning map amendment legislation

`extract_legislation_text.R` reads every legislation file downloaded by `tasks/download_elms_matters` (9,341 files
for 6,729 zoning map amendments) and saves one row per file with its text, page count and how it was read, in
`output/zoning_legislation_text.parquet`. Run `make` in `code/`. Requires poppler (`pdftotext`, `pdftoppm`,
`pdfinfo`) and tesseract on the PATH.

PDFs are read from their text layer. A PDF averaging fewer than 100 characters of text a page is a scan: each page is
rendered at 300 dpi and read by tesseract. Nearly all files from mid-2023 on, when the City Clerk moved to its new
system, are scans (1,779 files); earlier files carry text layers, themselves produced by OCR, so district codes
contain OCR errors that the cleaning task repairs. The two Word documents are read from their XML and the one image
is read by OCR. Pages are separated by form feeds.
