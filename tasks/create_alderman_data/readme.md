# Alderman Terms

The recorded terms are in `adjudication/alderman_terms.csv`; historical evidence for corrected transitions is listed below. The CSV preserves the complete date table formerly embedded in the R script, including terms after the paper period.

This task reads those dates and expands the terms
to a monthly ward panel through December 2022.

It writes:
- `output/chicago_alderman_panel.csv`
- `output/chicago_alderman_terms.csv`

## Terms through September 2026

Terms still running end on September 27, 2026, the date the current council was last checked against the City
Council's membership (Wikipedia's list of current members, which gives every member's start date). Two seats changed
after the 2023 election:

- Ward 35: Carlos Ramirez-Rosa's own site states "As of March 24, 2025, I am no longer Alderman of Chicago's 35th
  Ward" (https://www.aldermancarlosrosa.org/), so March 23 is his last day. Anthony Quezada was confirmed and sworn in
  on April 7, 2025 (Block Club Chicago and WTTW, April 7, 2025). The intervening period is left vacant.
- Ward 27: Walter Burnett Jr.'s resignation took effect August 7, 2025 (WTTW, "As Ald. Walter Burnett Officially
  Resigns From City Council"; Wikipedia), so August 6 is his last day. His son Walter Redmond ("Red") Burnett was
  confirmed and took his seat on September 25, 2025 (WTTW and Block Club Chicago, September 25, 2025). The intervening
  period is left vacant.

These terms had previously run to June 24, 2025 for every seat, including Ramirez-Rosa's after his resignation.

## Corrected historical transitions

- Ward 26: the May 13, 2009 Council journal says Billy Ocasio would resign on
  May 29. The July 29 journal records the Council's approval of Roberto
  Maldonado and his oath that day. The intervening period is left vacant.
- Ward 7: Sandi Jackson's resignation letter makes her resignation effective
  January 15, 2013, so January 14 is her last day in the term table.
- Ward 1: the January 13, 2010 Council journal records Manuel Flores attending
  and voting, while the February 10 roll call omits him. January 13 is therefore
  his last verified service day. The exact resignation date has not been
  recovered, so later days remain vacant until Proco Joe Moreno's March 26
  appointment.

Sources:

- https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public-1/reports/2009_05_13_VI_VII_VIII.pdf
- https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public-1/reports/2009_07_29_VI_VII_VIII.pdf
- https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public-1/reports/2010_01_13_VI_VII.pdf
- https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public-1/reports/2010_02_10_VI_VII.pdf
- https://news.wttw.com/sites/default/files/Ald.%20Sandi%20Jackson%27s%20Resignation%20Letter%20to%20the%20Mayor_0.pdf
