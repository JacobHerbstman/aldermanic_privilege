# Sales-record quality audit

This task audits records and estimates explicitly labeled sensitivity checks.
Approved property cleaning lives in `tasks/prep_sales_border_data/`.
Run `make` from `code/`.

## Current resolution

The September 2 row review led to a production change on September 3, 2026.
The root alderman calendar now ends Ocasio's service on May 28, 2009, starts
Maldonado on July 29, 2009, and ends Jackson's service on January 14, 2013.
The January 13 Council journal is the last verified service date for Flores;
because his exact resignation date remains unavailable, the calendar leaves
the following days vacant until Moreno takes office.

Production now excludes an unrefined sale recorded on the first of the month
when either adjoining ward changes aldermen during that month or when the ward
map changes during the month. The rule removes 1,289 transaction-clean sales
citywide. After the existing property screens, production contains 233,699
sales. The preferred 500-foot regression uses 58,468 sales and estimates a
0.0300 log-price difference (standard error 0.0134). The material below reports
the current quality checks and summarizes the pre-correction date review that
motivated this rule.

The current findings were rebuilt on September 3, 2026. They refer to 233,744
sales after the date and structural screens and the 58,478-sale 500-foot sample
before the rooms screen.
This audit reruns the same property-data producer with the rooms rule disabled,
then reruns the same amenity-enrichment script in this task's own directory.
That reproduces the earlier panel without a copied dataset or a second cleaning
implementation. The paper never consumes these audit outputs. `in_main_rd` and
`main_rd_sales` describe that pre-screen sample; `in_current_main_rd` and
`current_main_rd_sales` describe current production eligibility. The latter
count is checked against the production property-class-FE result.

## Findings

| Issue | Broader panel | Pre-screen RD sample |
|---|---:|---:|
| Nominal price above $5,000 per building square foot | 4 | 1 |
| Above $2,000 per square foot, including the preceding cases | 24 | 10 |
| Below $5 per square foot, review only | 140 | 37 |
| Fewer total rooms than bedrooms | 45 | 10 |
| Class 211 without an explicit two-to-six apartment count | 7,419 | 2,051 |
| Unrefined dates that can change alderman or RD assignment | 0 | 0 |
| More than one actual source improvement card | 0 | 0 |

These are overlapping flags, not a proposed sequence of exclusions.
Machine-generated counts are in `output/quality_summary.csv` and
`output/date_summary.csv`.

### Prices and characteristics

The $134.9 million sale of PIN 25192170410000 in 2006 has 893 recorded square
feet. The raw sales file, production file, and current county API all contain
the same price. The building area remains 893 square feet throughout the
available history. Other recorded sales of that PIN are $25,000 in 2010 and
$115,000 in 2011. This is a severe unresolved anomaly, not evidence for a
particular replacement price. No original deed was obtained in this audit.

The four remaining above-$5,000 cases are extreme: their prices range from $134.9 million
to $225,000,001 and their price-per-square-foot values from roughly $38,136 to
$154,026. Three are outside the current RD window. The raw Chicago sales file
does not reveal multiple PINs on any retained sale document, and the source
card check finds no multicard properties in the retained panel. These checks
do not independently establish the contents of the original deed or rule out
source-level portfolio miscoding.

The ten RD room/bedroom inconsistencies are also in the source characteristics,
not introduced by our merge or numeric parsing. Examples include 24 rooms and
42 bedrooms, and four rooms and nine bedrooms. Several persist across years;
some change to coherent values later. Later values are not automatically valid
for the earlier sale. There are no negative room/bed/bath counts or future
construction years in the structurally eligible panel. Missing residence-type,
construction-quality, and repair-condition fields do not explain a difference
from the other project's sample: those fields are present for this whole panel.

Some less extreme high-price observations show a different problem: potentially
outdated building descriptions. PIN 13254290270000 sold for $1.45 million in
2022 with a sale-year description of a 680-square-foot building built in 1890.
The county's 2024 onward records describe 2,720 square feet built in 2022.
PIN 14291190090000 sold for $2.11 million in 2022 with 1,040 square feet and an
1882 construction year; 2024 onward records describe 2,765 square feet built in
2022. These patterns are consistent with delayed characteristic updates.
They do not establish whether construction was complete on the transaction
date, so neither the later characteristics nor a corrected price is substituted.

The $2,000 and $5,000 cutoffs are review thresholds exposed in Make, not
statistical evidence that every flagged sale is invalid. Low-price cases are
counted but have not been individually deed-verified. Avoid deleting them merely
because they are cheap. The high-price and inconsistent-characteristic review
rows, all available same-PIN sales, and historical cards are saved separately.

### Date precision and assignment

Cook County labels `sale_date` as the recording date, not the execution date.
The Clerk-index checks below show that this label does not describe all of the
historical rows we examined.
Its `is_mydec_date` flag identifies dates refined with IDOR information; the
historical ingest sometimes truncated dates to the first of the month. This
audit treats every unrefined date as month-only even if its displayed day is
not the first. It does not decode a day from the document number.
This is a conservative review convention, not proof that every unrefined date
has lost its day.

For each such sale the audit checks month endpoints, every term start and end,
and the 2015 map transition. Existing geometry helpers reassign locations where
the map can change. After the production exclusion, 21,198 preferred-sample
sales lack an IDOR-refined date, but none can change alderman, ward map, RD side,
or score eligibility within its reported month. Fifty observations in the
broader panel can fall in a verified vacancy, but none enters the preferred RD.

Five broader-panel sales have ambiguous timing around the May 2015 map change;
none is within 500 feet under either map. A separate check evaluates changes
in the set of operating CTA stations within each uncertain month: four broader-
panel sales can have different station distances, but none is in the current RD.
Quarter fixed effects and monthly CPI adjustment do not depend on the missing day.
That statement applies only to uncertainty within a given month; substituting
the Clerk's recording date can move a sale to another month or quarter.

The current public county API was queried for all remaining price and
characteristic review rows. The audit found 119 matching records. None has an
updated price, changed date, or newly refined IDOR date.
This does not prove that an original deed or another primary record could not
resolve the date. The complete API snapshots and fetch timestamps are retained
in audit outputs; production source snapshots are not refreshed.
The 69 price/characteristic review rows also all have matching current sale-year
improvement records, with no change to area, year built, rooms, or bedrooms.
Later changes do exist, as illustrated above. The prevalence of delayed updates
outside these flagged properties has not been measured in this audit.

### Apartment counts

Of the 2,051 affected pre-screen RD class-211 sales, most have a blank source apartment
label and a small number have the literal label `None`, which production converts to zero.
The latter is not independent evidence of a literal zero-unit building. Five
of those 54 also have a non-211 improvement class; the others are class 211 in
both sources.

Historical evidence uses only same-PIN, single-card, non-prorated class-211
records from the frozen 2006–2022 characteristics file. Among affected RD sales:

- 890 have a consistent two-to-six count recorded only before or only after
  the sale year.
- 31 have conflicting historical counts.
- 1,130 have no valid two-to-six count in the available comparable history.
- None has the same count observed on both sides of the missing sale year.

No counts are imputed. Missingness is not random across years: affected sales
are 492/2,652 class-211 RD sales in 2006 versus 93/1,459 in 2022. The missing-count
group has smaller buildings and lower nominal prices, but this is descriptive
composition evidence, not a transaction-validity test. Automatically deleting
the entire group would impose a new, selective restriction on the sample.

## Approved baseline and remaining limitations

The baseline retains missing apartment counts, excludes bedrooms greater than
total rooms, and adds no upper-tail price or price-per-square-foot trimming.
There are no hand-selected property exclusions or replacement characteristics.
Annual 99.9th-percentile trimming remains a programmatic sensitivity check in
`tasks/sales_price_tail_audit/`, not a production restriction.

This decision does not authenticate extreme recorded prices or resolve delayed
building descriptions. The later production change corrects the identified
term dates and excludes unrefined first-of-month sales whose assignment can
change within the month.

## Approved rooms screen and price sensitivity

The researcher approved retaining missing apartment counts and excluding
records with more bedrooms than total rooms. The date rule removes one of the
46 contradictory records first, so the final property-data producer removes
the remaining 45. The production panel contains 233,699 sales and the preferred
regression uses 58,468. Missing rooms alone are not a contradiction; missing
unit counts are not an exclusion. Main figures, placebos, and robustness
figures share this same producer. No price-per-square-foot exclusion has been
adopted.

The audit compares the same 100-foot binned RD with property-class controls,
segment-quarter fixed effects, existing property/amenity controls, and ward-pair
clustering. `output/quality_rd_sensitivity.csv` records the full estimates.

| Restriction relative to the pre-screen sample | Sales | Log-price coefficient | Standard error | Percent difference |
|---|---:|---:|---:|---:|
| None | 58,735 | 0.029216 | 0.013391 | 2.965 |
| Only the $134.9 million sale | 58,734 | 0.029216 | 0.013391 | 2.965 |
| Bedrooms exceed rooms (then-current production) | 58,725 | 0.029112 | 0.013377 | 2.954 |
| Price above $2,000/sqft | 58,725 | 0.028666 | 0.013384 | 2.908 |
| Both rooms and $2,000/sqft restrictions | 58,715 | 0.028564 | 0.013370 | 2.898 |

The extreme-price sale is the only observation in its segment-quarter group
in the preferred 500-foot sample. Its fixed effect absorbs its outcome level;
it supplies no within-group variation for the slope estimates. Removing it
leaves the coefficients essentially unchanged. This is specific to this
specification, not a justification for treating its recorded price as valid.
`output/flagged_price_influence.csv` records its group and group size.

These are data-quality sensitivity checks, not a search for a preferred
coefficient. They do not resolve the underlying price/characteristic anomalies.

## Pre-correction row-level date diagnosis, September 2, 2026

The pre-correction preferred sample contains 58,725 sales. Of these, 21,455 lack an
IDOR-refined date: 21,439 display day 1 and 16 display another day. Under the
existing term calendar, 103 can change RD side or eligibility (81 possible
side reversals and 22 possible term gaps). All 103 display day 1. Another 138
can change alderman names without changing RD assignment. These counts are
conditional on the calendar, whose historical accuracy is not assured.

The review covers those 241 rows plus 16 additional sales touching ward 26 in
May–June 2009. All 257 have matching identifiers and unchanged dates in the
current Assessor API. That does not mean the missing day is unrecoverable.

### What the Clerk index adds

The public [Clerk search](https://crs.cookcountyclerkil.gov/Search) supplies both
recording and execution dates. The pre-correction audit searched each reviewed
document number and checked the indexed PIN. These were public index checks,
not a reading of every original deed image. No documents were purchased. The
current `output/date_row_diagnosis.csv` is empty because no assignment-sensitive
sale survives the production rule.

All 257 document numbers were found. For 251, the indexed PIN matches exactly.
Five others have directly indexed parcels with unit identifiers and property-
type code C. Four link through the Clerk's explicit **Under PIN** table to our
PIN. Those links corroborate the document association, but do not establish
that our building-level characteristics describe the property sold. The fifth,
row 97119373, has Under PIN 17064400140000 rather than our 17064000140000; this
near-match is not accepted as an exact link. Row 96611165 also has an unresolved
PIN mismatch and no Under-PIN link. Only the first of those two unresolved rows
belongs to the 103 assignment-sensitive cases.

For all 103 assignment-sensitive documents, the indexed execution date is in
the month shown by the Assessor. Only 34 have a recording date in that month;
69 were recorded in a different month, including 15 in a different quarter.
Restricting to the 98 exact indexed-PIN matches gives the same pattern: all
98 execution months agree, 67 recording months differ, and 15 recording
quarters differ. The broader counts are document-number matches, including
the four Under-PIN links and the one unresolved PIN association.
Across all 257 reviewed documents, execution dates are available for 255, and
every one is in the Assessor month. This is a deliberately selected sample of
unrefined, transition-period dates, not a prevalence estimate for all sales or
for IDOR-refined dates.

Examples, all with an exact indexed-PIN match:

| Row ID | Assessor date | Clerk execution | Clerk recording |
|---|---|---|---|
| 97044961 | May 1, 2007 | May 18, 2007 | May 31, 2007 |
| 96867782 | July 1, 2009 | July 6, 2009 | August 12, 2009 |
| 96310327 | January 1, 2010 | January 13, 2010 | February 5, 2010 |
| 98310301 | January 1, 2013 | January 7, 2013 | February 21, 2013 |
| 98481960 | May 1, 2019 | May 15, 2019 | May 23, 2019 |

For example, row 97044961 was executed before the May 21 alderman transition
but recorded afterward. The two date concepts imply opposite sides under our
current calendar. Replacing dates mechanically would conceal that substantive
choice. Neither date is necessarily the date when the price was negotiated.

### Calendar corrections adopted

- **Ward 26:** the [May 13 Council journal](https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public-1/reports/2009_05_13_VI_VII_VIII.pdf),
  printed page 60205, says Ocasio would resign May 29. The
  [June 3 journal](https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public-1/reports/2009_06_03_VI_VII.pdf),
  printed page 63058, records his resignation letter on file. The
  [July 29 journal](https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public-1/reports/2009_07_29_VI_VII_VIII.pdf),
  printed pages 66402–66403, records Maldonado's approval and oath that day.
  Production now codes May 28 as Ocasio's final day and July 29 as Maldonado's
  first day, leaving the intervening vacancy explicit.
- **Ward 1:** the
  [January 13 journal](https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public-1/reports/2010_01_13_VI_VII.pdf),
  printed pages 82401 and 82411, records his attendance and votes. January 4
  therefore cannot be treated as a verified last day. Production uses January
  13 as the last verified date and leaves the later period vacant because the
  exact endpoint has not been recovered.
- **Ward 7:** Jackson's
  [resignation letter](https://news.wttw.com/sites/default/files/Ald.%20Sandi%20Jackson%27s%20Resignation%20Letter%20to%20the%20Mayor_0.pdf)
  specifies January 15 as the effective date. One January sale was flagged.
  The Clerk index places its execution on January 7, before either endpoint;
  its recording date is February 21. Production now codes January 14 as her
  final day.

This is not a completed historical audit of all alderman terms. Several other
entries have month-rounded transition dates that warrant source verification.
The calendar also assigns permits used to estimate stringency, and produces
the monthly panel used by rentals. The implications cannot be bounded simply
by dropping a few sales while keeping those scores fixed.

### Pre-correction sensitivity of the binned RD

`output/date_rd_sensitivity.csv` reproduces the production estimate and varies
only the reviewed sales. All fits retain property-class controls, the existing
property and amenity controls, segment-quarter fixed effects, 100-foot bins,
the 500-foot window, and ward-pair clustering.

| Diagnostic specification | Sales | Log-price coefficient | Standard error | Percent difference |
|---|---:|---:|---:|---:|
| Pre-correction production | 58,725 | 0.029112 | 0.013377 | 2.954 |
| Exclude the 103 assignment-sensitive dates | 58,622 | 0.029433 | 0.013360 | 2.987 |
| Exclude all 241 possible person changes | 58,484 | 0.029944 | 0.013388 | 3.040 |
| Assign month-end within the current cohort | 58,703 | 0.029292 | 0.013333 | 2.973 |

The month-end exercise holds the existing calendar and scores fixed and cannot
bring previously excluded sales into the sample. It is neither an identified
bound nor an estimate using repaired dates. Excluding the 103 dates plus the
ward-26 May–July review window removes 119 rows and gives 0.029436 (SE 0.013359).
These checks show that the identified rows do not explain the existing price
estimate. They do not validate the date concept, term calendar, or PIN linkage.

The September 3 production change described above follows this diagnosis. It
does not substitute Clerk dates, change the price-per-square-foot rule, or
accept the two unresolved PIN links.

## Sources and traceability

- [Cook County sales definitions](https://dev.socrata.com/foundry/datacatalog.cookcountyil.gov/wvhk-k5uv)
  describe recording dates and the IDOR refinement flag.
- [Cook County residential-model documentation](https://github.com/ccao-data/model-res-avm)
  defines building-level rooms/bedrooms and the apartment-count field.
- `output/property_review.csv`: high-price and internally inconsistent records.
- `output/property_sale_history.csv`: raw Chicago sale history, including transactions
  excluded from production; buyer/seller names are local audit data, not publication material.
- `output/property_improvement_history.csv` and `property_history_spells.csv`:
  historical source cards and changes in characteristics.
- `output/apartment_count_evidence.csv`, `apartment_count_by_year.csv`, and
  `apartment_count_composition.csv`: record-level corroboration and composition.
- `output/date_assignment_review.csv` and `date_cta_review.csv`: unresolved
  assignment alternatives and the separate CTA-distance check.
- `output/date_assignment_possibilities.csv`: explicit within-month intervals
  and assignments under the existing calendar, with current-sample membership.
- `output/date_row_diagnosis.csv`, `date_diagnosis_by_month.csv`, and
  `date_rd_sensitivity.csv`: current row review, public Clerk dates and PIN
  links, month counts, and fixed-calendar sensitivity estimates.
- `output/current_county_sales.csv`, `current_county_comparison.csv`,
  `current_county_improvements.csv`, and `current_characteristic_comparison.csv`:
  current public-source checks. Later improvement records are diagnostic only.

Generated outputs follow the repository's existing gitignore policy. Make links
concrete upstream inputs; no audit task is a prerequisite of the production paper.
