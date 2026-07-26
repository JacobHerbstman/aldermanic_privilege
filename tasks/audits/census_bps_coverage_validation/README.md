# Census Building Permits Survey coverage validation

This audit benchmarks the reconstructed new-construction ledger against an
external, independently compiled count of Chicago housing production. It does
not alter production data or paper results.

The Census Building Permits Survey (BPS) reports units **authorized by permit**
for the City of Chicago each year, split by the number of units in the building.
The ledger reports units **completed** and recorded by the Cook County Assessor.
The two differ in three known ways:

- authorization precedes completion, so BPS leads the ledger by one to two years;
- some authorized units are never built, so the ledger should account for
  somewhat less than the BPS total; and
- BPS buckets by units in the *building* while the ledger buckets by units in the
  *project*, so a development combining several small buildings falls in a larger
  BPS bucket than a ledger bucket.

A large shortfall would nevertheless indicate missing completions. That is the
question this task answers.

## Source

`tasks/download_census_bps` retrieves the place-level annual files from
`https://www2.census.gov/econ/bps/Place/Midwest%20Region/mw<year>a.txt` for
2004 through 2022 and parses them to one row per place-year.

Two published layouts exist. Files through 2006 carry 38 fields; later files
carry 41, because the later vintages split the place code and add population.
The trailing 24 unit columns and the two fields preceding them are stable in
both, so the parser reads identifiers from the front and unit counts from the
back. The download asserts that six-digit place id `147700` resolves to Chicago
in every survey year. Chicago reports 11 of 12 months in 2004 and 12 months
thereafter; totals use the imputation-inclusive columns.

The comparison uses the citywide 13,707-project ledger rather than the
boundary-restricted analysis sample, because BPS is a citywide count.

## Aggregate coverage

Ledger completions from 2006 through 2022 total **81,879 dwelling units across
13,705 projects**. Against BPS authorizations over matched windows:

| Authorization lag | BPS window | BPS units authorized | Coverage |
| ---: | --- | ---: | ---: |
| 0 years | 2006–2022 | 106,039 | 77.2% |
| 1 year | 2005–2021 | 109,674 | 74.7% |
| 2 years | 2004–2020 | 110,553 | 74.1% |
| 3 years | 2004–2019 | 106,420 | 76.9% |

Coverage is 74–77% and is insensitive to the lag assumption. Given that not all
authorized units are completed, and that the sample separately documents 179
completed projects excluded for lacking comparable density fields, this is
consistent with substantially complete coverage of completed construction. It
rules out gross undercoverage.

## Coverage differs sharply by building size

| Building size | BPS units | Ledger units | BPS share | Ledger share | Coverage |
| --- | ---: | ---: | ---: | ---: | ---: |
| 1 unit | 9,133 | 10,868 | 8.3% | 13.3% | **119%** |
| 2 units | 1,734 | 898 | 1.6% | 1.1% | **52%** |
| 3–4 units | 9,313 | 3,846 | 8.5% | 4.7% | **41%** |
| 5+ units | 89,494 | 66,267 | 81.6% | 80.9% | **74%** |

The aggregate share is carried by 5+ unit buildings, which dominate Chicago
production and are covered at the overall rate. Two- to four-unit buildings are
covered at 41–52%, and single-unit records exceed the BPS authorization count.

The gap is investigated in `decompose_small_multifamily_gap.R`.

### Decomposition of the 3-4 unit shortfall

| Component | Units | Share of BPS 3-4 |
| --- | ---: | ---: |
| BPS units authorized, 3-4 unit buildings | 9,313 | 100% |
| Ledger units booked as 3-4 unit projects | 3,846 | 41.3% |
| **Shortfall** | **5,467** | **58.7%** |
| Explained: project aggregation into the 5+ bucket | 1,734 | 18.6% |
| Explained: single-dwelling townhouse bucketing | 14 | 0.2% |
| **Unexplained residual** | **3,719** | **39.9%** |

Project aggregation is the largest identified source. A ledger project that
combines several small buildings is booked at its project-level unit count, so a
site of three 3-unit buildings lands in the 5+ bucket while BPS counts three
3-unit buildings. This is confirmed independently: ledger 5+ projects average
54.5 units against 34.1 units per BPS 5+ building, and the ledger holds only 46%
as many 5+ buildings as BPS while holding 74% of the units.

The single-dwelling townhouse figure of 14 is a strict lower bound, because it
uses only exact-PIN permit text. Matching permits by address instead raises it to
roughly 267 projects; permit 100100776, "ERECT A 5 DWELLING UNIT TOWNHOME
BUILDING," covers four separate class-295 ledger projects each recorded as one
dwelling. Even at 267 the mechanism explains under 5% of the shortfall.

The collapsed-unit-count hypothesis is ruled out: no class 211 or 212 record
appears among the ten most common classes of single-dwelling ledger projects,
which are dominated by class 278 (5,562 projects) and class 295 (2,299).

### The residual is not a lag artifact

A fixed one-year authorization-to-completion lag is wrong for the 2005-2008
boom, when large projects took several years. Splitting by housing-cycle era,
with 5+ unit buildings as a control:

| Authorization era | 3-4 unit coverage | 5+ unit coverage |
| --- | ---: | ---: |
| Boom, 2005-2008 | 0.38 | 0.40 |
| Trough, 2009-2012 | 0.61 | 1.46 |
| **Recovery, 2013-2021** | **0.44** | **0.89** |

Both buckets look equally undercovered in the boom, and the trough ratio above
one for 5+ buildings confirms that boom authorizations complete several years
later. The recovery window is the clean comparison: authorizations and
completions both fall inside the sample and lags are short. There, 3-4 unit
buildings are covered at half the rate of 5+ buildings. The gap is real.

### What remains undetermined

Three explanations for the residual cannot be distinguished with these data:

1. **Higher abandonment.** Two- to four-unit permits are filed by small builders
   and may be abandoned more often than institutional multifamily permits.
2. **Assessor recording.** A new three-flat replacing an older building on the
   same lot may carry a pre-2006 year built, placing it outside the
   new-construction universe entirely. The companion eligibility audit documents
   the reverse error, so this direction is plausible.
3. **BPS authorization counts.** BPS does not net out permits later revised or
   voided, so authorizations exceed starts by an unknown margin that may differ
   by building size.

**These are separated in `tasks/audits/acs_structure_age_validation`.** American
Community Survey table B25127 counts occupied units reporting built 2010 or
later. For the 2010s, ACS finds 3,254 occupied units in 2-4 unit structures
against 3,391 authorized by BPS -- 96%. The units exist and are lived in, so
neither abandonment nor BPS overcounting explains the gap. The ledger records
1,272 for the same window. Explanation 3 is ruled out, explanation 1 is largely
ruled out, and the residual is assessor undercoverage.

## Implication for the paper

The aggregate 74% coverage figure is carried by 5+ unit buildings. The paper's
multifamily sample is concentrated in the 2-6 unit range, where coverage against
BPS is roughly half that. This does not invalidate the boundary comparison, which
requires missingness to be balanced across ward boundaries rather than complete,
and the companion attrition test finds no imbalance. It does mean the sample
should not be described as a complete census of small multifamily construction.

## Year-by-year comparison

`bps_ledger_annual_comparison.csv` reports the annual series. Contemporaneous
ratios range from 0.38 in 2006 to 3.49 in 2009, which reflects the
authorization-to-completion lag across the housing cycle rather than coverage:
units authorized during the 2005–2007 boom complete in 2008–2010, so the ledger
exceeds contemporaneous authorizations in the trough years. The cumulative
comparison above is the interpretable one.

## Outputs

- `bps_ledger_annual_comparison.csv` — ledger and BPS units by year
- `bps_ledger_cumulative_coverage.csv` — coverage at each authorization lag
- `bps_ledger_size_composition.csv` — coverage by building-size bucket
- `bps_ledger_coverage_summary.csv` — one-row summary
- `small_multifamily_gap_decomposition.csv` — attribution of the 3-4 unit shortfall
- `small_multifamily_gap_by_era.csv` — coverage by housing-cycle era, 3-4 against 5+

No production or paper file is changed by this task.
