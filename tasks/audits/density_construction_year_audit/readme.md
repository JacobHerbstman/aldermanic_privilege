# Density Construction-Year Audit

This audit reconstructs the complete assessor history for every residential
PIN-card that is ever reported as built in 1999 or later. It distinguishes
old-to-new building transitions from revisions among post-1999 years, compares
several mechanical year-selection rules, and validates them against uniquely
matched new-construction permits where possible.

## Production rule being audited

The former production rule discarded assessor rows reporting a year built
before 1999 and selected the minimum remaining year at the PIN level. Production
now uses the latest assessor record available by 2022 for single-card PINs,
falling back to the latest pre-2026 record only when no earlier qualifying
record exists. Multicard PINs retain the old minimum-year rule so that this
change remains separate from the unresolved multicard observation-unit question.

## What changes in the assessor history

- 33,568 PIN-cards ever report a year built of 1999 or later.
- 7,049 report more than one post-1999 year built.
- There are 10,172 transitions from one post-1999 year to another. Of these,
  7,698 occur in the 2021 or 2022 tax records, 55.0 percent change the year by
  exactly one year, and 98.2 percent change it by no more than three years.
- Only 1.3 percent of post-1999 year revisions coincide with a change of at
  least 25 percent in building square footage or a change in apartments, use,
  or residence type. By comparison, 89.1 percent of transitions from a
  pre-1999 building to a post-1999 building have such a physical change.

The mass of modern-year revisions therefore looks like retrospective
correction of the same building. The old-to-new transitions generally look like
actual redevelopment.

[Cook County's dataset documentation](https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Single-and-Multi-Family-Improvement-Chara/x54s-btds)
says records before 2021 exclude
characteristic updates from Home Improvement Exemption applications, while
records from 2021 onward include them. That change is consistent with the
timing of the revision wave, although it does not establish that every revised
year is correct.

## Permit validation

The clean validation sample contains single-card PINs whose PIN10 identifies
one candidate PIN and exactly one completed, issued new-construction permit.
Among the 1,741 revised PIN-cards in this sample, the production minimum year
is between zero and three years after the permit application in 54.3 percent
of cases. The latest report available at the sample endpoint does so in 80.7
percent of cases. The production year predates the application in 790 cases;
the endpoint year does so in 309.

Of the endpoint rule's early dates, 241 are exactly one year before the permit
application. Treating those as ordinary timing disagreement expands the
validation window to one year before through three years after application and
raises agreement to 94.5 percent. Public property histories support this
tolerance but do not establish that either source is always exact: some
buildings were marketed as new before their matched permit application, while
others appear to have been completed after the assessor's reported year.

Four multifamily cases that move into the 500-foot sample after a 2014-to-2015
correction have direct permit matches:

| PIN | Permit address | Application | Reported history |
| --- | --- | --- | --- |
| 13362040040000 | 2527 W Fullerton Ave | 2013-12-24 | 1888 to 2014 to 2015 |
| 13364320270000 | 1632 N Western Ave | 2015-12-28 | 2014 to 2017 to 2015 |
| 14073180030000 | 4853 N Western Ave | 2015-08-24 | 1898 to 2014 to 2015 |
| 14292290220000 | 2812 N Mildred Ave | 2015-01-09 | 1888 to 2014 to 2017 to 2015 |

The last three could not have been built in 2014 if the application dates are
correct. Their 2015 values appear in the 2021 assessor refresh without a
corresponding physical-characteristic change.

The audit also flags 134 post-1999 year revisions with a large physical change.
Only four have an unambiguous new-construction permit that both supports the
new year and postdates the prior reported construction year. Three of those
rebuilds occur after 2022. This is why the preferred audit rule does not simply
use the latest report in the current file.

## Implemented rule

`latest_report_at_sample_end` uses the latest assessor report available by
2022. For PIN-cards that first appear after 2022, it uses the latest pre-2026
report, with the 2026 report used only when no earlier record exists. This uses
the large 2021-2022 correction wave while avoiding known post-sample rebuilds.

The rule changes the citywide single-card 2006-2022 count from 12,657 to 13,336.
With the current downstream inputs, the paper's 500-foot sample contains 4,113
observations overall and 881 multifamily observations.

## Regression results

All models use the paper's current 500-foot sample definition, binary and
locally centered continuous treatments, pair-average score control, segment
and year fixed effects, ward controls, and ward-pair clustering. The approved
construction-year zoning group is held fixed by project so that the comparison
isolates construction-year selection. Commercial multifamily records and
multicard residential PINs stay at their production values.

| Specification | Multifamily ln(FAR), continuous | Multifamily ln(DUPAC), continuous | Multifamily ln(FAR), binary | Multifamily ln(DUPAC), binary |
| --- | ---: | ---: | ---: | ---: |
| Former minimum-year rule, current downstream inputs | -.195 (.065) | -.151 (.048) | -.230 (.109) | -.240 (.100) |
| Current production | -.235 (.069) | -.176 (.057) | -.259 (.099) | -.319 (.122) |
| Exact one-year-early cases moved to permit year | -.235 (.069) | -.173 (.056) | -.268 (.100) | -.316 (.118) |
| Stable reported years only | -.236 (.068) | -.177 (.057) | -.262 (.097) | -.321 (.122) |

Correcting the year does not weaken the multifamily result. All-construction
estimates remain small and statistically insignificant under every rule. The
common-sample comparison shows that most of the multifamily change comes from
assigning existing projects to the corrected year, ward map, alderman, and year
fixed effect, not from admitting corrected pre-2006 records.

The exact-one-year sensitivity moves every mechanically matched revised PIN
from the endpoint assessor year to the permit application year. Eighty-one such
PINs are in the endpoint rule's 500-foot all-construction sample; five are
multifamily. The all-construction sample falls by four observations after
reassignment, while the multifamily sample count is unchanged. The estimates
are effectively unchanged, so the treatment of this narrow timing disagreement
does not drive the density results.

The model stage changes construction-year selection only for single-card
residential PINs. Commercial multifamily observations and multicard residential
PINs remain fixed so that the year question is not mixed with the unresolved
observation-unit question. All geography, scores, controls, and regression
specifications otherwise match the current paper. Alternative years retain the
production construction-year zoning group rather than inventing a zoning state
for a year the paper does not assign to the project.

This task records the evidence and sensitivity checks behind the production
rule. It does not modify production data or paper outputs.
