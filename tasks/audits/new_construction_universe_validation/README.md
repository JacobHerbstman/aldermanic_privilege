# New-construction universe validation

This audit asks whether the proposed new-construction sample can be built with
general rules rather than a large hand-coded ledger. It starts from the frozen
13,707-project citywide precursor ledger and does not alter production data or
paper results.

The frozen ledger has SHA-256
`8427b17255bb911fba5e90f2620d3016a466a793c5ad9af9917918bb1a566997`.
The audit links to this completed output rather than rerunning Cook County's
live historical-parcel service.

## Analysis scope

The evidence inventory contains 9,545 density-usable projects within 1,500ft
of a ward boundary. The reviewed analysis input contains 8,995. The difference
is fully reconciled:

- 535 successor parcels are suppressed because the same construction is
  represented by an earlier parent project;
- 17 projects are suppressed or pending in the external review;
- two projects satisfy both conditions; and
- no omission is unexplained.

`validate_analysis_scope.R` asserts this reconciliation and also confirms that
the analysis input contains no project absent from the evidence inventory.

## Duplicate projects

The duplicate screen examines all retained projects after multicard review. It
finds 26 candidate pairs. Eighteen are distinct contemporaneous assessor
parcels, including separately assessed class-295 rowhouse parcels. Eight have
different exact permits and supported addresses. No candidate pair remains
unresolved.

The parent-successor rule independently reproduces all 535 existing successor
suppressions. Thirteen projects still require field, year, or cross-project
decisions that cannot be derived mechanically from the available sources.
The project construction also consolidates the 17 prorated PIN groups that
previously appeared as 35 separate multifamily rows carrying duplicated
whole-building fields.

## Multifamily classification and unit counts

Assessor class is more reliable than the raw reported unit count for assigning
building type. Against 256 externally reviewed projects:

- the reported unit count agrees with the external classification for 123
  projects (48.0%);
- a class-first rule agrees for 249 projects (97.3%); and
- the remaining seven projects form the complete classification exception
  ledger.

The seven exceptions are four class-295 apartment buildings and three
class-211/212 collections of detached or attached single-family buildings.
Six corrupt six-unit counts attached to class-278/295 parcels are therefore
classified as nonmultifamily. Permits recover stable unit counts for the
apartment buildings at 1343 N Western Avenue and 4846 S St. Lawrence Avenue.

## New-construction eligibility

The eligibility rule uses evidence available before the selected construction
year. Historical residential assessor cards are aggregated to the PIN-year
level before comparison so that incomplete or changing card inventories do
not create false exclusions.

A project is excluded only when:

1. a complete pre-sample assessor record reports a pre-2006 structure and its
   total building area and dwelling units are each within 10% of the proposed
   project; or
2. an exact-PIN permit describes work on an existing building and there is no
   new-building permit, commercial completion record, or physical replacement
   in the assessor history.

Incomplete historical fields are not enough to exclude a project. A
new-building permit, commercial completion record, or clear assessor
replacement instead supports retention.

These rules exclude 266 projects within 1,500ft, including 126 within 500ft.
Forty-four excluded projects are classified as multifamily, including 18
within 500ft. No eligibility case remains for manual review.

Archived 2015 city footprints provide an independent check for projects
reported through 2015. They support 79 of the excluded unchanged structures
and 145 retained multifamily projects. Six early 2006--2007 multifamily
projects that the earlier year-recode screen would have excluded are
corroborated by the archive or external property records. These checks
validate the rule; they do not enter the decision logic.

## Commercial projects

The commercial reconstruction contains 815 final projects. Candidate records,
permit chains, completion evidence, land evidence, and final fields are joined
at the project level. A source alias may supply automatic permit evidence only
when it maps to one final project. Sitewide permits are not copied to every
child project at split developments.

The general source-priority rules reproduce the final fields for all but 32
commercial project decisions, 18 of which touch the 500ft sample. Those
decisions are retained in `commercial_minimal_exception_ledger.csv`.

## Completed permits missing density fields

The permit-completion audit identifies 179 completed construction chains that
are not in the project ledger because comparable assessor density fields are
unavailable. Eighty-one lie within 500ft, and 70 of those may be multifamily.
None can be recovered under the paper's existing outcome definitions using
the same building area, dwelling-unit, and land-area fields. Building
footprints or permit text could create alternative measures, but mixing those
with assessor density outcomes would change the estimand rather than complete
the current sample.

## Minimal manual ledger

After applying the programmatic rules, the remaining manual ledger contains:

| Decision family | All decisions | Within 500ft |
| --- | ---: | ---: |
| Commercial project fields | 32 | 18 |
| Multifamily classification | 7 | 2 |
| Residential parent-successor fields | 13 | 6 |
| Unresolved duplicates | 0 | 0 |
| **Total** | **52** | **26** |

Eligibility requires no manual decisions. The ledger records only cases where
the available sources do not determine a unique project-level value.

## Provisional sample and estimates

Applying the validated rules reduces the 1,500ft sample from 8,995 to 8,729
projects and the 500ft sample from 3,905 to 3,779. The 500ft multifamily sample
falls from 895 to 869 projects.

For the full-control score specification, the preferred
ward-pair-clustered estimates are:

| Sample | Outcome | Treatment | Estimate | SE | p-value |
| --- | --- | --- | ---: | ---: | ---: |
| All construction | Log FAR | Continuous | -0.047 | 0.031 | 0.131 |
| All construction | Log FAR | Binary | -0.107 | 0.054 | 0.049 |
| All construction | Log DUPAC | Continuous | -0.042 | 0.041 | 0.308 |
| All construction | Log DUPAC | Binary | -0.125 | 0.077 | 0.105 |
| Multifamily | Log FAR | Continuous | -0.092 | 0.062 | 0.142 |
| Multifamily | Log FAR | Binary | -0.176 | 0.076 | 0.023 |
| Multifamily | Log DUPAC | Continuous | -0.111 | 0.081 | 0.173 |
| Multifamily | Log DUPAC | Binary | -0.160 | 0.116 | 0.170 |

The audit also reports segment-clustered estimates, common-outcome samples,
and the four score-control variants in
`provisional_validated_density_results.csv`.

No production or paper file is changed by this task. The audit supports a
production design with explicit source-priority rules, one frozen
52-decision exception ledger, and a documented exclusion of completed projects
whose density outcomes cannot be measured consistently.
