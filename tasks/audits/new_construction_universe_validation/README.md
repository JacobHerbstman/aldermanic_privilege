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
building type. In the 256 externally reviewed projects:

- the reported unit count agrees with the external classification for 123
  projects (48.0%);
- a class-first rule agrees for 249 projects (97.3%); and
- the seven disagreements form the classification exception ledger for the
  reviewed projects.

The 97.3% figure is an in-sample description, not an external error rate. A
fixed-seed retrospective 50-project split produces 49 correct classifications,
but the rule was developed after examining the full reviewed set. It therefore
does not provide prospective validation. The task also draws a reproducible
random sample of 50 previously unreviewed class-211/212 projects in
`multifamily_classification_mode_b_review_sample.csv`. That file is deliberately
left unadjudicated so a later reviewer can supply an independent error estimate.
The audit separately reports the size of each recurring error mode in
`multifamily_classification_error_modes.csv`.

The seven exceptions are four class-295 apartment buildings and three
class-211/212 collections of detached or attached single-family buildings.
Six corrupt six-unit counts attached to class-278/295 parcels are therefore
classified as nonmultifamily. A single-family project represented by one PIN
and one assessor card receives one dwelling; grouped townhouse projects retain
their project-level number of homes. Permits recover stable unit counts for
the apartment buildings at 1343 N Western Avenue and 4846 S St. Lawrence
Avenue. The latter comes from spatially matched new-building permit 100853866,
which explicitly describes a six-dwelling-unit building; the output records
that source rather than the assessor row.

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

The existing-building screen covers renovation, alteration, conversion,
deconversion, and addition permits even when the description omits the word
"existing." This correction excludes 23 projects that the narrower text rule
retained, including 5907 W Waveland Avenue. The final programmatic rules exclude
289 projects within 1,500ft, including 134 within 500ft. Forty-eight excluded
projects are classified as multifamily, including 20 within 500ft.

One additional project requires external evidence. The five historical parcels
at 2421--2429 W Winnemac Avenue contain surviving early-twentieth-century
structures and lack a supporting new-building permit, so the assessor's 2006
episode is excluded in the committed eligibility exception file. This brings
the eligibility exclusions to 290 within 1,500ft and 135 within 500ft.

Default retention is intentionally asymmetric: missing corroboration alone
does not exclude an assessor new-construction report. There are 1,885 such
retained projects within 1,500ft, including 795 within 500ft. Among projects
classified as multifamily, the corresponding counts are 564 and 234. These are
reported in `eligibility_uncorroborated_retained.csv`; a zero-row review queue
must not be interpreted as external verification of every retained project.

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

`test_residual_density_attrition_balance.R` compares the 70 omitted permit
chains with 863 retained multifamily projects that have complete treatment
scores. With segment and year fixed effects, omission is not significantly
related to either the more-stringent-side indicator (0.039, SE 0.063,
p=0.535) or the continuous score difference (-0.038, SE 0.036, p=0.292).
The coarser ward-pair-and-year model is also insignificant for the binary
comparison but yields -0.041 (SE 0.020, p=0.043) for the continuous score
difference. Raw counts place 45 omitted permits on the more-stringent side and
25 on the less-stringent side. The preferred local test does not show
differential attrition, but the result is not uniformly insensitive to the
fixed-effect choice.

## Minimal manual ledger

After applying the programmatic rules, the remaining manual ledger contains:

| Decision family | All decisions | Within 500ft |
| --- | ---: | ---: |
| Commercial project fields | 32 | 18 |
| Multifamily classification | 7 | 2 |
| New-construction eligibility | 1 | 1 |
| Residential parent-successor fields | 13 | 6 |
| Unresolved duplicates | 0 | 0 |
| **Total** | **53** | **27** |

The ledger records only cases where the available sources do not determine a
unique project-level value.

## Provisional sample and estimates

Applying the validated rules reduces the 1,500ft sample from 8,995 to 8,705
projects and the 500ft sample from 3,905 to 3,770. The 500ft multifamily sample
falls from 895 to 865 projects.

For the full-control score specification, the preferred
ward-pair-clustered estimates use the common FAR-DUPAC sample:

| Sample | Outcome | Treatment | N | Estimate | SE | p-value |
| --- | --- | --- | ---: | ---: | ---: | ---: |
| All construction | Log FAR | Continuous | 3,752 | -0.049 | 0.031 | 0.115 |
| All construction | Log FAR | Binary | 3,752 | -0.110 | 0.053 | 0.042 |
| All construction | Log DUPAC | Continuous | 3,752 | -0.030 | 0.038 | 0.432 |
| All construction | Log DUPAC | Binary | 3,752 | -0.103 | 0.077 | 0.181 |
| Multifamily | Log FAR | Continuous | 850 | -0.095 | 0.064 | 0.138 |
| Multifamily | Log FAR | Binary | 850 | -0.177 | 0.076 | 0.022 |
| Multifamily | Log DUPAC | Continuous | 850 | -0.126 | 0.079 | 0.117 |
| Multifamily | Log DUPAC | Binary | 850 | -0.159 | 0.111 | 0.158 |

The audit also reports segment-clustered estimates, common-outcome samples,
and the four score-control variants in
`provisional_validated_density_results.csv`.

No production or paper file is changed by this task. The audit supports a
production design with explicit source-priority rules, one frozen
53-decision exception ledger, and a documented exclusion of completed projects
whose density outcomes cannot be measured consistently.
