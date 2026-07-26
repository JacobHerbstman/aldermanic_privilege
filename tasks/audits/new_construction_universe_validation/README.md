# New-construction universe validation

This audit tests whether every retained 2006--2022 project represents the
intended physical construction and whether it is genuine new construction.
It begins from the frozen 13,707-project audit ledger and does not alter
production data or paper results.

The 13,707-project file is the citywide precursor ledger. The active audit
sample after the earlier multicard and external-review decisions contains
8,995 projects within 1,500ft of a ward boundary, including 3,905 within
500ft. All duplicate, eligibility, and classification rules below are tested
against that later sample.

The audit creates one project-level evidence inventory. It records
assessor class, current physical fields, boundary-sample membership, exact
permit scope, permit unit counts, shared-permit links, and prior external-review
coverage. The broad `initial_review_queue.csv` contains mechanical flags only;
the narrower outputs described below supersede it.

The baseline ledger is frozen at SHA-256
`8427b17255bb911fba5e90f2620d3016a466a793c5ad9af9917918bb1a566997`.
The follow-up audit links to that completed output without rerunning Cook
County's live historical-parcel service.

## Parent and successor records

The existing card-to-successor matcher identifies direct card matches and
complete successor episodes. A general rule reproduces all 535 existing
successor suppressions with no disagreements. Most rows previously labeled
manual simply retain the contemporaneous card inventory. The genuinely
nonmechanical ledger contains 15 field, year, or cross-project exceptions on
13 projects. These are listed in
`parent_successor_cluster_review_queue.csv`.

## Duplicate projects

The duplicate screen runs on all retained projects after multicard review. It
finds 26 candidate pairs. Eighteen are distinct contemporaneous assessor
parcels, including separately assessed class-295 rowhouse parcels. Eight have
different exact permits and supported addresses. No pair relies only on a
prior manual disposition, and `unresolved_duplicate_candidates.csv` is empty.

## Multifamily classification and unit counts

Assessor class is substantially more reliable than the raw reported unit
count for assigning building type. Against 256 externally reviewed projects:

- the reported unit count agrees with the external classification for 123
  projects (48.0%);
- a class-first rule agrees for 249 projects (97.3%); and
- sending the seven class exceptions to review yields 249 correct decisions
  and no errors among programmatically decided projects.

The seven exceptions are four class-295 apartment buildings and three
class-211/212 collections of detached or attached single-family buildings.
They are listed in `multifamily_classification_known_exceptions.csv`.

No new unit-count case remains unresolved. The permits give one stable unit
count for the new buildings at 1343 N Western Avenue and 4846 S St. Lawrence
Avenue. The project at 12813 S Peoria Street is excluded by the eligibility
rule below because the same 1,184-square-foot structure appears in assessor
history as built in 1954 before its reported year changes to 2006.

## New-construction eligibility

The strongest programmatic exclusion is a structure observed in assessor
history before its reported construction year with no change in building area
or unit count and no new-building permit. Such a record cannot represent a
new structure in the later reported year. This rule excludes 220 projects
within 1,500ft, including 115 within 500ft and 10 current multifamily
observations within 500ft.

Positive evidence is handled separately. A project is retained when it has a
new-building permit, commercial completion evidence, or an assessor transition
that changes physical building fields. Commercial projects are not excluded
merely because a later permit describes work on an existing building. For
example, the One Chicago and Kingsbury Plaza projects are independently
supported by completed new-construction permits or city building-year
evidence.

Only one project remains in `eligibility_manual_review_queue.csv`, and it lies
outside 500ft. Its permit describes two-story construction over an existing
structure and a rear addition, which does not cleanly identify either a wholly
new building or a simple alteration.

## Permit PIN and geography conflicts

`permit_pin_geography_conflicts.csv` compares the project selected by the
permit PIN with the project polygon containing the permit point. These
conflicts are evidence-assignment diagnostics, not duplicate-project
decisions. Most do not change project values. Sixteen permit-project links
could mechanically suggest a unit-count recovery, but fifteen belong to
single-family or townhouse classes. The class-first rule prevents a sitewide
permit count from turning those individual parcels into apartment buildings.
The remaining link is 4846 S St. Lawrence, where the permit point lies inside
the class-211 project polygon and explicitly authorizes six dwelling units.

No production or paper file is changed by this task. The next step is to apply
the validated rules to a provisional audit sample and re-estimate the density
models before proposing production code.
