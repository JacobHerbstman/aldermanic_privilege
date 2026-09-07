# Construction cleaning

This task is being restored from the construction derivation preserved at
`research-archive`, commit `010a1f8497c1f32e2c79b5933d1c5baf9af44be3`.
It is not yet connected to the paper. The existing construction analysis
snapshot remains the production input until the restored chain is complete
and its output has been reconciled.

The first restored producer, `build_residential_cross_section.R`, selects
residential building records from the full assessment history. It preserves
the archived 1999 construction threshold and the preference for reports
through 2022, then through 2025, then later reports for otherwise missing
single-card buildings. Multicard records retain the original earliest-year
selection. Malformed CSV records fail explicitly rather than being silently
skipped by DuckDB.

`build_commercial_cross_section.R` restores the commercial selection and land
correction rules. The two previously embedded unit overrides now live in
`adjudication/initial_commercial_unit_overrides.csv`; their values and reasons
are unchanged. The fresh commercial source and rebuilt cross-section match
the archived source and cross-section byte-for-byte.

Historical-coordinate recovery now has explicit producers for the discovery
universe, project duplicate screen, address screen, spatial candidates, and
accepted coordinates. It reads preserved administrative parcel and address
snapshots from `data_raw/construction_review/`. A new API acquisition is a separate
source-refresh task. The four accepted address correspondences and two recorded
year corrections are explicit inputs.

On preserved inputs, the restored historical rules reproduced all 391 archived
coordinates byte-for-byte. Inspection also identified a real error in the original
conflict screen: distinct unit/area counts were computed after aggregation had
overwritten the underlying values. The restored screen counts first. This holds
out four additional PIN locations, all outside 500 feet. The corrected 387-row
coordinate output is identical with the preserved and fresh Assessor discovery
inputs. The corresponding later project decisions must be reconciled before
changing the production dataset. See the release audit's provenance follow-up.

Human reviews and historical parcel/zoning evidence are preserved under
[`adjudication/`](adjudication/README.md), with their original source locations
and hashes. The source of the residential cross-section implementation is
`tasks/residential_improvements_data_cleaning/code/residential_improvements_cleaning.R`
on the archive branch. No human decisions were changed.

The original historical-coordinate selection command has now been recovered.
`select_historical_coordinates.R` applies its project-lineage and address-duplicate
screen, then appends the accepted address matches. The discovery-stage screening inputs have been reconstructed. The
screen must compare against current coordinates before recovered projects are
added, to avoid matching recovered buildings to themselves.

Eight final-review scripts have also been restored from their original execution
sequences. They retain their assertions and decision rules; unused diagnostic
exports and subsequent reporting code are omitted. Their Make rules declare the
intermediate construction inputs that still need producers. The default `make`
therefore does not yet complete: this is an unfinished restoration, not an
alternative entry point that can presently replace the paper input.

A sequential replay of all seven final-review intermediates reproduced their
archived CSVs byte-for-byte, including the 795-row final decision ledger. The
restored eligibility/classification section separately reproduced its 8,995-row
eligibility table, 1,885-row uncorroborated queue, 13,707-row classification table,
and 8,705-row provisional sample byte-for-byte. These checks use preserved
upstream project inputs; they are not yet a raw-to-final rebuild.

The review requires pre-2006 sales and post-2022 permits. Preserved full sources
live in `data_raw/construction_review/`, with provenance and hashes.
Jacob approved removing the archived 2006 application-date lower bound from
`build_construction_permit_history.R` on September 7. Verification now uses all
dates available in the pinned source, subject to the existing coordinate and
processing-time validity rules. This recovers earlier applications for permits
issued during the study period. Analysis-specific year restrictions remain in
the consuming tasks.

The analytical exporter uses the recorded alderman terms at the assumed June 15
construction date. It records term gaps explicitly and rejects missing scores for
serving aldermen in the main bandwidth. This changes assignments relative to the
frozen file and requires comparison of downstream estimates. June 15 is an
imputed date for year-only construction records, not an observed completion date.
The field names and primary construction attributes otherwise retain the archived
export contract, with review and assignment-status fields available upstream.

Project aggregation, earlier duplicate resolution, eligibility, historical zoning,
and the complete source-to-output reconciliation remain unfinished. The original
8,648-row production input and the paper have not been replaced.

Restoration now includes the final residential and commercial decision hierarchies,
project/component reconciliation, permit revision chains, recovered-project
assembly, and model-input preparation without the old exploratory regressions.
Stage comparisons and remaining concerns are recorded in
`../working_paper_release_audit/construction_provenance_followup.md`.

Construction discovery uses four original ward maps, including Git-recovered
1998 and 2024 sources. Its distance input is produced directly from parcel
coordinates and ward boundaries; it no longer requires the old score-filtered
construction table. Pre-study ward 19 has no shared boundary in the 1998 source
map and is explicitly marked as lacking boundary coverage. Study-period
geographic coverage is validated independently of score availability.

The current full Assessor and historical-source snapshots are pinned in
`data_raw/construction_review/`. The exact original residential download remains
unlocated. Current-vintage differences, including newly appearing PINs, are
being reconciled. This task remains incomplete and disconnected from production;
its default target deliberately fails where an upstream producer is not yet
restored. Do not replace the existing paper input until the entire chain and its
sample/estimate differences have been verified.


Further replay restored commercial ground-up/permit evidence, historical project
geometry, preferred zoning, and tieback episode candidates. Standard reports
cover the newly restored CSV outputs. Zoning grouping now preserves a missing
map match instead of labeling it `Other`; this changes one 2012 evidence cell
and no final assignment on the preserved-input comparison. Both zoning stages
require the generated 2006 reconstruction and generated validated project
zoning. Their reconstruction is still unfinished. The separate release audit
records four retained main-bandwidth multicard land-denominator questions;
these have not changed production decisions.

The restored `build_residential_tieback_card_evidence.R` now reconstructs the
123-project card-evidence table and 596 construction-episode/assessment-year
rows exactly on the preserved manual-review bundle and Assessor history. The
Make 3.81 fixture also regenerated a deleted secondary output in a parallel
build; the next build ran no producer. This verifies that stage on recorded
inputs, not completion of the upstream bundle or reconciliation of the new vintage.
The two keyed data reports record the verified replay outputs.

On September 7, the residential manual-review bundle producer was restored.
Its 184 project rows and 76 columns reproduce the preserved bundle byte for byte
on the recorded upstream evidence. The build declares every evidence input and
produces a keyed report. This is a stage replay, not adoption of a new source vintage.

The committed `adjudication/density_denominator_decisions.csv` now controls the
three approved density exclusions in the restored final exporter. A controlled
full-export replay retained all 8,648 construction records and changed only
`allow_far`, `allow_dupac`, `density_far`, and `density_dupac` for those three IDs.
The frozen paper input remains unchanged pending the complete reconstruction.

Further September 7 replays restored the 184-row City-building evidence and the
8 accepted predecessor polygons, with identical tabular values and geometries.
The class-297 cohort (52 rows), resolution (57 rows), and source dispositions
(53 rows) also reproduce the preserved files. The restored class-297 script now
writes its required source-disposition output; the missing-output Make rule
regenerates both files. The cohort rule preserves the original earliest-year
selection; an older-than-target fallback, if encountered, now has an explicit
status. No such fallback occurs in the preserved 52-row cohort.

The restored full Assessor-history producer returns the same 4,627 records for
the preserved review scope. The pinned replacement source changes three fields
in 2026: row 1420404030000012026 reports construction year 2019 instead of 1895,
and rows 1707121002000012026 and 1707121048000012026 report PIN proration rates
0.6 and 0.4 instead of 1 and 1. Feeding this history through the card-evidence
producer adds one 2026/2019 episode row (596 to 597); the selected evidence for
all 123 reviewed projects remains identical. This narrow comparison does not
reconcile the changed initial discovery universe or all other uses of the source.

Further September 7 checks reproduce the project-overlap evidence (313 rows),
remaining residential cases (7), condominium request table (75), current parcel
links (1,789), and shared-site location review (2,699) byte for byte on preserved
inputs. Shared-site assignment loads the actual map eras in its records rather
than requiring unrelated historical and future layers. The condominium producer
also regenerates a deleted secondary file with GNU Make 3.81 in a parallel build.

The restored construction-year zoning replay reproduces 9,609 rows exactly.
Fourteen ordinance judgments, five existing exact-preconstruction support IDs,
and one recovered ordinance date are explicit adjudication inputs. Their notes
identify the original code and distinguish transcription from new review.
The five support IDs originally lacked separate row-level explanations; that
provenance limitation is documented rather than filled with invented evidence.
The 2006 map producer reproduces all 11,294 geometries. Its current archived
parser assigns polygon 4492's RT4A code to multifamily residential, whereas the
preserved map assigned Other; two group fields differ. Replaying the preferred
and final zoning stages with this map changes no output cells on preserved
project inputs. Ordinance extraction and matching upstream remain unfinished.

Jacob separately approved excluding all three Lenox representations from density
pending completion-year resolution. They are appended to the existing density
eligibility ledger. A controlled export comparison with the prior three-decision
ledger changes only four density fields on these three records; all 8,648 rows
remain. Neither this check nor the fixed-input sensitivity replaces the frozen
paper data or completes the source-to-final reconstruction.

Jacob approved a consistent two-year automatic construction-episode window on
September 7. `EPISODE_YEAR_WINDOW` now supplies the same explicit argument to
`build_multicard_episode_overlap.R` and `build_multicard_successor_building_candidates.R`.
The fixed-input comparison adds 62 overlap edges and 15 card matches. Existing
manual decisions require reconciliation with the changed grouping before final
adjudication can run; the producer fails explicitly instead of ignoring those
conflicts. The release audit's denominator review records the three newly
required episode reviews and the intermediate-versus-final unit-count discrepancy
at 33rd/Prairie. The matching change has not been propagated to the paper input.


The September 7 general-rule pass moves single-family unit normalization into
`build_preferred_residential_candidates.R`, using the same class set as the
final classifier. It also applies the study-period restriction there when
all candidate years lie outside the period. On preserved upstream inputs this
changes 28 candidate unit values and resolves 17 previously manual period
exclusions. Retained project membership and the residential boundary table
are unchanged; 11 early unit corrections reach the preferred project ledger.
All 11 already receive one unit in the preserved later classifier.

Multicard evidence now verifies that the selected card values coexist in an
actual assessment year. Matching joins candidate metadata by both parent and
successor, preventing dates and location flags from crossing parents in a
shared episode. A summary of complete, unambiguous matches feeds the existing
review-bundle producer. The unchanged-input replay preserves every matched
card/successor pair while correcting parent-specific metadata.

Web reviews now provide explicit classification exceptions and incremental
value corrections. The default card classification and computed unit/area
values do not require copied confirmations in the web ledger. The recorded
URLs and notes continue to supply construction evidence. A controlled replay
of this change preserves all 8,995 rows, numerical fields, and classifications.

Permit-unit extraction recognizes counts of rowhomes and townhouses separated
from the dwelling noun by a building height. It rejects counts of townhouse
buildings and does not treat a story count as a dwelling count. The three
permit-evidence consumers use the shared pattern. The multicard permit review
now covers the duplicate episodes of all reviewed parents, replacing a fixed
numbered-component selection that became incorrect after regrouping.

These are partial Make replays on recorded inputs, not a new paper build.
The complete source-vintage reconciliation and zoning reconstruction remain
unfinished. In addition, permit-reference components currently include related
applications for distinct buildings; their interpretation needs review before
using chain totals as building-specific measurements. The broader automatic
matching screen also awaits an explicit research decision.
