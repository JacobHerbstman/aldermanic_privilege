# Construction provenance follow-up

September 4, 2026. This corrects the scope of finding 2 in the release review. No production files or branch checkouts were changed.

## What the current build does

`tasks/new_construction_analysis_data/code/Makefile` has no producer script or raw-data prerequisites for its CSV. It requires the committed CSV to exist. Consequently, a fresh clone already contains the construction input; running the paper build estimates results from it rather than rebuilding its derivation. The documented July 30 fresh-clone run is consistent with this arrangement.

## Where the derivation is preserved

The local `research-archive` Git branch tracks:

- `tasks/audits/commercial_new_construction_sample_audit/`
- `tasks/audits/new_construction_universe_validation/`
- `tasks/audits/new_construction_project_verification/`

The last task includes a Makefile, `build_final_verified_density_input.R`, validation scripts, and CSV decision records including `project_manual_reviews.csv`, `final_project_overrides.csv`, `assessor_default_site_reviews.csv`, and `corrected_year_zoning_decisions.csv`. The inspected decision records have project IDs, evidence identifiers/URLs, explanations, and corrected values. Their existence is verified; their full substantive validity has not been re-audited in this follow-up.

The archive README describes review of 795 initially Assessor-supported projects, 740 retained and 55 excluded, and 51 corrected construction years. The surviving local check outputs report those counts and successful checks. These are prior validation results, not fresh checks of all original evidence.

## Git history

| Commit | Date | Relevant action |
|---|---|---|
| `f3cb817` | July 27 | “Port verified construction sample”: added frozen production CSV alongside verification code; the original README named the three derivation tasks explicitly. |
| `8d80baf` | July 29 | Updated the frozen CSV and its recorded hash. |
| `f98d40e` | July 30 | “Prepare paper replication archive”: removed the three explicit derivation-task references from the construction README and said the evidence was kept separately. |
| `6484c78` | July 31 | “Keep main limited to paper replication”: removed the verification producer from the main branch; it remains on `research-archive`. |

Current HEAD and `research-archive` contain the exact same Git blob for the frozen production CSV: `470a1e6f4f9de3fd6205cf5c30ce252af9f6ba62`.

## Comparison with surviving local verification output

Compared production `new_construction_analysis_data.csv` with `tasks/audits/new_construction_project_verification/output/final_verified_density_input.csv`, joining by project ID:

- Both contain exactly 8,648 rows and the same project IDs.
- Production has 36 columns; the verification output has those same columns plus `reviewed_project`, `final_evidence_tier`, and `construction_year_changed`.
- All nonnumeric fields match exactly.
- All shared numeric values agree up to floating-point serialization, except three fields for one project.
- The exception is `commercial_17173220138002`: lot area changes from 1 to 67,437.37997546978 square feet; FAR changes from 53,022 to 0.7862405096; DUPAC changes from 3,179,880 to 47.1530774350. Git records those changes in `8d80baf`.
- That project is approximately 1,054 feet from the boundary, outside the main 500-foot sample but inside the broader input used for placebo analysis.

Further tracing located both the correction's source and its generating code; see below. The comparison establishes a very close connection between the surviving reviewed output and production, not a fresh end-to-end raw-data reconstruction.

## Manual inputs versus generated outputs

Committing human adjudications is appropriate. Code cannot recreate the researcher's assessment of photographs, ambiguous permit descriptions, or conflicting records without those decisions as inputs. The archive preserves such inputs as CSVs, including explicit actions, corrected values, source identifiers, evidence URLs, and explanations. A CSV is a spreadsheet-readable decision record; a separate Excel workbook is not required for reproducibility.

The frozen 8,648-row production dataset is broader than a manual-decision file. Archived code combines administrative sources and adjudications, applies inclusion and field overrides, and calculates construction dates, ward geography, distances, zoning, densities, and analytical covariates. In particular:

| Stage | Archived producer or input | Function |
|---|---|---|
| Project construction | `commercial_new_construction_sample_audit/code/` and its `adjudication/` directory | Construct and reconcile Assessor and permit projects, component parcels, source fields, duplicate decisions, and historical zoning. |
| Eligibility and classification | `new_construction_universe_validation/code/build_provisional_validated_sample.R` | Apply eligibility and multifamily classifications to the reviewed project sample. |
| Final human review | `new_construction_project_verification/adjudication/` | Preserve project reviews, site decisions, project exceptions, final overrides, and corrected-year zoning decisions. |
| Decision application | `new_construction_project_verification/code/build_final_project_verification_ledger.R` | Combine reviewed evidence and adjudications into final inclusion, year, and field decisions; assert decision coverage and unique keys. |
| Analytical construction | `new_construction_project_verification/code/build_final_verified_density_input.R` | Apply the decisions to the provisional sample; reconstruct dates, ward assignments, distances, segments, zoning, aldermen, covariates, and density measures. |
| Release handoff | Current `tasks/new_construction_analysis_data/` | Supply a committed 36-column snapshot; no producer/export recipe connects the preceding steps to this target. |

All archived paths in this table start with `tasks/audits/` and were inspected at `research-archive`, commit `010a1f8497c1f32e2c79b5933d1c5baf9af44be3`. The final analytical generator retains the provisional sample's inclusion by default for projects outside its 795-project review scope; its review is not a claim that all 8,648 rows received that particular manual review. Current density estimation separately reloads the current stringency scores, so freezing the construction CSV does not freeze every downstream analytical calculation.

## Lot-area correction traced

For `commercial_17173220138002`, the surviving commercial audit outputs show:

1. `preferred_project_boundary_scope.csv` reports `project_land_area_sqft = 67437.37997546978` and `geography_status = complete_construction_year_geometry`.
2. `commercial_land_adjudication_evidence.csv` records Assessor land area of 1, that same parcel area, and `use_exact_construction_year_parcel_union`.
3. `commercial_post_evidence_resolution.csv` marks `exact_land_recovery = TRUE` with reason `complete_construction_year_parcel_union_recovers_land`.
4. `preferred_new_construction_project_ledger.csv` contains the corrected land area, `land_source = construction_year_parcel_union`, and `decision_source = evidence_rule`.
5. The surviving downstream `final_new_construction_audit_ledger.csv`, `final_density_model_input.csv`, and final verification output still contain the old value of 1. These local artifacts are not a synchronized rebuild of the corrected pipeline.

Crucially, commit `8d80baf` changes `build_commercial_post_evidence_queue.R` as well as the frozen production CSV. The code expands the land-recovery rule to recognize finite `land_sqft <= 1`, requires complete construction-year geometry and usable parcel area without an unresolved land review, and gives this recovery precedence over mechanical retention. `build_preferred_commercial_ledger.R` then selects parcel area for the flagged rows and records the source. Thus the correction is encoded upstream, rather than being an unexplained final-file adjustment. Its production values equal `53022 / 67437.37997546978` for FAR and `43560 * 73 / 67437.37997546978` for DUPAC.

This establishes source and code provenance for the change. It does not establish that every archived downstream task was rerun after it, nor independently remeasure the source parcel polygons.

## What the current production package is missing

- **Accessible decision records and scope documentation.** The archived CSV adjudications should accompany the release, with project IDs and evidence. Appendix A's promise of a decision spreadsheet should name the actual delivered records. The precise 52-citywide/26-within-500-foot statement still needs reconciliation with the several decision families and subsequent 795-project review; neither summing decision rows nor treating all reviews as exceptions establishes that count.
- **A documented and executable handoff.** The present Make target starts at the final curated file. It does not apply archived decisions, reconstruct derived fields, or export the verified result into the release schema. A frozen snapshot can remain convenient, but the source release and recipe used to create it should be pinned and available. Under this project's raw-to-paper Make standard, the deterministic producer should be connected to the production graph.
- **A reconciliation against a fresh, synchronized construction build.** Regenerate the necessary derivation outputs with the preserved decisions and the July 29 land rule; compare project membership and every released field against the committed snapshot. Current stale local outputs cannot certify that rebuild. Keep diagnostic and exploratory outputs outside the essential production path.

These omissions do not require replacing human judgment with an algorithm or repeating every web search on each paper build. They require preserving the decisions as inputs and making their deterministic application traceable. No production methods or outputs were changed in this follow-up.

## Revised assessment

The problem is a disconnected release dependency graph and incomplete provenance packaging, rather than demonstrated loss of the construction history. The archived producer, human decision records, and later lot-area correction code are preserved. The next verification should reconstruct the appropriate archived environment, rebuild with the existing correction from its documented inputs, and reconcile every production field. Until then, a successful current paper build confirms analysis reproducibility from a curated construction input, not full reconstruction of that input.


## Restoration investigation, September 4

The original export command for `density_historical_coordinates.csv` was recovered
from the saved July work history (executed July 18, 2026 at 00:02:42 UTC). It joined
`density_project_lineage.csv` to `density_parcel_address_lineage_evidence.csv` by
`project_key`, kept `recommended_action == "candidate_for_recovery"` unless the
address screen said `exclude_address_confirmed_duplicate`, expanded the retained
member PINs, and selected their exact historical coordinates. It appended four
`recover_unique_address_confirmed_by_parcel_history` address matches, asserting
387 exact-history rows plus four address rows. Two construction years were updated
from 2021 to 2022 on July 19 for PINs 14304091060000 and 14304091070000, alongside
the revised residential cross-section rule.

This is a recovered generating command, not a new justification for the selected
sample. The preliminary duplicate comparison must use the project universe before
historical coordinates were added; otherwise recovered buildings can match
themselves as already represented. A fresh reconstruction of this screening step
is being checked. The earlier 750-versus-391 candidate comparison omitted this
screen and should not be interpreted as evidence of 359 missing projects.

The zoning lookup handoff was also located in the saved history: on July 21 at
07:43:47 UTC, `historical_zoning_project_construction_year_lookup.csv` from the
historical-zoning validation task was copied to `density_construction_zoning.csv`.
The underlying zoning reconstruction still needs restoration; recovering this
copy command does not establish a raw-data rebuild.

Isolated checks of the restored final-review code reproduced the 795-row project
verification ledger, 43-row extended-permit summary, 795-row reviewed ledger, and
795-row final decision ledger byte-for-byte when each used its preserved
intermediate inputs. These are stage checks, not an end-to-end reproduction.
Other stage checks exposed inputs whose time coverage had changed: current
production sales omit pre-2006 transactions and current permits omit post-2022
applications that were used as construction-review evidence. Preserved full
sources are being checked separately.

The restored analytical exporter now joins the existing term table on the assumed
June 15 construction date. Relative to the frozen construction file, an isolated
check changes 17 alderman assignments: 15 projects (seven inside 500 feet) fall
in the 2009 ward-26 term gap already recorded by this branch, and two 2022 projects
inside 500 feet match Michael Scott Jr. on his recorded final service date. The
500-foot universe remains 3,710 projects; 3,701 have both assignments and scores,
while nine have no recorded term on one side. Assignment-status fields distinguish
these gaps from a serving alderman with a missing score. This deliberately differs
from the frozen export and still requires downstream estimation comparisons.
The existing production CSV and paper have not been replaced.


The discovery-stage replay subsequently reproduced the complete historical
coordinate file byte-for-byte: 391 rows, SHA-256
`1c96af5f6dd8be2f1b8655622b5827ec93438ffe80d240b382797aca605d0417`.
It uses the pre-July-19 residential selection, screens current-only project
locations, applies the recovered project/address rules, appends the four recorded
address matches, and applies the two recorded year corrections. The accepted
project groups have 388 member PINs; 387 have exact historical coordinates.
PIN 17173250540000 has no exact coordinate and is absent from the export,
consistent with the original inner join. This check used preserved historical
parcel/address records and a full residential source copy; moving every producer
and source snapshot into the production graph is still underway.

A sequential rebuild of the final-review section using preserved full sales and
permit sources also succeeded. All seven intermediate review outputs match the
archived files byte-for-byte: the verification ledger (795 rows), permit history
(176), extended-permit summary (43), reviewed ledger (795), Assessor-only evidence
(149), exact-permit summary (795), and final decision ledger (795). The permit
preparation retains the original 2006 lower bound and no study-period upper
bound. Both isolated Make graphs are incremental on an unchanged second build.


## Newly confirmed historical conflict-screen bug

The restored coordinate producers first reproduced the archived 391-row file
byte-for-byte, including all project/address screening outcomes. Inspection of
`build_density_project_lineage.R` then found that its historical-project summary
assigned `unitscount = max(unitscount)` before `n_distinct(unitscount)`, and likewise
for building area. In dplyr, the later expressions see the already aggregated
value. Both supposed conflict counts therefore equal one.

Ten historical groups had masked unit/area conflicts. Three were classified as
recovery candidates: `residential_14204010050000` (1,590 versus 1,740 square feet),
`residential_17054140070000` (four versus three units), and
`residential_19241040080000` (five versus four units). The first group's locations
were absent from the accepted coordinate export. The latter two contributed four
PIN locations, all beyond 500 feet. The frozen final dataset contains
`residential_tieback_19241040080000` at approximately 1,455 feet, with four units,
8,700 building square feet and 7,257 land square feet. This is not evidence that
its later adjudication was wrong; the downstream resolution remains to be traced.

The new screen counts distinct values before aggregating. Its 387-row coordinate
output has SHA-256 `440381a165846ba15edd4c8aeeda902d26099e19c4967effbaf57f6b4ca5477c`
and is identical with the preserved and fresh Assessor discovery inputs. Production
has not been replaced. Wider-bandwidth estimates and later project decisions need
reconciliation before promoting this correction.

The restored eligibility section also reproduced four archived files byte-for-byte
in a sequential Make replay: eligibility rules (8,995), uncorroborated retained
projects (1,885), multifamily classification (13,707), and the provisional sample
(8,705). Their earlier evidence producers are being restored separately.


## Additional producer checks

The historical permit evidence, project evidence inventory, permit classification,
and pre-period Assessor evidence each reproduce their archived tables byte-for-byte.
The predecessor lookup now aggregates each PIN-year before selecting the latest
eligible snapshot for a successor project. Its joins assert many-to-one contracts;
it avoids the original expansion of shared predecessor PINs against their full
assessment histories. All 12,892 predecessor-evidence rows remain byte-identical.

The commercial completion table, multicard external-review queue, and multicard
permit links are also byte-identical. Applying the external reviews reproduces
all 8,995 project IDs and construction attributes. Its score/side fields differ
because the preserved upstream multicard input and downstream reviewed snapshot
contain different inherited score vintages. The restored review application does
not estimate models or need alternate score specifications to apply decisions.
The final exporter independently recalculates analytical assignments and scores.

Rebuilding the 2015 footprint extract from the preserved shapefile ZIP and project
sites reproduces all 6,712 original features, including attribute values and exact
geometry blobs. GPKG container bytes are not an appropriate equality test because
metadata can change on writing.

`restored_stage_checks.csv` records the reference locations and SHA-256 values for
the completed CSV comparisons. These are explicitly partial-chain tests using
preserved upstream project inputs; they do not certify the unfinished full graph.


## Combined review-chain replay and source-vintage decision

The restored evidence, multicard, eligibility, footprint, and final-review stages
were then run together in one isolated Make graph. The resulting final export
contains 8,648 rows and is byte-identical to the earlier final-export check with
current scores and explicit term-date assignment. Its second `make -n` reports
nothing to do. Earlier project-construction and zoning inputs in this test remain
preserved intermediates; they are the next part of the restoration.

A wider filename search found no additional full historical Assessor copy. The
clean-rerun source produces 28,870 selected PINs (194 original PINs missing and one
extra); the broken-backup copy produces 28,967 (96 missing); the fresh source
produces 29,262 (all 29,063 original PINs plus 199 additional PINs). The researcher
does not know of an external backup. The fresh residential and commercial sources
have therefore been copied unchanged into the pinned source directory. Further
acquisition cannot silently overwrite these inputs.

Five fresh discovery PINs without current coordinates were outside the preserved
historical-address query scope: 17093140230000, 20032000620000, 20032250580000,
20033030390000, and 25031190320000. Their missing historical data must be treated
as unqueried, not evidence of absence. A complete historical-parcel acquisition
for the current missing-coordinate universe has been started, retaining its
request list for negative-response verification.

Further tracing of the early conflict-screen example found that
`residential_tieback_19241040080000` is resolved later by a stable, complete 2008
snapshot: four units, 8,700 building square feet and the summed 7,257-square-foot
lot. The recorded source IDs are 1924104008000012008 and 1924104009000012008.
This later mechanical resolution may preserve the project after the early screen
is corrected. Its actual source-to-final effect remains to be tested in the full
restoration; the early conflict alone does not invalidate its final attributes.

### September 4: refreshed historical coverage and additional restored stages

The researcher does not know of an external original-vintage backup. A fresh
full Assessor download is pinned explicitly; its vintage differences remain part
of the reconciliation, not assumed immaterial. Cook County parcel and address
history queries completed for every current discovery PIN without current
coordinates (1999–2025 history window). Both returned 15,455 rows, with no changed
fields on keys shared with the preserved extracts. The request list is pinned
alongside both new source files. The five additional unlocated PINs
17093140230000, 20032000620000, 20032250580000, 20033030390000, and 25031190320000
returned no parcel or address records. They remain unresolved. Fresh historical
inputs reproduce the corrected 387-coordinate export and the complete lineage
and address-screen tables byte-for-byte.

Four more multicard matching outputs reproduce exactly (273 adjudication rows,
1,082 successor-building candidates, 589 component matches, 630 episode edges).
The restored footprint evidence also reproduces all 273 rows exactly. Permit
revision links (19,461), unit mentions (3,960), and project summaries (9,674)
reproduce exactly, as do recovered-project duplicate screens and all 3,620
residual permit-chain dispositions. Successor condo requests (28) and links
(333) reproduce exactly. These are partial-chain checks using preserved earlier
project inputs.

The restored final audit ledger and boundary scope propagate the already
recorded commercial_17173220138002 land correction from their upstream ledger;
the old intermediate CSVs had retained land=1. Only that project's land and
associated provenance differ. The final production CSV already contains the
corrected land value.

Using fresh Assessor history with the preserved selected PINs changes ten
ordinary mechanical candidates to history-review cases and four tieback groups
from complete to incomplete proration. The row-level classification and field
changes are recorded in current_assessor_classification_changes.csv. The
fractional sibling-group table remains byte-identical. These changes have not
yet been propagated through the entire restored pipeline and cannot be read as
final-sample or estimate changes.

Geocoding on preserved input selections reproduces all 27,445 rows, every
attribute, and exact geometry blobs. The raw 1998 and 2024 ward maps were
recovered from Git before archive cleanup (f98d40e parent); the restored discovery
geography uses one copy of each of the four original maps. Discovery boundary
distances are being separated from the old score-filtered intermediate. This
also requires reconciliation: the old `parcels_with_ward_distances.csv` discarded
rows without signed distances after merging alderman scores. It was not a pure
geographic input. The effect on construction candidate selection is still being
measured.

### September 4: decision application, discovery distance, and current-source checks

The residential decision-application stage reproduces 13,088 projects, 13,580
component memberships, all 153 adjudicated project geometries, every centroid,
and all boundary-scope rows exactly on preserved candidate/evidence inputs. The
combined residential/commercial ledger reproduces 13,903 projects and 15,385
components exactly. The commercial decision hierarchy also reproduces all 1,128
source dispositions and 815 retained commercial projects exactly, including the
recorded land correction. This verifies recorded-decision application; it does
not yet establish a full raw-to-paper rebuild.

A standalone discovery-distance calculation reproduces every distance for the
25,013 PINs in the old scored input. The broader geocoded universe has 27,445 PINs.
Of the additional 2,432, only nine report study-period construction; six are
within 1,500 feet, four within 500 feet. All six are already in the preserved final
dataset through later project recovery. See score_filtered_discovery_pins.csv.
Discovery now retains geographic distances without conditioning on score
availability. The 1998 source map has no shared boundary for ward 19; its 94
pre-study records receive an explicit source-map coverage status and missing
distance. All other ward assignments must have a distance. This reproduces the
source limitation rather than inventing a boundary.

The real new task now runs fresh full Assessor selection through historical
screening, geocoding, and residential candidate inventory. It produces 29,262
selected residential records, 27,574 geocoded combined records, 1,292 tieback
groups, and 6,520 multicard rows. The 18 additional study-period PINs are listed in
current_assessor_new_study_period_pins.csv. Thirteen have current coordinates;
none are within 500 feet at those coordinates. Five lack coordinates. The
1999–2025 parcel/address queries return no rows for those five; an all-years
follow-up returns 2026 property addresses but still no parcel coordinates.
new_assessor_pin_address_followup.csv records those source observations. Four
addresses have possible older condominium/parcel matches; no automatic identity
or inclusion decision has been made from address similarity alone.

The final zoning assembly reproduces all 9,556 rows with preserved earlier zoning
inputs. The former model-estimation scripts have been split to restore only their
actual input-building operations. Those produce the same 9,663 preferred-card
and 9,545 final-pre-review project IDs. Differences are current scores, corrected
monthly-panel names, and the already recorded commercial land correction. The
final export separately reapplies daily-term assignments. The original earlier
zoning derivation and remaining project/evidence producers are still outstanding.

### Fresh-history temporal reconciliation still open

On the preserved candidate inventory, the fresh full Assessor history changes
four tieback lineages from having no complete snapshot to having a complete
2026 snapshot. Two candidates move from review_required to retain_mechanical:
14204040300000 and 17071210020000. Both are already in the frozen final data
through subsequent review. The latter is 419 feet from its boundary; its newly
combined snapshot reports 3,115 building square feet and 2,976 land square feet,
whereas the reviewed final project has 3,114 and 1,440. This is an upstream
source-vintage difference, not a demonstrated change in a rebuilt final sample.
The existing residential resolution script requires its original 184-project
review scope exactly and will reject an altered scope. Preserve and apply the
recorded resolutions explicitly; do not let a new mechanical status silently
bypass them. See current_assessor_temporal_candidate_changes.csv. The other two
lineages remain in their existing candidate categories. Full propagation with
the refreshed candidate universe is not yet complete.

Six additional supporting stages reproduce preserved CSVs byte-for-byte:
commercial land/unit evidence, address permit history, residential history,
spatial permit matches (46,348), and accepted episode geometry coverage (52).
The accepted episode geometries (48) and final commercial centroids (815) also
match attribute and geometry rows exactly. These remain stage replays with
preserved upstream project inputs, not a raw-to-paper completion claim.

### Commercial conflict evidence preserved

`build_commercial_project_candidates.R` previously replaced each family-vintage
field with a single value (NA when conflicting), then constructed its list of
source values from that replacement. The restored script keeps selected values
in separate columns until the original value lists have been recorded. On the
preserved source, this restores competing values in 9 year, 18 unit, 20 building
area, and 26 land area evidence cells. All other commercial candidate and permit
outputs remain byte-identical in the replay. Conflict counts and mechanical
selection decisions were already computed before collapse and are unchanged.

The fresh complete candidate inventory produces 1,045 temporal tieback lineages
versus 1,034 previously. Eleven are newly in scope, mostly post-2022; the
2007 lineage 17073250521001 needs review and the 2022 lineage 17194230070000 is
mechanically resolved. These are candidate-discovery differences, not final
inclusions. The raw-to-temporal chain completed, but changing its Makefile
prerequisite afterwards intentionally makes its configuration-dependent outputs
stale until the final integrated run.


## September 6: land scope and further replay checks

The new `review_multicard_land_scope.R` audit compares the frozen paper dataset
with the recorded multicard adjudication and card-to-successor matches. Its
`reference/` files are explicitly historical computed evidence, not raw inputs
or a substitute for the restored production derivation. The Make build produces
248 project rows, including 103 with a recorded successor match and 89 with every
target card matched. Four projects within 500 feet have complete matches that
reproduce final building totals and a parent-to-successor-land ratio above two:
19111200130000, 13252030410000, 17271290150000, and 17271290140000.

The first project has 7 units and 13,753 building square feet on 307,739 land
square feet. Its seven matched successor lots total 21,069 square feet. Each
card has a close successor building-area match; all seven successor project
rows are absent from the frozen final dataset. The earlier manual episode
review retains the seven cards, while the July 24 external review explicitly
cannot independently verify the parent total. The unresolved issue is whether
the parent land and the retained construction episode describe the same site.
No replacement denominator or exclusion has been adopted. Subdivision and common
land must be investigated before interpreting a large ratio as an error.

The parcel-universe acquisition now verifies and replays the preserved native
source on ordinary builds; `download_recipes.make` writes an intentional API
refresh to a different output. The rewritten 882,741-row, 124-column dataset is
byte-identical to the prior consumer file. Source and consumer key checks pass.
Disposable Make 3.81 checks verified first replay, missing-output recovery,
unchanged reuse, and preservation of the previous output on source/checksum
failure. Separate historical matching tests reject unqueried PIN-year and
point-year requests before writing accepted output. Other mutable acquisition
tasks still need vintage preservation before a full paper build.

Two additional commercial producers reproduce their preserved outputs exactly:
ground-up evidence (800 rows) and permit evidence by project (169), chain (322),
and address (300). Historical project geometries reproduce attributes and
geometry exactly. Tieback episode candidates (79 rows) and the no-snapshot
review (23 rows) reproduce exactly. These comparisons still use preserved
upstream products where their producers are missing.

Zoning replay exposed a confirmed missing-data bug: the zone grouping function
classified an absent polygon match as `Other`. At residential_17211280030000,
the 2012 map has no intersecting polygon. Both zoning scripts now preserve that
missing value. The 9,674-row preferred zoning file and 9,556-row final zoning
file each change only that project's `zone_group_2012` evidence cell. Final
construction zoning groups and assignment sources are unchanged. The project's
Downtown assignment comes from validated component evidence. The 2006 zoning
map and validated project zoning are now declared generated inputs, but their
historical reconstruction producers remain unfinished.

## September 7: residential evidence producers

The restored City-building evidence reproduces 184 rows exactly. Eight selected
predecessor polygons reproduce both their attributes and WKB geometry. The
class-297 cohort, decision and source-disposition outputs reproduce 52, 57 and
53 rows respectively. A missing write of the already-computed source-disposition
object was restored; no decision changed. The Make recovery check deleted only
a generated fixture output and verified regeneration in a parallel build.

For the fixed preserved review scope, the rebuilt full Assessor history contains
4,627 rows with unchanged row IDs. The replacement raw source changes exactly
three fields: year_built 1895 to 2019 on row 1420404030000012026, and PIN proration
1 to 0.6 and 1 to 0.4 on rows 1707121002000012026 and 1707121048000012026. A
subsequent card-evidence replay adds only the 2026 assessment / 2019 construction
episode for residential_tieback_14204040300000. Its episode table has 597 rather
than 596 rows; the selected 123-project evidence table remains unchanged.
This does not settle the broader source-vintage issue: the initial candidate
universe and other source uses must still be reconciled.
