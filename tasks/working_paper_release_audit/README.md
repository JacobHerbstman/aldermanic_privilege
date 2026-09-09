# Working paper release audit

## Current unfinished construction records

`output/current_construction_questions.csv` is the current list, rebuilt with
`make ../report/current_construction_questions.csv.log` in `code/`. It uses the
current residential decisions, selected commercial records, final commercial
locations, and eight existing decision tables. It excludes closed source records
and density-ineligible commercial records without locations. Each source project
appears once; source projects are not necessarily distinct buildings.

The list distinguishes a recorded decision awaiting verification/application,
a building without a matching recorded decision, overlap between the residential
and commercial sources, a specific conflicting building decision, and an
unfinished location. Individual homes still located at an older larger parcel's
center are explicitly unfinished even when they have a numerical distance.
The named conflict reference records questions, not approved production changes.
Earlier map-shape flags are not automatically new building decisions: a concave
parcel center or multipart map does not invalidate the retained source-reported
land by itself. The explicit unresolved individual-location label is treated
differently because it identifies a different, larger property as the location.
The six non-new-construction decisions and one verified successor-duplicate
suppression now run in the residential candidate producer. The earlier Sinclair,
Landmark, Trio and Aqua recommendations have also been applied in the existing
commercial decision and location inputs; their settled building decisions are
removed from the question reference. Missing locations remain listed separately.

The older case writeups below preserve the research history; their old counts
are not a current work list. Final assembly and the unavailable original source
vintage remain separate replication requirements, not additional case rows here.

Independent checks of frozen paper data and restored construction stages. This
audit does not supply replacement production observations or certify an unfinished
raw-to-paper build. The unit of observation varies by explicit output: project,
project/successor PIN, or permit identifier.

The denominator review uses the frozen project export, recorded successor matches,
pinned historical parcel features, and the pinned full Assessor history. Its
`reference/README.md` distinguishes computed historical evidence from raw inputs.
`denominator_permit_evidence.csv` selects a broad, explicit address search from the
pinned full City permit source. A search hit is not an adjudicated project match;
revisions, replacements, and later construction remain visible. The normal Make
rules generate keyed data reports for these audit datasets.

See `denominator_review.md` for the case interpretations and unresolved decisions.
Public planned-development PDFs are acquired by the separate
`download_construction_development_records` task. Planning permission does not
establish completion or resolve the allocation of common land.

`compare_density_exclusions.R` repeats the current main density specification
for the frozen projects and existing scores, with and without the committed
three-project denominator ledger. It saves all nine estimated distance-bin
coefficients for four panels and two scenarios. This isolates the approved
exclusions; it does not incorporate other unfinished reconstruction changes.
For this fixed-input comparison run Make with both existing upstream snapshots
held fixed:

```
make -o ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv -o ../../new_construction_analysis_data/output/new_construction_analysis_data.csv ../output/density_exclusion_comparison.csv ../report/density_exclusion_comparison.csv.log
```

The score file used here was last generated September 3. An initial ordinary
Make invocation attempted to rebuild upstream Census controls and failed on a
Census hostname lookup; it did not update the score file or ward-control CSVs.
The fixed-input invocation is a sensitivity calculation, not a successful
end-to-end production build.

The expanded `multicard_parent_coverage.csv` screen covers all 248 retained
multicard projects, including incomplete and absent card matches. It uses only
actual parcel polygons for containment; search buffers and missing geometry are
explicitly unassessed. `multicard_followup_permits.csv` preserves the broad Lenox,
Rockwell and 33rd/Prairie permit searches, including revisions and reinstatements.
The findings and unresolved questions are recorded in `denominator_review.md`.

The density sensitivity now includes three scenarios: current frozen eligibility,
the three original land-allocation exclusions, and all six approved exclusions
including Lenox. The saved table contains 108 coefficient rows. It holds current
construction records and scores fixed, as in the command above; it is not a
full rebuild or a prediction of all reconstruction changes.

## Manual-decision dependence

`manual_review_inventory.md` inventories the restored decision/review inputs and
identifies candidates for general rules. Counts are ledger rows, not distinct
projects or a count of unavoidable human judgments. `review_manual_review_increment.py`
compares all 273 preserved external-review rows against their preceding,
already-adjudicated input. The output distinguishes agreement from type or
numerical changes. Agreement at this layer does not establish independence from
manual decisions earlier in the pipeline and does not validate the retained
project's land denominator or completion year. No production decisions change.

`review_manual_episode_increment.py` separately compares the 42 recorded episode
decisions with the computed disposition and numerical fields in the preserved
adjudication. It does not certify successor suppressions or the expanded graph.
