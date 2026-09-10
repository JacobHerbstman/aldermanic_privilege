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

### Individual-lot location comparison

`review_individual_home_locations.R` tests later exact-parcel coordinates for retained individual residential records still using a historical predecessor centroid. It requires exact agreement in 2025 units, floor area, reported land area and construction year, one occupied assessment card, and containment in the record’s historical site. It compares boundary distances using construction-year ward maps and separately matches the frozen paper input by project identifier. The output is an audit proposal, not a production input. Run the specific report target while holding `../../new_construction_analysis_data/output/new_construction_analysis_data.csv` with Make `-o` to preserve the frozen comparison.

### The 69 location exceptions

`review_remaining_individual_locations.py` follows the fixed 69-record cohort in `reference/individual_location_remaining_cases.csv`, compares all available residential assessment years and saved parcel points, and records the current production location result. It generates `remaining_individual_location_findings.csv` and its standard report. The reference records which cases were investigated, not cleaning decisions. `remaining_individual_locations.md` records the substantive follow-up and initial verdicts. Three construction-age concerns are also retained in the consolidated questions reference so successful geocoding does not close them accidentally.

The `added_zoning_eighteen_review.csv` review uses the fixed cohort in
`reference/added_zoning_eighteen.csv`, the preferred zoning output, approved
preserved historical zoning, and the recorded City matters extract. It checks
construction-year zoning, not pre-project zoning. Seventeen classifications
remain unchanged; Ontario is corrected after a passed amendment between its
borrowed historical year and construction. This cohort file is an audit baseline,
not a production correction input.

### Added observations and historical aldermen (September 10)

`review_entering_home_assignments.py` identifies the 368 entering observations
outside the external multifamily category in the saved attenuation comparison.
FAR and DUPAC have the same entering cohort. It reports their construction-year
wards, serving aldermen on both sides, and score ordering, and checks names
against the recorded terms. This is a description of assignments, not a new
spatial verification or a ward-level decomposition of the coefficient change.
The category includes 355 one-dwelling observations and 13 with multiple dwellings;
calling every member a single house is inaccurate.

The full rebuild is paused at Jacob's request. This audit used existing saved
inputs without refreshing production. From `code/`, the exact invocation was:

```
make -o ../output/construction_estimation_project_changes.csv -o ../input/current_construction_analysis.csv -o ../input/terms.csv -o ../input/scores.csv ../report/entering_home_assignments.csv.log
```

The `-o` options deliberately hold these four existing inputs fixed for this
historical comparison. They are not part of the fresh-clone validation protocol.

The follow-up `review_ward_two_years.py` checks all 113 second-ward entrants
against the preserved Assessor history. Every selected year matches its cited
Assessor row; that does not make the year correct. Thirty-seven properties have
exactly the selected floor and land areas in an assessment predating the selected
construction year. Thirty-five are selected as 2006, two as 2007. Eleven more
selected as 2007 have historical 2005 reports, but their available history starts
in 2009. The audit changes no production values. Reproduce against saved inputs:

```
make -o ../output/entering_home_assignments.csv -o ../input/current_construction_analysis.csv -o ../input/residential_assessor_history.parquet -o ../input/parcel_addresses_2025_chicago.csv ../report/ward_two_year_review.csv.log
```

### Web checks and proposed chronological year rule

September 10 follow-up: Browser searches and recorded permit descriptions support
separating the 37 direct chronological conflicts from the 11 Union Row homes.
These are recommendations, not applied production corrections.

* Aberdeen: Homes.com reports a March 16, 2004 purchase from the development
  corporation at 26 S Aberdeen. This page omits the unit, so it corroborates the
  development's early existence, not every individual PIN. Listing year fields
  disagree. https://www.homes.com/property/26-s-aberdeen-st-chicago-il/pdhnxc2t63dk5/
* Monroe: Redfin repeats 2007 for 1143 and 1125 W Monroe; this is not independent
  evidence that overrides their 2006 assessments with matching measurements.
  https://www.redfin.com/IL/Chicago/1143-W-Monroe-St-60607/home/12588619
  https://www.redfin.com/IL/Chicago/1125-W-Monroe-St-60607/home/12602934
* Union Row: recorded new-construction permits 100178653, 100178882 and 100178883,
  issued May 17, 2007, explicitly describe the 648, 644/634 and 630/622 W 16th
  homes. The older Assessor year 2005 precedes those permits. The developer says
  the 35-home development was completed in 2010, without individual completion
  dates. Do not impose either 2005 or the entire development's 2010 date on every
  house. https://belgraviagroup.com/developments/union-row
  Zillow reports 2007 for parcel 17211320080000:
  https://www.zillow.com/homedetails/648-W-16th-St-2-Chicago-IL-60616/101420619_zpid/

Proposed rule: after identifying the same building, retain the latest reported
Assessor year that is consistent with its recorded chronology. A reported year
later than an earlier assessment of that same building is inadmissible without
specific contrary evidence. A year before an applicable original new-building
permit is also contradictory and requires reconciliation (permit reinstatements
must not be confused with original permits). Use reported years, not the year of
first assessment as an invented completion date. Do not use an older building on
the parcel to constrain a genuinely new building. Conflicting evidence that does
not identify one defensible year stays explicitly unresolved.

For the 37 direct conflicts, the latest historically reported year no later than
the first matching assessment gives 33 dates of 2004 and four of 2005. All would
leave the study period. Of these 37, 35 match the earlier card on parcel/card,
class, floor area, land area, bedrooms, full/half baths, residence type and apartment
count; two differ only in half-bath count among these fields. This is strong
continuity evidence, not a universal replacement-building detector.

All 113 histories were examined: 37 direct conflicts; 11 Union Row conflicting
years with 2007 permit evidence; one Haddon parcel with both old and new cards;
one 57 E 23rd record with a transient 2010 report amid 2007 reports; and 63 with
one reported year. This is a year audit, not certification of all land/identity
choices. The production rule and a population-wide impact check remain to be
implemented after settling its treatment of ambiguous building histories.

### Population trial, before adoption

The draft chronological rule is in `select_residential_assessments.R`, at the
existing year-selection step. It matches parcel, card, class, floor area, land
area, residence type and apartment count, then chooses the latest reported year
no later than the first matching assessment. Existing exact-permit corrections
and reviewed construction years take precedence. This is a trial: downstream
building corrections, final geography and estimation have not been rebuilt.
Do not interpret the draft's other changes as author-approved decisions.

The original selected assessments are preserved in
`reference/residential_assessments_before_chronology.csv`. The narrow selection
build held its six existing data inputs fixed with Make `-o`; no upstream refresh
or clean-clone validation was attempted. Its 28,100 candidates retain identical
units, floor areas, land areas and measurement-row identifiers. It changes 981
years, including all approved 37 (33 to 2004 and four to 2005); all eleven Union
Row years remain 2007.

`review_population_chronology.py` compares that trial to every one of the 13,714
saved final projects. Of these, 869 would change under the single-card trial;
273 are within 500 feet, and 60 of those move before 2006. Twenty-six changes
exceed three years and deserve scrutiny before adoption; three years is a
reporting description here, not a cleaning cutoff. The final dataset is unchanged.

A second screen compares full project measurements against preserved residential,
commercial and condominium parcel-year reports. It requires all component parcels
and excludes ambiguous duplicate source reports. Summed parcel areas can fail to
match shared-site measurements, so absence of a comparable snapshot is explicitly
unverified, not a pass. The screen finds 924 potential chronological contradictions
(922 residential, two commercial), 11,958 without that direct contradiction and
832 without comparable measurements. The two commercial cases are 4654 N Sheridan
and 202 W Hill: 2021 reports say 2020, while later reports say 2022. No commercial
correction is adopted. This screen is not an independent building-identity audit.

From `code/`, reproduce the population comparison using the saved trial:
```
make -o ../input/residential_selected_assessments.csv -o ../input/preferred_new_construction_project_ledger.csv -o ../input/current_construction_analysis.csv -o ../input/residential_assessor_history.parquet -o ../input/commercial_value_raw.csv -o ../input/construction_condominium_history.csv ../report/population_chronology_review.csv.log
```

### Review of all 26 large year changes

`reference/large_year_change_recommendations.csv` records recommendations, reasons,
original permit identifiers and web corroboration. These are audit recommendations,
not adopted production overrides. `review_large_year_changes.py` attaches addresses
and current sample membership. All 26 histories were inspected, and the preserved
City permit file was searched by full recorded street address as well as existing
project links. Missing links did not mean missing permits: several replacement
buildings have demolition and new-building permits at the exact street address.

Recommendations: seven retain as new construction (some with revised completion
proxies), fourteen exclude as existing buildings/additions, two exclude before
2006, and three hold out for insufficient new-building identity evidence. Four
are within 500 feet: retain Arthington, exclude Leavitt and Monitor, hold La Salle.
No decisions in this table have been applied to production.

This review falsifies unrestricted adoption of the draft rule. Early reports can
attach a demolished building's year to its replacement's measurements. Arthington
has 2008 demolition/new-building permits and a listing explicitly reporting a
2010 rebuild, despite early reports carrying 1892 onto its new floor area. The
rule needs original permit/episode evidence before accepting old matching-year
reports. A recorded exact-address permit can supply evidence even when the prior
PIN/polygon matcher did not link it. These are two source issues to address, not
26 independent code patches. Do not resume downstream estimation using the draft.

Reproduce the saved review from `code/` without refreshing production:
```
make -o ../output/population_chronology_review.csv -o ../input/parcel_addresses_2025_chicago.csv ../report/large_year_change_review.csv.log
```

### Adopted correction: 37 reviewed pre-period buildings (September 10)

The broad automatic year correction was withdrawn. Only the 37 approved buildings
from the ward-two review receive recorded year corrections: 33 to 2004 and four
to 2005. These decisions use the existing residential reviewed-year input and
therefore exclude these buildings from the 2006–2022 sample. Comparison of the
rebuilt assessment selection with the pre-trial reference confirms exactly these
37 year changes, with no changes to units, floor area or land area.

The population and 26-case tables above describe the withdrawn experiment; their
recommendations are not adopted. The current selection input no longer contains
the trial, so the earlier population-comparison command does not recreate that
experiment from current production inputs. Preserve the recorded audit outputs
as the historical comparison. Main density estimation was rebuilt successfully using
only the approved 37 corrections; this does not constitute the final clean-clone
replication test.

The final construction ledger falls from 13,714 to 13,677 records and the analysis
data from 9,530 to 9,493. All columns of every surviving record are unchanged.
All-construction FAR changes from -0.104 (SE 0.047) to -0.097 (SE 0.045), and
DUPAC from -0.151 (SE 0.073) to -0.148 (SE 0.073). Regression counts fall by 37
to 4,023 and 4,034 respectively. Multifamily estimates and counts are unchanged.
The density task's subsequent dry run schedules no substantive producer.
