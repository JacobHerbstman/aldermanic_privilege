# Commercial and residential new-construction sample audit

This task reconstructs the 2006-2022 new-construction sample from the Cook County
residential improvements and commercial valuation files. It is an audit only:
no production dataset, regression, figure, table, or paper file is changed.

The project definition and source hierarchy are in
`ADJUDICATION_PROTOCOL.md`. The protocol was fixed before re-estimating any
density model. All results remain audit-only.

## Initial reconstruction

Before the permit-universe closure described below, the combined preferred file
contains:

| Sample | Projects | FAR eligible | DUPAC eligible |
|---|---:|---:|---:|
| Residential, citywide | 13,088 | 13,079 | 13,088 |
| Commercial, citywide | 815 | 804 | 813 |
| Combined, within 1,500 feet | 9,674 | 9,657 | 9,672 |
| Combined, within 500 feet | 4,177 | 4,164 | 4,176 |

The 13,903 projects contain 15,385 component PINs. Project IDs and component
PINs are globally unique, every project has a location, and the residential and
commercial sources no longer overlap.

## Final permit-universe closure

The final pass starts from every issued residential new-construction permit and
reconciles the 3,620 permit chains not already classified by the preferred
project ledger. It also rechecks predecessor-successor and cross-source
duplicates in that ledger.

| Final ledger action | Projects |
|---|---:|
| Retained from the preferred ledger | 13,672 |
| Recovered completed projects added | 35 |
| Duplicate projects suppressed | 231 |
| Final projects | 13,707 |

The final ledger contains 15,151 unique component PINs. Project IDs, component
PINs, and exact address-year-physical-record combinations are unique. All 1,836
previous duplicate candidate pairs have dispositions. A separate adversarial
screen compares every recovered project with every retained project using
normalized addresses and a 100-foot, four-year, similar-building rule. It finds
no recovered-to-retained candidate. Nine recovered-to-recovered pairs are
adjacent one-family buildings with different addresses, PINs, and permit
chains; each is explicitly retained in
`adjudication/recovered_project_duplicate_pair_decisions.csv`.

The residual permit chains have the following final dispositions:

| Disposition | Permit chains |
|---|---:|
| Already represented | 1,885 |
| Recovered project added | 35 |
| Collapsed into a recovered project | 1 |
| Completed, but no defensible whole-building density fields | 179 |
| Completion or density fields insufficient | 3 |
| Ambiguous without independent completion evidence | 346 |
| No independent completion evidence | 934 |
| Not a residential building | 236 |
| Permit not completed | 1 |

The 179 completed cases are not overlooked usable observations. They lack a
defensible combination of whole-building units, building area, and lot area,
often because a multi-parcel footprint cannot be reduced to one building-level
assessor record. Adding them would require inventing a density outcome.

All 35 recovered projects fall within 1,500 feet of a ward boundary, and six
fall within 500 feet. The canonical final ledger has 9,556 projects within
1,500 feet and 4,123 within 500 feet. The regression input has 4,118 projects
within 500 feet because the existing multicard-year consistency rule excludes
five projects; that analytical rule is unchanged.

Five projects close to the 500- or 1,500-foot thresholds have independent
coordinate checks and fixed inclusion decisions in
`adjudication/threshold_sensitive_coordinate_decisions.csv`. None remains
unresolved. Construction-year zoning is assigned for all 35 recovered projects.
Twenty-nine are stable over the relevant official map interval, five match an
already validated project PIN and year, and 2410 S Canal is assigned Commercial
from the January 11, 2006 City Council journal ordinance for application 15256.

The final recovered-project file retains the representative application and
issue dates, the assessor completion-year interval, and lower and upper bounds
for application-to-completion and issue-to-completion time. The broader
`permit_project_timeline_candidates.csv` and
`permit_project_timeline_detail.csv` files retain candidate permit histories
for later time-to-build work; they should not be treated as unique project
matches without a separate linkage decision.

## Final density comparison

The final comparison preserves the production-selected card for multicard PINs
and the active score, controls, fixed effects, and ward-pair clustering. The
table below uses the common FAR-DUPAC sample so each outcome has the same
observations.

| Multifamily outcome | Previous preferred | Deduplicated existing | Final |
|---|---:|---:|---:|
| FAR, continuous | -0.115 (0.077) | -0.127 (0.076) | -0.127 (0.074) |
| DUPAC, continuous | -0.148 (0.086) | -0.159 (0.086) | -0.158 (0.084) |
| FAR, binary | -0.182 (0.088) | -0.206 (0.083) | -0.205 (0.081) |
| DUPAC, binary | -0.184 (0.122) | -0.199 (0.114) | -0.198 (0.110) |
| Observations | 903 | 890 | 893 |

The recovered projects barely change the deduplicated estimates. The larger
movement comes from suppressing confirmed duplicate project records. Complete
all-construction, multifamily, common-sample, and outcome-specific estimates
are in `final_density_model_results.csv`.

## Residential construction

The final residential ledger has 13,088 projects and 13,580 component PINs.
It combines:

- 12,935 mechanically retained projects after duplicate and invalid-site
  exclusions;
- 139 projects resolved from the main review queue; and
- 14 projects from the final adjudicated tail.

Nine projects have verified units and land but no comparable whole-building
area. They enter DUPAC but not FAR.

The review queue covers 184 source projects that could enter the 1,500-foot
universe. It produces 139 final projects, with 21 source records excluded, 22
replaced by commercial projects, and one replaced by another residential
project. Forty-four difficult tail records are fully dispositioned; none
remain unresolved.

Historical predecessor sites initially gave 2,834 projects shared coordinates.
An exact successor-PIN centroid or accepted address point could be placed inside
the selected historical site for 2,699 of them. The remaining 135 retain the
historical site centroid. Among projects inside 500 feet, none of those retained
site polygons intersects the ward boundary.

## Commercial construction

The commercial source begins with 1,128 candidate source families. The final
source disposition is:

| Disposition | Source families |
|---|---:|
| Retained | 762 |
| Retained as multiple projects | 10 |
| Excluded outside 2006-2022 | 330 |
| Excluded after review | 22 |
| Replaced by residential projects | 4 |

Those decisions produce 815 projects. All 1,128 source families have one
disposition, and no project remains unresolved.

The final projects contain 1,805 component PINs. Official historical polygons
locate 1,596 exact PIN14s and six unique PIN10 predecessors. Of the 203
components absent from the historical service, 202 use exact current PIN
centroids. One component is unlocated, but its project has 13 other located
components and remains spatially identified.

The official historical parcel snapshot and the 203 known-absence responses are
stored in `adjudication/`. This makes the current audit reproducible without
resending the complete parcel list to Cook County. A live query is needed only
if the requested year-PIN10 universe changes.

## Final cross-source case

Residential PIN `16242010150000` and commercial project
`commercial_16242010140000` describe the same 1218-20 S Washtenaw site.
Permit 100349959 and the later residential assessment support one five-unit
building completed in 2010. Stable commercial records supply 7,635 square feet
of building area and 5,691 square feet of land for the two-PIN site. The
commercial project is retained and the residential record is suppressed.

## Outputs

The final audit files are:

- `preferred_new_construction_project_ledger.csv`
- `preferred_new_construction_project_components.csv`
- `preferred_new_construction_project_centroids.gpkg`
- `preferred_new_construction_boundary_scope.csv`
- `preferred_new_construction_validation_summary.csv`
- `final_new_construction_audit_ledger.csv`
- `final_new_construction_boundary_scope.csv`
- `final_new_construction_zoning.csv`
- `final_new_construction_duplicate_validation.csv`
- `final_adversarial_duplicate_candidates.csv`
- `final_residual_permit_chain_dispositions.csv`
- `final_density_model_input.csv`
- `final_density_model_results.csv`

The corresponding residential and commercial ledgers remain available
separately. Adjudication decisions are in `adjudication/`; intermediate evidence
and review queues are in `output/`.

## Intermediate density decomposition

The preferred project ledger was re-estimated with the active Table 2
specification: 500-foot bandwidth, construction-year broad zoning group,
1,320-foot boundary-segment and construction-year fixed effects, side-specific
linear distance controls, ward controls, the through-2022 alderman score, and
ward-pair clustered standard errors. The audit reproduces the active production
table exactly before changing the sample.

| Sample and outcome | Production | Preferred |
|---|---:|---:|
| Multifamily FAR, continuous | -0.140 (0.081) | -0.085 (0.057) |
| Multifamily DUPAC, continuous | -0.158 (0.079) | -0.118 (0.066) |
| Multifamily FAR, binary | -0.278 (0.103) | -0.142 (0.101) |
| Multifamily DUPAC, binary | -0.247 (0.092) | -0.157 (0.133) |
| All-construction FAR, continuous | -0.037 (0.031) | -0.036 (0.032) |
| All-construction DUPAC, continuous | -0.011 (0.048) | -0.019 (0.047) |
| All-construction FAR, binary | -0.100 (0.057) | -0.100 (0.057) |
| All-construction DUPAC, binary | -0.068 (0.080) | -0.115 (0.080) |

The preferred FAR models use 4,159 all-construction projects and 973
multifamily projects. DUPAC also retains projects with verified units and land
but missing building area, producing samples of 4,172 and 986. Four otherwise
eligible projects are excluded because the June 15 construction proxy falls in
a documented aldermanic vacancy month, so a cross-boundary score comparison
cannot be formed.

Replacing only the commercial sample leaves the multifamily estimates close to
production. Replacing only the residential sample produces most of the
attenuation. The full decomposition is in
`preferred_density_model_results.csv`.

The preferred residential ledger above sums same-year cards for a multicard
PIN. That is not the production rule, which selects one assessor card per PIN.
To separate this choice from the location correction, the audit also keeps the
production-selected card values. Five projects inside 500 feet have different
construction years under the two rules; one is outside the production study
period and the other four are omitted from both sides of the matched
comparison.

| Multifamily outcome | Production card rule | Add ordinary geography recoveries | Fully corrected, production card rule |
|---|---:|---:|---:|
| FAR, continuous | -0.116 (0.073) | -0.086 (0.074) | -0.115 (0.077) |
| DUPAC, continuous | -0.135 (0.076) | -0.109 (0.076) | -0.142 (0.087) |
| FAR, binary | -0.235 (0.106) | -0.193 (0.102) | -0.182 (0.088) |
| DUPAC, binary | -0.207 (0.092) | -0.170 (0.100) | -0.193 (0.127) |

The middle column adds historically located single-PIN projects to the
production sample but does not add class-297 resolutions, tieback projects, or
commercial revisions. It adds 48 multifamily projects inside 500 feet: 39
single-card PINs and 9 multicard PINs evaluated using the production-selected
card. The final column applies the commercial and residential reconstruction
while continuing to use the production-selected card for multicard PINs.

Holding that fully reconstructed production-card sample fixed, the four
stringency-score control choices give:

| Multifamily outcome | Keep income and bachelor's share | Drop income | Drop bachelor's share | Drop both |
|---|---:|---:|---:|---:|
| FAR, continuous | -0.115 (0.077) | -0.150 (0.067) | -0.094 (0.077) | -0.157 (0.069) |
| DUPAC, continuous | -0.142 (0.087) | -0.119 (0.072) | -0.102 (0.090) | -0.132 (0.076) |
| FAR, binary | -0.182 (0.088) | -0.193 (0.101) | -0.185 (0.099) | -0.194 (0.101) |
| DUPAC, binary | -0.193 (0.127) | -0.232 (0.143) | -0.260 (0.138) | -0.233 (0.143) |

The FAR continuous estimate is significant at 5 percent when income is
omitted, with or without bachelor's share. Under the full-control score, only
binary FAR is significant at 5 percent. All-construction continuous estimates
are small and positive under all four score constructions; all-construction
binary estimates remain negative and statistically insignificant. Complete
results are in `new_sample_score_control_density_results.csv`.

Clustering by boundary segment instead of ward pair leaves every coefficient
unchanged and replaces 90 multifamily ward-pair clusters with 433 FAR or 439
DUPAC segment clusters. For the full-control score, the multifamily results
become:

| Outcome | Ward-pair clustering | Segment clustering |
|---|---:|---:|
| FAR, continuous | -0.115 (0.077), p = 0.140 | -0.115 (0.066), p = 0.081 |
| DUPAC, continuous | -0.142 (0.087), p = 0.106 | -0.142 (0.079), p = 0.073 |
| FAR, binary | -0.182 (0.088), p = 0.042 | -0.182 (0.101), p = 0.071 |
| DUPAC, binary | -0.193 (0.127), p = 0.131 | -0.193 (0.119), p = 0.105 |

Segment clustering therefore does not uniformly reduce uncertainty. It makes
the continuous estimates more precise but makes binary FAR less precise.
Ward-pair clustering remains the more conservative design choice because
separate segments comparing the same two wards share aldermen, scores, and
potentially common shocks. Complete results for both cluster definitions are
in `new_sample_score_control_density_cluster_results.csv`.

The outcome-specific multifamily samples differ because 13 projects have
verified dwelling counts and lot area but no defensible gross building area.
Seven are commercial assessor projects and six are residential class-297 or
tieback projects. All 13 can be used to calculate DUPAC, but none can be used
to calculate FAR; there are no FAR-only observations. Restricting both outcomes
to the 903 FAR-eligible projects barely changes the full-control,
segment-clustered DUPAC estimates: the continuous coefficient changes from
-0.142 (0.079) to -0.148 (0.080), and the binary coefficient changes from
-0.193 (0.119) to -0.184 (0.119). The four score variants on this common sample
are in `new_sample_score_control_density_common_sample_results.csv`.

On the common sample, omitting both income and bachelor's share changes
continuous FAR from -0.115 to -0.157 and binary DUPAC from -0.184 to -0.250.
The relative changes are concentrated. For continuous FAR, the largest
leave-one-pair-out contributions to the difference are 3-4, 1-26, and 4-25.
For binary DUPAC, 2-27 dominates, followed by 1-26 and 44-46. At 2-27, removing
the pair changes the full-control estimate to -0.271 and the neither-control
estimate to -0.134, reversing their ordering. The mechanism is the
Burnett-Fioretti comparison: the full-control score places Burnett slightly
above Fioretti, while the neither-control score places Fioretti above Burnett,
flipping 30 of the pair's 63 projects. Continuous DUPAC is not more negative
under the neither-control score. Complete leave-one-pair-out results and score
changes are in `neither_score_pair_leave_one_out.csv` and
`neither_score_pair_treatment_changes.csv`. These pair contributions come from
separate re-estimations and are not additive.

The reconstructed sample does not uniformly increase standard errors. Relative
to production, multifamily FAR standard errors fall from 0.081 to 0.077 in the
continuous model and from 0.103 to 0.088 in the binary model. Continuous DUPAC
rises from 0.079 to 0.087, while binary DUPAC rises from 0.092 to 0.127. The
last change reflects ward-pair covariance rather than a smaller project count:
the reconstructed binary DUPAC model has an IID standard error of 0.102 but a
ward-pair-clustered standard error of 0.127. The old model had an IID standard
error of 0.114 and a smaller clustered standard error of 0.092.

The reconstructed sample contains 916 DUPAC observations in 90 ward-pair
clusters, compared with 881 observations in 93 clusters in production.
Recovered projects are concentrated within existing pairs rather than adding
independent comparisons. Binary DUPAC is also more pair-sensitive: removing
2-27 changes the coefficient from -0.193 to -0.283. Across all pair omissions,
the largest change is 0.090 in the reconstructed sample and 0.044 in
production. Thus the loss of DUPAC precision is real, but it is localized; the
FAR standard errors do not increase.

The earlier frozen geocoder correction was narrower. The current production
cross-section contains 13,773 residential PINs built from 2006 through 2022.
The 2025 Parcel Universe locates 12,490; the frozen historical file adds 385
that are in the current study-period cross-section; 898 still have neither
source. The frozen file contains 391 rows in total because six now fall outside
the current study-period cross-section. It fixed the approved subset of the
2025-survivor problem, but it did not reconstruct parcel lineages for every
unlocated building. The present audit uses historical polygons, predecessor
and successor evidence, permits, and adjudication to resolve additional
projects. For PINs observed in both current and historical parcel files, the
coordinates are essentially identical; the material issue is missing former
PINs, not drift in surviving PIN coordinates.

The geography attenuation is concentrated in a few ward pairs. Removing only
the recovered projects from one pair at a time gives the following changes;
positive contributions mean that the pair moves the estimate toward zero.
Because each row comes from a separate re-estimation, the contributions are
not additive.

| Ward pair | Aldermen represented among recovered projects | New multifamily projects | FAR continuous contribution | DUPAC continuous contribution |
|---|---|---:|---:|---:|
| 5-8 | Leslie Hairston; Todd Stroger | 2 | +0.013 | +0.013 |
| 3-4 | Pat Dowell; Toni Preckwinkle | 2 | +0.008 | +0.011 |
| 2-27 | Brian Hopkins; Robert Fioretti; Walter Burnett, Jr. | 7 | +0.008 | +0.008 |
| 32-44 | Scott Waguespack; Tom Tunney | 2 | +0.007 | approximately zero |

The 2-27 projects have the largest effect on the binary estimates, moving both
coefficients about 0.025 log points toward zero. Several recovered Burnett-side
projects are relatively dense while the score ranks Burnett slightly more
stringent than Fioretti in those years. At 5-8, one recovered project is on
each side: the more-stringent-side project has FAR 1.25 and the
less-stringent-side project has FAR 0.42. The complete project records and
leave-one-addition-pair-out estimates are in
`geography_recovery_multifamily_projects.csv` and
`geography_recovery_pair_influence.csv`. At the alderman level, Hairston's one
recovered project has the largest continuous influence, followed by Dowell and
Burnett. Fioretti's three recovered projects have the largest binary influence.
Those results are in `geography_recovery_alderman_influence.csv`.

The zoning extension first transfers a validated construction-year assignment
from an exact or adjacent-year component PIN. Remaining projects are assigned
from the official 2006, 2012, 2014, 2016, and 2025 zoning maps using the
validated anchor rule: stable snapshot intervals, a current polygon whose
latest ordinance predates construction, or the last official snapshot before
construction. All 4,177 projects inside 500 feet receive a broad zoning group.
The frozen validated reconstruction inputs have SHA-256 hashes
`e1d876c3af89e8429050a9ea1353f2bc0b73066c4977ade4bdb35558a2a1c023`
and
`60d90d6da8da0a463271ff982625284a487cfaf907112f28ff895297c8bed815`.

## Final multicard adjudication

The earlier results above preserve the production-selected card for residential
PINs with multiple assessor cards. The final audit resolves that issue rather
than choosing one card or treating each card as a separate project. The
observation is the construction episode on the development parcel. Assessor
cards describe that project's building inventory.

The adjudication applies four rules:

1. Contemporaneous cards on the construction-year parcel are aggregated.
2. Later successor rows are suppressed only when card-level matching or the
   complete successor episode reproduces the parent inventory.
3. Unmatched nearby rows remain separate projects.
4. Permits, successor buildings, or documented completed-project evidence
   override assessor values or construction years when the records conflict.

All 273 multicard projects within 1,500 feet receive a row-level decision in
`multicard_final_adjudication.csv`. The card matcher identifies 520 project rows
already represented by a predecessor card. Twelve additional successor rows
complete card inventories that cannot be matched one at a time. Two 2021
Vernon rows repeat a retained 2019 five-townhouse episode. One repeated
multicard parent at 4434 S Drexel is also suppressed. No suppressed row remains
in the final file.

The retained-project duplicate screen compares every multicard parent with
every retained project. It flags shared identity evidence and nearby projects
within 200 feet and four years with similar physical fields. All 91 candidate
pairs are resolved: 11 multicard-parent pairs, 74 distinct one-unit pairs with
different retained PINs, current addresses, and centroids, and six manually
reviewed cross-project pairs. None remains unresolved. The ten-unit buildings
at 500 W 66th and 501-505 W 65th Place are retained separately because they
occupy different parcels and different 2008 building footprints.

Five retained projects require value overrides:

- Two Ravenswood PINs each contain two identical cards but correspond to one
  separately permitted single-family house.
- Permits authorize seventeen townhomes at 3601-3603 W 53rd, while the assessor
  records sixteen identical cards.
- A permit authorizes nine townhomes at 4300-4318 S Drexel, while the assessor
  records ten identical cards.
- Eight successor condominium PINs establish eight units and 10,845 square feet
  at 5619 S Calumet.

Five construction years are corrected from permits, assessor transitions, and
completed-property evidence. The corrections cover 3416 N Bell, 3431 N Hoyne,
1544 W North, 1841 N Sheffield, and 3535 S Maplewood. Broad zoning is stable
across the relevant archived maps for all five.

The complete evidence is in
`adjudication/multicard_manual_overrides.csv`,
`adjudication/multicard_manual_episode_decisions.csv`,
`adjudication/multicard_year_overrides.csv`, and the two pair-decision files.

The duplicate-free audit input contains 9,010 projects within 1,500 feet,
compared with 9,545 before multicard de-duplication. Within 500 feet, the count
falls from 4,118 to 3,911. The multifamily count rises from 908 to 950 because
the selected-card rule classified some multi-card projects as single-family.
After the common regression restrictions, the multifamily sample rises from
893 to 935.

That last reclassification is not appropriate for a building-type sample.
Every one of the 56 new multifamily observations consists entirely of
single-unit assessor cards. They are groups of single-family buildings on a
shared parcel. They belong in all construction, but summing their sitewide
units should not turn them into multifamily buildings. The clearest example is
3636 W 51st Street: seven single-family cards occupy a 307,739-square-foot
parent parcel. Treating the card total as a seven-unit multifamily building
both changes the sample and attaches the entire parent-parcel land area to
that outcome.

The model decomposition is:

| Multifamily outcome | Existing input | Card values only | Final adjudicated |
|---|---:|---:|---:|
| FAR, continuous | -0.127 (0.074) | -0.088 (0.056) | -0.091 (0.056) |
| DUPAC, continuous | -0.158 (0.084) | -0.099 (0.069) | -0.104 (0.070) |
| FAR, binary | -0.205 (0.081) | -0.154 (0.099) | -0.155 (0.101) |
| DUPAC, binary | -0.198 (0.110) | -0.128 (0.127) | -0.133 (0.129) |

These are common-sample estimates with the full-control score and ward-pair
clustering. `multicard_sample_decomposition_results.csv` contains the complete
decomposition. Card aggregation explains most of the attenuation; successor
de-duplication changes the coefficients only modestly.

The more detailed attenuation diagnostic separates card-value revisions from
the mistaken single-family-site entrants:

| Multifamily outcome | Existing selected-card input | All site-unit entrants | Building-type sample |
|---|---:|---:|---:|
| FAR, continuous | -0.127 (0.074) | -0.091 (0.056) | -0.097 (0.065) |
| DUPAC, continuous | -0.158 (0.084) | -0.104 (0.070) | -0.132 (0.082) |
| FAR, binary | -0.205 (0.081) | -0.155 (0.101) | -0.170 (0.075) |
| DUPAC, binary | -0.198 (0.110) | -0.133 (0.129) | -0.170 (0.107) |

The building-type sample contains 879 common-sample observations. It retains
the 56 single-family sites in all construction but excludes them from
multifamily.

One valid card aggregation explains most of the remaining difference from the
old selected-card estimates. The parcel at 500 W 66th Street has five
persistent assessor cards, each reporting two apartments and 2,210 square
feet. Their combined 11,050 square feet is approximately three times the
3,717-square-foot building footprint, consistent with the assessor's
three-story classification. The preferred rule therefore treats it as one
ten-unit building rather than selecting one two-unit card. Reverting only that
project to its old selected-card value produces continuous estimates of
-0.140 for FAR and -0.176 for DUPAC. This sensitivity identifies the source of
the remaining attenuation; it does not justify discarding the card inventory.
Complete stage, project, and ward-pair decompositions are in the five
`multicard_attenuation_*.csv` files.

The four score-control variants on the final common sample are:

| Multifamily outcome | Keep income and bachelor's share | Drop income | Drop bachelor's share | Drop both |
|---|---:|---:|---:|---:|
| FAR, continuous | -0.091 (0.056) | -0.097 (0.067) | -0.051 (0.074) | -0.106 (0.068) |
| DUPAC, continuous | -0.104 (0.070) | -0.058 (0.072) | -0.048 (0.087) | -0.073 (0.075) |
| FAR, binary | -0.155 (0.101) | -0.117 (0.111) | -0.109 (0.109) | -0.118 (0.111) |
| DUPAC, binary | -0.133 (0.129) | -0.159 (0.142) | -0.148 (0.138) | -0.160 (0.142) |

All multifamily coefficients remain negative under every score construction,
but none is statistically distinguishable from zero at the 10 percent level
with ward-pair clustering. The all-construction binary FAR estimate under the
full-control score is -0.118 (0.054); the other all-construction estimates are
less precise. Full results, including segment clustering, are in
`multicard_adjudicated_density_results.csv`,
`multicard_adjudicated_density_common_sample_results.csv`, and
`multicard_adjudicated_density_cluster_results.csv`.

### External review of every multicard project

The mechanical building-type rule above is superseded by a row-level external
review of all 273 multicard projects within 1,500 feet. Each row records at
least one external source, a physical building type, a multifamily
disposition, and review notes in
`adjudication/multicard_external_web_reviews.csv`. The same rule is applied to
every project:

- completed apartment buildings remain in multifamily;
- detached and attached single-family developments remain only in all
  construction;
- duplicate, spurious, and unbuilt records are removed from both samples; and
- unresolved records are excluded rather than assigned by assumption.

The review classifies 44 projects as multifamily, 212 as single-family or
townhouse developments, and 16 as duplicate, spurious, or unbuilt. One
outer-band record at 6106 S Peoria remains unresolved and is excluded. No
unresolved record enters either regression sample. The retained-project
duplicate audit continues to have zero unresolved candidate pairs.

The 500 W 66th Street record should not be reverted to one selected card.
Cook County history contains five persistent two-unit, 2,210-square-foot
cards. External property records describe one apartment building with 10
units and 11,050 square feet:
[Crexi](https://www.crexi.com/property-records/20211200290000-CHICAGO-IL-60621-2608/d0caa43adc7491025adeee405a575a8895b7acc0)
and
[LoopNet](https://www.loopnet.com/property/500-w-66th-st-chicago-il-60621/17031-20211200290000/).
The city footprint file contains one 3,717-square-foot footprint, and the
assessor classifies each card as part of a building with at least three
stories. The summed card area is therefore physically coherent; the
selected-card value is not. Crexi reports 2008 as the construction year,
while the assessor history reports 2006. The audit retains the assessor year.
Both years assign the same ward pair and aldermen.

The external review changes the preferred common multifamily sample as
follows:

| Stage | N | FAR, continuous | DUPAC, continuous | FAR, binary | DUPAC, binary |
|---|---:|---:|---:|---:|---:|
| Mechanical multicard adjudication | 935 | -0.091 (0.056) | -0.104 (0.070) | -0.155 (0.101) | -0.133 (0.129) |
| Remove invalid and unresolved rows | 933 | -0.096 (0.055) | -0.108 (0.070) | -0.163 (0.103) | -0.139 (0.131) |
| Apply externally verified building types | 880 | -0.103 (0.062) | -0.137 (0.081) | -0.179 (0.075) | -0.175 (0.108) |
| Apply externally verified values | 880 | -0.103 (0.062) | -0.135 (0.081) | -0.179 (0.075) | -0.173 (0.108) |

These use the score with the full ward control set and ward-pair clustering.
The binary FAR estimate is significant at five percent. The continuous FAR
and DUPAC estimates are marginal at approximately ten percent. Deliberately
replacing 500 W 66th with its old selected-card value produces -0.146 for
continuous FAR and -0.179 for continuous DUPAC. That comparison measures the
leverage of a valid apartment building; it is not an admissible sample rule.

The reviewed model input, complete model grid, stage decomposition, changed
rows, and ward-pair reversion exercise are in the
`multicard_external_reviewed_*.csv` files.

Run the task from `code/`:

```sh
make
```
