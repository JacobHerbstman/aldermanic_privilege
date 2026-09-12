# Review of four construction land denominators

September 6, 2026. Codex review; recommendations below are not newly adopted
research decisions. The committed construction dataset remains unchanged.

The parent land values are genuinely present in the Assessor source. They are
not decimal errors or accidental sums over repeated building cards. The issue
is which buildings and common land each parent represents. Replacing parent
areas with the sum of individual townhouse lots would not consistently solve it.

| Parent project | Retained units | Assessor land (sq ft) | Same-assessment-year mapped parent (sq ft) | Review conclusion |
| --- | ---: | ---: | ---: | --- |
| Park Place, PIN 19111200130000 | 7 | 307,739 | 313,309 (2007) | Do not clear: the parent includes a much larger development tract. |
| Fletcher, PIN 13252030410000 | 2 | 6,552 | 6,795 (2008) | Do not clear: the parent covers five homes, three retained separately. |
| Eastgate, PIN 17271290150000 | 29 | 74,192 | 74,192 (2008) | Do not clear yet: the parent contains six additional reported homes. |
| Calumet, PIN 17271290140000 | 13 | 27,738 | 27,738 (2008) | Retain the existing denominator: the site and 13-home inventory agree; smaller successor lots omit additional site land. |

## Findings

**Park Place.** All seven selected 2007 assessment cards report year built 2006,
13,753 square feet of building area in total, and 307,739 square feet of land
per PIN. The historical parcel has approximately the same large area, so the
number is a real parent-parcel quantity. Its mapped extent reaches well beyond
the seven matched homes. The recorded current-parcel evidence contains 39 parcel
points inside it: 14 reported residential units, numerous parcels without a
residential building record, and a commercial successor associated with a later
78-unit project. The seven matched residential successors are suppressed in the
final dataset. Other residential buildings are represented through separate
multicard parents. The concern is land allocation across construction phases,
not a demonstrated duplication of the seven matched housing units. Neither
307,739 nor the 21,069 square feet of matched individual lots is established as
the appropriate completed-project denominator. Do not clear this density value
without allocating the development land to the represented episode.

**Fletcher.** The selected 2008 cards reproduce two homes, 3,738 building square
feet and 6,552 land square feet. The historical parcel contains five current
home-parcel points. The two matched successors are suppressed, but the other
three are retained individually: PINs 13252030510000 (year built 2006),
13252030540000 and 13252030550000 (both year built 2008). Those three individually
retained land values sum to 2,519 square feet. Thus the dataset combines the
whole parent land denominator for two homes with separate lot denominators for
other homes inside that parent. This is a concrete inconsistency in the scope
of the land represented. A coherent site treatment must allocate shared land
and handle the two construction years; merely replacing 6,552 with the two lots'
1,527 square feet would omit shared land.

**Eastgate, 29 homes.** The 2008 exact-parent polygon measures 74,191.6 square
feet, confirming the 74,192 Assessor value much more closely than the broader
2007 predecessor used in earlier geographic matching. The 29 selected cards
and matched successor homes reproduce 55,997 building square feet exactly.
However, the exact 2008 parent still contains six additional reported homes:
17271290680000 and 17271290750000--17271290790000. Five have reported year built
2005; one reports 2008. Reported years alone do not independently establish their
physical completion dates. None is a separate retained individual project in
the frozen export. The area includes shared land, but the correspondence between
the 29-home numerator and the whole site remains unresolved. Do not automatically
reduce the denominator to 25,792 square feet of individual lots or increase the
numerator to 35 without resolving the additional buildings and timing.

**Calumet, 13 homes.** The 2008 exact-parent polygon measures 27,738.45 square
feet, effectively identical to the retained land value. It contains all 13
matched residential successors and no additional reported residential units.
Five further parcel points are class 100; two have addresses ending in `CA`,
consistent with common areas. The 13 individual home lots sum to only 11,428
square feet. The observed difference is consistent with inclusion of shared
site land, rather than evidence that the Assessor denominator is too large.
Retain 27,738. This is an empirical site-scope assessment, not a title opinion
or an independently surveyed apportionment of every common parcel.

The City’s [Eastgate planned-development record](https://gisapps.chicago.gov/gisimages/zoning_pds/pd986.pdf)
explicitly describes interior drives and shared parking in the development;
it supports considering shared site land but does not establish these PIN-level
allocations. The [2013 Park Place amendment submission](https://occprodstoragev1.blob.core.usgovcloudapi.net/lsmatterattachmentspublic/844d10cb-411f-4793-be9c-b7a4b75f4069.pdf)
also distinguishes multiple development subareas. Neither document supplies a
justified numerical replacement for the three unresolved denominators.

## Reproduction and limits

Run `make ../output/denominator_case_review.csv` from this audit's `code/`
directory, followed by the two corresponding `report/` targets. The script also
writes `denominator_case_members.csv` and `denominator_case_maps.pdf`. It checks
the selected historical cards against the pinned full Assessor CSV, reconstructs
parent polygons from the pinned administrative feature extract, and recomputes
point containment from recorded current-parcel evidence. Its selected card
counts, building totals and land values all equal the frozen paper values.
The case geometry uses the assessment year supplying the cards (2007 or 2008),
not an assertion that a later map establishes the precise construction-date
boundary. Current-parcel membership is based on centroids, not a surveyed parcel
intersection or proof that every building is represented in the source table.

The reference successor evidence is an explicitly preserved historical computed
product. This focused audit is reproducible with its supplied inputs; it does
not certify the unfinished raw-to-paper production chain. Missing upstream
producers, historical zoning reconstruction, source-vintage reconciliation and
the final paper rebuild remain outstanding. No final density values, sample
flags, or estimates were changed during this review.

## Permit and planning follow-up, September 6

A fresh search of the pinned City permit source changes the interpretation of
some reported construction years. The reproducible address search is
`code/review_denominator_permits.py`; its 196-row output includes nearby work,
revisions and later projects, not 196 accepted matches.

* Fletcher: permits 100110249, 100110230 and 100109483 identify 2452, 2454 and
  2456 W Fletcher as units 3W, 2W and 1W of building 2. All were issued December 1,
  2006. The same-day permits 100109482 and 100109481 identify 3139 and 3141 N
  Campbell as units 10W and 9W. Building 2 also extends east to addresses beyond
  this parent parcel. Thus neither the two-card parent nor simply aggregating its
  five parcel points establishes a complete permitted building. Listings disagree
  on completion year: the [Zillow record](https://www.zillow.com/homedetails/2454-W-Fletcher-St-Chicago-IL-60618/113954692_zpid/)
  repeats 2006 whereas [Redfin](https://www.redfin.com/IL/Chicago/2454-W-Fletcher-St-60618/home/40376229)
  reports 2008. These are leads, not independent resolution of the conflict.
* Eastgate: permit 100125952, issued August 10, 2006, covers five new homes at
  351--359 E Eastgate Place. Four of these five successors report year built
  2005, so treating them as established pre-2006 housing is not supported by the
  permit chronology. Permit 100125948, issued August 11, 2006, covers seven units
  at 340--352 E 25th Place, including the address of the sixth extra parcel.
  Permit issuance does not prove completion. The 2006 approval letter in the
  City's PD 986 PDF (pages 34--36) covers a broader subarea and explicitly
  includes existing housing and shared roads. Its totals cannot be substituted
  for this parent's numerator or denominator.
* Park Place: the 2013 amendment's subarea drawing (PDF page 107) places G1 and
  G2 on opposite sides of Millard. The seven matched successors include homes
  on both sides. The drawing's seven-unit G1 total therefore does not identify
  this seven-card parent. Its G1 land total is not a valid automatic replacement.
  The earlier permits also include address changes and later construction; the
  area cannot be allocated by counting nearby permit rows.

The official PDFs are now downloaded through Make in
`tasks/download_construction_development_records/code/`. Both downloads completed
and could be read. The Eastgate PDF has a malformed form-fields warning from
Poppler but its pages and text are readable. The documents establish planning
history, not completion or a surveyed allocation of shared land.

No replacement density value or completion year has been adopted. The pending
research choice is whether unresolved density observations should remain outside
the density regressions, with their construction records preserved, or whether an
explicit shared-land imputation should be developed. An exclusion would resolve
sample treatment, not recover the true missing denominator. The same measurement
rule would need to apply to other cases with comparable evidence, regardless of
location or their effect on the estimates.

## Adopted treatment, September 7

Jacob approved excluding the three unresolved observations from density
regressions, while retaining their construction records. The decisions and
specific evidence are committed in `new_construction_cleaning/adjudication/`
as `density_denominator_decisions.csv` (commit `79d35a3`). The restored final
exporter reads this ledger as a declared Make prerequisite and disables both
density eligibility flags before computing FAR and units per acre.

Re-estimating the current main specifications and inspecting `fixest::obs(model)`
confirmed that all three currently enter both all-construction regressions
(3,692 observations). None enters either multifamily-only regression (822
observations). A controlled replay of the restored exporter retained all 8,648
rows and changed exactly the two eligibility flags and two density measures for
the three ledger IDs. The paper's frozen data and figures have not been updated.

## September 7: wider screen and Lenox timing decision

The expanded screen includes all 248 retained multicard projects. Of these,
239 have recorded construction-year parcel polygons, four have only search
buffers, and five have no matching geometry in this preserved query file.
The latter nine are unassessed by this containment check. Among 103 projects
with a successor match, 61 have complete matches reproducing both final units
and building area, 28 have complete card matches whose totals differ, and 14
have incomplete matches. The other 145 have no recorded successor match.

Twenty-seven parent polygons contain current parcel points belonging to other
individually retained projects; eleven parents are within 500 feet. This is
an investigation queue, not a finding that 27 projects are duplicates or should
be excluded. The screen is independent of regression outcomes and includes
ratios below two and incomplete matches. All ten additional complete-match
cases with land ratios above two contain other retained project points.
Current parcel points and construction-year polygons alone do not establish
construction timing or an appropriate shared-land allocation.

The Lenox parent (13042080370000) reports two houses in 2008, and its successors
13042080410000 and 13042080420000 report one house each in 2019. The old review
recognized the shared houses but treated the later dates as reporting changes.
The full permit source contains original permits 100163191 and 100163192,
issued February 28, 2007, and their reinstatements 100784145 and 100784138,
issued September 20, 2018. Contractor changes continue into 2019. This prevents
assuming that the earlier construction date identifies completed houses.
Jacob approved excluding all three records from density pending completion-year
resolution, while preserving all records in the construction ledger.

A controlled exporter replay changes exactly three rows and four density fields
relative to the previous three-exclusion ledger, retaining all 8,648 records.
The fixed-input regression comparison now includes current eligibility, the
three original land exclusions, and all six exclusions. The all-construction
sample is 3,692, 3,689 and 3,688 respectively. Adding Lenox leaves the headline
12.59% FAR reduction and 16.28% units-per-acre reduction unchanged at displayed
precision. Multifamily results are unchanged. These estimates isolate ledger
changes and do not include unfinished upstream reconstruction changes.

Two further issues remain open. The Rockwell parent 13254280400000 and its two
successors are both retained. The full permit file identifies the two houses
at 2419 and 2421 N Rockwell, with applications in November 2005 and permits
issued February 23, 2006. The construction verification producer currently drops
applications before 2006, so it misses those permits. The old external note's
2417 address is not the second successor address in the recorded parcel data.
No Rockwell exclusion or construction-year change has been adopted.

At 33rd/Prairie, one May 17, 2019 permit (100789260) authorizes five townhouses.
The retained 2019 parent and five retained 2021 successors have five units each and close building-area totals (8,684 versus 8,646 square
feet). The mechanical overlap screen
requires construction dates within one year, while the later successor matcher
allows two years. The parent therefore never enters the component graph and
is classified as having no successor overlap, despite five eligible successor
candidates. Jacob approved a consistent two-year automatic window. Its first fixed-input
replay adds 62 edges, expands the component graph from 694 to 758 nodes and
103 to 107 groups, and increases card matches from 589 to 604. Eight parents
change review class. Three now require manual episode decisions; three existing
manual-review parents become mechanically resolved. The old ledger also uses
ordinal component IDs, so its identifiers and substantive decisions need
reconciliation before the new grouping can pass final adjudication.

Reproduction uses `multicard_parent_coverage.csv` and
`multicard_followup_permits.csv` in this audit's output directory. These files
are evidence, not production adjudications. The Lenox decision is an explicit
input in the construction cleaning task's density eligibility ledger.

The three newly required episode reviews under the approved two-year rule are
17271290060000 (Finsbury, three cards), 17341211170000 (33rd/Prairie, five cards),
and 17043200080000 (Cleveland/Hobbie, 39 cards). The matching stage finds 3, 5 and
39 card matches respectively. This does not automatically resolve land allocation,
construction dates or the unmatched broader development. Finsbury's preserved
query geometry is only a search buffer, not a parcel boundary. Cleveland's parent
polygon includes another retained project.

At 33rd/Prairie the component summary still totals 20 units because it consumes
the earlier project ledger (three successor rows report six units). The later
single-family classification rule reduces those same records to one each.
Consequently the component summary and the final five-unit inventory disagree.
This is an ordering/consistency issue to resolve at the producer; no new unit
rule or additional exclusion has been adopted. The final adjudicator now reports
missing and obsolete review IDs explicitly and continues to fail while the
manual ledger is inconsistent with the expanded grouping. It does not silently
apply obsolete ordinal component IDs or discard earlier decisions.

A comparison of all 758 graph nodes with the preserved later classification
finds five differing unit counts in two components. Three Prairie successor
records change from six to one; two Drexel successors (20023050350000 and
20023050360000) change from three to one. This comparison uses preserved
classification decisions, not a rerun under the expanded graph. The successor
candidate builder with the explicit two-year argument reproduces its preserved
CSV byte-for-byte. Both tested stages are incremental on unchanged inputs.

Jacob approved removing the construction verification history's application-date
cutoff on September 7. All dates in the pinned source are available to this stage;
its existing coordinate and processing-time validity rules remain. This changes
the evidence available for review, not the construction sample's study dates.

The all-date permit replay contains 833,465 records, up from 828,237. All
previous records are identical except storage row numbering; the added 5,228
records have application dates before 2006. Both Rockwell house permits
100076611 and 100076635 are present, with application date 2005-11-03 and issue
date 2006-02-23. The unchanged second Make build runs no producer. Downstream
construction adjudication and estimates have not been rebuilt with this evidence.
