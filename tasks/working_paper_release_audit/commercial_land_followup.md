# Commercial land review and accepted retention decisions

Agent-drafted review, September 8, 2026. The 56 commercial land decisions have now been applied in the selecting script. The tables below preserve the preceding comparison and recommendations; the implementation update immediately below states the final outcome. Approved residential exclusions, Kildare's holdout, Lamon's retention, La Casa's source-coded 15 units, and the previously listed retention locations have been applied in the construction task. The paper's frozen input is unchanged.

## Implementation update: September 8, 2026

All 41 records with usable matching Assessor land now use that source. Thirty-nine use the candidate source; Clark and Roosevelt use the exact matching 2021 parcels. The rule is applied in the existing commercial selection script after building membership is established. It does not add 41 manual decisions.

Of the other 15, **10 have recovered source land**, correcting the earlier shorthand count of nine: five use complete, disjoint 2021 Assessor lots, and five use reported measurements from SOM, LoopNet, Redfin, Realtor, or PD1412. The five external measurements are recorded in `commercial_reported_land_decisions.csv` with URLs, scope explanations, units and review date. These web facts are transcribed reviewed inputs; compilation does not query changing property websites. The approved Montclare PDF is preserved by its acquisition task. Cottage Grove's 0.54 acre is rounded source acreage, converted to square feet, not a map estimate.

Michigan's complete earlier floor area is 40,923 square feet; Prairie's is 9,816. Both earlier unit totals equal the selected group totals (35 and 12). Existing grouping and construction years are preserved in this land correction. At 333 E 55th the full two-row floor area already equals the selected 70,376; the previously approved 54 apartments remain. Prairie 6049 and Indiana 6145 retain apartments-per-acre but lose FAR because the building measurements conflict. Taylor also loses FAR because apartment and whole-building/library floor areas have different scopes. Montclare was already excluded from FAR and remains so.

The five without full-property source land are **Lathrop, 3216 S Ashland, 4220 S Prairie, Woodlawn Commons, and 120 E 60th**. They remain recorded but are ineligible for both density measures. Ashland's near-matching listing identifies only one of the two selected parcels; no unsupported allocation is used.

The saved comparison population is the original 56 IDs and map areas in `reference/commercial_map_land_baseline.csv`. Make regenerates the comparison against current production, including current areas and eligibility. This baseline is audit-only and cannot supply production measurements. All 815 selected records remain recorded; the correction changes measurements and density eligibility, not the number of selected records. No regressions or paper input were rebuilt.

## What was completed

- Ten residential records without supported addresses/locations and two unsupported combined records are excluded by the existing source-exclusion ledger. Kildare's two records are held out because the reviewed listings identify new homes in 2024. The one-square-foot area placeholder is ineligible under a general source-measurement rule.
- Lamon retains 2022: [Redfin reports 2022 and a September 2022 sale](https://www.redfin.com/IL/Chicago/184-N-Lamon-Ave-60644/home/180693566). The completed exact new-building permit locates its three apartments.
- La Casa retains **15**, never 100, units. Its coherent 2021 Assessor row 83380 reports 27,150 building and 6,250 land sq ft for parcels 17194110010000/17194110020000. The later four-parcel assessment adds the separately located resource-center parcels. The reviewed component ledger selects the earlier residential-building row, with the [owner's October 2012 opening](https://resurrectionproject.org/es/governor-quinn-joins-trp-at-la-casa-ribbon-cutting-trp-announces-la-casa-giving-campaign/). Assessor area is retained as reported; independent architect gross area has a different scope. [Separate resource center](https://nbm.org/2018/03/29/making-room-case-study-liscs-la-casa/).
- Twenty-three reviewed parcel points locate the nine Dearborn homes, Oak, Newport, Hermitage, 32nd/33rd and Bowen records. Current points are used only for specifically reviewed individual homes and checked inside the historical site where a polygon exists. Four exact completed permits locate Kenmore's two homes, Lamon and Washington. Lincoln home 6, Irving Park and Federal already have accepted geometry and retain their source measurements. All 30 residential records have retained status and finite boundary distances.
- The seven named commercial records (Wells, Western, Agatite, Belden, Division, 50th/49th Place, and La Casa) retain measured source fields and have final construction-year boundary distances. The commercial location producer now uses selected years/components, reuses earlier locations only when those match, and records reviewed exact-source points. It does not substitute polygon area for density land.

This closes the agreed retention list. It does not certify the entire final dataset. The commercial location output preserves 31 other selected records without a supported final location and makes them ineligible there, rather than averaging a partial set of parcel points. Several involve already recorded changes of year or membership. Their decisions need not be reopened merely because the corresponding map query is missing. The preceding review's other tower recommendations and residential review reconciliation remain outside this turn's accepted decisions.

## The 41 with an available candidate Assessor land value

The median absolute difference between the old map denominator and candidate Assessor land is **0.747%**. Of 41, **23 differ by at most 1%, 34 by at most 5%, and 36 by at most 10%**. These comparisons describe agreement, not an automatic acceptance threshold.

Thirty-nine retain the same listed components as the candidate Assessor row. For the other two, the 2021 source matches the selected construction-time components: **633 S Clark has 30,529 sq ft** (old map 30,705), and **150 W Roosevelt has 338,807 sq ft** (old map 338,806). Comparing Clark with its later 17,860-sq-ft record exaggerated the disagreement by comparing different parcels.

Recommendation: use compatible source land, including the two matching older assessments, rather than the map measurement. The larger same-component differences deserve explicit review: Sheridan, Keeler, Rockwell, Arlington, Division, and Hubbard. Equal parcel lists establish which records are being compared; they do not prove every source measurement correct. These recommendations are now implemented as described in the dated update above.

| Address | Map land, sq ft | Candidate Assessor land, sq ft | Map minus Assessor | Same parcel list? |
|---|---:|---:|---:|---|
| 1531 W HOWARD CHICAGO | 14,914 | 15,011 | -0.65% | Yes |
| 6418 N SHERIDAN CHICAGO | 40,262 | 45,094 | -10.72% | Yes |
| 5522 W HIGGINS CHICAGO | 7,618 | 7,500 | +1.57% | Yes |
| 4738 N KIMBALL CHICAGO | 24,837 | 24,835 | +0.01% | Yes |
| 3939 N KEELER CHICAGO | 10,246 | 9,594 | +6.80% | Yes |
| 2943 N ROCKWELL CHICAGO | 4,946 | 5,255 | -5.88% | Yes |
| 2510 N WILLETTS CHICAGO | 5,853 | 6,000 | -2.45% | Yes |
| 2346 N CALIFORNIA CHICAGO | 30,166 | 31,548 | -4.38% | Yes |
| 2404 W. Moffat St. | 4,408 | 4,450 | -0.94% | Yes |
| 5077 N BROADWAY CHICAGO | 13,331 | 13,500 | -1.25% | Yes |
| 4410 N CLARK CHICAGO | 10,029 | 10,000 | +0.29% | Yes |
| 3839 N CLARK CHICAGO | 5,142 | 5,140 | +0.04% | Yes |
| 1118 W PATTERSON CHICAGO | 11,989 | 11,868 | +1.02% | Yes |
| 3600 N HALSTED CHICAGO | 163,152 | 163,152 | +0.00% | Yes |
| 3415 N ASHLAND CHICAGO | 16,371 | 16,585 | -1.29% | Yes |
| 3300 N CLARK CHICAGO | 23,268 | 23,267 | +0.00% | Yes |
| 3478 N BROADWAY CHICAGO | 68,669 | 69,112 | -0.64% | Yes |
| 434 W MELROSE CHICAGO | 10,758 | 10,956 | -1.81% | Yes |
| 518 W ARLINGTON CHICAGO | 4,842 | 4,334 | +11.72% | Yes |
| 3113 N LINCOLN CHICAGO | 3,825 | 3,992 | -4.18% | Yes |
| 3123 N OAKLEY CHICAGO | 5,681 | 5,681 | +0.00% | Yes |
| 1057 N SACRAMENTO CHICAGO | 37,737 | 37,732 | +0.01% | Yes |
| 2751 W DIVISION CHICAGO | 14,828 | 14,640 | +1.28% | Yes |
| 3250 W DIVISION CHICAGO | 12,401 | 12,401 | +0.00% | Yes |
| 740 N AVERS CHICAGO | 6,032 | 6,000 | +0.53% | Yes |
| 900 S WESTERN CHICAGO | 4,606 | 4,607 | -0.02% | Yes |
| 2652 W 12TH | 2,638 | 2,650 | -0.45% | Yes |
| 1437 N SEDGWICK CHICAGO | 3,359 | 3,350 | +0.27% | Yes |
| 1367 N SEDGWICK CHICAGO | 16,235 | 16,286 | -0.31% | Yes |
| 1517 W HADDON CHICAGO | 12,404 | 12,936 | -4.11% | Yes |
| 1640 W DIVISION CHICAGO | 29,623 | 21,959 | +34.90% | Yes |
| 1554 W MADISON CHICAGO | 7,209 | 7,207 | +0.03% | Yes |
| 71 W HUBBARD CHICAGO | 35,163 | 39,324 | -10.58% | Yes |
| 633 S CLARK CHICAGO | 30,705 | 17,860 | +71.92% | No; use matching 2021 source |
| 150 W ROOSEVELT CHICAGO | 338,806 | 341,355 | -0.75% | No; use matching 2021 source |
| 1461 S BLUE ISLAND CHICAGO | 54,129 | 54,127 | +0.00% | Yes |
| 1140 W 13TH | 7,682 | 7,682 | +0.00% | Yes |
| 1417 S STATE CHICAGO | 40,733 | 40,379 | +0.88% | Yes |
| 2111 S WABASH CHICAGO | 42,542 | 42,541 | +0.00% | Yes |
| 3256 W 47TH CHICAGO | 15,510 | 15,724 | -1.36% | Yes |
| 4434 S DREXEL | 47,436 | 47,700 | -0.55% | Yes |

## The 15 without usable candidate Assessor land

This table preserves the initial recommendations; see the implementation update above for final dispositions. Source-reported square feet and sums of disjoint recorded lots are distinguished from calculated map areas.

| Property | Existing map land | What the recorded evidence supplies | Initial verdict |
|---|---:|---|---|
| 2737 N Hoyne / Lathrop | 161,663 | Source land zero. The new 59-unit building sits within a much larger development containing rehabilitation. Published large site totals do not isolate it. | Exclude density without a building-specific reported site. Do not assign the whole Lathrop site. |
| 1346 W Taylor | 67,437 | [SOM reports 38,640 site sq ft, 73 apartments and completion in 2019](https://www.som.com/projects/taylor-street-apartments-and-little-italy-branch-library/). | Strong source-based land correction available. Recommend 38,640 and 2019. Whole-building gross area is 72,830, while the Assessor reports 53,022; review the apartment/library floor-area scope together before adopting FAR. |
| 3216 S Ashland | 6,062 | [Zillow reports 6,050 lot sq ft](https://www.zillow.com/homedetails/3216-S-Ashland-Ave-Chicago-IL-60608/158883698_zpid/) but identifies only PIN 1731214039 and still calls it vacant land; the source building lists two PINs. | Promising, but not yet a verified full-site denominator. Do not automatically adopt the near-match. |
| 4137 S Cottage Grove | 23,077 | [The preconstruction listing covers the exact three PINs and reports 0.54 acre](https://www.loopnet.com/Listing/4137-S-Cottage-Grove-Ave-Chicago-IL/11308857/), equivalent to 23,522.4 sq ft, about 1.9% above the map. | Source-reported site exists, but rounded acreage is less precise. Recommend using it only with that precision recorded. Completion sources differ between expected 2022 and a later pipeline listing; Assessor 2022 remains a proxy. |
| 4101 S Drexel | 9,232 | [Redfin reports 8,880 land sq ft for the exact PIN](https://www.redfin.com/IL/Chicago/4101-S-Drexel-Blvd-60653/home/143265179). | Good source correction candidate; recommend 8,880, about 3.8% below the map. |
| 920 E 43rd | 16,232 | [The sold-land listing reports 16,117 sq ft for the exact PIN](https://www.realtor.com/realestateandhomes-detail/920-E-43rd-St_Chicago_IL_60653_M87588-88076). | Good source correction candidate; recommend 16,117. Do not recompute its displayed dimensions, which imply a slightly different number. |
| 4251 S Michigan group | 58,338 | Three disjoint 2021 rows cover all four parcels: land totals **58,338**, units **35**, floor area **40,923**. Later 28,341 floor area repeats only the 24-unit component. | Strong recovery from a complete source package. Prefer the separately measured buildings where their years/identities agree; otherwise preserve the coherent 35-unit group. Do not retain the incomplete later floor area. |
| 4200 S Prairie group | 24,247 | Four disjoint 2021 rows cover all four parcels: land **24,247**, units **12**, floor area **9,816**. Later 3,272 floor area repeats only one four-unit building. | Strong recovery from individual source rows. Keep the separately measured buildings, with their complete reported fields. |
| 4220 S Prairie | 32,083 | Earlier land **28,070** covers only one of the later two listed parcels. | Full-site source land remains unsupported. Recommend exclusion rather than adding map area for the extra parcel. |
| 1156 E 61st / Woodlawn Commons | 126,343 | Earlier **118,643** land covers five parcels; later six-parcel record has zero. Building areas also differ sharply. The record is already excluded from units-per-acre because beds are not apartments. | No coherent complete source measurement established; recommend excluding FAR as well rather than mixing scopes. |
| 333 E 55th group | 24,724 | Two disjoint 2021 source rows report land **29,093 + 24,707 = 53,800**, floor area **70,376**, and 27/30 units. Existing approved permits support 27 units in each building. | Recommend the complete reported site land **53,800**, preserving the approved 54-unit count. Existing map denominator is approximately one lot, not the two-building source site. |
| 120 E 60th | 33,915 | Earlier **16,612** land covers one of the three selected parcels and only 11 of the later 19 units. | Full-site source land remains unsupported; recommend exclusion rather than assuming the partial row measures all 19 units. |
| 6049 S Prairie group | 19,223 | Two disjoint earlier rows cover all five parcels and total **19,220** land. Earlier units/floor area are 4/6,382; later are 12/15,049. Permit evidence supports 12. | Reported land is recoverable. Units-per-acre is plausible using 12 and 19,220; do not certify FAR by combining inconsistent building snapshots. |
| 6145 S Indiana group | 32,399 | Two disjoint earlier rows cover both parcels and total **32,398** land. Earlier units/floor area are 10/14,568; later are 14/8,829. Permit evidence supports 14. | Reported land is recoverable. Units-per-acre is plausible using 14 and 32,398; floor-area scope remains inconsistent. |
| 9401 S Stony Island / Montclare | 239,421 | The **2018 approved PD1412 table, PDF page 33**, reports residential Subarea A net land **152,677 sq ft** and 134 apartments. The two commercial subareas have zero apartments. | Strong correction: recommend **152,677**, excluding unrelated commercial land. The source plan reports the area directly; no polygon measurement or allocation is needed. |

The [56-row comparison](output/commercial_reported_land_comparison.csv) is regenerated through Make from the selected commercial file, candidate Assessor fields and source-version rows. It checks unique project/source keys and reports earlier totals only for exact, disjoint component coverage. Earlier units/floor-area disagreements are visible beside the land totals.
