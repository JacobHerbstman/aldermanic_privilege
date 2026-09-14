# Developer donations in the main specifications

This exploratory audit replaces the permit-based ordering with the existing `developer_verified` donation share. It compares the original full sample, the original ordering on a common sample, and the developer ordering on that same sample. It does not change the manuscript or production datasets.

Run `make` in `code/` and open `output/developer_results.html`. Three scripts compute the scores and main models, examine the permit ward pairs, and display the results. Analytical periods, controls, fixed effects, bandwidths, and clustering are explicit in the Makefile. SaveData writes reports when saving datasets.

## Score definition and direction

Developer share is developer-linked cash contributions divided by eligible recorded cash contributions and transfers to personal campaign committees while the alderman was in office. It excludes recognizable own-committee transfers. The existing literal developer wording and documented company rules from `../donation_score_robustness/` are unchanged. These are identified contributions, not a claim that every developer or LLC is classified. An observed zero share is a valid value; absent campaign receipts are missing.

The boundary comparison uses 2006–2022 donations. The 2015 permit-remap comparison uses 2006–2014 donations so post-remap fundraising cannot determine treatment. Only ordering matters, so replacing shares by their ranks gives the same sign except for unchanged ties. Higher developer funding is the positive side. It is not assumed to mean greater stringency or leniency. The 2006–2022 share is an exploratory retrospective measure, not a predetermined instrument for construction or prices.

## Models and sample comparisons

- Density: the paper's common FAR/DUPAC-eligible construction sample, 2006–2022, within 500 feet; all construction and multifamily separately. Outcomes are logged FAR and DUPAC. Controls are the five own-ward demographics; fixed effects are construction year, zoning group, and boundary segment; clustering is by ward pair.
- Rents: 2014–2022, the same cleaned location and positive-outcome/hedonic filters as the paper, with the existing building-type and property/amenity controls and segment-by-month fixed effects.
- Sale prices: 2006–2022, the same positive-outcome, location, hedonic, and property-class controls as the paper and segment-by-quarter fixed effects.
- All six boundary models use the paper's ten 100-foot bins in a 500-foot window. The reported coefficient compares [0,100) with [-100,0). Geography, nearest boundaries, and segments remain fixed; the developer comparison changes only distance's sign.
- Permits: high-discretion applications in 2010–2020 for blocks within 500 feet, both incumbents stable, and positive pre-remap high-discretion volume. Use the same signed Poisson and separate-direction models, block and ward-pair-by-year fixed effects, and ward-pair clustering. Plot the signed event study and reproduce the paper's joint pretrend F test. Report clustered t-based p-values and intervals, as the published permit producer does.

For density and prices, missing scores and equal developer shares prevent ordering. Drop those observations from both common-sample versions. For permits, require both wards in the recorded ward pair to have distinct pre-remap developer shares, and drop the whole unsupported pair including controls. Equal-share switched blocks are not relabelled as untreated. Original unchanged blocks remain controls. These are explicit exploratory sample choices, not new donor adjudications.

The input observation universe is the existing production analysis datasets. This test does not recover observations previously excluded upstream, recompute geometry, or use developer funding to alter the building/transaction sample. Fixed effects and Poisson may make their usual automatic removals; output N is the fitted model's N. The coverage table counts observations before estimation (blocks for permits).

The estimator verifies identical fitted observations in the two common-sample versions, including the permit event studies. The changed-direction percentage for permits counts only reassigned blocks, since unchanged blocks have no reassignment direction.

## Findings from the September 14 run

All seven original main coefficients and standard errors reproduce the paper's displayed three-decimal values. With developer funding used for ordering, FAR and DUPAC differences are imprecise, for both all construction and multifamily construction. Rent and sale-price estimates are also insignificant at 10%; the sale-price estimate remains about +3%, so its change is principally precision, not magnitude.

The signed permit effect remains negative: −10.7% (p=0.038; 95% interval −19.8% to −0.7%). The original permit ranking on the same observations gives −8.0%; its full-sample estimate is −11.9%. Developer ordering reverses 166 of 583 supported reassigned blocks. Its pretrend test has p=0.500. These results do not establish that donations measure regulatory behavior, and neither the donor definition nor the ordering direction was chosen to strengthen the result.

## Which permit comparisons explain the result?

`estimate_developer_results.R` saves the actual permit comparison panel before estimation. `compare_permit_pairs.R` reads that panel, without repeating its sample cleaning. It describes every observed boundary pair, fits separate signed effects for pairs where the rankings agree or reverse, and omits each of the 37 pairs with reassignments in turn. The other pairs retain their original comparison blocks. Group effects are estimated jointly, with the same fixed effects and ward-pair clustering as the pooled model. The code verifies common fitted observations and that reversing an entire group's ranking reverses only that group's coefficient. Pair omission effects are sensitivity measures, not additive contributions to a nonlinear estimate.

Twenty pairs account for 417 reassigned blocks whose ordering stays the same; their estimate is −12.9% under either measure. Seventeen pairs account for 166 reversed blocks; the original-ranking estimate is +5.5% and the developer-ranking estimate is −5.2% (both p=0.635). The negative pooled result primarily reflects comparisons where more developer funding accompanies higher measured permit stringency.

Moreno–Maldonado (wards 1–26) and Dowell–Cochran (3–20) provide the strongest support by the omission check. Removing the first pair gives −8.0% (p=0.158), and removing the second gives −8.6% (p=0.089). Waguespack–Smith (32–43) is the most consequential reversal; omitting it strengthens the negative developer estimate to −13.2%. Thus the pooled estimate's significance at 5% is sensitive to individual pairs, even though its sign remains negative in every one-pair omission.

`permit_pair_comparisons.csv` records names, both permit scores, developer dollars and total eligible dollars, sample counts, and both sets of omission results. `permit_reassignment_flows.csv` records origin and destination aldermen with raw pre/post permit counts and rates per block-year. Those raw rates are descriptive, not estimated treatment effects. The report displays every informative pair and lists pairs dropped for missing or tied funding.

## Recorded benchmark inputs

To keep a score experiment from rebuilding or changing production inputs, four existing analysis files were copied byte for byte to `data_raw/score_robustness/`. `sources/benchmark_sources.csv` records the original producer paths and hashes. Their sources are the current construction, rental, sales, and permit panels used by the submitted paper. The files are ignored by Git; a separate clone must obtain these snapshots or reproduce and verify them from those recorded producers. This audit is locally replayable, not a newly distributed source archive.

The donation input is a concrete Make dependency on the recorded donation audit's receipt output. No new download, donor classification, or manuscript update is part of this test.
