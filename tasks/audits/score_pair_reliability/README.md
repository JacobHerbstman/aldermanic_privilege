# Reliability of local alderman comparisons

This exploratory audit asks whether the submitted permit-score method reliably orders the particular aldermen compared in the paper. It does not change the score definition, production data, manuscript, or housing estimates. Run `make` in `code/`; open `output/pair_reliability.html`.

Two scripts estimate repeated scores and summarize the actual local comparisons. The Makefile declares score controls, years, sample restrictions, replication counts, seeds, and minimum split-sample support. The observed 2006–2022 source and existing analysis snapshots are recorded with hashes in `sources/benchmark_sources.csv`. The frozen permit-remap score ends in 2014. Published benchmark scores are the committed snapshots in the donation audit. Both baseline reconstructions must match those scores within 1e-8 before experiments run.

## Score estimator

The first stage regresses log processing time on the submitted neighborhood and lagged-workload controls, with month, permit-type, and review-type fixed effects. The second stage regresses ward-month mean residuals on alderman indicators and lagged workload, weighted by the contributing permit count. Effects are centered, shrunk using the existing centered heteroskedastic covariance calculation, and standardized across aldermen. A short estimator is reused for every experiment, including the all-one-weight baseline. Controls and both estimation stages are refitted in every draw and half; fitted outcomes and residuals are never shared between halves.

The permit population, valid-log restriction, and workload construction reproduce the original source. Workload is counted before dropping incomplete observations. This audit then conditions on that actual observed workload when splitting or reweighting the score observations. It does not interpret half a research sample as half the real workload. This is a conditional score-stability exercise, not a bootstrap of the raw permit-acquisition process.

## Positive-weight resampling

Each method has 499 draws. An independent mean-one exponential weight is assigned to each group, then normalized to mean one across permits. Weighted residual means use those same weights; second-stage weights are their ward-month sums. Centering, the second-stage covariance, shrinkage, and standardization are recomputed. No fitted score is simulated independently of the other aldermen.

- **Properties:** permits connected through any recorded ten-digit parcel root receive one common weight, across all years. This includes connections through permits listing multiple parcels. Missing or malformed parcel identifiers remain singleton permit groups; they are not manually corrected for this audit. This captures repeated recorded properties but not shared shocks across otherwise unrelated properties.
- **Calendar quarters:** every permit in the same quarter, across all wards, receives one common weight. Separate quarters are reweighted independently.
- **Calendar years:** the same approach with entire years. There are only 17 blocks through 2022 and nine through 2014; some aldermen have much shorter coverage. This permits longer within-block dependence but is not automatically conservative and cannot identify dependence across years.

These are alternative dependence assumptions, not three independent pieces of corroborating evidence. Positive weights retain every observed alderman, unlike resampling wards or years with replacement and losing short-tenure aldermen. Draw shares describe how often the current ordering survives a perturbation, not posterior probabilities that an ordering is true. The 2.5th–97.5th percentile draw range is descriptive and pointwise. No simultaneous rank confidence set, valid familywise discovery claim, or formal small-time-cluster coverage guarantee is asserted. With 499 draws, Monte Carlo uncertainty in an estimated 50% retention share is about 2.2 percentage points; the pair file reports it.

The distinction between estimated rankings and their uncertainty follows [Mogstad, Romano, Shaikh, and Wilhelm](https://www.nber.org/papers/w26883). Exponential reweighting is related to [Newton and Raftery's weighted bootstrap](https://sites.stat.washington.edu/raftery/Research/PDF/newton1994.pdf). Our two-stage, shrunk, clustered implementation is an exploratory adaptation; it is not a direct implementation of either paper's formal inference procedure.

## Separate permit samples

Every permit belongs to exactly one half per split. Half-samples fit separate adjustment coefficients, alderman effects, and shrinkage. Four or more observed months are required to fit an alderman, following the existing score eligibility rule. Reporting agreement additionally requires at least 30 permits and six months for each alderman in each half. These thresholds concern support, not the observed score or housing results; excluded comparisons remain visible through coverage counts.

- **Random properties, 50 splits:** whole connected property groups are assigned to disjoint halves. No recorded linked property appears in both halves. Early records often lack parcel links, so unlinked permits from one physical project can still cross halves. This tests repeatability under new observed projects while mixing calendar periods; it does not remove common shocks.
- **Random quarters, 50 splits:** randomly assign two quarters of each year to each half, with the same assignment throughout Chicago. Both halves span the same years. Distinct quarters can remain serially correlated, and repeated properties can span halves.
- **Odd versus even years:** alternate calendar years, retaining coverage across the study period while separating whole years. Adjacent years need not be independent.
- **Early versus late:** 2006–2014 versus 2015–2022 for the full score; 2006–2010 versus 2011–2014 for the frozen pre-remap score. This checks persistence over a long gap as well as sampling noise, and only compares aldermen with adequate data in both periods.

All random seeds and splits are retained in the reproducible design. Results are not selected for favorable agreement. Disjoint observations do not imply statistically independent errors. These exercises cannot distinguish real changes in behavior from all changes in project selection, permit composition, or unobserved administrative conditions.

## Local comparisons and outputs

The summary script reconstructs the fitted rows of the current main boundary specifications for all construction, multifamily construction, rents, and sales. Density uses the common FAR/DUPAC-eligible sample. A separate frozen-score comparison uses the actual reassigned blocks in the main stable-incumbent permit-remap model. Unchanged blocks remain in that model, but have no origin–destination ordering to assess. Pair identifiers sort alderman names and retain both endpoints; different incumbents at the same geographic boundary are different alderman pairs.

Each pair's score difference is recomputed jointly in every draw. Summaries show equal-pair and observation-count representations. Observation counts mean buildings, rental observations, sales, or reassigned blocks—not regression leverage or additive contributions to the treatment coefficient. Split agreement is between the two halves, rather than between each half and the overlapping full score. Unsupported pairs are excluded from agreement denominators and included in coverage denominators.

`comparison_pairs.csv` supplies pair counts by analysis; `bootstrap_pairs.csv` supplies every pair's retention and gap range; `split_summary.csv` retains every repetition and coverage denominator; `split_pairs.parquet` preserves pair-level half-score differences; `split_rank_correlations.csv` provides citywide correlations on supported common aldermen. Underlying baseline, draw, and split scores are saved for both periods. SaveData generates reports as side effects. Output tables and figures are generated together through Make, and any missing named output can be regenerated independently.

The local snapshots are ignored by Git. A separate clone needs these recorded inputs or must reproduce their documented producer outputs and verify the hashes; this audit does not claim a new distributed source archive. No donor definitions or housing-result-based weights are used. Full uncertainty combining scores and outcomes remains a separate question.
