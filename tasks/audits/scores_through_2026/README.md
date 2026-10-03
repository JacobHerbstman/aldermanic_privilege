# Stringency scores through June 2026 (exploratory)

Nothing here feeds the paper or slides. Run `make` in `code/`.

- `prepare_permits_2023_2026.R`: the uncertainty-index permit data for high-discretion permits filed January 2023
  through June 2026 (`tasks/clean_building_permits`, 2023-2026 period), a copy of the production preparation with
  three changes: those permits, the 2023 ward map from May 15, 2023, and the alderman terms recorded through
  September 2026 on branch `explore-alderman-measures` (`sources/alderman_terms_through_2026.csv`, from commit
  cdef7ae7).
- `build_scores_through_2026.R`: the paper's two-stage score, first rebuilt for 2006-2022 and checked against the
  published score (difference under 1e-10), then estimated through June 2026, each with and without
  self-certification permits.
- `density_side_flips.R`: the multifamily buildings whose more-stringent side differs between the published score
  and the score through 2026 without self-certification permits, by alderman pair, and the change in the paper's
  estimate from flipping each pair alone.
- `results_with_scores_through_2026.R`: the density, rent and sales boundary results with each score version,
  re-deciding the more-stringent side of each boundary. The event study is unaffected (2006-2014 score).

## Findings (October 2, 2026)

- **The permit records change in the second half of 2023.** Self-certification permits were issued the day they were
  filed in 97 percent of cases in 2020-2022 and in under 1 percent from 2024; the same-day share of high-discretion
  permits falls from about 50 percent to 10-15 percent and the median time from 0-4 to 33-43 days. Because the score
  uses log days, self-certification permits enter it for the first time; with them, Brendan Reilly's score falls from
  0.71 to -1.27. Excluding self-certification permits in every year leaves the 2006-2022 ranking close to the
  published one overall (Spearman 0.96) but moves some aldermen far: Reilly from 0.71 to -0.69, Madeline Haithcock from
  0.05 to 0.89, Theodore Matlak from 0.80 to 0.23. Standard-plan-review permits also stop being issued the same day (18 to 1 percent), which
  this exclusion does not address.
- **Without self-certification permits the score is stable**: Spearman 0.97 between the 2006-2022 and through-2026
  versions (0.93 among the 49 aldermen with permits after 2022); 16 aldermen first serving in 2023-2025 are added.
- **Density does not hold.** Multifamily units per acre, average difference within 500 ft: -0.061 (0.021) published,
  -0.038 (0.023) without self-certification permits through 2022, +0.001 (0.026) without them through 2026; 5+ units
  -0.100, -0.066, -0.008. Moving from the 2022 to the 2026 score changes the more-stringent side for 12 percent of
  multifamily buildings.
- **Prices hold or strengthen.** Rents +0.017 (0.010) published, +0.032 (0.009) through 2026 without
  self-certification permits; sales +0.011 (0.007) and +0.014 (0.007); the sales difference at the boundary is +0.023
  (0.011) and +0.027 (0.010).
- **The multifamily density result rests on a near tie and on a few aldermen's self-certification permits.** 265 of
  2,151 multifamily buildings (17 alderman pairs) change sides between the published score and the score through 2026
  without self-certification permits, moving the average difference from -0.061 to +0.001; flipping the pairs one at
  a time accounts for +0.058 of that. Ward 1 and 32 (Proco Joe Moreno and Scott Waguespack, 127 buildings, 2011-2019)
  alone moves it by +0.027: their published scores differ by 0.09 SD (1.15 and 1.06) and by -0.06 in the later version
  (0.82 and 0.88). Wards 2 and 27 (Madeline Haithcock and Walter Burnett, Jr., 30 buildings, 2006-2007) moves it by
  +0.019, because dropping self-certification permits raises Haithcock's score from 0.05 to 0.89; the next largest
  are Ariel Reboyras and Felix Cardona Jr. (+0.009) and Theodore Matlak and Tom Tunney (+0.006).
