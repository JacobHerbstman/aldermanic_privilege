# union_donations_permit_correlations

Purpose: Exploratory audit comparing alderman-level union donation shares and construction-trades donation shares with alderman-level permit, new-unit, density, and EB-shrunk stringency measures.

The permit measures use the high-discretion permit input through 2026. The residential/commercial measures use the combined geocoded residential plus commercial multifamily new-construction parcel source through 2026. Real dollar measures deflate cycle-level campaign receipts to 2022 dollars with the Chicago all-items CPI and treat each observed campaign cycle as four office-years.

Residualized checks use Frisch-Waugh-Lovell residuals of both the outcome and construction-trades share on the same episode controls: median household income, share Black, share Hispanic, homeownership, population, distance to CBD, distance to Lake Michigan, CTA stations within 800 meters, log real total receipts per cycle-year, and tenure years. Socioeconomic controls are tenure-window averages from the alderman monthly panel joined to ward-year controls; place controls are alderman-year averages of the existing Stage 1 permit-location controls. Share White is excluded from the residualization control set to avoid race-share collinearity.

Produces:

- `output/union_donations_permit_join.csv`
- `output/union_donations_permit_summary_stats.csv`
- `output/union_donations_permit_correlations.csv`
- `output/union_donations_permit_top_bottom.csv`
- `output/union_donations_permit_unmatched_aldermen.csv`
- `output/union_share_processing_time_scatter.png`
- `output/union_share_processing_time_scatter_ytrimmed.pdf`
- `output/union_share_permit_counts_scatter.png`
- `output/union_share_permit_counts_scatter_ytrimmed.pdf`
- `output/union_share_units_density_scatter.png`
- `output/union_share_units_density_scatter_ytrimmed.pdf`
- `output/union_share_stringency_score_scatter.pdf`
- `output/union_share_stringency_score_regression.csv`
- `output/construction_trades_share_stringency_score_scatter.pdf`
- `output/construction_trades_share_stringency_score_regression.csv`
- `output/construction_trades_share_score_binscatter.pdf`
- `output/construction_trades_share_score_bins.csv`
- `output/construction_trades_share_score_rank_rank.pdf`
- `output/construction_trades_share_score_rank_regression.csv`
- `output/construction_trades_score_extensive_margin.pdf`
- `output/construction_trades_score_extensive_margin.csv`
- `output/construction_trades_log_dollars_score_scatter.pdf`
- `output/construction_trades_score_robust_slopes.csv`
- `output/construction_trades_score_leaveout_checks.csv`
- `output/construction_trades_share_score_labeled_scatter.pdf`
- `output/construction_trades_residualized_join.csv`
- `output/construction_trades_residualized_correlations.csv`
- `output/construction_trades_residualized_leaveout_checks.csv`
- `output/construction_trades_residualized_ppml.csv`
- `output/construction_trades_residualized_ppml_effects.pdf`
- `output/construction_trades_residualized_ppml_adjusted_rates.pdf`
- `output/construction_trades_residualized_score_binscatter.pdf`
- `output/construction_trades_residualized_score_rank_rank.pdf`
- `output/construction_trades_residualized_score_labeled_scatter.pdf`
- `output/construction_trades_residualized_permit_count_scatter.pdf`
- `output/construction_trades_residualized_permit_count_binscatter.pdf`
- `output/construction_trades_share_raw_correlations.csv`
- `output/construction_trades_share_processing_time_scatter.pdf`
- `output/construction_trades_share_permit_counts_scatter.pdf`
- `output/construction_trades_share_log_permit_count_correlations.csv`
- `output/construction_trades_share_log_permit_counts_scatter.pdf`
- `output/construction_trades_share_units_density_scatter.pdf`

This task is descriptive only. It does not define a causal specification or change production union donation outputs.
