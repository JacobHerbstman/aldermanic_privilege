# Alderman effects on permit applications (exploratory, branch `alderman-effects`)

A measure of alderman stringency that does not use processing times: each alderman's effect on the number of permit
applications on the blocks they represent, identified by changes in who represents a block (the 2015 remap and
turnover). Run `make` in `code/`.

`build_block_alderman_panel.R` builds a panel of every 2010 census block in Chicago for 2006–2022 (787,287
block-years), with high- and low-discretion permit applications by application year, placed in blocks as in the
permit event study (the counts match its panel exactly on the 359,579 block-years they share). Blocks follow the 2003
ward map through 2014 and the 2015 map from 2015, the map in force for most of that year; each ward-year's alderman is
the one who served it the most days, from the alderman terms. 110 aldermen represent blocks, and 83 percent of blocks
change alderman at least once.

`estimate_alderman_permit_effects.R` fits Poisson regressions of block-year counts on alderman indicators with block
fixed effects and either year fixed effects (`all_changes`) or 2003-ward-by-year fixed effects (`within_2003_ward`,
identified only by blocks of one 2003 ward represented by different aldermen after 2015). Effects are log points
relative to the average alderman, with standard errors clustered by block, and are shrunk toward zero by empirical
Bayes. It also estimates the all-changes high-discretion effects separately on odd and even years and on blocks with
odd and even block numbers.

## Findings (September 27, 2026)

The spread of alderman effects beyond sampling noise is 0.155 log points for high-discretion applications and 0.037
for low-discretion ones (0.135 and zero within 2003 wards); clustering by ward-year instead of block gives 0.157.
The two designs rank aldermen alike (Spearman 0.92). Effects from two interleaved halves of each ward's blocks agree
(correlation 0.55), but effects from odd and even years barely do (0.14), so the differences are shared across a
ward's blocks at a time but do not persist over an alderman's tenure: they look like ward-period development shocks
attributed to whoever is serving, which turnover and one remap cannot separate from alderman behavior. The effects
are unrelated to the processing-time index (Spearman -0.03 for all changes, +0.13 within 2003 wards, where a
stricter index should mean fewer applications).
