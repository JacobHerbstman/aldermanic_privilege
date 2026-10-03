# Checks suggested after the 2026 UEA presentation (exploratory)

Nothing here feeds the paper or slides. Run `make` in `code/`. Event-study checks use the paper's blocks and PPML
specification (`tasks/run_event_study_permit`) in two comparisons: the paper's (unchanged blocks on both sides of the
old boundary, 2010-2020) and the same-side one (unchanged blocks in the ward the block left, 2012-2018, before the
2019 election). Every binary estimate reproduces the corresponding production number.

- `processing_time_event_study.R`: permit-level event study of days from application to issue for high-discretion
  permits (log days, days, PPML on days; block or group fixed effects).
- `continuous_stringency.R`: binary treatments against the continuous score change or score gap, for the event
  study, density, rents and sales.
- `density_continuous_suite.R`: every density specification in the paper and appendix, binary and continuous.
- `density_continuous_plot.R`: the paper's density figure with the continuous treatment (per SD of score gap).
- `discretion_difference_event_study.R`: high- minus low-discretion permits as one stacked (triple-difference) event
  study.
- `lenient_size_by_boundary.R`: effects by size of move, and the continuous lenient effect leaving out each move.
- `reassignment_cost.R`: a term for being reassigned at all plus the continuous score change (or its sign).
- `event_study_segment_fe.R`: the paper's event study with boundary-segment-by-year in place of ward-pair-by-year
  fixed effects (each block on the nearest 2003-map segment of its ward pair).
- `reassignment_by_direction.R`: reassignment plus separate stricter and lenient indicators, with moves under a
  score-change cutoff counting as reassignment without a change in stringency.

## Findings (October 1-2, 2026)

- **Processing time does not respond.** All 17,988 high-discretion permits on the event-study blocks are assigned
  (40 percent are issued the day they are filed). No outcome, fixed-effect choice or comparison gives an effect
  (stricter moves: -0.19 to +0.26 log points or -3 to +8 days, none significant), and pre-trends are often poor.
- **Continuous variation adds power in the event study and rents, not in density.** Combined event-study t-statistics
  rise from 2.6 to 3.3 (paper's comparison) and 1.8 to 2.2 (same side); rents from 1.7 to 2.9; multifamily density
  falls from 2.9 to 2.1. Across the density suite the continuous estimates keep the sign of the binary ones but are
  smaller, and boundaries with larger score gaps do not show larger differences.
- **High minus low discretion works as one event study.** Moves toward stricter aldermen: -0.269 (0.111) in the
  paper's comparison, -0.316 (0.173) same side, with flat pre-trends; moves toward more lenient aldermen about zero.
- **Being reassigned at all lowers high-discretion permits for about two years.** With the continuous change, the
  reassignment term is -0.077 (0.054) and -0.176 (0.073) and the slope per SD more stringent -0.162 (0.047) and
  -0.224 (0.095); the reassignment term is concentrated in 2015-2016, while the stringency term persists. Neither
  appears for low-discretion permits. Leaving out any one of the 54 moves keeps both terms negative; the
  near-zero move 3->20 (43 blocks) carries the most weight for the reassignment term.
- **Clearly lenient moves raise permits.** Counting moves under 0.25 SD as reassignment only, moves toward clearly
  more stringent aldermen change permits by -0.232 (0.091) and -0.296 (0.159) relative to unchanged blocks, and
  moves toward clearly more lenient aldermen by +0.149 (0.059) and +0.178 (0.103); cutoffs of 0.10 and 0.50 SD give
  similar totals. The split into a reassignment cost and direction effects rests on 11 near-zero moves whose
  pre-period is not flat (same-side pre-trend p = 0.04), so only the totals are reliable.
- **Segment-by-year fixed effects roughly halve the event-study estimates.** Combined -0.076 (0.047) against -0.127
  (0.049); moves toward more stringent aldermen -0.110 (0.094, pre-trend p = 0.03) against -0.231 (0.096), with the
  2015 drop unchanged (-0.40); toward more lenient +0.058 against +0.067. 618 of the 687 reassigned blocks share a
  segment with unchanged blocks, but 517 of 752 segments hold only unchanged blocks, and 19 percent of block-years drop
  out (segment-years without permits).
