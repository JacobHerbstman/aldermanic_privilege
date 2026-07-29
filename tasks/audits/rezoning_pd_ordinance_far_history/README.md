# rezoning_pd_ordinance_far_history

## Purpose
Audits unresolved `PD -> PD` amendments in the Khan comparison sample and continues
the same ordinance-supported review through December 2020 introduction dates. The
task downloads every available ELMS attachment for the selected matters and extracts
FAR statements and references to earlier PD approvals. It also downloads DPD's public
PD-number archive, which appends the original ordinance and earlier amendments to the
current PD file.

This task is diagnostic. Its outputs do not enter the production rezoning data.

## Inputs
- `tasks/audits/rezoning_khan_comparison/output/khan_reconciled_sample.csv`
- `tasks/audits/rezoning_khan_comparison/input/khan_pd_underlying_evidence.csv`
- `tasks/elms_enrich_matters/output/matter_attachments_20101101_20160831.csv`
- `tasks/elms_fetch_ordinances/output/matters_20101201_20260212.csv`
- `tasks/elms_fetch_ordinances/output/candidate_seed_ids_20101201_20260212.csv`
- `input/pd_far_manual_decisions.csv`: link to the frozen Khan-window FAR decisions in `tasks/rezoning_hand_adjudications/output/`.
- `input/post_khan_pd_queue_manual_classifications.csv`: link to frozen classifications for
  incomplete post-Khan zoning parses.
- `input/post_khan_pd_far_manual_decisions.csv`: link to the frozen post-Khan FAR decisions in `tasks/rezoning_hand_adjudications/output/`.

## Outputs
- `output/unresolved_pd_to_pd_queue.csv`: the 85 unresolved amendments.
- `output/pd_all_attachment_text.csv`: page text from all available attachments.
- `output/pd_all_attachment_fields.csv`: download and parse status by attachment.
- `output/pd_far_evidence.csv`: normalized FAR and prior-approval text evidence.
- `output/pd_far_review_queue.csv`: one-row-per-matter coverage summary.
- `output/pd_archive_text.csv`: page text from the live DPD PD-number archive.
- `output/pd_archive_fields.csv`: archive download, page, OCR, and file-hash metadata.
- `output/pd_archive_far_evidence.csv`: FAR statements found throughout each PD history.
- `output/pd_archive_review_queue.csv`: archive coverage joined to the 85 matters.
- `output/pd_far_decisions_audit.csv`: accepted ordinance-supported old/new pairs.
- `output/pd_far_decision_summary.csv`: recovery counts and FAR-change summary.
- `output/pd_far_unresolved.csv`: remaining matters and the reason no pair was accepted.
- `output/pd_far_khan_comparison_summary.csv`: effect of accepted pairs on the Khan benchmark.
- `output/post_khan_pd_to_pd_queue_20160901_20201231.csv`: the 84 passed post-Khan
  PD amendments found from September 2016 through December 2020 introduction dates.
- `output/post_khan_discovery_summary_20160901_20201231.csv`: candidate, parse, and
  manual-classification coverage.
- `output/post_khan_pd_far_decisions_audit_20160901_20201231.csv`: accepted post-Khan
  old/new FAR pairs with separate source-page citations.
- `output/post_khan_pd_far_unresolved_20160901_20201231.csv`: post-Khan matters without
  a comparable scalar old/new scope.
- `output/post_khan_pd_far_decision_summary_20160901_20201231.csv`: post-Khan recovery
  counts and FAR-change summary.
- `output/pd_far_20101101_20201231_summary.csv`: combined coverage for both audit cohorts.

## Run
```bash
cd tasks/audits/rezoning_pd_ordinance_far_history/code
make link-inputs
make
```

## Decision Standard
An old/new FAR pair is usable only when both values refer to the same whole-PD or
affected-subarea geography. Base FAR, bonus-inclusive total FAR, and subarea FAR
are not interchangeable. Ambiguous cases remain unresolved.

## Source Status
The Khan-window ELMS attachment list is frozen by the upstream production task. The
post-Khan attachment inventory is rebuilt from ELMS's live matter-detail endpoint for a
cohort selected from the frozen full matter export. The DPD PD-number archive is also a
live official source at `gisapps.chicago.gov/gisimages/zoning_pds/` and can change when
DPD posts later amendments. File hashes and page-level extraction methods are recorded on
every audit run. The two manual decision CSVs and the incomplete-parse classification CSV
are frozen inputs; accepted decisions cite source pages so live-source updates cannot
silently redefine an old/new pair.

The post-Khan cohort is selected on introduction dates from September 1, 2016 through
December 31, 2020, matching the date convention used to extend the November 2010 Khan
window. A matter must have final ELMS status before entering the PD queue. Discovery
uses one priority attachment, then every available attachment is parsed for matters with
an incomplete first parse. Every available attachment is subsequently parsed for the
final PD queue.

## Findings
- The live DPD pass found all 73 requested PD archives, covering 83 of 85 matters.
- The archives contain 8,017 pages (592 MB); 1,957 pages required OCR.
- Comparable ordinance-supported pairs were accepted for 42 of 85 matters: 41 whole-PD
  pairs and one affected-subarea pair.
- Among accepted pairs, 30 do not change FAR, nine increase FAR, and three decrease FAR.
- The accepted subset averages 3.672 before and 3.606 after, a mean change of -0.066.
  This selected unresolved-PD subset is not directly comparable to Khan's full sample.
- Applying the accepted pairs to the documented-rules benchmark changes the observed
  old/new means from 1.997/2.334 to 2.032/2.360. It does not reconcile Khan's reported
  2.344/2.393 means.
- Of the 43 matters left unresolved, 39 contain FAR evidence but lack a comparable scalar
  old/new scope, two have no FAR statement in either source, and two lack a defensible PD
  number.
- The post-Khan discovery pass covered 1,959 zoning candidates, including 1,742 final
  matters. ELMS returned attachments and histories for every candidate with zero API errors.
- The automatic and fallback parsers found 74 post-Khan `PD -> PD` matters. Manual review
  of 22 incomplete one-sided PD parses added ten true PD amendments and excluded twelve
  base/PMD-to-PD or PD-to-PMD changes, producing an 84-matter queue.
- Ten of the 84 matters were introduced by December 2020 but passed in 2021. They remain
  in this audit because the Khan comparison and this extension define the sample window
  by introduction date; a passage-date event-study window must exclude them explicitly.
- The 84 amendments use 74 distinct DPD archives containing 8,681 pages; 2,331 pages
  required OCR. All requested archives downloaded successfully.
- Comparable ordinance-supported pairs were accepted for 55 of 84 post-Khan matters:
  53 whole-PD pairs and two affected-subarea pairs. Thirteen increase FAR, five decrease
  FAR, and 37 do not change FAR.
- The accepted post-Khan subset averages 6.555 before and 6.689 after, a mean change of
  0.134. These selected amendments are not representative of all rezoning events.
- Of the 29 post-Khan matters left unresolved, 27 lack a comparable scalar old/new scope
  and two lack a usable FAR statement.
- Across the two audit cohorts, 97 of 169 reviewed PD amendments have accepted pairs and
  72 remain unresolved.

The audit therefore does not support treating all `PD -> PD` amendments as no-change or
assigning them the underlying base district FAR. Unresolved rows should remain excluded
from scalar FAR-change analysis unless a later source establishes a same-scope pair.
