# Rezoning PD FAR Merge-Ready Handoff

This task publishes committed, merge-ready PD FAR review decisions for downstream
rezoning datasets.

The current handoff file is:

`output/merge_ready/pd_far_text_review_updates_2011_2020.csv`

It was produced from the packet review workflow in
`tasks/rezoning_pd_far_text_review`, using the static decision source
`tasks/rezoning_pd_far_text_review/code/pd_far_text_review_decisions.csv`.

The file contains only resolved 2011-2020 non-PD-to-PD review updates. PD-to-PD
amendments remain excluded from this handoff. Unresolved reviewed rows remain in
`tasks/rezoning_pd_far_text_review/output/pd_far_text_review_unresolved_2011_2020.csv`.

Downstream tasks should read this committed handoff instead of reading review
packet outputs directly.
