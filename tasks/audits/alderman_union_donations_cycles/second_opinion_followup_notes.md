# Browser Second-Opinion Follow-Up Notes

Review date: 2026-06-04

Browser ChatGPT review URL:
https://chatgpt.com/g/g-p-67fe5a56bcc881919196a57727f0879e-aldermanic-privilege/c/6a2213ac-4564-8325-b587-f1be776739d3

Inputs shown to ChatGPT:

- `tasks/audits/alderman_union_donations_cycles/output/second_opinion_packet.md`
- `tasks/isbe_campaign_disclosure_cycle_extract/code/extract_receipts_full_cycles.R`
- `tasks/alderman_union_committee_crosswalk_cycles/code/build_committee_crosswalk_cycles.R`
- `tasks/alderman_union_committee_crosswalk_cycles/code/apply_committee_manual_decisions_cycles.R`
- `tasks/alderman_union_donor_classify_cycles/code/build_donor_union_crosswalk_cycles.R`
- `tasks/alderman_union_donations_cycles/code/build_union_donation_panel_cycles.R`
- `tasks/alderman_union_candidate_aggregates_cycles/code/build_union_candidate_aggregates_cycles.R`

Main second-opinion findings:

- The current target population is elected aldermen and associated committees by governing cycle, not all union contributions in aldermanic races. Any paper text should use that estimand precisely.
- Donor `review_flag` rows were not manually adjudicated before downstream counting. A manual-decision layer now exists, with seeded accept-auto decisions that preserve current results but still require Jacob confirmation before paper use.
- Browser/ChatGPT pre-reviewed the 218 review-required donor rows on 2026-06-05. Its suggestions are stored in `chatgpt_*` columns in `tasks/alderman_union_donor_classify_cycles/code/donor_manual_review_decisions_cycles.csv` and in `tasks/audits/alderman_union_donations_cycles/output/donor_chatgpt_pre_review_suggestions.csv`.
- ChatGPT gave 189 clear accept suggestions, 2 clear exclude suggestions, and left 27 rows for Jacob review. The 189 clear accepts and 2 clear excludes have been incorporated into the operative manual decision fields, with `needs_jacob_spotcheck` statuses. The 27 unresolved rows remain seeded from the auto classification until Jacob reviews them.
- The donor classifier used a broader receipt universe than the final panel because it did not first apply the non-archived D2Part 1A, 2A, and 5A analytic filter.
- D2Part filtering by first character was fragile. The reviewer recommended tabulating exact raw values and filtering on exact normalized D2Part codes.
- Amount parsing should not silently turn failed parses into zero. The reviewer recommended a parse-failure audit and a stricter money parser.
- Heuristic repair of shifted receipt rows should produce a quarantined row-level audit before the repaired rows are treated as production data.
- Including D2Part 2 transfers can double count money inside alderman political networks when multiple related committees are included. The reviewer recommended gross and net-of-internal-transfer versions.
- The reviewed named-recipient committee definition is broader and more subjective than the strict candidate definition. The reviewer recommended presenting strict candidate as the most conservative candidate measure and reviewed named-recipient as an expanded/network measure unless every named-recipient committee is documented as controlled by or fundraising for the alderman.
- The committee matching audit should expose all plausible candidate matches before choosing a best match, including nickname and name-variant cases.
- The parity report should classify every failed row as a documented prototype bug, known estimand change, or unresolved difference.

Immediate interpretation:

- The R port is reproducible and the known prototype extract issue is documented, but the second opinion says the current reviewed named-recipient main measure should be treated as a promising working output, not a final report-ready measure.
- Before using the measure in a paper table, the highest-priority remaining additions are Jacob confirmation of the 27 rows still flagged by ChatGPT plus spot-checking the clear suggestions, repair-row quarantine, and internal-transfer sensitivity outputs.
