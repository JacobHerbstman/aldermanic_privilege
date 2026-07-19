# Short Paper Variant

This directory contains a shorter manuscript variant built from the same
pipeline outputs as `paper/`.

The long draft remains in `paper/`. This version follows the shorter
submission-style draft recovered from the old `paper_submission_rewrite`
checkout. It combines the empirical results in one main-text section and keeps
the compact appendix set:

- Appendix A: data documentation
- Appendix B: alderman fixed effects and the stringency index
- Appendix C: density boundary robustness checks
- Appendix D: additional permit evidence
- Appendix E: rent and home-sales boundary checks

Build with:

```sh
make
```

The Makefile calls the same task-level outputs under `../tasks/` as the long
paper, so changes to source plot scripts propagate to both manuscript versions.
