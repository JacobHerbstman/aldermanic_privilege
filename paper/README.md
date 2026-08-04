# Paper

This directory contains the journal-submission files and a complete working-paper
version. The appendix is organized as follows:

- Appendix A: data documentation
- Appendix B: alderman fixed effects and the stringency index
- Appendix C: additional permit evidence
- Appendix D: density boundary robustness checks
- Appendix E: rent and home-sales boundary checks

From this directory, build the paper with:

```sh
make
```

The Makefile lists every generated table and figure used by both documents.
Running `make` checks the task that creates each one before compiling:

- `paper.pdf`: the main manuscript
- `online_appendix.pdf`: the separately submitted online appendix
- `working_paper.pdf`: the complete paper with figures, tables, appendices, and
  internal links in one document

From the repository root, the equivalent command is simply `make`.
