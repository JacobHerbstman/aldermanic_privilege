# Paper

This directory contains the current manuscript and its appendices:

- Appendix A: data documentation
- Appendix B: alderman fixed effects and the stringency index
- Appendix C: additional permit evidence
- Appendix D: density boundary robustness checks
- Appendix E: rent and home-sales boundary checks

From this directory, build the paper with:

```sh
make
```

The Makefile lists every generated table and figure used by the manuscript.
Running `make` checks the task that creates each one before compiling
`paper.pdf`. From the repository root, the equivalent command is simply
`make`.
