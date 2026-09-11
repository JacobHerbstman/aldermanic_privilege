# Recorded replication sources

Run `make` in this directory before the first analysis build. It retrieves the
September 10, 2026 source archive from the repository's GitHub release, verifies
its SHA-256 checksum, and restores the files listed in `source_files.txt` under
`data_raw/`. Missing members cause the archive to be restored again. Ordinary
unchanged builds reuse the downloaded archive and extracted files.

These exact recorded inputs supplement the code repository. They are necessary
because later queries to live agencies can return different historical records.
The archive contains no task-output caches or credentials. Source descriptions
and qualifications remain in the owning task READMEs and the two source-folder
READMEs. The preserved reviewed zoning history remains a recorded input as
approved; the ordinary build does not reconstruct every historical ordinance.

For maintainers deliberately assembling a new vintage, `make -f build_archive.make`
creates the archive from the explicitly listed local sources. A new vintage needs
its own release name, checksum, source comparison and documentation; do not replace
an existing release's source bytes silently.

The September 10 clean-checkout test is in progress. Publication of the source
archive alone does not establish that the full paper has been rerun successfully.

The diagnostic run has now compiled `paper.pdf`, `online_appendix.pdf`,
`working_paper.pdf`, and `word_count.pdf` after download and validation repairs.
It began at `2c7166ca` and was patched during execution, so it is not the final
fresh-clone test. The complete construction ledger and sales-parcel coordinates
match the prior approved files exactly; main density and price estimates match
at reported precision. Some other sources are still live: the permit download
produced small score changes, though boundary-side assignments and the main
reported results did not change. The manuscript prose was left untouched and
contains older density numbers and sample counts that need a separate update.
GNU Make 3.81 passed missing-price-output recovery, missing-source-report
recovery, and an unchanged full-paper rebuild with no producer or compiler runs
in the diagnostic checkout.

Verified locally: a download through the public release URL matches the recorded
SHA-256; extraction restores all 103 listed files; deleting a non-primary member
causes restoration; an unchanged second Make run performs no work (GNU Make 3.81).

### Fresh-clone validation requirement (September 10, 2026)

Current instruction: first finish a diagnostic build, fixing failures and resuming
the same checkout so all necessary repairs can be collected together. Record
every repair, commit and push the complete set, then perform one final fresh-clone
test. A patched diagnostic run is not evidence of an unattended fresh build.
The strict restart requirement below applies to that final validation, not to
the ongoing diagnostic run.

The exploratory clone and local rebuild were stopped at the author's request.
They do not count as the final replication test. Before a new test, all required
code, recorded decisions, source references and acquisition instructions must be
committed and available from the remote repository. Start from a new clone of
that commit and obtain inputs through the documented acquisition process; do
not overlay uncommitted working files or copy local derived caches. If a required
committed file is missing or any build step breaks, fix and commit the cause,
then restart the test from another new clone. Do not patch and resume the failed
clone as evidence of a successful from-scratch build. Wait for the author's
instructions before restarting; the manuscript must remain unchanged.

The author authorized a new test on `conference-replication`. Use a new empty
`TIGRIS_CACHE_DIR` for each attempt, exported before running setup, source
acquisition and the paper Makefile. This prevents the Census geography package
from borrowing an existing user cache. The documented Census and Dewey credentials
and installed software remain prerequisites. Compile the manuscript only in the
fresh clone; leave the development checkout's manuscript untouched.

The first isolated-cache run at commit `29dc42a4` failed on one of 470 live Cook
County sales-parcel requests (2022, batch 19). The same request subsequently
returned all 500 records. Retry ordering was revised to revisit failures after
the other requests, and a new clone is required to test the revision. This failed
run does not establish end-to-end replication.
