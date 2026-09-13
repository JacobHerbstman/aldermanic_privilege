# Recorded replication sources

Run `make` in this directory before the first analysis build. It retrieves the
September 10, 2026 source archive from the repository's GitHub release, verifies
its SHA-256 checksum, and restores the 103 files listed in `source_files.txt`
under `data_raw/`. A missing member is restored through the same extraction rule.
Unchanged builds reuse the archive and extracted files.

The building-permit task separately restores the exact 721,958-record extract
preserved on September 12, 2026. The additional asset is available in the same
release; its public download was verified against the preserved file.
Its filename and checksum are
documented in [`tasks/download_building_permits/README.md`](../tasks/download_building_permits/README.md).
The original September 10 archive has not been replaced.

The recorded inputs supplement the code repository. Source descriptions and
qualifications belong to the owning task READMEs. Reviewed zoning history remains
a recorded input; ordinary builds apply those decisions. Other services still
used for source acquisition, including Census and Dewey, retain their documented
access and credential requirements.

After environment setup and source acquisition, run `make` in `paper/`. That is
the entry point for the analysis and all four paper PDFs. Standard data reports
are written when their datasets are produced; they are not separate build targets.

For maintainers deliberately assembling a new vintage, `make build-source-archive`
writes `source-archive-current.tar.gz` from the explicitly listed sources. It does
not overwrite the downloaded archive or publish a release. A new vintage needs
its own release name, checksum, source comparison and documentation.

The September 10 clean-clone test passed at commit `da074ae3`. That result applies
to that commit. Later changes require their own verification; a patched local
build is not a new unattended-clone test. Earlier run details and decisions are
preserved in the [build history](../tasks/audits/construction_review_history/release_audit/replication_build_history.md).
