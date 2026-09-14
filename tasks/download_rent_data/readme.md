# RentHub source files

The rental analysis uses the 2,545 files listed in `sources/renthub_manifest.csv`.
This recorded list contains each provider filename, source date, byte size, and
MD5 checksum. It preserves the files used in the completed conference run.

The Makefile declares every listed file as a prerequisite of the output manifest.
The list is expanded because the provider supplies thousands of daily files.
A missing file runs the downloader; an unchanged build reuses the recorded files.

Retrieval requires `DEWEY_API_KEY`. Downloads go to `temp/` and move to `output/`
only after their sizes and checksums match the recorded list. Resuming a download
checks existing files and retrieves the missing ones. A changed provider file is
an error, not an automatic source refresh. Restoring licensed files from an
existing archive also satisfies the same declared inputs.

The period is January 2014 through December 2022. Changing that period or adopting
revised provider files requires recording a new source list as a research change.
