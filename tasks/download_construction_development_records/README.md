# Construction development records

Public development records used to review land denominators and construction
timing for Eastgate, Park Place, 4400 Grove, and Natchez. These are planning documents, not evidence that every
proposed building was completed or that a particular tax PIN owns all common land.

Run `make` in `code/`. The explicit URLs identify PD 986 and the 2013 Park Place
PD 989 amendment submission. Files are downloaded unchanged and reused on an
unchanged build. Consumers must record the relevant page and distinguish proposal
from approval. A failed download leaves no completed output.

`grove_pd1395.pdf` was acquired September 7, 2026 from the City's PD 1395
compilation. It contains the January 17, 2018 adoption and 2022/2023 letters.
PDF page 12 reports the original subarea land allocations; page 16 maps them.
The 2023 letter identifies the completed 84-unit first phase as Subarea A.
The URL is mutable: reuse the recorded file on ordinary builds, preserve these
bytes with the source archive, and treat a subsequent replacement as a source
refresh. Its checksum is recorded in `SHA256SUMS`.

`northmarq_chicago_2025q2.pdf` was acquired September 7, 2026 from Northmarq.
Page 4 lists North Oak Lofts with 237 units and year built 2020. It is a market
report about the broader complex, not proof of the completion of each earlier
subarea. Preserve the recorded bytes and checksum under the same vintage policy.
The May 2023 Roosevelt conference PDF linked in the release audit returned HTTP
403 on direct download; it remains unpinned web evidence, not a production input.
