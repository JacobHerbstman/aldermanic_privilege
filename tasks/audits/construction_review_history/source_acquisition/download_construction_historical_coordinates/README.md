# Historical parcel source acquisition

This task queries Cook County Parcel Universe assessment-year records for every
construction-discovery PIN lacking usable current coordinates. It preserves the
request list as well as the returned records, so an empty response can be
distinguished from a PIN that was never queried. The default history window is
1999–2025, matching the original historical source window.

It creates a new source snapshot for reconciliation. Construction cleaning reads
the pinned source files in `data_raw/construction_review/`; running an acquisition
does not replace those files automatically. The old source does not cover all
newly appearing PINs in the current Assessor vintage. Their history must be
queried and reviewed before they can enter the paper sample.
