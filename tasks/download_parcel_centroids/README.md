# Parcel centroids

`download_parcel_centroids.R` queries the Cook County Assessor parcel universe
(Socrata `nj4t-kc8j`) for the City triad, 1999–2025. It saves one row per 10-digit
parcel number ever assessed in those years, with the mean EPSG:3435 centroid and
the first and last assessment years, so parcels later retired by a condominium
declaration or subdivision keep a location.

This is a live download: the Assessor revises these records, so a later run can
differ from the recorded report. Run `make` in `code/`.
