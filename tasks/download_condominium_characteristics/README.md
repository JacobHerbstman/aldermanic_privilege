# Condominium characteristics

`download_condominiums.R` queries the Cook County Assessor condominium
characteristics (Socrata `3r7i-mrz4`) for Chicago townships 70–77. It finds every
building (10-digit parcel number) with a unit reported built in 2000 or later,
then downloads all records of those buildings, and saves
`output/condominium_characteristics.csv` with its data report.

This is a live download: the Assessor revises these records, so a later run can
differ from the recorded report. Run `make` in `code/`.
