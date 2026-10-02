# Condominium characteristics

`download_condominiums.R` downloads the Cook County Assessor condominium unit
characteristics (Socrata `3r7i-mrz4`) for every unit record in the Chicago
townships 70–77, every tax year, and saves `output/condominium_characteristics.csv`
with its data report. Each township-year is one request, and its row count must
equal the API's count; the download stops if the source was updated while it ran.
Years are kept as the source writes them ("2026" or "2026.0").

This is a live download: the Assessor revises these records, so a later run can
differ from the recorded report. Run `make` in `code/`.
