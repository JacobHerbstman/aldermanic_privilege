# Energy benchmarking

`download_energy_benchmarking.R` downloads the City of Chicago Energy
Benchmarking data (Socrata `xq83-jr8c`): one row per building of 50,000 square
feet or more and reporting year (2014 on), with its reported year built,
property type, floor area, address and location. It saves
`output/energy_benchmarking.csv` with its data report. The year built is
reported by building owners, independently of the Assessor.

This is a live download: the City revises these records, so a later run can
differ from the recorded report. Run `make` in `code/`.
