# Software Setup

This task installs any missing R packages and records their versions in
`output/R_packages.txt`. The Dewey package is installed from the exact GitHub
revision listed in `packages.R`; the remaining packages come from CRAN.

`setup_python_env.sh` builds the Python environment that writes the data
reports: a virtual environment in `output/python-env` with the packages pinned
in `report_requirements.txt`. pandas 3 needs Python 3.11 or later, so the script
uses the first such interpreter on the PATH and stops with instructions if none
is installed. Nothing is installed into the system Python. The report calls in
`tasks/shared/code/save_data.R` and `shell_functions.sh` run through this
environment; the versions are recorded in `output/Python_report_packages.txt`.
