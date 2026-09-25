#!/bin/bash
# Builds the Python environment that writes the data reports (tasks/shared/code/report.py): a virtual environment in
# ../output/python-env with the packages pinned in report_requirements.txt. pandas 3 needs Python 3.11 or later; the
# first such interpreter found on the PATH is used. Nothing is installed into the system Python.
set -euo pipefail

python=""
for candidate in python3.14 python3.13 python3.12 python3.11 python3; do
  if command -v "$candidate" > /dev/null && "$candidate" -c 'import sys; sys.exit(sys.version_info < (3, 11))'; then
    python="$candidate"
    break
  fi
done
if [ -z "$python" ]; then
  echo "The data reports need Python 3.11 or later, and none was found on the PATH." >&2
  echo "Install one (for example 'brew install python@3.13' or from https://www.python.org) and rerun make." >&2
  exit 1
fi

rm -rf ../output/python-env
"$python" -m venv ../output/python-env
../output/python-env/bin/python -m pip install --quiet --disable-pip-version-check -r report_requirements.txt
{
  ../output/python-env/bin/python --version
  ../output/python-env/bin/python -m pip freeze --disable-pip-version-check
} > ../output/Python_report_packages.txt
