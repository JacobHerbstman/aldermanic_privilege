from importlib.metadata import PackageNotFoundError, version
from pathlib import Path
import subprocess
import sys

requirements = Path('report_requirements.txt').read_text().splitlines()
missing = []
for requirement in requirements:
    name, expected_version = requirement.split('==')
    try:
        installed_version = version(name)
    except PackageNotFoundError:
        installed_version = None
    if installed_version != expected_version:
        missing.append(requirement)
if missing:
    subprocess.run([sys.executable, '-m', 'pip', 'install', *missing], check=True)
lines = []
for requirement in requirements:
    name, expected_version = requirement.split('==')
    installed_version = version(name)
    if installed_version != expected_version:
        raise RuntimeError('Report dependency version differs from the required version: ' + name)
    lines.append(name + ' : ' + installed_version)
Path('../output/Python_report_packages.txt').write_text('\n'.join(lines) + '\n')
