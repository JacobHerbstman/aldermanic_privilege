include ../../shared/code/shell_functions.make

START_YEAR = 2006
END_YEAR = 2022

all: ../output/building_permits_2006_2022.csv

../output/building_permits_2006_2022.csv: ../input/building_permits_2006_2022_preserved_20260912.csv.gz source_snapshot.sha256 ../../shared/code/report.py | ../output ../report
	shasum -a 256 -c source_snapshot.sha256
	gzip -dc $< > $@.tmp
	mv $@.tmp $@
	$(PYTHON) ../../shared/code/report.py $@ ../report/building_permits_2006_2022.csv.log id

../input/building_permits_2006_2022_preserved_20260912.csv.gz: source_snapshot.sha256 | ../input
	wget -O $@.tmp --no-use-server-timestamps https://github.com/JacobHerbstman/aldermanic_privilege/releases/download/recorded-sources-2026-09-10/building_permits_2006_2022_preserved_20260912.csv.gz
	sed 's/\.csv\.gz$$/.csv.gz.tmp/' source_snapshot.sha256 | shasum -a 256 -c -
	mv $@.tmp $@

download-current: ../temp/building_permits_current_$(START_YEAR)_$(END_YEAR).csv

../temp/building_permits_current_$(START_YEAR)_$(END_YEAR).csv: download_building_permits.sh Makefile | ../temp
	bash $< $(START_YEAR) $(END_YEAR)

include ../../shared/code/generic.make
