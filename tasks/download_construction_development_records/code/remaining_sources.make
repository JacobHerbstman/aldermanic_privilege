all: ../output/deming_pd853.pdf ../output/huron_pd356_2011.pdf

../output/deming_pd853.pdf: remaining_sources.make | ../output ../temp
	curl --fail --location 'https://gisapps.chicago.gov/gisimages/zoning_pds/PD853.pdf' --output ../temp/deming_pd853.pdf && pdfinfo ../temp/deming_pd853.pdf >/dev/null && mv ../temp/deming_pd853.pdf $@

../output/huron_pd356_2011.pdf: remaining_sources.make | ../output ../temp
	curl --fail --location 'https://occprodstoragev1.blob.core.usgovcloudapi.net/lsmatterattachmentspublic/30a5edca-e7c8-4504-b855-5b0e55389c2b.pdf' --output ../temp/huron_pd356_2011.pdf && pdfinfo ../temp/huron_pd356_2011.pdf >/dev/null && mv ../temp/huron_pd356_2011.pdf $@

all: ../output/huron_energy_benchmarking.csv

../output/huron_energy_benchmarking.csv: remaining_sources.make | ../output ../temp
	curl --fail --location --get 'https://data.cityofchicago.org/resource/xq83-jr8c.csv' --data-urlencode '$$where=upper(address) like "%910%HURON%"' --data-urlencode '$$limit=1000' --data-urlencode '$$order=data_year,id' --output ../temp/huron_energy_benchmarking.csv && python3 -c 'import csv; r=list(csv.DictReader(open("../temp/huron_energy_benchmarking.csv"))); assert r and len(r)<1000; assert len({x["row_id"] for x in r})==len(r); assert all(x["id"]=="251926" for x in r)' && mv ../temp/huron_energy_benchmarking.csv $@

all: ../output/huron_pd356.pdf

../output/huron_pd356.pdf: remaining_sources.make | ../output ../temp
	curl --fail --location 'https://gisapps.chicago.gov/gisimages/zoning_pds/PD356.pdf' --output ../temp/huron_pd356.pdf && pdfinfo ../temp/huron_pd356.pdf >/dev/null && mv ../temp/huron_pd356.pdf $@

all: ../report/huron_energy_benchmarking.csv.log

../report/huron_energy_benchmarking.csv.log: ../../shared/code/report.py ../output/huron_energy_benchmarking.csv | ../report
	$(PYTHON) $< ../output/huron_energy_benchmarking.csv $@ row_id

../output/montclare_pd1412.pdf: remaining_sources.make montclare_pd1412.sha256 | ../output ../temp
	curl --fail --location 'https://gisapps.chicago.gov/gisimages/zoning_pds/PD1412.pdf' --output ../temp/montclare_pd1412.pdf && pdfinfo ../temp/montclare_pd1412.pdf >/dev/null && shasum -a 256 -c montclare_pd1412.sha256 && mv ../temp/montclare_pd1412.pdf $@
