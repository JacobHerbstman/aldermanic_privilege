SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

PYTHON ?= python3
START_DATE ?= 2010-12-01
END_DATE ?= 2026-02-12
SLEEP_SECONDS ?= 0.2
MAX_RETRIES ?= 4
MAX_ATTACHMENTS_PER_MATTER ?= 1
REQUEST_TIMEOUT_SECONDS ?= 120
PARSE_TIMEOUT_SECONDS ?= 180
PROGRESS_EVERY ?= 25
DATE_TAG := $(subst -,,$(START_DATE))_$(subst -,,$(END_DATE))
FULL_DATE_TAG := 20101201_20260212
YEARLY_DATE_TAGS := \
	20101201_20101231 \
	20110101_20111231 \
	20120101_20121231 \
	20130101_20131231 \
	20140101_20141231 \
	20150101_20151231 \
	20160101_20161231 \
	20170101_20171231 \
	20180101_20181231 \
	20190101_20191231 \
	20200101_20201231 \
	20210101_20211231 \
	20220101_20221231 \
	20230101_20231231 \
	20240101_20241231 \
	20250101_20251231 \
	20260101_20260212

all: ../output/pdf_text_$(DATE_TAG).csv ../output/pdf_zoning_fields_$(DATE_TAG).csv

ifeq ($(DATE_TAG),$(FULL_DATE_TAG))

link-inputs:

../output/pdf_text_$(FULL_DATE_TAG).csv: combine_pdf_text_chunks.py \
	$(foreach tag,$(YEARLY_DATE_TAGS),../output/pdf_text_$(tag).csv) \
	| ../output
	$(PYTHON) combine_pdf_text_chunks.py $(FULL_DATE_TAG) $(YEARLY_DATE_TAGS)

../output/pdf_text_20101201_20101231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2010-12-01 END_DATE=2010-12-31 ../output/pdf_text_20101201_20101231.csv

../output/pdf_text_20110101_20111231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2011-01-01 END_DATE=2011-12-31 ../output/pdf_text_20110101_20111231.csv

../output/pdf_text_20120101_20121231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2012-01-01 END_DATE=2012-12-31 ../output/pdf_text_20120101_20121231.csv

../output/pdf_text_20130101_20131231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2013-01-01 END_DATE=2013-12-31 ../output/pdf_text_20130101_20131231.csv

../output/pdf_text_20140101_20141231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2014-01-01 END_DATE=2014-12-31 ../output/pdf_text_20140101_20141231.csv

../output/pdf_text_20150101_20151231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2015-01-01 END_DATE=2015-12-31 ../output/pdf_text_20150101_20151231.csv

../output/pdf_text_20160101_20161231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2016-01-01 END_DATE=2016-12-31 ../output/pdf_text_20160101_20161231.csv

../output/pdf_text_20170101_20171231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2017-01-01 END_DATE=2017-12-31 ../output/pdf_text_20170101_20171231.csv

../output/pdf_text_20180101_20181231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2018-01-01 END_DATE=2018-12-31 ../output/pdf_text_20180101_20181231.csv

../output/pdf_text_20190101_20191231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2019-01-01 END_DATE=2019-12-31 ../output/pdf_text_20190101_20191231.csv

../output/pdf_text_20200101_20201231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2020-01-01 END_DATE=2020-12-31 ../output/pdf_text_20200101_20201231.csv

../output/pdf_text_20210101_20211231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2021-01-01 END_DATE=2021-12-31 ../output/pdf_text_20210101_20211231.csv

../output/pdf_text_20220101_20221231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2022-01-01 END_DATE=2022-12-31 ../output/pdf_text_20220101_20221231.csv

../output/pdf_text_20230101_20231231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2023-01-01 END_DATE=2023-12-31 ../output/pdf_text_20230101_20231231.csv

../output/pdf_text_20240101_20241231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2024-01-01 END_DATE=2024-12-31 ../output/pdf_text_20240101_20241231.csv

../output/pdf_text_20250101_20251231.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2025-01-01 END_DATE=2025-12-31 ../output/pdf_text_20250101_20251231.csv

../output/pdf_text_20260101_20260212.csv: process_pdfs.py pdf_parsers.py
	$(MAKE) START_DATE=2026-01-01 END_DATE=2026-02-12 ../output/pdf_text_20260101_20260212.csv

else

link-inputs: ../input/matter_attachments_$(DATE_TAG).csv ../input/candidate_final_ids_$(DATE_TAG).csv

../input/matter_attachments_$(DATE_TAG).csv: ../../elms_enrich_matters/output/matter_attachments_$(DATE_TAG).csv | ../input
	ln -sf "$<" "$@"

../input/candidate_final_ids_$(DATE_TAG).csv: ../../elms_enrich_matters/output/candidate_final_ids_$(DATE_TAG).csv | ../input
	ln -sf "$<" "$@"

../output/pdf_text_$(DATE_TAG).csv: process_pdfs.py pdf_parsers.py ../input/matter_attachments_$(DATE_TAG).csv ../input/candidate_final_ids_$(DATE_TAG).csv | ../output ../temp run.sbatch
	$(PYTHON) process_pdfs.py \
		--in-attachments-csv ../input/matter_attachments_$(DATE_TAG).csv \
		--in-candidates-csv ../input/candidate_final_ids_$(DATE_TAG).csv \
		--pdf-dir ../output/pdfs_$(DATE_TAG) \
		--sleep-seconds $(SLEEP_SECONDS) \
		--max-retries $(MAX_RETRIES) \
		--max-attachments-per-matter $(MAX_ATTACHMENTS_PER_MATTER) \
		--request-timeout-seconds $(REQUEST_TIMEOUT_SECONDS) \
		--parse-timeout-seconds $(PARSE_TIMEOUT_SECONDS) \
		--progress-every $(PROGRESS_EVERY) \
		--out-text-csv ../output/pdf_text_$(DATE_TAG).csv \
		--out-fields-csv ../temp/pdf_zoning_fields_download_parse_$(DATE_TAG).csv

endif

../output/pdf_zoning_fields_$(DATE_TAG).csv: parse_pdf_text_fields.py pdf_parsers.py ../output/pdf_text_$(DATE_TAG).csv | ../output
	$(PYTHON) parse_pdf_text_fields.py $(DATE_TAG) \
		--parse-timeout-seconds $(PARSE_TIMEOUT_SECONDS)

test: test_pdf_parsers.py test_process_pdfs.py test_parse_pdf_text_fields.py pdf_parsers.py process_pdfs.py parse_pdf_text_fields.py
	$(PYTHON) -m unittest test_pdf_parsers.py test_process_pdfs.py test_parse_pdf_text_fields.py
