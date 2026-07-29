DATE_TAG ?= 20101101_20201231

all: ../output/rezoning_external_geocodes_$(DATE_TAG).csv

../output/rezoning_external_geocodes_$(DATE_TAG).csv:
	@test -f "$@" || { echo "Missing committed external geocodes: $@"; exit 1; }

link-inputs:
