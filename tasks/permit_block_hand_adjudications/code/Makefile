all: ../output/manual_permit_block_assignments.csv

../output/manual_permit_block_assignments.csv:
	@test -f "$@" || { echo "Missing committed permit block assignments: $@"; exit 1; }

link-inputs:
