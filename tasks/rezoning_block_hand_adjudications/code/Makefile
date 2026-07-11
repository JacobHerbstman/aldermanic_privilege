all: ../output/reviewed_rezoning_block_assignments.csv

../output/reviewed_rezoning_block_assignments.csv:
	@test -f "$@" || { echo "Missing committed rezoning block assignments: $@"; exit 1; }

link-inputs:
