all: ../output/make_dag_audit.md

../output/make_dag_audit.md: audit_make_dag.py ../../../../paper/Makefile | ../output
	python3 audit_make_dag.py

../output/make_dag_audit.csv: ../output/make_dag_audit.md
	@test -s "$@"
include ../../../generic.make
