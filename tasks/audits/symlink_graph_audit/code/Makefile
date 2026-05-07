all: ../output/make_dag_audit.md

../output/make_dag_audit.md ../output/make_dag_audit.csv: audit_make_dag.py ../../../../paper/Makefile | ../output
	python3 audit_make_dag.py

include ../../../generic.make

link-inputs:
