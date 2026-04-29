SHELL := bash
include ../../shell_functions.make

all: ../output/residential_cross_section.csv ../output/residential_condo_review.csv

../output/residential_cross_section.csv: residential_improvements_cleaning.R ../input/residential_improvement_characteristics.csv ../input/approved_assessor_corrections.csv | ../output
	$(R) residential_improvements_cleaning.R ../input/residential_improvement_characteristics.csv ../input/approved_assessor_corrections.csv ../output/residential_cross_section.csv ../output/residential_condo_review.csv

../output/residential_condo_review.csv: residential_improvements_cleaning.R ../input/residential_improvement_characteristics.csv ../input/approved_assessor_corrections.csv | ../output
	$(R) residential_improvements_cleaning.R ../input/residential_improvement_characteristics.csv ../input/approved_assessor_corrections.csv ../output/residential_cross_section.csv ../output/residential_condo_review.csv

../input/residential_improvement_characteristics.csv: ../../../data_raw/Assessor_-_Single_and_Multi-Family_Improvement_Characteristics_20251111.csv | ../input
	ln -sf "$<" "$@"

../input/approved_assessor_corrections.csv: ../../new_construction_manual_corrections/manual/approved_assessor_corrections.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/residential_improvement_characteristics.csv ../input/approved_assessor_corrections.csv
