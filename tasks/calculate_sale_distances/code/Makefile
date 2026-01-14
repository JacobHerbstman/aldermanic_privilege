SHELL := bash
include ../../shell_functions.make

SAMPLE := FALSE

all: ../output/sales_with_ward_distances.csv

../output/sales_with_ward_distances.csv: calculate_sale_distances.R ../input/Assessor_-_Parcel_Sales_20251123.csv ../input/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/alderman_restrictiveness_scores_month_FEs.csv ../input/ward_controls.csv | ../output
	$(R) calculate_sale_distances.R --sample=$(SAMPLE)

../input/Assessor_-_Parcel_Sales_20251123.csv: ../../../data_raw/Assessor_-_Parcel_Sales_20251123.csv | ../input
	ln -sf $< $@

../input/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv: ../../../data_raw/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/alderman_restrictiveness_scores_month_FEs.csv: ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	ln -sf $< $@

../input/ward_controls.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/Assessor_-_Parcel_Sales_20251123.csv ../input/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/alderman_restrictiveness_scores_month_FEs.csv ../input/ward_controls.csv
