SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

OUTPUT_FILE := ../output/ward_year_sales.csv

# This is the SYMBOLIC LINK this task needs in its input folder.
INPUT_FILES := ../input/Assessor_-_Parcel_Sales_20251123.csv \
               ../input/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv \
               ../input/ward_panel.gpkg

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: all
all: $(OUTPUT_FILE)

include ../../generic.make

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output
# ─────────────────────────────────────────────────────────────────────────────
$(OUTPUT_FILE): clean_sales_data.R $(INPUT_FILES) | ../output
	@echo "→ [sales_data_cleaning] Running R script to clean sales data..."
	$(R) clean_sales_data.R

# ─────────────────────────────────────────────────────────────────────────────
# Rule to create the input symbolic link
# ─────────────────────────────────────────────────────────────────────────────
../input/Assessor_-_Parcel_Sales_20251123.csv: ../../../data_raw/Assessor_-_Parcel_Sales_20251123.csv | ../input
	@echo "→ [sales_data_cleaning] Linking sales data..."
	ln -sF $< $@

../input/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv: ../../../data_raw/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv | ../input
	@echo "→ [sales_data_cleaning] Linking parcel universe data..."
	ln -sF $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@echo "→ [sales_data_cleaning] Linking ward panel..."
	ln -sF $< $@

link-inputs: $(INPUT_FILES)

