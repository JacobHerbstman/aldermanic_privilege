SHELL := bash
include ../../shell_functions.make

SCRIPT := create_rezoning_strictness_score_simple.R

OUTPUT_CSV := ../output/alderman_rezoning_strictness_scores.csv
OUTPUT_PLOT := ../output/rezoning_strictness_index.pdf
USE_ALL_REZONINGS ?= TRUE

INPUT_REZONING := ../input/rezoning_dataset_usable_19990101_20260212.csv
INPUT_ALDERMAN_PANEL := ../input/chicago_alderman_panel.csv

all: $(OUTPUT_CSV) $(OUTPUT_PLOT)

$(OUTPUT_CSV) $(OUTPUT_PLOT): $(SCRIPT) $(INPUT_REZONING) $(INPUT_ALDERMAN_PANEL) | ../output
	$(R) $(SCRIPT) --input_rezoning=$(INPUT_REZONING) --input_alderman_panel=$(INPUT_ALDERMAN_PANEL) --output_csv=$(OUTPUT_CSV) --output_plot=$(OUTPUT_PLOT) --use_all_rezonings=$(USE_ALL_REZONINGS)

$(INPUT_REZONING): /Users/jacobherbstman/Desktop/alderman_data/tasks/rezoning_dataset/output/rezoning_dataset_usable_19990101_20260212.csv | ../input
	ln -sf "$<" "$@"

$(INPUT_ALDERMAN_PANEL): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: $(INPUT_REZONING) $(INPUT_ALDERMAN_PANEL)
