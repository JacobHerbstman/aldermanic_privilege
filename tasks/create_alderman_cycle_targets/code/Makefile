R := Rscript

all: ../output/alderman_cycle_targets.csv

../output/alderman_cycle_targets.csv: build_alderman_cycle_targets.R ../input/chicago_alderman_panel.csv | ../output
	$(R) $<

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/chicago_alderman_panel.csv

include ../../generic.make
