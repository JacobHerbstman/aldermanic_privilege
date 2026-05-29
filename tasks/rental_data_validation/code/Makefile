all: ../output/rental_data_validation_growth.tex

../output/rental_data_validation_growth.tex: build_rental_data_validation.R ../input/chicago_rent_panel.parquet | ../output
	Rscript $<

../input/chicago_rent_panel.parquet: ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/chicago_rent_panel.parquet

include ../../generic.make
