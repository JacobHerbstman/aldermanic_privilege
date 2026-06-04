all: ../output/block_group_controls.csv ../output/block_group_geometry_2019.gpkg

../output/block_group_controls.csv: create_block_group_controls.R | ../output
	Rscript $<

../output/block_group_geometry_2019.gpkg: ../output/block_group_controls.csv
	@test -s "$@"

include ../../generic.make
