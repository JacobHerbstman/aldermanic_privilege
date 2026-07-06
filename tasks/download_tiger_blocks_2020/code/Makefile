all: \
	../output/tl_2025_17_tabblock20.shp \
	../output/tl_2025_17_tabblock20.dbf \
	../output/tl_2025_17_tabblock20.prj \
	../output/tl_2025_17_tabblock20.shx

../output/tl_2025_17_tabblock20.shp: download_tiger_blocks_2020.sh | ../output ../temp
	bash $<

../output/tl_2025_17_tabblock20.dbf \
../output/tl_2025_17_tabblock20.prj \
../output/tl_2025_17_tabblock20.shx: ../output/tl_2025_17_tabblock20.shp
	@test -s "$@"

include ../../generic.make
