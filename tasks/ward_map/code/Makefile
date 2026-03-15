all: ../output/ward_map_2024.pdf

../output/ward_map_2024.pdf: ward_map.R ../input/Wards_2024.geojson | ../output
	Rscript ward_map.R ../input/Wards_2024.geojson ../output/ward_map_2024.pdf

../input/Wards_2024.geojson: ../../../data_raw/Wards_2024.geojson | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/Wards_2024.geojson
