#!/usr/bin/env bash
# Download Illinois 2020 Census block TIGER shapefiles.

set -euo pipefail

tmp_dir=$(mktemp -d "../temp/.download_tiger_blocks_2020.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT

zip_file="$tmp_dir/tl_2025_17_tabblock20.zip"

curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 1800 \
	-sL -o "$zip_file" \
	"https://www2.census.gov/geo/tiger/TIGER2025/TABBLOCK20/tl_2025_17_tabblock20.zip"

unzip -q -j "$zip_file" \
	"tl_2025_17_tabblock20.shp" \
	"tl_2025_17_tabblock20.dbf" \
	"tl_2025_17_tabblock20.prj" \
	"tl_2025_17_tabblock20.shx" \
	-d "$tmp_dir"

for suffix in shp dbf prj shx; do
	test -s "$tmp_dir/tl_2025_17_tabblock20.${suffix}"
	mv "$tmp_dir/tl_2025_17_tabblock20.${suffix}" "../output/tl_2025_17_tabblock20.${suffix}"
	touch "../output/tl_2025_17_tabblock20.${suffix}"
done

echo "Downloaded Illinois 2020 Census block TIGER shapefile"
