#!/usr/bin/env bash
set -euo pipefail

curl --fail --location \
	"https://data.cityofchicago.org/api/views/syp8-uezg/files/Wir2BTHPb7-BTOWMkr8XcKCfKCt8U6y8wK20cV4Tjhw?download=true&filename=buildings.zip" \
	--output ../temp/chicago_building_footprints_2015.zip
