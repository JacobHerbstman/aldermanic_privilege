# Parcel lot areas, 2021

`download_parcel_polygons.R` downloads the Cook County GIS tax-parcel polygons for tax year 2021 ("ccgisdata - Parcel
2021", Cook County open data dataset `77tz-riq7`, a fixed historical vintage) for the City of Chicago, one query per
3-digit parcel-number prefix, and writes each parcel's lot area in square feet (EPSG:3435). A parcel drawn as several
polygons gets their summed area. Run `make` in `code/`; the received pages go to `temp/`. Downloaded September 28,
2026: 612,202 polygons for 604,830 parcels. Consumer: `tasks/explore_land_prices`.
