# Construction address geocoder responses

This task preserves complete Census and Chicago geocoder responses for the
historical addresses requested by construction cleaning. It does not accept or
reject a candidate location. The cleaning task owns request eligibility and
acceptance rules, and retains unresolved project requests without addresses.

The Census service is
`https://geocoding.geo.census.gov/geocoder/locations/onelineaddress`, with
`benchmark=Public_AR_Current`, `format=json`, and the historical address followed
by `Chicago, IL`. The City service is
`https://gisapps.cityofchicago.org/arcgis/rest/services/Chicago_Addresses/GeocodeServer/findAddressCandidates`,
with `SingleLine` equal to the historical address, `outFields=*`, `outSR=3435`,
and `f=json`. Both services are mutable; old chosen-match exports do not contain
the full historical responses.

Run intentional acquisition through `make -f download_recipes.make` in `code/`.
Each saved row contains the exact address query and the complete returned JSON,
including all candidates or an empty match list. Request failures and API errors
stop the build before replacing an output. Record a dated source snapshot and
its hashes before using a response table in ordinary production. Default Make
replays pinned responses; it never silently refreshes them from the service.

The September 7, 2026 snapshot contains 196 queried addresses for each service,
covering the 211 original project-component-year requests (seven had no selected
address, and some shared an address). Replaying the original acceptance rules on
these complete responses reproduces every field of both original chosen-match
tables, including coordinates. This verifies this particular replay; it does not
establish that every accepted location is correct. Four Census matches change the
requested street direction or type, and their acceptance remains under review.

The immutable inputs are `address_geocodes_census_2026-09-07.csv` and
`address_geocodes_chicago_2026-09-07.csv` in `data_raw/construction_review/`;
`code/source_snapshot.sha256` fixes their bytes. Ordinary `make` copies these
inputs and generates reports. Intentional acquisition writes separate
`address_geocodes_census_download.csv` and `address_geocodes_chicago_download.csv`
files so it cannot replace the production snapshot. The cleaning task fails if
its requests exceed the recorded responses. A new request scope requires an
explicit acquisition and snapshot update.
