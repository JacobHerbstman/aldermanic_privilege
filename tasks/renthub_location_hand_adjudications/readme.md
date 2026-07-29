# renthub_location_hand_adjudications

Purpose: Preserves the manually verified coordinates used to resolve unstable high-volume RentHub addresses.

Each accepted row records a normalized address, verified coordinates, review status, and source URL. The production location-correction task applies a manual coordinate only when the row is marked `verified`.

Produces: `output/manual_verified_address_locations.csv`.
