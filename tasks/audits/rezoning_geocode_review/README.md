# Rezoning Geocode Review

This audit queries the public Census Geocoder for stage-one unmatched matters
that have a usable street address and are not already covered by the frozen
Chicago or Census handoff.

Results are review material only. Coordinates enter production only after their
input and matched addresses are checked and the accepted rows are added to the
committed table in `tasks/rezoning_geocode_external_merge_ready/output/`.
