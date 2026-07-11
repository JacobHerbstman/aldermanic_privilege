# Rezoning Block Assignment Review

This audit reconstructs 2010 Census-block assignments for nine ordinances that
cannot be represented by an ordinary address point. It uses City zoning polygons
linked by Clerk document, ordinance, or planned-development number and four
explicit legal-description or parcel decisions. Polygon/block intersections
smaller than 100 square feet are treated as boundary slivers.

The reviewed production decisions are frozen in
`tasks/rezoning_block_hand_adjudications/`.
