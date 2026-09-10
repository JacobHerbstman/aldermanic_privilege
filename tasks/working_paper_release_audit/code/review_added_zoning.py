import pandas as pd

cohort = pd.read_csv('../reference/added_zoning_eighteen.csv', dtype=str)
zoning = pd.read_csv('../input/preferred_new_construction_zoning.csv', dtype=str)
history = pd.read_csv('../input/historical_zoning_project_construction_year.csv', dtype=str)
matters = pd.read_csv('../input/zoning_review_matters.csv', dtype=str)
assert cohort.project_id.is_unique and zoning.project_id.is_unique
review = cohort[['project_id', 'construction_zone_group']].rename(columns={'construction_zone_group': 'previous_group'}).merge(zoning, on='project_id', validate='one_to_one')
assert len(review) == 18
history = history[['pin', 'construction_year', 'anchor_date', 'anchor_zone_group', 'construction_zone_group_supported']]
assert not history.duplicated(['pin', 'construction_year']).any()
review = review.merge(history, left_on=['nearest_validated_pin', 'nearest_validated_year'], right_on=['pin', 'construction_year'], suffixes=('', '_historical'), how='left', validate='many_to_one')
passed = matters.loc[matters.legacyStatus.eq('Passed'), ['matter_file', 'matter_passed_date', 'matter_title']].copy()
passed['clerk_document_2025'] = passed.matter_file.str.replace(r'^SO', 'O', regex=True)
passed = passed[passed.clerk_document_2025.isin(review.clerk_document_2025)].drop_duplicates()
assert passed.clerk_document_2025.is_unique
review = review.merge(passed, on='clerk_document_2025', how='left', validate='many_to_one')
maps = ['zone_group_2006', 'zone_group_2012', 'zone_group_2014', 'zone_group_2016', 'zone_group_2025']
review['all_five_maps_agree'] = review[maps].notna().all(axis=1) & review[maps].nunique(axis=1).eq(1)
review['group_changed'] = review.previous_group.ne(review.construction_zone_group)
review['evidence'] = 'Same parcel in preserved reviewed history; later maps agree with its broad group'
review.loc[review.all_five_maps_agree, 'evidence'] = 'All five recorded zoning maps agree on the broad group'
current = review.zoning_assignment_source.eq('current_polygon_last_event_preconstruction') | review.group_changed
assert review.loc[current, 'matter_passed_date'].notna().all()
assert (pd.to_datetime(review.loc[current, 'matter_passed_date']) < pd.to_datetime(review.loc[current, 'construction_year'] + '-06-15')).all()
review.loc[current, 'evidence'] = 'Passed ordinance predates construction; current map reports its destination zoning'
review['verdict'] = review.group_changed.map({True: 'Correct zoning group', False: 'Retain zoning group'})
review.sort_values('project_id').to_csv('../output/added_zoning_eighteen_review.csv', index=False)
