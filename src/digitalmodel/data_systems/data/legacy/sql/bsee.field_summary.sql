select
  drilling_completion_summary
  , wellhead_distances
  , production
FROM bsee.output_field_summary WHERE "BOEM_FIELDS" = '{0}';
