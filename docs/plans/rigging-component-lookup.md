# Rigging component lookup

## Plan-lite

The bounded change reports an unknown sling or shackle part number with a
`ValueError` naming the component kind and requested identifier before indexing
the filtered table. Successful row selection and numeric conversion retain
their existing behavior.

Validation uses synthetic tables offline in
`tests/specialized/rigging/test_component_lookup.py`:

- Missing text and numeric identifiers, including empty tables.
- Successful selection for both supported sling subcategories and all three
  supported shackle grades.
- Preserved capacities, plain numeric types and strict JSON serialization.

Workbook access and solver execution are excluded. The synthetic cells provide
test expectations only; they do not qualify component ratings. Workbook
integration, unsupported subcategories and broader rigging behavior remain
outside this change.
