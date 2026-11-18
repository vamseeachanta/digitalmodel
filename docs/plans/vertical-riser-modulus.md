# Vertical riser modulus conversion

## Plan-lite

Scope: convert only elastic modulus `E` from psi to kPa through the shared
unit registry in `update_property_dictionary_to_si_units`. Preserve the
in-place dictionary API and all neighboring property conversions.

Offline approach: exercise the actual source class with the real unit helper,
without spreadsheet or model-builder imports. Use synthetic inputs and expected
values calculated independently from force divided by area. Check zero and
nonzero moduli, linear scaling, returned units, input identity and neighboring
fields, then run the shared unit suite.

```bash
PYTHONPATH=src uv run --offline --no-project --with 'pytest>=7.4.3,<9.0.0' --with pint python -m pytest tests/subsea/vertical_riser/test_component_units.py tests/test_units.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/modulus-tests"
```

Acceptance: all seven component cases and eleven shared unit cases pass;
repository structure, engineering register and changed-file hygiene checks pass.

Exclusions: full module integration, report generation, model builder and index
completion, spreadsheet execution and licensed solver behavior. The repeated
`SMUS` conversion is characterized by the existing regression and requires a
separate correction. This change introduces no new engineering coefficients.
