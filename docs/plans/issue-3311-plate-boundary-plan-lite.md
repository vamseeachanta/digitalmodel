# Plan-lite: plate buckling boundary contract

## Bounded scope

The change will make the boundary argument of
`PlateBucklingAnalyzer.elastic_buckling_stress` explicit: only
`simply_supported` will be supported, including when the argument is omitted.
The implementation will be limited to an entry guard and its docstring in
`src/digitalmodel/structural/structural_analysis/buckling.py`, with contract
tests in `tests/structural/structural_analysis/test_plate_boundary_contract.py`.

## Rejection behavior

Any other boundary string will raise `ValueError` before the calculation starts.
Tests will cover `clamped`, an unknown string and an empty string, checking that
the error identifies `simply_supported` as the only supported value. Callers
using unsupported strings will need to handle the exception or select the
supported boundary model.

## Supported calculation

The default and explicit `simply_supported` calls will retain the existing
calculation. No production formula, coefficient or design factor will change.
An independent square-plate sine-mode eigenvalue calculation will use synthetic
geometry and material inputs; expected results will not come from the analyzer.

## Test acceptance criteria

- The three unsupported-string cases will raise the documented exception.
- Default and explicit supported calls will match the independent eigenvalue
  calculation within a relative tolerance of `1e-12` and match each other exactly.
- The boundary-contract file and existing `test_buckling_capacity.py` will
  collect 32 tests, with 32 passed, zero failed and zero skipped.
- An AST comparison will confirm that excluding the guard and docstring leaves
  all other production statements unchanged from before this change.

## Deferred work

Additional boundary models and other buckling formulations will remain outside
this slice. The remaining module review, worked example, report, builder,
rebuild test and index pointer will remain open under issue #3311.
