# Pipe geometry finite-input validation

`PipeGeometry` rejects nonfinite constructor inputs before range validation.
This bounded part of issue #3306 preserves finite geometry arithmetic.

## Plan-lite

- Scope: constructor validation in
  `src/digitalmodel/structural/analysis/wall_thickness.py` only.
- Check `outer_diameter`, `wall_thickness`, `corrosion_allowance` and
  `fabrication_tolerance` with `math.isfinite` before any range comparison;
  raise `ValueError` identifying the nonfinite field.
- Regression coverage: NaN and both infinities for each field, validation
  precedence over unrelated range errors, and independent synthetic arithmetic
  for effective thicknesses, inner diameter and diameter-to-thickness ratio.
  Include zero allowance, zero tolerance and both zero as finite controls.
- Acceptance checks: run the 20 geometry regressions and the 58 existing
  wall-thickness tests; require no failures or skips. Check repository structure,
  path hygiene, Python syntax, diff whitespace and publication hygiene. Compare
  the production AST with the merge base to confirm that the finite-input
  block is the only production change. Inspect hosted failures for attribution.

## Limits

Material validation, engineering factors, design strategies, reporting and the
remaining issue scope require separate work. No licensed solver is required.
