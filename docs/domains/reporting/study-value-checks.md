# Study automation engineering value checks

Synthetic engineering values are checked through specification parsing, numeric
contracts, offline readback and HTML rendering. These checks establish arithmetic
and value preservation at those boundaries. They do not qualify a native solver
or establish design suitability.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-21 | Added sourced hand calculations and boundary checks for study automation. |

## Scope

| Slice | Module | Value assertion |
| --- | --- | --- |
| 1 | `common` | Parse a synthetic dimension and explicitly convert international feet to metres. |
| 1 | `contract_validation` | Admit the declared total only when it matches the sum of positive segment lengths. |
| 1 | `specs` | Preserve a finite scalar result and its declared units across JSON validation. |
| 2 | `solvers/smoke` | Preserve a synthetic elastic-bar extension through selected YAML readback; reject a doubled extension. |
| 2 | `solvers/blender_automation` | Export and reimport cubes, then measure signed volume; requires Blender. |
| 3 | `web` | Render the admitted pressure and units as separately identifiable, escaped HTML text. |
| 3 | `citations` | Existing pressure-preservation tests check live resolution against isolated synthetic metadata. |

`RiserLengthContract` is a small numeric contract separate from the existing
parent-contract composition schema. It checks the declared, unstretched segment
length sum only. The caller must supply an absolute comparison tolerance in
metres. No engineering acceptance limit is built in. All fields are required;
nonpositive lengths, nonfinite numbers, booleans and unknown fields are rejected.

`EngineeringResult` requires a quantity, finite numeric value, units and source.
It is immutable after validation. Its source string is descriptive metadata;
it does not establish citation resolution or validate the calculation. Unit
strings are not interpreted. `length_in_metres` accepts only explicit `m` and
international `ft`; survey feet and other spellings require caller resolution.

## Worked example

Run from the repository root with the normal dependencies available:

```sh
PYTHONPATH=src uv run python examples/reporting/study_value_checks.py
```

The example prints results and an HTML fragment without creating files. Every
physical input is synthetic, not a material property or design default.

| Quantity | Inputs | Method and independent answer |
| --- | --- | --- |
| Dimension | 10 international feet | NIST conversion: 10 × 0.3048 = **3.048 m**. |
| Segment total | 12.5 m, 7.5 m, 10 m | Length additivity: 12.5 + 7.5 + 10 = **30 m**. |
| Gauge pressure | Density 1000 kg/m³, gravity 10 m/s², depth 2.5 m | Constant-density fluid: 1000 × 10 × 2.5 = **25000 Pa**. |
| Bar extension | Load 2000 N, length 2 m, modulus 200000000 Pa, area 0.01 m² | Linear axial elasticity: (2000 × 2) / (200000000 × 0.01) = 4000 / 2000000 = **0.002 m**. |
| Exported cube volume | Side 1 m; side 2 m | Geometry: 1 × 1 × 1 = **1 m³**; 2 × 2 × 2 = **8 m³**. |

The geometry cases run separately in `test_box_volume.py`. Signed volume is
measured from the reimported PLY mesh, rather than from the generated input mesh.
If Blender is unavailable, those cases are skipped and native export remains
**NOT_EVALUATED**. The example itself always reports native box export and native
bar solution as **NOT_EVALUATED**, because it executes neither.

The bar fixture uses synthetic YAML with an `extension_m` field. It checks the
existing generic selected-value reader and finite-number adapter; it does not
represent a vendor file schema, run a solver, or establish solver accuracy. A
native elastic-bar result remains a later qualification step.

The pressure check uses gauge pressure, with no atmospheric-pressure assumption.
The renderer preserves Pa and does not convert to kPa or round the number. The
bar equation assumes a uniform cross-section and linear axial elasticity; no
yield, buckling or allowable-stress check is evaluated. Exact comparisons in
these examples are software checks of hand-selected fixtures, not physical
acceptance tolerances. Floating-point conversion may round other input values.

## Sources

- NIST Special Publication 811, 2008 edition, Appendix B.8, alphabetical entry
  “foot”: international foot to metre conversion.
  [Public conversion table](https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8).
- OpenStax, *University Physics Volume 1*, 2016, section 14.1, equation 14.4:
  constant-density pressure at depth, with gauge pressure obtained by subtracting
  surface pressure.
  [Public textbook section](https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure).
- OpenStax, *University Physics Volume 1*, 2016, section 12.3, equation 12.36:
  Young's modulus relation, rearranged as extension = load × length / (modulus × area).
  [Public textbook section](https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus).
- Blender Python API, `bpy.ops.mesh.primitive_cube_add(size)`, accessed 2026-10-21:
  [Public API reference](https://docs.blender.org/api/current/bpy.ops.mesh.html).

Length additivity and cube volume are elementary geometric identities; their
independent arithmetic is written out above and in the test docstrings. No
empirical coefficient is required. Public source pages were opened for the
conversion and physics equations. Citation-test metadata is synthetic and does
not retain or authenticate textbook content.

## Remaining qualification

Native Blender exports require execution where Blender is installed. Native
elastic-bar output and design acceptance checks remain NOT_EVALUATED. The legacy
integration tests' references to removed APIs require separate reconciliation;
these new numeric contracts do not restore those interfaces.
