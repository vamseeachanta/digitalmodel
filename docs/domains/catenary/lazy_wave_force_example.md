# Lazy-wave hang-off force analysis

## Summary

The modern `LazyWaveSolver` reports the vertical component as `Fv = w*S1`
and total tension as `T = hypot(Fh, Fv)`. Both the kernel and the four historical
reference cases already contain this correction. The example below supplies an
independent arithmetic reference and distinguishes the corrected dictionary
conversion from the unmodified legacy solver.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-20 | Added independent force triangles, a runnable example and a consumer inventory to explain the corrected force definition. |

## Method and source

The equation source is [NREL/TP-500-41958, *Dynamics Modeling and Loads Analysis
of an Offshore Floating Wind Turbine*](https://www.osti.gov/servlets/purl/921803),
November 2007, section 2.5, “Mooring System Modeling”, printed page 41.
The public institutional copy was inspected for equations (2-41a,b),
(2-43a,b) and (2-44). These are equations for a uniform suspended cable;
their use here is a local derivation rather than evidence validating the entire
lazy-wave construction.

Let `w > 0` be submerged weight per unit length, `a = H/w` the vertex radius,
`S1` the arc length from the sag vertex to hang-off, and `d` its vertical rise.
The vertex has zero vertical tension component. Applying (2-41a,b) gives
`H = w*a` and `V = w*S1`. Taking axial stiffness to infinity in (2-43b) gives
`d = sqrt(a*a + S1*S1) - a`. Equation (2-44) then gives
`T = sqrt(H*H + V*V) = w*(a+d)`. The tangent is aligned with the tension,
so the angle from vertical is `q = atan2(H, V)`.

For the whole suspended riser, apply (2-41b) successively to each segment:
`V_top = sum(w_i*S_i)` with zero vertical force at touchdown. Net buoyed
weights retain their negative sign. Horizontal tension is constant in this
model. Summing absolute weights would break vertical equilibrium.

## Worked example

All inputs are constructed arithmetic examples, not field data or recommended
design values. No material coefficient, density or gravity value is assumed.
Choose the exact length triangle `(a, S1, a+d) = (300, 400, 500) m`.
Then `d = 500 - 300 = 200 m`, and `q = atan(3/4)`.
The input uses that unrounded angle; displayed angles are rounded to one decimal.

| Input | Value | Basis |
| --- | --- | --- |
| Hang-off to seabed | 300.0 m | Synthetic choice, giving `d = 300 - 100` |
| Sag elevation above seabed | 100.0 m | Synthetic choice |
| Hog elevation above seabed | 200.0 m | Synthetic choice above the sag |
| Hang-off depth below mean sea level | 0.0 m | Reporting datum |
| Hang-off angle from vertical | 36.9 degrees | `atan(3/4)`, supplied without rounding |
| Bare submerged weight `w` | 10.0 N/m | Synthetic choice |
| Net buoyed weight | -10.0 N/m | Synthetic choice equal and opposite to `w` |

The hand calculation is:

```text
H = 10 * 300 = 3000.0 N
V = 10 * 400 = 4000.0 N
T = 10 * 500 = 5000.0 N
```

The two sag-to-hog arcs have equal lengths and opposite weights and cancel
in the signed-weight sum. The two hog-to-touchdown arcs do likewise.
Only the hang-off arc contributes net weight, giving `V_top = 4000.0 N`.
The former defective expression would produce `7000.0 N`, which fails the
hand-calculated vertical force and the input angle.

Run from the repository root with the package dependencies available:

```bash
PYTHONPATH=src python examples/domains/mooring/lazy_wave_example.py
```

The example prints the inputs, governing equations, force components, total
tension, signed suspended weight, recovered angle, closure and converted
summary. Its force answer is the hand result above. The conversion preserves
`Summary['Fv'] = 4000.0 N` and exports `Summary['T'] = 5000.0 N`.

## Test basis

`tests/test_lazy_wave_worked_example.py` tests the constructed 3–4–5 triangle
and a second triangle `(a, S1, a+d) = (500, 1200, 1300) m`, with synthetic
weights of 10.0 and 25.0 N/m. The second triangle gives `d = 800 m` and,
at 10.0 N/m, forces `(H, V, T) = (5000, 12000, 13000) N`. At 25.0 N/m,
each force scales by `25/10`. Expected values are derived in the test
docstrings, independently of the code under test and the historical fixture.
The global-weight cases use net buoyed weight `-2*w`, exercising unequal
buoyed and bare lengths.

The relative tolerance `1e-12` follows the numerical regression precision in
issue #2044, Plan-lite tests 2 and 4. It tests floating-point arithmetic for
these examples; it is not an engineering acceptance limit. The existing
four-case fixture tests separately retain geometry, declination, corrected
vertical force and total-tension identities.

## Consumer inventory

A repository text search of `src/`, `examples/`, `scripts/`, `tests/` and the
catenary documentation found the following modern force consumers. Historical
use before the correction is not inferred from current call sites.

| Consumer | Current force use and implication |
| --- | --- |
| `riser_configuration_screen/static.py`, `_lazy_shape` and `_lazy_row` | Uses `vertical_force` to integrate signed segment forces and recover top angle; uses `total_tension` for top tension. These require the corrected component. |
| `examples/domains/mooring/lazy_wave_example.py` | Prints vertical and total forces and the converted summary; the hand result is tested. |
| `scripts/orcaflex/check_generic_scr_slwr.py`, `slwr_reference` | Uses `total_tension` for the modern reference and geometry for the segmented comparison; no direct vertical-force read. Inspected as text only. |
| `LazyWaveSolver.to_legacy_dict` | Exports the corrected summary, including `Fv` and `T`; does not invoke the legacy solver. |
| Catenary and marine engineering package exports | Re-export the solver and results; perform no force calculation. |
| `tests/test_lazy_wave_catenary.py`, `test_lazy_wave_kernel_defects.py`, `test_lazy_wave_force_equilibrium.py` | Assert corrected forces and retained geometry. |
| `tests/test_lazy_wave_legacy_comparison.py` | Compares geometry and horizontal force; explicitly excludes legacy vertical-force parity and links the regression proof. |
| `tests/subsea/catenary_riser/test_static_reconciliation.py` | Independently integrates modern equilibrium; two strict expected failures record the remaining legacy force defect. |
| `docs/domains/catenary/lazy_wave_migration_summary.md` | The stale force equation is corrected and total tension documented. Historical test counts remain dated migration evidence. |

The `vertical_force` fields in `drilling_riser/` describe a separate tensioner
representation and do not consume `LazyWaveResults`. The quarantined
`subsea/catenary_riser/lazy_wave.py` directs callers to the modern solver and
does not produce modern force results.

## Limits and way forward

The legacy `lazyWaveCatenaryEquation` still adds horizontal force to its
vertical component. The legacy riser route calls that implementation directly;
it does not use the corrected modern dictionary conversion. No legacy source
or caller is changed here. Its separate force defect requires explicit
migration or repair before those results can be treated as equilibrium forces.

This example covers static, inextensible, uniform-weight segments without
bending stiffness, external distributed loads or dynamic response. It is not a
design acceptance assessment. No licensed solver is imported or run. The
historical fixture remains a geometry regression reference and is supplemented
by the independent force calculations above.
