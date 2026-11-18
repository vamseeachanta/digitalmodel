# MoorPy single-line adapter

`solve_single_line` computes the loads from one uniform elastic line at two
prescribed endpoints. It imports MoorPy only when a valid solve is requested.
This implements the single-line quasi-static portion of issue #174.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| Initial | 2026-10-27 | Add a prescribed-endpoint adapter and independent force balances. |

## Inputs and force convention

Import from `digitalmodel.subsea.mooring_analysis.moorpy_adapter`.
The arguments are `endpoint_a`, `endpoint_b`, `unstretched_length`, `ea`,
and `submerged_weight_per_length`.

- Each endpoint is a sequence of three finite Cartesian coordinates in metres.
  The endpoints must be distinct. Global x and y are horizontal; z is upward.
- Unstretched length is positive, in metres. EA is positive, in newtons.
- Submerged weight is nonnegative, in newtons per metre of unstretched line,
  acting downward. Buoyant lines are outside this initial interface.
- `SingleLineResult.force_a` and `.force_b` are global Cartesian force tuples
  in newtons, applied **by the line to the attachments**. Forces applied by
  the attachments to the line have the opposite sign.

For a weightless taut line, A's force points toward B and B's force toward A.
For a weighted suspended line, the sum of the endpoint loads is the downward
total submerged weight: `(0, 0, -submerged_weight_per_length * unstretched_length)`.

## Optional library and solver choice

The tested library is [MoorPy 1.3.0](https://pypi.org/project/MoorPy/1.3.0/),
already pinned in the repository's `fowt` extra. It can also be installed
separately with `pip install moorpy==1.3.0`. Missing imports raise an actionable
`ImportError`; other mooring-analysis imports do not require MoorPy.

The implementation uses `moorpy.Catenary.catenary` for positively weighted
lines, passing infinite bottom clearance to exclude seabed contact. It retains
the library's default displacement tolerance and near-vertical approximation.
A reported failure, nonfinite load, or approximate taut fallback raises
`RuntimeError` instead of returning that result as converged equilibrium.

The published catenary implementation divides by weight and cannot handle
exactly zero weight. That case uses `moorpy.nonlinear.nonlinear` with an EA-based
linear tension-strain curve. The upper curve node is beyond the requested
strain, avoiding interpolation clipping. Negative vertical rise is reflected
before solving and its force signs restored afterward. Slack weightless lines
carry zero tension; their shape is not uniquely determined or returned.

Python 3.11 was used for focused library tests. With Python 3.12 and the
repository's warnings-as-errors policy, MoorPy's published `Catenary.py` raises
`SyntaxError: invalid escape sequence` during import. No warning filter or
repository gate is changed by this adapter.

## Saved example and independent calculation

Run the [saved example](../../../examples/subsea/mooring_analysis/moorpy_single_line.py)
with the optional library installed:

```bash
PYTHONPATH=src uv run --python 3.11 --no-project --with moorpy==1.3.0 \
  python examples/subsea/mooring_analysis/moorpy_single_line.py
```

All example inputs are synthetic: A at the origin, B at `(6.6, 8.8, 0.0)` m,
unstretched length `10.0` m, EA `1000.0` N, and zero submerged weight.
The span is `sqrt(6.6² + 8.8²) = 11.0` m. From strain = extension / original
length and axial force = EA × strain:

```text
T = 1000.0 × (11.0 / 10.0 - 1) = 100.0 N
direction A to B = (0.6, 0.8, 0.0)
Force at A [N]: 60.0, 80.0, 0.0
Force at B [N]: -60.0, -80.0, -0.0
```

The example checks these hand-derived loads before printing. The adapter tests
also cover rotated, vertical, descending, slack and extended weightless lines.
Weighted tests derive endpoints by integrating the elastic-catenary equations
with prescribed H and V, then recover those forces with real MoorPy. The
derivation and synthetic inputs are recorded in the test file; expected forces
are not computed by the adapter or by the existing touchdown-catenary API.

## Scope and remaining work

The existing `CatenaryAnalyzer.solve_catenary` models an inextensible touchdown
line with prescribed horizontal tension and uses kN-based forces. Its API and
tests are unchanged; no implicit conversion between its properties and this
SI interface is provided.

This interface returns endpoint forces only. No MoorDyn dynamics, multibody
equilibrium, seabed or surface contact, current, bending, empirical line tables,
line profile, stiffness result, or design approval is included. Remaining issue
scope includes dynamic coupling, system equilibrium, and their independent
validation. Material-specific validity of the linear EA law requires separate
engineering assessment.
