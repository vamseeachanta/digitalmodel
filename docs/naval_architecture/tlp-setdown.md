# Idealized TLP set-down geometry

The `tlp_setdown` helper returns vertical span and downward set-down for an
initially vertical, straight, inextensible tendon attached to a rigid body.
This is a geometry slice of the floating-platform work in issue #1748;
the broader stability issue remains open.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-26 | Add an explicit geometric derivation and synthetic example to bound the helper's use. |

## Geometric derivation

This self-contained right-triangle derivation is the reference for the API and
its numerical constants. Let `L` be the initial vertical tendon length, `x` the
signed horizontal displacement, `h` the remaining vertical span, and `s` the
downward displacement of the attachment point. All lengths use metres.

Orthogonal horizontal and vertical components give:

```text
L² = x² + h²
h = sqrt(L² - x²)
s = L - h
  = (L² - h²) / (L + h)
  = x² / (L + h)
```

The supported geometric domain is finite real scalars with `L > 0` and
`abs(x) < L`. Booleans are rejected. At `abs(x) = L`, vertical span vanishes;
that horizontal-tendon limit and larger offsets are unsupported. This is a
geometric boundary, not a design or allowable-offset limit.

For numerical evaluation define `r = abs(x)/L`. The implementation uses:

```text
h = L sqrt(((L - abs(x))/L) (1 + r))
s = abs(x) (r / (1 + h/L))
```

Factoring avoids squared overflow and underflow. Rationalization avoids
subtracting nearly equal lengths at small offsets. Subtracting `L - abs(x)`
before dividing retains precision near the horizontal-tendon limit. Outputs
use floating-point arithmetic; a span may round to `L` while the separately
calculated set-down remains positive. Extremely small results may underflow.

The binomial expansion independently supplies the small-offset limit:

```text
s = x²/(2L) + x⁴/(8L³) + higher-order terms
s / (x²/(2L)) -> 1 as abs(x)/L -> 0
```

The coefficients follow from expanding `sqrt(1 - (x/L)²)`; there are no empirical
coefficients, licensed limits or platform database values.

## Synthetic example

Use the hand-calculated right triangle with `L = 5 m`, `x = 3 m`:
`h² = 25 - 9 = 16 m²`, so `h = 4 m` and `s = 1 m`.
These are synthetic geometric inputs, not platform measurements.

From the repository root, in an environment providing the package's import
dependencies (including PyYAML), run offline:

```sh
PYTHONPATH=src python examples/naval_architecture/tlp_setdown.py
```

Expected output:

```text
Tendon length: 5.0 m
Horizontal offset: 3.0 m
Vertical span: 4.0 m
Set-down (positive downward): 1.0 m
Tendon load: NOT_EVALUATED
Slack: NOT_EVALUATED
Pretension: NOT_EVALUATED
Restoring force: NOT_EVALUATED
Stability: NOT_EVALUATED
```

## Assumptions and exclusions

The anchor is fixed. The rigid body translates without roll or pitch. A straight
tendon is assumed taut and its length stays fixed: no axial stretch, bending,
sag, seabed contact or installation effects are modelled. Multiple tendons with
different lengths or initially inclined geometry are outside this helper.

Tendon load, slack, pretension, restoring force and stability are all
`NOT_EVALUATED`. Geometry alone cannot establish positive tendon tension,
equilibrium, load capacity, stability acceptance or an operating envelope.
Those require separate physical models and explicit inputs before design use.

## Independent checks

`tests/naval_architecture/test_tlp_setdown.py` uses hand-calculated right
triangles, high-precision decimal subtraction, sign symmetry and the
small-offset expansion above. Synthetic scale factors and numerical tolerances
are test choices, not engineering limits. Run:

```sh
python -m pytest tests/naval_architecture/test_tlp_setdown.py -q
```
