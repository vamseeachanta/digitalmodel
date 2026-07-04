# Class-Rule Scantling Checks — Validation Record (2026-06-28)

Backing for `src/digitalmodel/naval_architecture/scantlings.py` (issue #1084,
EPIC #1080 workstream A). Prescriptive class-rule minimums — the counterpart to
the buckling utilisation (`structural_analysis.panel_buckling`): "is it thick
enough / strong enough", not "does it buckle".

Each `check_*` returns the same dict shape as
`naval_architecture/compliance.py` (`name / code / pass / value / required /
unit`), so the scantling checks aggregate with the other compliance checks.

## Checks & formulas

| Check | Formula | Units |
|---|---|---|
| Min plate thickness (by location) | `t_min = (a + b·L)·√(235/ReH)` | mm |
| Plate thickness (lateral pressure) | `t = 15.8·k_a·s·√p/√σ + t_k` | mm |
| Min stiffener section modulus | `Z = 1000·p·s·l²/(m·σ)` | cm³ |

with `k_a = (1.1 − 0.25·s/l)² ≤ 1.0` (aspect factor), `s` = spacing (m), `l` =
span (m), `p` = lateral pressure (kN/m²), `σ` = permissible bending stress
(N/mm²), `m` = 12 (clamped) / 8 (simply supported), `t_k` = corrosion addition.

## Validation (hand-computed against the rule formulas)

- **Aspect factor:** square plate (s=l) → `k_a = (1.1−0.25)² = 0.7225`;
  s=2/l=2.5 → `(1.1−0.2)² = 0.81`; s=0.8/l=2.5 → 1.0404 → **capped at 1.0**.
- **Plate (pressure):** s=0.8, l=2.5 (k_a=1.0), p=100, σ=160 →
  `t = 15.8·0.8·√100/√160 = 9.99 mm`.
- **Section modulus:** same inputs, m=12 →
  `Z = 1000·100·0.8·2.5²/(12·160) = 260.4 cm³`; m=8 (simply supported) is 1.5×
  larger.
- **Min thickness:** L=150, bottom (5.0 + 0.04·L) → 11.0 mm; AH36 (355 MPa) →
  `11.0·√(235/355) = 8.95 mm`.

## Simplification / scope disclosure

The **lateral-pressure plate thickness and stiffener section modulus** are the
rigorous, formula-validated core (the DNV/IACS forms). The **location minimum
thickness** uses representative DNV-RU-SHIP general coefficients (keel/bottom/
side/deck/tank-top/bulkhead); the exact rule table (edition- and ship-type-
specific, e.g. IACS CSR bulk carriers vs tankers) should be consulted for a class
submission, and the coefficients are overridable. The permissible stress `σ` is a
caller input (it depends on location and load case), not hard-coded.

## Builds on

Generalises the single ABS hull-girder section-modulus check in
`naval_architecture/compliance.py` (`check_section_modulus`) to local
plate/stiffener scantlings, in the same dict-check style. Complements the
buckling utilisation from `structural_analysis.panel_buckling` and the
hull-girder longitudinal-strength checks (#1082).

## Tests

`tests/naval_architecture/test_scantlings.py` — 9 tests: aspect factor (incl.
cap), plate thickness for pressure (+ corrosion addition), section modulus
(clamped vs simply supported), location minimum thickness (+ material scaling),
material factor, and the compliance-style pass/fail dicts. black + flake8 clean;
runs under the `naval-architecture` CI domain.
