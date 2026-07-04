# API 5CT Casing + API 11B Sucker Rod — Validation Record (2026-06-28)

Backing for `src/digitalmodel/well/tubulars/casing.py` (#1090) and
`sucker_rod.py` (#1091), EPIC #1080 workstream D. Both are product catalogs on
the tubular-design package, mirroring the line-pipe pattern.

## API 5CT casing (#1090)

`Casing` = OD + nominal weight (ppf, → wall) + API 5CT grade, with API 5C3
ratings computed (not table-looked-up). It computes the very
`burst_rating_psi` / `collapse_rating_psi` that the existing
`TubularGeometry` / VME / API-ellipse envelopes require as inputs — see
`Casing.to_tubular_geometry()`.

Grades: H40, J55, K55, N80, L80, C90, C95, T95, P110, Q125 (min **and** max
yield; min tensile). Sizes: 4½″–20″ × standard weights.

**Formulas (API 5C3):**
- Internal yield (burst): `0.875 · 2·Yp·t / D`.
- Pipe-body yield (axial): `Yp · A`.
- Collapse: four regimes (yield / plastic / transition / elastic) selected on
  D/t, with the grade-dependent A/B/C/F/G constants (polynomials in Yp).

**Validation vs published API ratings:**

| Casing | D/t | Collapse | Burst | Body yield | Regime |
|---|---|---|---|---|---|
| 7″ 26# P110 | 19.3 | 6230 (calc 6232) | 9960 | 830,000 | plastic |
| 9⅝″ 47# N80 | 20.4 | 4760 (calc 4754) | 6870 | 1,086,000 | plastic |
| 13⅜″ 68# K55 | 27.9 | 1950 (calc 1949) | 3450 | 1,070,000 | transition |

All within 0.3 %. Yield- and elastic-regime selection are also covered (5″ 23.2#
P110 → yield; 20″ 94# K55 → elastic), plus collapse monotonicity in D/t. The
4-regime collapse is the critical validation: a regime-boundary bug only shows
up against a published rating, not a hand-derived number.

## API 11B sucker rod (#1091)

`SuckerRod` = nominal diameter (eighths) + API 11B grade + service factor, with
the **modified-Goodman** allowable-stress fatigue check.

Grades: C (90 ksi), K (85 ksi), D (115 ksi) min tensile. Sizes: ½″…1¼″.

**Modified Goodman (API 11BR):** `SA = (T/4 + 0.5625·Smin)·SF`. The allowable
peak stress rises with the minimum stress (the Goodman line), so the check takes
both peak and minimum polished-rod loads; the service factor `SF` carries
sour-service derating. `goodman_check(peak, min)` returns min/max/allowable
stresses, loading (= Smax/SA) and a pass flag. Tapered strings checked
per-section via `rod_string_goodman`. `to_rod_section()` ties into the dynacard
rod-buckling workflow with the correct grade tensile.

Validated: `SA(D, Smin=10 ksi)=34,375 psi`, `SA(C, 0)=22,500`, SF derating,
higher-Smin-lifts-allowable, and the dynacard `RodSection` tie-in.

## Tests

`tests/well/tubulars/test_casing.py` (21) + `test_sucker_rod.py` (12) — 33 tests;
full `well/tubulars` suite 68 passed (incl. the 47 existing envelope tests).
black + flake8 clean. Tests run under the `marine-ops-other` CI domain
(`tests/well/` is already mapped).

## Relates to

Builds on `well/tubulars/design_envelope.py` (TubularGeometry / API-ellipse) and
`marine_ops/artificial_lift/dynacard/rod_buckling.py` (RodSection). Grades live
in the product modules (casing min/max yield and rod tensile-only don't fit the
line-pipe `MaterialGrade` shape). Code-reference provenance is a self-contained
string here; #1093 can fold it into the `codes` register once #1092 merges.
