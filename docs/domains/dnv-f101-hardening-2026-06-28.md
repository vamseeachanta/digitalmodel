<!-- ABOUTME: DNV-RP-F101 full-fidelity hardening (issue #1094 findings #1,#3,#4,#6,#7) — -->
<!-- ABOUTME: interacting area-averaged combined depth, Part-A PSF Table 3-2/3-7/3-8, validity layer, with old->new values. -->

# DNV-RP-F101 hardening — interacting area-averaging + Part-A PSF tables + validity layer

**Issue:** [#1094](https://github.com/vamseeachanta/digitalmodel/issues/1094) (adversarial-review findings on the FFS strength methods)
**Module:** `src/digitalmodel/asset_integrity/dnv_rp_f101.py`
**Tests:** `tests/asset_integrity/test_dnv_rp_f101.py`
**Standard:** DNV-RP-F101 *Corroded pipelines* (January 2015)
**Date:** 2026-06-28

This change is **behaviour-changing**. It corrects the interacting-defect combined
depth (finding #1), replaces the scalar PSF defaults with the tabulated
DNV-RP-F101 Part-A partial-safety factors (finding #6), and adds an
applicability/validity layer plus a usage-factor clarification (findings #3/#4/#7).
Every old → new value is documented below.

> **Verification note.** An independent review against the DNV-RP-F101 (Jan-2015)
> text corrected two values in the first implementation pass: (1) the combined
> depth divides by the **total** combined length (gaps included), not by the sum
> of member defect lengths; and (2) γ_m follows the four-class Table 3-2, where
> **0.77 = Very High / absolute** (the prior code's "Normal/absolute" label was
> wrong). Both are fixed here and reflected in the goldens.

---

## (A) Interacting-defect combined depth — finding #1

### Before

For any contiguous run of interacting defects `i..j`, `dnv_f101_interacting`
formed a composite with `comp_depth = max(depth[i..j])` (MAX depth). The
max-depth grouping is conservative but is **not** the DNV-RP-F101 combined-defect
rule; it was incorrectly presented as "the DNV method".

### After (DNV-RP-F101 Sec. 3.8.2 combined-defect rule)

```
Step 7  L_comb = l_m + Σ_{i=n..m-1}(l_i + s_i)   = total span from the first
        start to the last end, INCLUDING the inter-defect spacings s_i.
Step 8  d_comb = Σ(d_i · l_i) / L_comb            = total metal-loss area
        divided by the TOTAL combined length (gaps included → the spacings
        dilute the average depth).
```

The composite is assessed as a single defect with `(L_comb, d_comb)`. The same
total span feeds both the length-correction factor `Q` and the depth average, so
length and depth are consistent. Every individual defect and every contiguous
interacting sub-run are still evaluated; the governing (lowest-capacity) case is
returned.

**Key consequence:** equal-depth members reduce to the maximum depth **only when
they are touching** (zero spacing). A non-zero gap dilutes the combined depth
below the members' depth (because the zero-depth gap is part of the averaged
length). The earlier intuition that "equal-depth always collapses to max" is only
true for `s = 0`.

### old → new values (golden test cases)

Reference pipe: D = 30 in, t = 0.375 in, X52 (SMTS = 66 700 psi), interaction
limit `2·√(D·t) ≈ 6.71 in`.

| Colony (pos, len, depth in) | Governing | OLD (max depth) | NEW (DNV area-avg) | Δ |
|---|---|---|---|---|
| `[(0,4,0.20),(5,4,0.10)]` | composite, d_comb = 1.2/9 = **0.1333**, L = 9 | **1120.40 psi** | **1356.49 psi** | +236 psi (less conservative) |
| `[(0,4,0.30),(5,4,0.20),(10,4,0.25)]` (3-defect) | composite 0..2, d_comb = 3.0/14 = **0.2143**, L = 14 | (max 0.30) | **934.83 psi** | governs vs deeper-but-shorter singles |
| `[(0,4,0.15),(5,4,0.15)]` equal depth, **gapped** (s = 1) | composite, d_comb = 1.2/9 = **0.1333**, L = 9 | (max 0.15, L9 → 1303.10) | **1356.49 psi** | gap dilutes, ≠ max |
| `[(0,4,0.15),(4,4,0.15)]` equal depth, **touching** (s = 0) | composite, d_comb = **0.15**, L = 8 | 1334.19 | **1334.19 psi** | reduces to max (s = 0) |

The corrected method is generally **less conservative** than the old max-depth
grouping whenever member depths differ or gaps are present, because area-averaging
spreads the metal loss over the full combined length — which is the intended DNV
behaviour. A dedicated regression test
(`test_interacting_total_length_denominator_not_member_length`) pins the
denominator to the total span (0.1333, not the member-length 0.15).

---

## (B) DNV-RP-F101 Part-A partial-safety-factor tables — finding #6

`dnv_f101_psf` previously hard-coded scalar defaults
`gamma_m = 0.77, gamma_d = 1.0, epsilon_d = 1.0, StD = 0.08`. These are now
**looked up** from the DNV-RP-F101 Part-A tables, selected by **safety class**
(low / medium / high / very high) and **sizing-accuracy band** `StD[d/t]`. Three
new public helpers expose the tables: `gamma_m_factor`, `gamma_d_factor`,
`epsilon_d_fractile`. Any factor may still be passed explicitly to override.

### γ_m — model factor (Table 3-2, by safety class and sizing method)

| Safety class | Relative sizing (e.g. MFL) | Absolute sizing (e.g. UT) |
|---|---|---|
| Low | 0.90 | 0.94 |
| Medium | 0.85 | 0.88 |
| High | 0.80 | 0.82 |
| Very High | 0.76 | **0.77** |

The legacy scalar `0.77` is the **Very High / absolute** cell — *not* Normal as
the prior code asserted. The default is therefore `safety_class="very high"`,
`measurement_method="absolute"` (the most conservative class), preserving the
legacy γ_m = 0.77. `"normal"` is accepted as an alias for `"medium"`. **Callers
should set the pipeline's actual safety class rather than relying on the
conservative default.**

### ε_d — fractile factor (Table 3-7/3-8), `(d/t)* = (d/t) + ε_d·StD`

With `a = StD[d/t]` (all safety classes):

```
a ≤ 0.04        →  ε_d = 0
0.04 < a ≤ 0.16 →  ε_d = -1.33 + 37.5 a - 104.2 a²     (clamped at 0.16 above)
```

Self-consistency: `ε_d ≈ 0, 1.003, 2.002` at `a = 0.04, 0.08, 0.16`, so the
legacy scalar `ε_d = 1.0` is the DNV fractile at StD = 0.08 (the depth-tolerance
term is **unchanged** at the default accuracy band).

### γ_d — depth partial safety factor (Table 3-8, by class and StD band)

With `a = StD[d/t]`:

```
Low       : a < 0.04         → 1.0 + 4.0 a
            0.04 ≤ a < 0.08   → 1.0 + 5.5 a - 37.5 a²
            0.08 ≤ a ≤ 0.16   → 1.2
Medium    : a ≤ 0.16          → 1.0 + 4.6 a - 13.9 a²
High      : a ≤ 0.16          → 1.0 + 4.3 a - 4.1 a²
Very High : a < 0.03          → 1.0 + 4.0 a
            0.03 ≤ a ≤ 0.16   → 0.92 + 7.1 a - 8.3 a²
(a > 0.16 clamped at the a = 0.16 point for all classes)
```

At StD = 0.08: γ_d = 1.200 (Low) / 1.279 (Medium) / 1.318 (High) / **1.435
(Very High)** — matching the Table 3-7 tabulated points (Very High also gives
1.844 at StD = 0.16, matching the table). Larger StD and higher safety class
both raise γ_d.

### old → new PSF default value

At the default (Very High / absolute / StD = 0.08), only γ_d changes vs the old
scalar default (`1.0 → 1.43488`); γ_m stays 0.77 and ε_d ≈ 1.003. For the
reference single defect (D = 30, t = 0.375, d = 0.15, L = 8, X52):

| Quantity | OLD scalar default | NEW tabulated default |
|---|---|---|
| safety class (label) | "Normal" (mislabelled) | **Very High** (true Table-3-2 home of 0.77) |
| γ_m | 0.77 | 0.77 (unchanged) |
| ε_d | 1.0 | 1.00312 (≈ unchanged) |
| γ_d | 1.0 | **1.43488** |
| **allowable P_corr** | **950.59 psi** | **690.45 psi** (more conservative, consistent) |

The previous scalar defaults were internally inconsistent (`γ_d = 1.0` ↔ StD = 0,
but `ε_d = 1.0` ↔ StD = 0.08); the tabulated lookup makes the factors mutually
consistent at the stated accuracy band and the correct safety class.

### Sources

γ_m / γ_d / ε_d values and equations are the DNV-RP-F101 (Jan-2015) Part-A
partial-safety-factor tables: **Table 3-2** (γ_m by safety class Low/Medium/High/
Very High × relative/absolute), **Table 3-7** (tabulated γ_d / ε_d points) and
**Table 3-8** (γ_d / ε_d polynomials in `a = StD[d/t]`). The polynomials were
cross-checked against the tabulated points (e.g. γ_d Very-High = 1.43 @ 0.08,
1.84 @ 0.16; ε_d = 0 / 1 / 2 @ 0.04 / 0.08 / 0.16), and the combined-defect rule
against Sec. 3.8.2 Steps 7–8 (combined length and area-averaged depth over the
total combined length).

---

## (C) Validity / applicability layer — findings #3, #4, #7

* **d/t > 0.85** (DNV-RP-F101 capacity-equation validity limit). `dnv_f101_psf`
  now mirrors the single-defect `within_applicability` flag: results with
  `d/t > 0.85` carry `details["within_applicability"] = False` and an
  `applicability_note`. The interacting assessment surfaces the governing case's
  `within_applicability` flag in its details.
* **StD[d/t] > 0.16** (outside the Part-A PSF calibration range). `dnv_f101_psf`
  flags this and notes that γ_d / ε_d are clamped at the 0.16 point.
* **Usage-factor 0.72 clarification (findings #4/#7).** The allowable-stress
  `usage_factor` default `0.72` is the **ASME B31.8 location-class-1 design
  factor**, *not* a DNV safety-class usage factor. This is now stated in the
  module docstring and at the constant's point of use, directing callers to the
  PSF format (`dnv_f101_psf` with an explicit `safety_class`) for DNV-RP-F101
  code-compliant safety. ASME B31.8 location classes 2/3/4 use 0.60/0.50/0.40.

---

## Summary of behaviour changes (old → new)

| Area | Old | New |
|---|---|---|
| Interacting combined depth | `max(depth)` over the run | DNV Sec. 3.8.2 area-avg `Σ(d·l)/L_comb`, L_comb = total span incl. gaps |
| Interacting `[(0,4,0.20),(5,4,0.10)]` | 1120.40 psi | 1356.49 psi |
| Interacting `[(0,4,0.30),(5,4,0.20),(10,4,0.25)]` | (max-depth grouping) | 934.83 psi (composite 0..2) |
| Equal-depth gapped colony | (collapsed to max) | dilutes via gap (0.1333, not 0.15) |
| Equal-depth touching colony | max | max (s = 0) — unchanged |
| γ_m table | 3-class 0.79/0.74/0.70 (wrong) | **Table 3-2** 4-class 0.90/0.85/0.80/0.76 (rel), 0.94/0.88/0.82/0.77 (abs) |
| γ_m = 0.77 home | "Normal/absolute" (wrong) | **Very High / absolute** |
| PSF `gamma_m/gamma_d/epsilon_d` | scalars 0.77 / 1.0 / 1.0 | table lookup by class + StD |
| PSF default class | "normal" | "very high" (conservative; preserves γ_m = 0.77) |
| PSF default allowable (ref defect) | 950.59 psi | 690.45 psi |
| PSF `safety_class` / `measurement_method` params | — | added |
| PSF validity flags | none | `within_applicability` for d/t > 0.85 and StD > 0.16 |
| `usage_factor` 0.72 semantics | implied DNV | documented as ASME B31.8 class-1 design factor |

All 30 tests in `tests/asset_integrity/test_dnv_rp_f101.py` pass (16 new); the
full `tests/asset_integrity/` suite is green (483 passed, 8 skipped).
