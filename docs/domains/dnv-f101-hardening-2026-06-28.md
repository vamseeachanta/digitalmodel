<!-- ABOUTME: DNV-RP-F101 full-fidelity hardening (issue #1094 findings #1,#3,#4,#6,#7) — -->
<!-- ABOUTME: interacting area-averaged combined depth, Part-A PSF tables, validity layer, with old->new values. -->

# DNV-RP-F101 hardening — interacting area-averaging + Part-A PSF tables + validity layer

**Issue:** [#1094](https://github.com/vamseeachanta/digitalmodel/issues/1094) (adversarial-review findings on the FFS strength methods)
**Module:** `src/digitalmodel/asset_integrity/dnv_rp_f101.py`
**Tests:** `tests/asset_integrity/test_dnv_rp_f101.py`
**Date:** 2026-06-28

This change is **behaviour-changing**. It corrects the interacting-defect combined
depth (finding #1), replaces the scalar PSF defaults with the tabulated
DNV-RP-F101 Part-A partial-safety factors (finding #6), and adds an
applicability/validity layer plus a usage-factor clarification (findings #3/#4/#7).
Every old → new value is documented below.

---

## (A) Interacting-defect combined depth — finding #1

### Before

For any contiguous run of interacting defects `i..j`, `dnv_f101_interacting`
formed a composite with

```
L_comb     = end[j] - start[i]          # total span incl. interior gaps
comp_depth = max(depth[i..j])           # MAX depth   <-- mislabelled as DNV
```

The max-depth grouping is conservative but is **not** the DNV-RP-F101 Sec. 8
combined-defect rule; it was incorrectly presented as "the DNV method".

### After (DNV-RP-F101 Sec. 8, length-weighted / area-averaged)

```
L_comb     = end[j] - start[i]                         # unchanged (incl. gaps)
d_comb     = sum(d_k * l_k) / sum(l_k)                 # length-weighted avg depth
             (k in i..j; l_k = member defect lengths, NOT the interior gaps)
```

The composite is assessed as a single defect with `(L_comb, d_comb)`. Every
individual defect and every contiguous interacting sub-run are still evaluated;
the governing (lowest-capacity) case is returned. When the member depths are
**equal**, the area average equals the maximum, so equal-depth colonies are
unchanged.

### old → new values (golden test cases)

Reference pipe: D = 30 in, t = 0.375 in, X52 (SMTS = 66 700 psi), interaction
limit `2*sqrt(D*t) ≈ 6.71 in`.

| Colony (pos, len, depth in) | Governing | OLD (max depth) | NEW (area-avg) | Δ |
|---|---|---|---|---|
| `[(0,4,0.15),(5,4,0.15)]` (existing test, equal depth) | composite (0.15, 9.0) | **1303.10 psi** | **1303.10 psi** | unchanged (equal depths) |
| `[(0,4,0.20),(5,4,0.10)]` (new) | composite | max 0.20 → **1120.40 psi** | avg 0.15 → **1303.10 psi** | +163 psi (less conservative) |
| `[(0,3,0.30),(4,3,0.10),(8,3,0.20)]` (new, 3-defect) | composite members 0..2, L=11 | max 0.30 → **548.49 psi** | avg 0.20 → **1059.42 psi** | +511 psi (less conservative) |

The corrected method is **less conservative** than the old max-depth grouping
whenever member depths differ, because area-averaging dilutes a single deep pit
across the combined length — which is the intended DNV behaviour. The existing
equal-depth interaction test is numerically unchanged.

---

## (B) DNV-RP-F101 Part-A partial-safety-factor tables — finding #6

`dnv_f101_psf` previously hard-coded scalar defaults
`gamma_m = 0.77, gamma_d = 1.0, epsilon_d = 1.0, StD = 0.08`. These are now
**looked up** from the DNV-RP-F101 Part-A tables, selected by **safety class**
(low / normal / high) and **sizing-accuracy band** `StD[d/t]`. Three new public
helpers expose the tables: `gamma_m_factor`, `gamma_d_factor`,
`epsilon_d_fractile`. Any factor may still be passed explicitly to override the
lookup.

### gamma_m — model factor (by safety class and sizing method)

| Safety class | Relative sizing (e.g. MFL) | Absolute sizing (e.g. UT) |
|---|---|---|
| Low | 0.79 | 0.82 |
| Normal | 0.74 | **0.77** |
| High | 0.70 | 0.72 |

The legacy scalar `0.77` is exactly **Normal class / absolute sizing**, which is
kept as the default (`safety_class="normal"`, `measurement_method="absolute"`).

### epsilon_d — fractile factor for corrosion depth, `(d/t)* = (d/t) + epsilon_d·StD`

With `a = StD[d/t]`:

```
a <= 0.04        -> epsilon_d = 0
0.04 < a <= 0.16 -> epsilon_d = -1.33 + 37.5 a - 104.2 a^2
a  > 0.16        -> clamped at the a = 0.16 value
```

Self-consistency check: `epsilon_d(0.04) ≈ 0`, `epsilon_d(0.08) ≈ 1.003`,
`epsilon_d(0.16) ≈ 2.003`. The legacy scalar `epsilon_d = 1.0` is therefore the
DNV fractile at `StD = 0.08` — so the depth-tolerance term is **unchanged** at
the default accuracy band.

### gamma_d — depth partial safety factor (by StD band and safety class)

With `a = StD[d/t]`:

```
Low    : a < 0.04         -> 1.0 + 4.0 a
         0.04 <= a < 0.08  -> 1.0 + 5.5 a - 37.5 a^2
         0.08 <= a <= 0.16 -> 1.2
Normal : a <= 0.16         -> 1.0 + 4.6 a - 13.9 a^2
High   : a <= 0.16         -> 1.0 + 4.3 a - 4.1 a^2
(a > 0.16 clamped at the a = 0.16 value for all classes)
```

At the default `StD = 0.08`: `gamma_d` = 1.200 (Low), **1.279** (Normal),
1.318 (High). Larger StD and higher safety class both raise `gamma_d`.

### old → new PSF default value

The only changed factor at the default (normal / absolute / StD = 0.08) is
`gamma_d`: `1.0 → 1.27904`. For the reference single defect
(D = 30, t = 0.375, d = 0.15, L = 8, X52):

| Quantity | OLD scalar default | NEW tabulated default |
|---|---|---|
| gamma_m | 0.77 | 0.77 (unchanged) |
| epsilon_d | 1.0 | 1.00312 (≈ unchanged) |
| gamma_d | 1.0 | **1.27904** |
| **allowable P_corr** | **950.59 psi** | **795.49 psi** (more conservative, correct) |

The previous scalar defaults were internally inconsistent (`gamma_d = 1.0` ↔
StD = 0, but `epsilon_d = 1.0` ↔ StD = 0.08); the tabulated lookup makes the two
consistent at the stated accuracy band. This was finding #6.

### Sources

The γ_m / γ_d / ε_d values and equations are the DNV-RP-F101 (2015 / GL-2017
"Corroded pipelines") Part-A partial-safety-factor tables (Sec. 3 / Sec. 8),
as reproduced in the DNV-RP-F101 standard and widely cited in the corroded-
pipeline literature. The γ_m table (Low/Normal/High = 0.79/0.74/0.70 relative,
0.82/0.77/0.72 absolute) and the γ_d / ε_d piecewise polynomials in `StD[d/t]`
were cross-checked against the published standard; the ε_d quadratic reproduces
the canonical 0 / 1 / 2 fractile points at StD = 0.04 / 0.08 / 0.16.

---

## (C) Validity / applicability layer — findings #3, #4, #7

* **d/t > 0.85** (DNV-RP-F101 capacity-equation validity limit). `dnv_f101_psf`
  now mirrors the single-defect `within_applicability` flag: results with
  `d/t > 0.85` carry `details["within_applicability"] = False` and an
  `applicability_note`. The interacting assessment surfaces the governing case's
  `within_applicability` flag in its details.
* **StD[d/t] > 0.16** (outside the Part-A PSF calibration range). `dnv_f101_psf`
  flags this and notes that `gamma_d` / `epsilon_d` are clamped at the 0.16 point.
* **Usage-factor 0.72 clarification (findings #4/#7).** The allowable-stress
  `usage_factor` default `0.72` is the **ASME B31.8 location-class-1 design
  factor**, *not* a DNV safety-class usage factor. This is now stated in the
  module docstring and at the constant's point of use, which directs callers to
  the PSF format (`dnv_f101_psf` with an explicit `safety_class`) for
  DNV-RP-F101 code-compliant safety. ASME B31.8 location classes 2/3/4 use
  0.60/0.50/0.40 respectively.

---

## Summary of behaviour changes (old → new)

| Area | Old | New |
|---|---|---|
| Interacting combined depth | `max(depth)` over the run | length-weighted `sum(d·l)/sum(l)` (DNV Sec. 8) |
| Interacting golden `[(0,4,0.20),(5,4,0.10)]` | 1120.40 psi | 1303.10 psi |
| Interacting golden `[(0,3,0.30),(4,3,0.10),(8,3,0.20)]` | 548.49 psi | 1059.42 psi |
| Interacting equal-depth case | 1303.10 psi | 1303.10 psi (unchanged) |
| PSF `gamma_m/gamma_d/epsilon_d` | scalars 0.77 / 1.0 / 1.0 | table lookup by class + StD |
| PSF default allowable (ref defect) | 950.59 psi | 795.49 psi |
| PSF `safety_class` parameter | — | added (default `normal`) |
| PSF `measurement_method` parameter | — | added (default `absolute`) |
| PSF validity flags | none | `within_applicability` for d/t > 0.85 and StD > 0.16 |
| `usage_factor` 0.72 semantics | implied DNV | documented as ASME B31.8 class-1 design factor |

All 28 tests in `tests/asset_integrity/test_dnv_rp_f101.py` pass (14 new); the
full `tests/asset_integrity/` suite is green (481 passed, 8 skipped).
