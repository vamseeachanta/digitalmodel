# RSTRENG 2D River-Bottom Effective-Area — Validation (2026-06-29)

Module: `src/digitalmodel/asset_integrity/rstreng_2d.py`
Tests: `tests/asset_integrity/test_rstreng_2d.py` (16 golden / behavioural)
Issue: digitalmodel #1094 (extension)

## Purpose

Inline-inspection and laser-scan metal-loss data come as a 2D **grid**
`d[a][c]` — depth at axial station `a` and circumferential column `c`. The
validated 1D RSTRENG engine
(`corroded_pipe.rstreng_effective_area(D, t, positions, depths, smys)`) only
consumes a single **axial** river-bottom profile `d_eff(x)`.

This module is a thin **projection layer**: it reduces the grid to a 1D
effective axial profile `d_eff(x_a)` and then calls the 1D engine **verbatim**
(no engine changes, `rstreng_effective_area` imported and reused unchanged). The
1D engine already searches every axial sub-segment for the governing (lowest
failure-pressure) flaw and applies the `d/t <= 0.80` B31G validity flag; both
are surfaced through the 2D result.

## Projection rules

### MAX (default — conservative river-bottom)

```
d_eff[a] = max_c d(x_a, theta_c)
```

The deepest pit at each axial station. This is the standard, defensible way
RSTRENG forms the river-bottom path: it never under-predicts metal loss because
it takes the worst pit at every station, independent of whether the deepest
pits actually line up axially.

### AREA-WEIGHTED (alternative — less conservative)

```
d_eff[a] = (1 / w) * sum_c d(x_a, theta_c) * dtheta_c,   w = sum_c dtheta_c
```

An arc-width-weighted circumferential average over the affected width `w`.
Because the 1D engine has **no notion of circumferential width**, this rule
**requires an explicit affected-width input** — `circ_weights`, the arc width
`dtheta_c` represented by each column (their sum is the affected width `w`).
When `circ_weights` is omitted, equal angular spacing across the affected arc is
assumed, so the rule reduces to the plain circumferential mean.

> **Caveat.** Area-weighting **under-predicts** the hazard (predicts a higher,
> less conservative burst pressure) when the deepest pits do not co-locate at
> the same axial station — averaging dilutes an isolated deep pit. Use MAX for
> fitness-for-service verdicts; AREA-WEIGHTED is for sensitivity / less-
> conservative screening only.

## Golden case (vetted, independently reproduced)

`D = 24 in`, `t = 0.5 in`, X52 (`SMYS = 52000 psi`, flow = SMYS + 10 ksi =
`62000 psi`). Axial `x = [0, 2, 4, 6, 8] in`; 3 circumferential columns:

| x (in) | col 1 | col 2 | col 3 |
|-------:|------:|------:|------:|
| 0      | 0     | 0     | 0     |
| 2      | 0.10  | 0.20  | 0.05  |
| 4      | 0.15  | 0.30  | 0.10  |
| 6      | 0.05  | 0.25  | 0.00  |
| 8      | 0     | 0     | 0     |

Intact flow-stress pressure: `2*flow*t/D = 2*62000*0.5/24 = 2583.3 psi`.

Folias (Modified-B31G / RSTRENG): `M = sqrt(1 + 0.6275 z - 0.003375 z^2)` for
`z = L^2/(D t) <= 50`. Failure pressure:
`pf = (2 flow t/D) * (1 - A/A0) / (1 - (A/A0)/M)`.

### MAX projection

`d_eff = [0, 0.20, 0.30, 0.25, 0]`. The 1D engine's sub-segment search picks the
governing segment **(1, 4)** (x = 2 → 8):

- `L = 6 in`
- Effective metal-loss area `A_eff = 1.30 in^2` (trapezoidal:
  `0.5+0.55+0.25`), `A0 = t*L = 3.0`, `A/A0 = 0.4333`
- `z = 36/12 = 3.000`, `M = 1.6888`
- **`pf = 1969.2 psi`** → RSF = `1969.2/2583.3 = 0.7623`,
  safe @ 1.39 = `1416.7 psi`

### AREA-WEIGHTED projection (circumferential mean)

`d_eff = [0, 0.1167, 0.1833, 0.10, 0]` (station means `0.35/3`, `0.55/3`,
`0.30/3`). The governing segment is the **full length (0, 4)** (x = 0 → 8):

- `L = 8 in`, `A/A0 = 0.2000`
- `z = L^2/(D t) = 64/12 = **5.333**` (note: **not** 4.0)
- `M = 2.0617`
- **`pf = 2288.7 psi`** → RSF = `0.8859`, safe @ 1.39 = `1646.5 psi`

Area-weighted is **~16 % higher** than MAX (`2288.7 / 1969.2 ≈ 1.16`),
demonstrating projection-rule sensitivity: here the deepest pits (col 2) do not
fully co-locate with the rest, so averaging materially relaxes the result.

## Simplification / scope

This is a **simple projection** (collapse each axial station to one scalar, then
run the validated 1D method). It is **not** a full 2D interacting-flaw
coalescence assessment: no circumferential interaction-spacing rules, no
combined axial + circumferential (hoop/longitudinal) stress evaluation, no
network/box growth of adjacent pits. The MAX rule is conservative for axial
burst; circumferential-extent acceptance (e.g. for leak vs. rupture or
longitudinal-stress checks) is out of scope and must be assessed separately.

## Result surface

`RiverBottom2DResult` exposes: `projection_rule`, `axial_positions_in`,
`d_eff_in`, `governing_segment` `(i, j)`, `within_applicability` (max `d/t <=
0.80`), `details` (`n_axial`, `n_circ`, `affected_width`, `critical_length_in`,
`max_d_over_t`), and the full 1D `CorrodedPipeResult` as `result` (plus
pass-through properties `failure_pressure_psi`, `intact_pressure_psi`,
`safe_pressure_psi`, `rsf`).
