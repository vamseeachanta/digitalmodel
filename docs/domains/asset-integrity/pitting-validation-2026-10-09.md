# Pitting workflow qualification record — 2026-10-09

Workflow: `api579-pitting-screen`. Scope: remaining-wall UT grid in inches,
closed-form screen consistent with Part 6 philosophy, followed by the existing
Part 5 equivalent-LTA estimate. Dataset owner: digitalmodel; source: synthetic
geometry in `examples/workflows/api579-pitting-screen/input.yml`, version: synthetic-input-v1. No measured/client data or licensed tables are used.

## Formula anchor

Case SYNTHETIC-PITTING: OD 24 in, nominal WT 0.500 in, required wall 0.350 in,
FCA 0.000 in, axial pitch 1 in; five separated 0.300 in readings in a 8 × 8
0.500 in grid. Rows are axial. The effective axial extent is 3 in. The mean is
computed over the five pitted cells, not diluted by the sound background.

The independently recomputed engine relation uses engine reference diameter OD − 2 t_min = 23.3 in,
Rt = 0.300 / 0.350, lambda = 1.285 × 3 / sqrt(23.3 × 0.350),
Mt = sqrt(1 + 0.48 × lambda²), and RSF = Rt / (1 − (1 − Rt)/Mt).
These are the existing simplified Part 5 relations documented in
`assessment/level2_engine.py`; this anchor checks workflow wiring, not a
primary-source confirmation of those relations.

| Case / quantity | Hand recomputation | Workflow value | Absolute difference | Criterion |
|---|---:|---:|---:|---|
| SYNTHETIC-PITTING, RSF (1) | 0.9569915955570703 | 0.9569915955570703 | < 1e-12 | Absolute tolerance 1e-9 |
| Level 1 mean wall after FCA (in) | 0.300 | 0.300 | < 1e-12 | Below required 0.350: FAIL_LEVEL_1 |

Table 1. Synthetic formula checks; numbers are regenerated from the equations.

RSF 0.9569915955570703 exceeds the existing RSFa 0.90 and the shared
ACCEPT threshold 0.95. Level 1 fails its mean-wall criterion (0.300 < 0.350 in),
but Level 2 passes on RSF; the shared synthetic screening disposition is ACCEPT.
With FCA 0.020 in, the regenerated RSF is in the MONITOR band
(0.90 <= RSF < 0.95). With axial pitch 10 in and FCA 0.000 in, RSF is
0.8703619120925993, below RSFa 0.90; Level 2 fails and the workflow escalates.
The mean-wall criterion is not repeated after Level 2; t_min remains its
reference wall. Equality at RSFa passes. No life projection or pressure
rerating is calculated. The deepest-ligament and applicability gates take
precedence over either level. These dispositions qualify only the synthetic
formula checks, not acceptance of a measured component.

## Published cases and applicability

API 579-1/ASME FFS-1 Part 6 chart/coupled-pit worked examples: **not evaluated**.
No accessible verified example input/reference-value pair within this engine's
method is established in this record; a published numerical difference cannot
be calculated. No example number or reference result is invented.

The equivalent LTA is an assumed representation of uniform synthetic pit fields.
A bound on actual pitting failure mechanisms and published-case qualification
are not established. The result shall not be treated as a qualified Part 6
coupled-pit assessment or measured-component acceptance. Nonuniform pitted-cell
depths are gated to ESCALATE; their mean estimate is reference only.
Pit-pair input is not implemented. The result field `qualification` is
`synthetic_formula_anchor_only`.

Inputs shall have OD > 2 WT, 0 < t_min <= WT, finite rectangular remaining-wall
readings in (0, WT], and positive axial pitch. Over-nominal readings are rejected,
not clipped. FCA is deducted once for Level 2. Remaining deepest ligament / WT
shall meet the existing 0.20 screening default; lower configured floors are
rejected. Non-positive net ligament yields ESCALATE without a strength number.
Non-pitted background readings shall establish the reference wall: their
minimum after FCA shall be at least t_min. Missing background or thinner
background yields ESCALATE for metal-loss assessment, independent of pit RSF.
The existing Folias applicability flags are propagated. Circumferential extent,
absolute ligament and distance-to-discontinuity checks are **not evaluated**.

Catalog disposition: **workflow**, pending published-case validation. Promotion
to `live` is conditional on a verified supported published case, reference-value
readback and a passing comparison. Formula-check success alone does not meet
that criterion. Failed screens escalate; no pressure derating or life-based
repair choice is inferred. Pit-free grids require the metal-loss workflow.
Single-row equivalent regions use two half-pitch rows to preserve axial length.

Input integrity: SHA-256 `72ab283893a8b38bee89306f3c998e00cb0c3585a4a07a372e620ebf7504e9d7`.
