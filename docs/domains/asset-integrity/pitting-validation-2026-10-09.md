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

RSF exceeds RSFa 0.90, but the mean effective wall is below the supplied
required wall. The workflow therefore does not issue ACCEPT or MONITOR for
this case; the shared screening disposition is ESCALATE (further assessment).
No life projection or pressure rerating is calculated. The deepest-ligament gate takes precedence over both levels.

## Published cases and applicability

API 579-1/ASME FFS-1 Part 6 chart/coupled-pit worked examples: **not evaluated**.
No accessible verified example input/reference-value pair within this engine's
method is established in this record; a published numerical difference cannot
be calculated. No example number or reference result is invented.

Equivalent-LTA conservatism is conditional on the assumed uniform pit-field
representation. Replacing nonuniform pit depths by their mean is not established
as a conservative bound on every local failure mechanism. The result shall not
be treated as a fully qualified Part 6 coupled-pit assessment. Nonuniform
pitted-cell depths are gated to ESCALATE; the mean estimate is reference only.

Inputs shall have OD > 2 WT, 0 < t_min <= WT, finite rectangular remaining-wall
readings in (0, WT], and positive axial pitch. Over-nominal readings are rejected,
not clipped. FCA is deducted once for Level 2. Remaining deepest ligament / WT
shall meet the existing 0.20 screening default; lower configured floors are
rejected. Non-positive net ligament yields ESCALATE without a strength number.
The existing Folias applicability flags are propagated. Circumferential extent,
absolute ligament and distance-to-discontinuity checks are **not evaluated**.

Catalog disposition: **workflow**, pending published-case validation. Promotion
to `live` is conditional on a verified supported published case, reference-value
readback and a passing comparison. Formula-check success alone does not meet
that criterion. Failed screens escalate; no pressure derating or life-based
repair choice is inferred. Pit-free grids require the metal-loss workflow.
Single-row equivalent regions use two half-pitch rows to preserve axial length.

Input integrity: SHA-256 `72ab283893a8b38bee89306f3c998e00cb0c3585a4a07a372e620ebf7504e9d7`.
