# Plan: digitalmodel #283 — WRK-131 Passing ship mooring response (Phase 2)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/283
**Status:** plan-review
**Tier:** T3 (multi-module feature: AQWA/OrcaFlex coupling + parametric driver + benchmarks)

## Context

Issue #283 (WRK-131) extends the existing passing-ship force calculator (already implemented per the checked acceptance bullets) into a full mooring-response pipeline. The codebase already has a substantial passing-ship module — `src/digitalmodel/hydrodynamics/passing_ship/` contains `calculator.py`, `force_time_history.py`, `formulations.py`, `input_schemas.py`, `cli.py`, `exporters.py`, `benchmark_report.py`, `visualization.py`, plus a templates dir and CONVENTIONS.md (verified via `ls`). The parametric sweep stub lives at `src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py`, the test fixture at `tests/fixtures/test_vectors/hydrodynamics/passing_ship.yaml`, and tests under `tests/hydrodynamics/passing_ship/`. Sample doc artifacts exist at `docs/domains/orcaflex/passing_ship/sample/`.

What is **not yet built** (from the unchecked acceptance bullets): the mooring-response coupling (line tensions, vessel motions under passing-ship force time histories), the cross-product parametric matrix (speed × distance × wind × passing angle), 5–7 reproducible historical benchmarks with sanitized `spec.yml` artifacts, and a standardized HTML report aligned with WRK-129 conventions.

**Stale-flag:** Not stale. This is in-flight work with a clear remaining scope; the issue is `blocked` only because external resources (`G:\ACMA Tool\` and `R:\Archive - Drive J\`) live on a Windows machine. Plan must accommodate the resource-access dependency.

## Plan

### Task 1 — Stand up the mooring-response coupling
Add a `MooringResponseRunner` in `src/digitalmodel/hydrodynamics/passing_ship/mooring_response.py` that consumes a `PassingShipForceTimeHistory` (existing, in `force_time_history.py`) and drives either an OrcaFlex model or an AQWA mooring solve. Define the dispatch via a `solver: orcaflex|aqwa` field on `PassingShipSpec`. For machines without OrcaFlex license, emit a deterministic synthetic-tension stub so tests run on dev-primary.

### Task 2 — Parametric matrix driver
Extend `passing_ship_sweep.py` to consume the matrix axes (passing-ship speed × lateral distance × wind on/off × passing angle) declared in `PassingShipSpec.analysis_matrix`. Cartesian-product the axes; persist each case's results YAML to a run directory; write a manifest `cases.yaml` keyed by case-id.

### Task 3 — Historical benchmark reproduction (5–7 cases)
Reproduce 5–7 archived projects from `R:\Archive - Drive J\`. For each, sanitize inputs (vessel name → "Vessel-A", project codes redacted), produce a `spec.yml` under `docs/domains/orcaflex/passing_ship/benchmarks/<case-id>/`, run the new pipeline, and capture original-vs-new tension envelopes with explicit pass/fail tolerance (target ±10% peak-line-tension; widen to ±20% if the historical run lacks documented sea-state). Drive this from `acma-ansys05` (the only machine with archive access).

### Task 4 — HTML report aligned with WRK-129
Add an `html_report` exporter to `src/digitalmodel/hydrodynamics/passing_ship/exporters.py` (or a new `report.py`) producing the WRK-129 layout: force time-history plots, mooring-tension envelopes per line group, vessel-motion (surge/sway/yaw) traces, and a parametric-summary table. Reuse the visualization helpers already in `visualization.py`.

### Task 5 — Test coverage and CI
Add unit tests in `tests/hydrodynamics/passing_ship/test_mooring_response.py` (synthetic stub path), `test_parametric_sweep.py` (matrix expansion with 2×2×2 axes), and `test_benchmark_regression.py` (fixture-based, license-free). Mark license-required tests with `@pytest.mark.requires_orcaflex` so dev-primary CI skips them.

## Acceptance Criteria

- [ ] `MooringResponseRunner` runs end-to-end on a small synthetic case (no license) and an `acma-ansys05`-licensed case (OrcaFlex or AQWA).
- [ ] Parametric driver produces a results manifest for a 3×3×2×2 = 36-case matrix in <30 minutes on the licensed machine.
- [ ] 5–7 sanitized benchmark `spec.yml` files committed under `docs/domains/orcaflex/passing_ship/benchmarks/`; each reproduces original peak line tensions within the documented tolerance band.
- [ ] HTML report renders with all four section types (forces, tensions, motions, parametric summary).
- [ ] `uv run pytest tests/hydrodynamics/passing_ship/ -v` passes on dev-primary; license-marked tests pass on `acma-ansys05`.
- [ ] No raw client identifiers in any committed file; sanitization checked by an explicit grep gate.

## Open questions

- Solver choice for Task 1: OrcaFlex (Python API mature) vs. AQWA (matches the issue title) — the issue mentions both. Confirm the primary path with the user; the other becomes a fallback via the `solver:` switch.
- Benchmark tolerance: ±10% is aggressive for older AQWA runs without documented sea-state. Should the plan widen to ±20% by default and tighten per-case where evidence supports it?
