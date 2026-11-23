# Plan: digitalmodel #467 — Subsea structure installation analysis: vessel performance + real-time motion feedback

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/467
**Status:** plan-review
**Tier:** T3 (multi-phase, three phases)
**Related:** #464 (Capytaine), `marine_ops/installation/`

## Context

`src/digitalmodel/marine_ops/installation/` already carries the building blocks:
- `crane_tip_motion.py` — RAO-based crane-tip motion transfer (DNV-RP-H103)
- `splash_zone.py` — splash-zone slamming and varying-buoyancy assessment
- `operability.py` — `compute_operability`, `hs_limit_for_criterion`, `weather_window_operability`
- `vessel_screening.py` — multi-vessel comparison (`VesselScreeningResult`, operability matrix)
- `realtime_feedback.py` — `MRUReading` data model + criteria checker (Phase 3 design layer landed)
- `models.py` — `Vessel`, `Structure`, `InstallationCase`, `InstallationCriteria`, `OperabilityResult`, `SplashZoneResult`

The issue's three phases:
- **Phase 1** (Installation Analysis Engine): mostly already implemented — needs a parametric-sweep driver and a quick-assess CLI
- **Phase 2** (Multi-Vessel Comparison): mostly there; needs a curated vessel database with crane curves + RAO library, plus cost-weighted ranking
- **Phase 3** (Real-Time Motion Feedback): data model exists; the **alarm-state machine + replay** is missing, dashboard UI explicitly deferred

This plan delivers Phase 1's gap (parametric sweep + quick-assess CLI) and Phase 2's vessel database; Phase 3 spawns a follow-on issue for replay + dashboard.

## Plan

1. **Confirm gap before writing code.** `grep -n "def " src/digitalmodel/marine_ops/installation/*.py | grep -iE "screen|sweep|matrix|quick"` to enumerate what already exists. The issue asks for "Quick-assess mode: screen multiple vessels against a given structure" — confirm whether `vessel_screening.compute_screening` covers it; if so, the work is documentation + CLI wrapper only.

2. **Phase 1 — Parametric sweep driver.** New module `src/digitalmodel/marine_ops/installation/parametric_sweep.py`:
   - `ParametricSweepConfig(structure_lengths, water_depths, vessels, sea_states)`
   - `run_sweep(cfg) -> SweepResult` — produces an N-D xarray Dataset (dims: structure_length, water_depth, vessel, hs) with operability % per cell
   - Reuses `crane_tip_motion`, `splash_zone`, `operability` for cell evaluation
   - Backed by `joblib.Parallel` for n_jobs > 1 (already in deps)
   - Outputs both DataFrame for tables and xarray for downstream plotting

3. **Phase 1 — Quick-assess CLI.** New entry-point `python -m digitalmodel.marine_ops.installation.quick_assess --structure <yaml> --vessels <yaml> --hs 1.5 --tp 8` returning a vessel ranking table. Add a `console_scripts` entry in `pyproject.toml` `[project.scripts]` if that conventions used in this repo.

4. **Phase 2 — Vessel database.** New `src/digitalmodel/marine_ops/installation/data/vessels/` directory with one YAML per vessel (start with three: a generic CSV, a heavy-lift example, and an AHTS). Each carries crane SWL-vs-radius curve, cost-per-day, and a pointer to a `raos/<vessel>.nc` xarray netCDF (placeholder netCDFs synthesized from `crane_tip_motion.crane_tip_raos` if real RAOs absent). Loader: `vessel_database.load_vessel(name) -> Vessel`.

5. **Phase 2 — Cost-weighted ranking.** Extend `vessel_screening.VesselScreeningResult` with `cost_per_workable_day = vessel.cost_per_day / operability_pct` and a `rank_by_cost()` method. Add `tests/marine_ops/installation/test_vessel_screening.py::test_cost_ranking`.

6. **Phase 3 — Defer to follow-up issue.** File new digitalmodel issue "Real-time motion alarm state machine + replay (#467 Phase 3)". This issue body: in-memory + file-backed time-series replay, alarm threshold tied to `InstallationPhase` (lift-off, splash zone, landing), plus a dashboard hand-off design doc. Out of scope for #467's PR — this lets #467 close with Phases 1-2 done and Phase 3 acknowledged.

7. **Tests.** New cases in `tests/marine_ops/installation/`:
   - `test_parametric_sweep.py::test_run_sweep_3x3x2_grid` — tiny grid, asserts xarray dims and finite values
   - `test_vessel_database.py::test_load_three_vessels` — fixture vessels parse correctly
   - `test_quick_assess_cli.py` — invokes via `subprocess`, asserts ranking-table format

8. **Smoke check.** `uv run python -m digitalmodel.marine_ops.installation.quick_assess --structure tests/fixtures/sample_structure.yml --vessels src/digitalmodel/marine_ops/installation/data/vessels/ --hs 1.5 --tp 8` prints a 3-row ranking table.

## Acceptance Criteria

- [ ] `parametric_sweep.py` runs an N-D sweep across structure × depth × vessel × Hs and returns xarray
- [ ] `quick_assess` CLI returns ranked vessel table for a given structure + sea state
- [ ] `data/vessels/` carries ≥ 3 vessel YAMLs with SWL curves and cost-per-day fields
- [ ] `VesselScreeningResult.rank_by_cost()` produces a deterministic ranking
- [ ] All new tests in `tests/marine_ops/installation/` pass
- [ ] Phase 3 follow-up issue filed and linked

## Open Questions

- Are vessel cost numbers public-shareable? Default: use synthetic placeholder rates with a module-level note; real rates live in a `client_projects/` private file the loader will fall back to.
- RAO file format: `.nc` (xarray) vs OrcaFlex YAML — pick xarray for portability, but emit a YAML round-trip helper for OrcaFlex compatibility.
