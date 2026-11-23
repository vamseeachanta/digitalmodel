# Plan: digitalmodel #489 — HIPPS modeling (API 17O)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/489
**Status:** plan-review
**Tier:** T3 (new submodule)
**Priority:** low

## Context

HIPPS (High Integrity Pressure Protection Systems, API 17O) protect downstream pipelines from wellhead overpressure by automatically closing isolation valves when pressure exceeds setpoint. Field-development safety analysis needs HIPPS pressure-surge modeling, valve sizing, and SIL (Safety Integrity Level) reliability assessment. No code today.

Direct integration target: existing `src/digitalmodel/subsea/pipeline/pipeline_pressure.py` and `pipeline_pressure_workflow.py` — HIPPS sets the upper bound on the pipeline maximum allowable operating pressure (MAOP).

## Plan

1. **Create submodule skeleton.** New `src/digitalmodel/subsea/hipps/` with `__init__.py`, `pressure_surge.py`, `valve_sizing.py`, `sil_analysis.py`, plus `fixtures/canonical_hipps_loop.yml` (2-of-3 voting pressure transmitters + 2 isolation valves, 250 ms response time, SIL 3 target).

2. **Pressure surge analysis (`pressure_surge.py`).** Wave-equation transient solver (method of characteristics) for a pipeline segment with a closing valve. Inputs: pipe length, wave speed, fluid bulk modulus, valve closure profile. Output: pressure-time series at upstream and downstream points. Reference: API RP 14C and Joukowsky surge formula for sanity check (`Δp = ρ * a * Δv`).

3. **Valve sizing (`valve_sizing.py`).** `size_isolation_valve(flow_max, dp_design, fluid_props)` returns required Cv and recommended actuator response time to keep `surge_peak <= MAOP * 0.9`. Iterate with `pressure_surge` for closure-profile sensitivity.

4. **SIL analysis (`sil_analysis.py`).** PFD (Probability of Failure on Demand) calculation per IEC 61508/61511. Inputs: voting architecture (1oo2, 2oo3), component failure rates, proof test interval. `compute_pfd_avg()` returns PFD_avg and the corresponding SIL band (1-4). Provide a fixture with the canonical 2-of-3 voting layout and a SIL 3 target.

5. **Pipeline integration and tests.** Add an optional `hipps:` section consumer to `subsea/pipeline/pipeline_pressure_workflow.py` so MAOP can reference a HIPPS protection setpoint. Tests in `tests/subsea/hipps/`: surge solver vs Joukowsky on a step-closure case (within 5%), valve-sizing convergence, PFD calc against IEC 61508 Annex B worked example. Smoke: `uv run pytest tests/subsea/hipps/ -x`.

## Acceptance Criteria

- [ ] `src/digitalmodel/subsea/hipps/` submodule with 3 core modules + 1 fixture
- [ ] Surge solver matches Joukowsky's analytic surge magnitude within 5% for a sudden valve closure
- [ ] `size_isolation_valve()` returns a Cv and response time that keep peak pressure ≤ 0.9 × MAOP on the fixture
- [ ] `compute_pfd_avg()` matches IEC 61508 Annex B worked example within 1%
- [ ] `pipeline_pressure_workflow.py` accepts an optional HIPPS setpoint and propagates it to the MAOP check
- [ ] All tests pass; API 17O citation emitted

## Open questions

1. Source for component failure-rate library — OREDA Handbook (licensed) or fall back to public exida tables? Document choice in module docstring.
2. Should the surge solver also support 1D pipeline networks (tee junctions) in v1, or single-segment only? Default: single-segment v1, network as a follow-up.
