# Plan: digitalmodel #1359 — derived operability quantities + rolling go/no-go with lead time

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1359
**Epic:** #1356 · **Status:** plan-review · **Tier:** T2/T3 (Route B/C — decision layer on the merged #1358 engine; reuses tested criterion primitives)

> Adversarially reviewed against the code (2026-07-04): APPROVE-WITH-CHANGES. Blockers folded in — the consistency test now uses the deterministic spectral significant (not the dimensionally-wrong `operation_envelope` Hs-limit, and not a scatter-prone single-record std); governing velocities are magnitudes before classification; lead-time edge cases and the two-threshold→`warning_factor` mapping are pinned; YAML relocated to per-module package data.

## Context

#1358 (merged) produces a time-domain `MotionForecast` (6-DOF + lever-arm point-of-interest motion). #1359 turns that into the quantities operators gate on and a **rolling GO / CAUTION / NO-GO with lead time** — the decision the live demo (#1361) prototypes. The merged `operation_envelope.py` even annotates its heavy-lift limit *"crane-tip refinement later"*; this issue is that refinement. Not stale.

## Reuse (verified 2026-07-04)
- **Motion source (module functions, not methods):** `motion_forecast.MotionForecast` (`.dof`, `.t`, `.significant(dof)`), `vertical_motion_at(motion, offset)`, `time_derivative(t, x)` — all merged/exported in #1358.
- **Criterion primitives:** `marine_ops/installation/go_no_go.py` — `_check_criterion(name, value, limit, unit, above_is_safe=False, warning_factor=...)`, `CriterionResult`, `CriterionState` (PASS/WARNING/FAIL), `DecisionState` (GO/MARGINAL/NO_GO). Reused directly. **Do NOT call `evaluate_go_no_go`** — verified jumper-lift-specific (hardcodes crane SWL 77.5 Te, sling MBL 1200 Te, Saipem deck, Ballymore baselines).
- **Operation model (pattern only):** `operation_envelope.py::Operation`, `MotionLimit`, `OPERATIONS`. Note `operation_envelope()` returns a *limiting Hs* (`EnvelopeResult.hs_limit_m`), and the significant response lives in the private `_significant_response(...)` — so it is **not** used as the consistency oracle (see acceptance).

## Net-new
Time-domain **governing quantities** and a **rolling decision** — today only static significant-amplitude limits exist.

## Design (all under `src/digitalmodel/motion_forecast/`)

- `derived.py` — governing time series from a `MotionForecast` + point-of-interest offset. **All velocity/accel series are magnitudes** (`abs(time_derivative(...))`, or a running-peak envelope) so a large *downward* swing cannot pass a below-limit test:
  - **Crane lift:** crane-tip vertical velocity (& acceleration) from `time_derivative(motion.t, vertical_motion_at(motion, tip))`. (DNV-ST-N001 / DNV-RP-N103.)
  - **Gangway (W2W):** gangway-tip vertical velocity & excursion vs stroke. (DNV-ST-0358 / Walk2Work.)
  - **Helideck:** inclination `hypot(roll, pitch)` (deg, ≥0) and deck heave velocity. (CAP 437 / HCA HMS: incl > 1°, heave vel > 0.4 m/s.)
  - **Hose/riser top:** `top_connection_vertical_velocity` — an explicit **kinematic screen only** (NOT a tension; snatch load needs slack-taut/line-stiffness dynamics — deferred to a fast-follow).
- `criteria.py` — per-operation records `{governing, caution, limit, unit, alpha, basis, poi_offset}` loaded from **package-data YAML** shipped beside the module (`motion_forecast/criteria/*.yml`, read via `importlib.resources`) — a *data* file, distinct from the `base_configs/modules/<m>/<m>.yml` engine-cfg templates. Loader validates `0 < caution ≤ limit`. Reusing `_check_criterion` means setting **`warning_factor = caution/limit`** (its WARNING band is `[warning_factor·limit, limit]`).
- `decision.py` — **rolling go/no-go**: scan each governing series over `[now, now+horizon]`, classify each instant via `_check_criterion(above_is_safe=False)`, and return `RollingDecision{state: DecisionState, lead_time_to_caution, lead_time_to_no_go, timeline, criterion}`. Semantics pinned:
  - already over limit at `now` → `state=NO_GO`, `lead_time_to_no_go=0`.
  - in caution band at `now`, no limit breach ahead → `state=MARGINAL`.
  - clear now, breaches later → `state=GO`, lead time = first-breach − now.
  - never breaches in horizon → lead time = `None` (clear to horizon).
  `DecisionState.MARGINAL` is reused; **"CAUTION" is a display label only**.

## Plan
1. `derived.py` + tests (magnitude governing series; exact analytic checks).
2. `criteria.py` + package-data YAML + loader/validation (crane / gangway / helideck defaults with DNV/CAP basis).
3. `decision.py` + tests (rolling scan, both lead-times, edge + negative-going cases; reuse `_check_criterion`/`DecisionState`).
4. Extend `workflow.router` to emit `cfg["motion_forecast"]["decision"]` (non-breaking).
5. Consistency test (below).

## Acceptance Criteria
- [ ] `derived.py` governing quantities match analytic values (<1e-6) for a known motion: crane-tip vel, gangway excursion/vel, helideck inclination + heave vel; velocity series are magnitudes.
- [ ] Rolling verdict: breach engineered at t* → `lead_time_to_no_go == t* − now`; **already-breached** → 0/NO_GO; **never-breached** → None/GO; a large **negative-going** velocity is classified NO-GO (sign guard).
- [ ] Thresholds in reviewable package-data YAML; loader rejects malformed criteria and enforces `0 < caution ≤ limit`; zero hardcoded limits.
- [ ] **Consistency (deterministic):** the derived governing quantity's spectral significant `2·√(Σ ½ aᵢ² |H_gov(ωᵢ)|²)` from the shared components matches an independent closed-form for a simple analytic transfer within ≤1e-3 (time-vs-frequency identity; no single-record `std`, no `operation_envelope` Hs-limit).
- [ ] `router` emits `cfg["motion_forecast"]["decision"]`; existing #1358 tests still green.
- [ ] Pure-numerical; full suite green (no license).

## Open questions (recommendations in parens)
1. Top-tension: kinematic velocity screen now + full riser/mooring-model tension as fast-follow (**recommended**), or proxy sufficient?
2. Reuse `DecisionState.MARGINAL` internally, expose "CAUTION" as display label (**recommended**), vs adding a distinct CAUTION state?
3. Per-operation point-of-interest offsets (crane radius, gangway reach) in the same YAML, caller-overridable (**recommended**)?
