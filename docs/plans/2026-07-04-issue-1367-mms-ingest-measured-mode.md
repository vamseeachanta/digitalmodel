# Plan: digitalmodel #1367 — real-time MMS / measurement-system ingest (measured-motion mode + reconciliation)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1367
**Epic:** #1356 · **Status:** plan-review · **Tier:** T2/T3 (Route B/C — ingest + measured mode reusing #1358/#1359; time-alignment is the real work)

> Adversarially reviewed (2026-07-04): APPROVE-WITH-CHANGES. Reuse claims all verified. Blockers folded in — a **shared wall-clock time base** and an explicit **resampling contract** for reconciliation; `measured_status` uses `decision._classify` (DecisionState + NaN fail-close), not `_check_criterion`; router gates reconciliation on a real forecast; a rich `MeasuredDecision` mirrors `RollingDecision`.

## Context

Two data modes (owner): *Predicted* (#1358 forecast) and **Measured** — live vessel & structure motions from an onboard MMS / MRU as the "now" ground truth (the demo #1361 splits every panel `MMS MEASURED | FORECAST`). This issue builds the measured half: ingest, a measured representation, **measured-mode go/no-go** on real motion, and **reconciliation** of measured vs forecast — the signal #1360 consumes. Not stale (nothing exists).

## Reuse (verified against merged code)
- **Derived layer duck-types on `.t` + `.dof` only** — confirmed: `derived.compute_governing` and its targets (`vertical_motion_at`, `time_derivative`, `heave_velocity_magnitude`, `inclination_deg`) touch **no** `.origin_time`/`.horizon`/methods. A `MeasuredMotion` with `.t` + `.dof` flows through unchanged.
- **Classification:** reuse `decision._classify` (DecisionState; strict `>` boundaries + NaN→NO_GO fail-close) — **not** `_check_criterion` (which yields CriterionState). Guarantees measured & forecast classify identically. `criteria.load_criteria`/`Criterion` reused.
- **Router pattern:** `router` mutates + returns cfg; a `mf.get("measured")` branch is non-breaking.

## Time base (blocker fix — the load-bearing decision)
`MotionForecast.t` is **absolute** (reconstruct builds `t = linspace(origin_time, origin_time+horizon)`). `MeasuredMotion.t` is likewise **absolute seconds on the same wall-clock**; `now = t[-1]`. All alignment is on this shared clock. Overlap of two records = `[max(starts), min(ends)]`.

## Design (under `src/digitalmodel/motion_forecast/`)

- `measured.py` — `MeasuredMotion{t (absolute s), dof}`; `now` property = `t[-1]`; `at_now()` (latest 6-DOF), `window(seconds)` (trailing slice). Docstring warns: **do not** pass to `rolling_decision` (which treats `t[0]` as now — see footgun note); `measured_status` is the sanctioned entry.
- `measured_source.py` — `MeasuredMotionSource` protocol (`read() -> MeasuredMotion`); `SyntheticMMS` (deterministic, seeded, from a reference motion + optional sensor noise); `from_csv(path)` reading columns `t, surge, sway, heave, roll, pitch, yaw` with **rotations in degrees** (model convention). Real MMS/MRU protocols (NMEA/network) are partner/deployment.
- `reconcile.py`:
  - `_resample(forecast, t_query)` — per-DOF `np.interp` of the forecast onto query timestamps **within the forecast's absolute range** (raise if out of range; no extrapolation).
  - `seam_offset(measured, forecast)` — per-DOF `measured(now) − forecast(now)`, where `forecast(now)` = forecast interpolated at `measured.now`; require `measured.now` within the forecast's `[t0, t1]` (else raise). Anchoring/continuity check.
  - `overlap_error(measured, forecast)` — on the absolute-time overlap: interpolate the forecast onto the measured timestamps in-overlap, then per-DOF **RMSE, bias, correlation**. Require a minimum overlap length; correlation is `None` on a zero-variance/too-short window (RMSE/bias still defined). Metrics only — recalibration is #1360.
  - `measured_status(measured, criterion, offset=None) -> MeasuredDecision` — instantaneous classify of the **latest** governing value via `derived.compute_governing` + `decision._classify`.
- `MeasuredDecision` dataclass — mirrors `RollingDecision` **minus lead times**: `state, display, current_value, caution, limit, governing, unit, t, values, states` — so the demo (#1361) and #1360 consume measured & forecast uniformly.
- `workflow.router` — optional `mf["measured"]` (a source spec / CSV path): emit `mf["measured_status"]`. Emit `mf["reconciliation"]` **only** when a genuine forecast for the same scenario is present (explicit `sea`/forecast), never against the default synthetic sea.

## Plan
1. `measured.py` + tests (representation, `now`, `window`, duck-type through `compute_governing`).
2. `measured_source.py` + tests (SyntheticMMS deterministic; CSV round-trip w/ tolerance; protocol conformance; rotations-in-deg).
3. `reconcile.py` + tests — **including a deliberately different forecast/measured grid** scored vs closed form (exercises resampling); seam alignment + out-of-range raise; correlation zero-variance guard; `measured_status` vs `_classify` at `t[-1]`; NaN → NO_GO.
4. Workflow wiring + tests (measured_status emitted; reconciliation gated on real forecast).

## Acceptance Criteria
- [ ] `MeasuredMotion` flows through `derived.compute_governing` unchanged (test).
- [ ] `SyntheticMMS` deterministic (seeded); `from_csv` round-trips a 6-DOF series within float tolerance; rotations documented as degrees.
- [ ] `seam_offset` = 0 (per DOF) when measured equals forecast at the same instant; raises when `measured.now` is outside the forecast range.
- [ ] `overlap_error`: RMSE/bias = 0 on identical overlap; **matches a closed-form value when forecast & measured are on DIFFERENT grids** (resampling path); correlation guarded on zero-variance/short overlap.
- [ ] `measured_status` returns a `MeasuredDecision` equal to `_classify` at `t[-1]`; NaN → fail-closed NO-GO; unit/thresholds from the #1359 criteria YAML.
- [ ] `router` emits `measured_status` when a feed is supplied and `reconciliation` **only** with a real forecast; existing #1358/#1359 tests still green.
- [ ] Pure-numerical; zero hardcoded thresholds.

## Deferred / out of scope (documented)
Real MMS/MRU wire protocols + hardware time-sync; recalibration & skill-tracking-over-time (**#1360**, which inherits this issue's resampling + metric definitions).

## Open questions — all resolved to recommended defaults (owner 2026-07-04)
Distinct `MeasuredMotion` type · reconciliation metrics here / recalibration in #1360 · CSV reader.
