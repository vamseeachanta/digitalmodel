# Plan: digitalmodel #1360 — learning loop (forecast-vs-measured skill + recalibration + engineering feedback)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1360
**Epic:** #1356 · **Status:** plan-review · **Tier:** T3 (Route C — analytics + a fitted correction; extends #1367's reconcile)

> Adversarially reviewed (2026-07-04): APPROVE-WITH-CHANGES. Blocker folded in — `overlap_error` discards residuals, so #1360 **adds `overlap_residuals` to `reconcile.py`** (edits #1367's module ⇒ #1367 must be merged first) and refactors `overlap_error` onto it. Statistical should-fixes (pooled residuals vs correlation, noise-only/identity, phase-vs-amplitude caveat, zero-variance guard) folded in.

## Context

#1367 (merged) reconciles one forecast vs one measured record. #1360 closes the loop across **many** pairs: aggregate skill (how it degrades with lead time), fit a correction that provably reduces held-out error, and roll up an operability-fraction feedback number. Makes the offering *improve*. Not stale.

**Depends on #1367 merged** — imports (and extends) `reconcile.py`; uses `MotionForecast`/`MeasuredMotion`.

## Reuse + the one edit to #1367's module
- Reuse `MotionForecast` (`.t` absolute, `.origin_time == t[0]` by construction, `.dof`), `MeasuredMotion` (`.now = t[-1]`), `models.DOF_NAMES`, `decision.DecisionState`.
- **Add `reconcile.overlap_residuals(measured, forecast, *, min_overlap_samples=3) -> (tq_abs, {dof: residual = measured − forecast_resampled})`** and refactor `overlap_error` to compute `rmse=sqrt(mean(err²))`, `bias=mean(err)` from it (behaviour unchanged; residuals now recoverable). This is the blocker fix — `error_vs_lead_time`/`aggregate_skill` are impossible on the current aggregate-only return.

## Design (under `src/digitalmodel/motion_forecast/`)
- `skill.py`:
  - `SkillRecord{forecast, measured}` — validator enforces a non-empty overlap on the shared clock.
  - `error_vs_lead_time(records, dof, n_bins) -> {lead_bin_center: rmse}` — for each record, `lead = tq − forecast.origin_time` (≥0, verified); **pool residuals** across records into lead bins; RMSE per bin (the honest skill-decay curve).
  - `aggregate_skill(records) -> Dict[dof, {rmse, bias, correlation}]` — **RMSE & bias pool** residuals across records (correct for unequal-length overlaps; not a mean-of-RMSEs). **Correlation does NOT pool** (concatenating records with different DC offsets is a Simpson artifact): demean each record before pooling *or* report the per-record correlation distribution (median + IQR). Plan chooses: report pooled RMSE/bias + per-record correlation median/IQR.
- `recalibrate.py`:
  - `@dataclass Correction{gain: Dict[dof,float], bias: Dict[dof,float]}`; `apply(forecast) -> MotionForecast` (`x_corr = gain·x + bias`).
  - `fit_correction(records) -> Correction` — per-DOF least squares `gain = cov(fc,m)/var(fc)`, `bias = mean(m) − gain·mean(fc)` over pooled overlap residuals. **Zero-variance guard:** when `var(fc) ≈ 0`, fall back to bias-only (`gain=1`), mirroring `overlap_error`'s `std>0` guard.
  - Before/after report on a **held-out split** returns **both RMSE and correlation** per DOF.
  - **Caveat (documented):** an affine correction captures only systematic **amplitude scaling + DC bias**, NOT the phase decorrelation that dominates lead-time skill decay. RMSE↓ alone is *not* evidence of a better forecast (a shrinking `gain` regresses toward the mean); that is why correlation is reported alongside. This is an **output-space** correction, explicitly NOT RAO-model recalibration (deferred).
- `feedback.py`:
  - `operability_summary(states: Sequence[DecisionState], *, marginal_operable=False) -> {operable_fraction, downtime_fraction, longest_operable_run, waiting_on_weather_fraction}`. `operable = (state is GO)` (or GO+MARGINAL when `marginal_operable`); `waiting_on_weather_fraction = 1 − operable_fraction`; `longest_operable_run` in samples. Feeds field-development economics.
- `workflow.router` — optional `cfg['motion_forecast']['skill']['records']` (in-memory `SkillRecord`/pairs) → emit aggregate skill (+ optional fitted-correction before/after summary). Additive; existing workflow tests stay green.

## Plan
1. `reconcile.overlap_residuals` + refactor `overlap_error`; assert `overlap_error` outputs unchanged (regression).
2. `skill.py` + tests (pooled-residual RMSE ≠ mean-of-RMSE on unequal overlaps; lead-time bins recover an injected lead-dependent error; correlation reported per-record not pooled-naively).
3. `recalibrate.py` + tests (recover injected affine gain/bias <1e-6; held-out RMSE reduced *on affine-structured* error; near-identity + not-worsened on noise-only; zero-variance DOF → bias-only; correlation reported).
4. `feedback.py` + tests (operable fraction, WoW, longest run on a known state sequence; marginal handling).
5. Workflow wiring + test.

## Acceptance Criteria
- [ ] `overlap_error` outputs byte-for-byte unchanged after the `overlap_residuals` refactor (regression test).
- [ ] `error_vs_lead_time` recovers a known injected lead-dependent error curve (bins increase as designed).
- [ ] `aggregate_skill`: RMSE/bias pool correctly (differs from mean-of-per-record-RMSE on unequal overlaps — tested); correlation reported per-record (median/IQR), not a naive pooled scalar.
- [ ] `fit_correction` recovers injected `gain,bias` <1e-6; **on affine-structured error** held-out RMSE is reduced; **on noise-only** correction ≈ identity and held-out RMSE not worsened beyond tolerance; **zero-variance forecast DOF** → `gain=1` bias-only (no blow-up). Before/after reports RMSE **and** correlation.
- [ ] `operability_summary` returns correct operable/downtime/WoW/longest-run on a known `DecisionState` sequence, with `marginal_operable` honoured.
- [ ] `router` emits `skill` when records supplied; existing #1358/#1359/#1367 tests still green.
- [ ] Pure-numerical; no hardcoded thresholds.

## Deferred / out of scope (documented)
Online data assimilation (EnKF live re-anchoring) — seam only. RAO-model (hydrodynamic-coefficient) recalibration — this fits output space, not coefficients. Deep weather-downtime stats beyond the operable/WoW proxy. Frequency-dependent correction (fast-follow to the affine one).

## Open questions — resolved to recommended defaults (owner pattern)
Per-DOF affine correction · operable-fraction/WoW proxy now · in-memory `SkillRecord` batch.
