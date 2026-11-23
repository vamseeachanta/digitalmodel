# Plan: digitalmodel #1358 — motion_forecast keystone (time-domain 6-DOF motion reconstruction via RAO)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1358
**Epic:** #1356 · **Status:** plan-review · **Tier:** T3 (Route C — new numerical module; phase-preserving reconstruction; domain-convention work; validated against an existing tested engine)

> This plan was adversarially reviewed against the codebase (2026-07-03). The review returned **REWORK** on a first draft; the blockers below are folded in: correct registration via `engine.py` (not `registry.yaml`); the incident-wave **spatial-phase transfer** term; the fact that **no AQWA/OrcaFlex phase-convention normalisation exists** and must be authored; the `UnifiedRAOData`↔interpolator impedance mismatch; a phase-catching validation; and Task 6 rescoped as net-new (there is no ready "socket").

## Context

Epic #1356 turns a phase-resolved wave forecast into an *asset-motion* forecast + rolling go/no-go. This is the **keystone**: the engine that reconstructs the **time-domain 6-DOF motion** of a specific asset a short horizon ahead, from a *phased* incident-wave forecast **propagated to the asset location** and transferred through that asset's RAO (amplitude **and** phase).

**Why net-new (verified).** `hydrodynamics/seakeeping.py::compute_response_spectrum` gives spectral response *statistics* (`S_resp = |RAO|²·S_wave`, phase-blind). No phase-preserving time-domain reconstruction exists (`grep -r motion_forecast src/` = nothing). `vessel_seakeeping/workflow.py` and `rao_spectral_fatigue/workflow.py` are the workflow-wrapping precedents but stay statistical.

**Stale-flag:** Not stale — new capability under an open, owner-approved epic.

**Upstream dependency.** The natural source is #1357 `wave_forecast/` (phased surface + predictable-zone), not yet built. This issue defines a small **`WaveForecast` protocol** — carrying not just `{ω, a, φ, heading}` components but **`phase_reference_location` and the wave time/phase convention** (see Design) — plus an internal fallback synthetic generator so #1358 builds/tests standalone and #1357 slots in behind the protocol. **Open item:** confirm with the #1357 owner that a discrete-component representation (vs a gridded η(x,t) surface / directional wavenumber spectrum) is acceptable, else the protocol adapts.

## Reuse vs. net-new glue (verified against code)
- **RAO parsing:** `marine_ops/marine_analysis/unified_rao_reader.py` → `models/rao_data.py::UnifiedRAOData` (auto-detects AQWA `.lis`/`.AH1`/OrcaFlex YAML; `.displacement.surge` = `DOFData` with amplitude+**raw** phase in degrees).
- **⚠ Impedance mismatch (real glue, not thin):** `rao_interpolator.py::RAOInterpolator.interpolate_2d` consumes a *different* shape — `rao_processor.py::RAOData` (`.raos` dict, **mutable** `.metadata` dict), not `UnifiedRAOData`. The adapter must either translate shapes or call the lower-level `_interpolate_1d_complex` directly. Budget this.
- **⚠ No phase-convention normalisation exists:** both `parsers/aqwa_lis_parser.py` (`'phase': values[phase_idx]`) and `parsers/orcaflex_yml_parser.py` (`'phase': row[2]`) store phase raw; nothing reconciles AQWA vs OrcaFlex sign/lag/reference. The adapter must **define and enforce** the convention — domain work, not a wrapper.
- **Wave spectra (fallback generator):** `hydrodynamics/wave_spectra.py` (JONSWAP etc.).
- **Cross-check oracle:** `hydrodynamics/seakeeping.py::compute_response_spectrum` (energy/amplitude only — phase-blind; see validation caveat).
- **Post-stats:** `orcaflex/postprocessor.py` (peaks/envelopes; no OrcFxAPI).
- **Dispatch convention (corrected):** pure-numerical modules register via a `basename` branch in `src/digitalmodel/engine.py` (e.g. `vessel_seakeeping` at ~:295), **not** `usecase_registry/registry.yaml` (that file is licensed-solver dispatch — all 37 entries carry a `solver:`; the cited precedents aren't in it).

## Design

Package `src/digitalmodel/motion_forecast/`:
- `models.py` — `WaveComponent{omega, amplitude, phase, heading}`; `WaveForecast` (Protocol: `.components`, `.horizon`, `.origin_time`, **`.phase_reference_location (x,y)`**, **`.convention` {time_sign, heading_dir}**); `MotionForecast{t, dof6, point_of_interest_motion, derived}`.
- `conventions.py` — **first-class:** the wave time convention (e^{−iωt} vs e^{+iωt}), RAO lead/lag **sign per source** (AQWA vs OrcaFlex), and heading direction (going-to vs coming-from). Documented + enforced; this is the highest-risk unit under test.
- `wave_source.py` — fallback synthetic phased-sea generator implementing `WaveForecast` (seeded), with an explicit `phase_reference_location`. The seam #1357 replaces.
- `rao_adapter.py` — load `UnifiedRAOData`; normalise to the internal convention; expose `H(dof, omega, heading) -> complex`. Handles the interpolator impedance mismatch.
- `reconstruct.py` — **core.** For each component *i* at asset location **x_a**, propagate the incident phase from the reference **x_ref** via deep-water dispersion `k_i = ω_i²/g`:
  `θ_i(t) = ω_i t + φ_i − k_i·((x_a − x_ref)·d̂_i) + arg H_dof(ω_i, β_i)`
  `x_dof(t) = Σ_i a_i · |H_dof(ω_i, β_i)| · cos(θ_i(t))`
  Rigid-body **lever-arm** transfer to a point-of-interest (crane tip / gangway tip / deployment point): combine translations with rotations (deg→rad) at offset **r**. Velocities/accelerations by spectral differentiation (×ω, phase +90°/+180°) with a numeric cross-check.
- `workflow.py` — `router(cfg) -> cfg` following convention: **mutates and returns `cfg`** with the serialised MotionForecast under `cfg["motion_forecast"]` (mirrors `vessel_seakeeping/workflow.py`).

Horizon honored from `WaveForecast.horizon`; **fail-closed** beyond it.

**Explicitly OUT of scope (follow-on issue):** IRF/Cummins time-domain path (retardation kernel, added-mass-at-∞, convolution/state-space) — a separate workstream; not in this issue's acceptance.

## Plan

### Task 1 — Models + WaveForecast protocol (with reference location & convention) + fallback generator
`models.py`, `wave_source.py`. Deterministic synthesis; test variance matches parent spectrum (m0 ⇒ Hs).

### Task 2 — Conventions module + RAO adapter
`conventions.py` (define/enforce wave-time sign, RAO lead/lag sign per source, heading direction) + `rao_adapter.py` (consume `UnifiedRAOData`, resolve the interpolator impedance mismatch via low-level complex interp, return complex `H`). Per-source round-trip tests (AQWA `.lis`, OrcaFlex YAML) — see fixture caveat in Acceptance.

### Task 3 — Reconstruction core (with spatial-phase transfer + lever arm)
`reconstruct.py`. Long-crested first (single heading/component), then directional superposition. Includes the `−k·Δx` propagation term and rigid-body lever-arm transfer to a point-of-interest. Velocities/accels via spectral differentiation + numeric cross-check.

### Task 4 — Dispatch + config
Add `elif basename == "motion_forecast":` branch in `engine.py` importing `motion_forecast.workflow.router` (mirror `vessel_seakeeping`). YAML config schema under `base_configs/modules/`. (No `registry.yaml` entry — that's licensed-solver dispatch.)

### Task 5 — Validation suite (`tests/motion_forecast/`)
- **Regular-wave (exact) — the sole phase guard, strengthened:** single-frequency wave through a known analytic RAO reproduces motion amplitude **and signed phase** (incl. the `−kΔx` term at a non-zero offset). <1e-6.
- **Phase-sign-convention regression:** explicit test that a known AQWA-convention and OrcaFlex-convention RAO produce the physically same motion after `conventions.py` normalisation.
- **Irregular cross-check (phase-blind, made deterministic):** compare **analytic variance** `Σ½aᵢ²|Hᵢ|²` on the shared grid/interpolation against oracle `compute_response_spectrum` m0 — removes FFT quadrature + one-realisation sampling scatter (a naive FFT-m0 vs trapz comparison false-fails).
- **Energy/variance conservation** of the synthesis.
- **Horizon fail-closed** beyond the predictable zone.
- **engine.py dispatch** smoke test (basename routes to router; cfg returned with `motion_forecast` key).

### Task 6 — Point-of-interest motion path in seastate_limits (net-new, feature-flagged)
There is **no ready socket** — `subsea/rov/seastate_limits.py` is Hs-based screening; the "RAO-based response solver" line is a docstring aside. Add a **new** RAO-based path (new signature: vessel, RAO, spectrum/forecast, deployment point) that computes motion at the ROV splash-zone/A-frame point via the lever-arm transfer, behind a feature flag, leaving the existing Hs screen untouched. Scope honestly as additive.

## Acceptance Criteria
- [ ] `src/digitalmodel/motion_forecast/` with `models`, `conventions`, `wave_source`, `rao_adapter`, `reconstruct`, `workflow`.
- [ ] Regular-wave test: reconstructed amplitude **and signed phase** (with non-zero `Δx`) match analytic to <1e-6.
- [ ] Phase-sign-convention regression test passes (AQWA-conv vs OrcaFlex-conv → same physical motion post-normalisation). *If no matched-vessel fixture pair exists, author a minimal synthetic pair; do not claim cross-source validation from unrelated fixtures.*
- [ ] Irregular test: analytic variance `Σ½aᵢ²|Hᵢ|²` within ≤2% of oracle m0 on shared grid.
- [ ] `engine.py` `basename=="motion_forecast"` dispatch test green; `router` returns mutated `cfg` with `cfg["motion_forecast"]`.
- [ ] Horizon fail-closed enforced (test).
- [ ] `seastate_limits.py` new RAO path added without regressing existing Hs-screen tests.
- [ ] Full digitalmodel suite green on dev-primary (pure numerical + fixtures; no license).
- [ ] IRF/Cummins explicitly filed as a follow-on issue (not in this PR).

## Open questions (for owner)
1. **#1357 interface** — is a discrete `{ω,a,φ,β}` component list + `phase_reference_location` acceptable, or will #1357 emit a gridded η(x,t) / directional spectrum the protocol must accept?
2. **Directional handling** — long-crested keystone with directional spreading as a fast-follow (recommended), or spreading now?
3. **Water depth** — deep-water `k=ω²/g` for v1 (recommended), finite-depth parametrised behind the same interface?
4. **Convention source of truth** — adopt OrcaFlex's convention as internal canonical and normalise AQWA to it, or vice-versa? (Affects `conventions.py` sign tests.)
