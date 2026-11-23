# Plan: digitalmodel #493 — River / shallow-water current profile modeling

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/493
**Status:** plan-review
**Tier:** T3 (new submodule)
**Priority:** low

## Context

Existing hydrodynamics (`src/digitalmodel/hydrodynamics/`) covers ocean current loading via OCIMF (`ocimf_loading.py`) and DNV-RP-H103 surface profiles via `metocean/`. Towing operations in rivers and confined waterways have fundamentally different current physics — bank confinement, depth-dependent power-law profiles, tidal reversal, and seasonal flood scaling. None of this lives in digitalmodel today.

Issue #487 (towing/marine operations) is a sibling: river currents feed bollard-pull and drift-force checks. This plan delivers the current-profile primitives so #487 has a calling surface.

## Plan

1. **Create submodule skeleton.** New directory `src/digitalmodel/hydrodynamics/river_currents/` with `__init__.py`, `profiles.py`, `bank_effects.py`, `vessel_forces.py`, `extremes.py`, `visualization.py`. One fixture: `fixtures/mississippi_lower_baton_rouge.yml` (representative river: depth 12 m, surface velocity 2 m/s, channel width 600 m).

2. **Velocity profiles (`profiles.py`).** Implement two profile families: power-law `u(z) = u_surf * (z/H)^(1/n)` with `n` defaulting to 7 (USACE handbook), and exponential `u(z) = u_surf * exp(-k*(H-z)/H)`. Each as a class with `velocity_at(z)` and `depth_averaged()`. Add a `from_log_law(z0, ustar)` constructor for rough-bed conditions.

3. **Bank effects (`bank_effects.py`).** Confinement factor from Soulsby blockage correction: `Cd_eff = Cd_open * (1 + alpha * A_vessel / A_channel)`. Provide `apply_bank_correction(profile, vessel_beam, vessel_draft, channel_width, channel_depth)` returning the corrected centerline velocity at the vessel.

4. **Vessel forces (`vessel_forces.py`) and extremes (`extremes.py`).** `vessel_forces.compute_drag(profile, vessel)` integrates `0.5 * rho * Cd * A(z) * u(z)^2` over draft. `extremes.fit_weibull(time_series)` and `return_period_velocity(weibull_params, T_years)` for shallow-water extreme analysis. Wrap with citation hooks for USACE EM 1110-2-1100 / DNV-RP-C205 §3.4.

5. **Tests, visualization, worked example.** `tests/hydrodynamics/river_currents/` mirroring the four core modules with one happy-path test each. `visualization.plot_profile(profile, vessel)` produces a PNG showing velocity-with-depth + vessel outline (matplotlib, save to a tmp_path in tests). Worked-example notebook deferred to follow-up unless explicitly requested. Smoke: `uv run pytest tests/hydrodynamics/river_currents/ -x`.

## Acceptance Criteria

- [ ] `src/digitalmodel/hydrodynamics/river_currents/` submodule exists with 5 modules and one fixture
- [ ] Power-law and exponential profile classes implement `velocity_at(z)` and `depth_averaged()` and pass unit tests against analytic references
- [ ] Bank-correction factor matches Soulsby's blockage formula on a hand-checked case (within 2%)
- [ ] `compute_drag()` returns the analytic constant-current force when the profile is uniform (regression sanity)
- [ ] All tests pass: `uv run pytest tests/hydrodynamics/river_currents/ -x`
- [ ] Citation emitted for the profile family (USACE EM 1110-2-1100 or DNV-RP-C205)

## Open questions

1. Does #487 plan to consume `vessel_forces.compute_drag` directly, or via an adapter? If direct, add the function signature to its plan as a contract.
2. Tidal-reversal modeling — scope to a follow-up issue (signed velocity series + harmonic constituents) unless the immediate use case is intra-day operability windows.
