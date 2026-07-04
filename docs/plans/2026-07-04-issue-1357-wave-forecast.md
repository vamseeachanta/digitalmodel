# Plan: digitalmodel #1357 — wave_forecast: phase-resolved surface + physics predictable-zone (+ directional spreading)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1357
**Epic:** #1356 (last core child) · **Status:** plan-review · **Tier:** T2/T3 (Route B/C)

## Scope framing (read first — honesty boundary)
The genuinely hard part — reconstructing a phase-resolved surface from X-band radar / wave lidar and evolving it nonlinearly (HOS-Ocean) — is **partner/licensed research** (Next Ocean / Miros / the DSWP literature). #1357 does **NOT** reimplement it. #1357 delivers the *physics-honest* pieces the offering owns, behind the `WaveForecast` protocol #1358 defined:
1. a **principled predictable-zone horizon** (replaces the current hardcoded `horizon`),
2. **directional spreading** (replaces the current long-crested single-heading synthesis),
3. a first-class **phase-resolved surface-elevation evaluator** η(x,y,t),
4. a **sensing-agnostic ingest interface** (stub) — real reconstruction/HOS evolution is the partner front-end behind the same interface.

## Context
`wave_source.synthesize_forecast` (merged, #1358) is long-crested and takes `horizon` as a hardcoded argument (verified: wave_source.py:49-102 — single `heading`, `horizon` passed in). #1357 makes the wave forecast physics-honest. The synthetic generator stays as the deterministic test stub, now enriched.

**Stale-flag:** Not stale — final core child of the open epic.

## Reuse (verified)
- `models.WaveComponent` (omega, amplitude, phase, heading) / `WaveForecast` (components, horizon, origin_time, phase_reference_location, water_depth) — the protocol produced.
- `wave_source.jonswap_spectrum` (spectrum) + `reconstruct.wavenumber` (deep/finite-depth k). `models.GRAVITY`.
- `reconstruct.reconstruct_motion` already consumes components — no change; a richer/directional/physics-horizon `WaveForecast` flows through unchanged.

## Design (in `motion_forecast/wave_forecast.py` — cohesion with the protocol + reconstruct; NOT a new top-level package, see open-q 1)
- **Two honestly-named horizon functions** (the coherence proxy is NOT the DPZ — the name must say so):
  - `coherence_horizon(components, *, tau_min, tau_max, k_coh) -> float` — **indicative** bandwidth-limited coherence/decorrelation time. Spectral width `ν = sqrt(max(0, m0*m2/m1² − 1))` from the component set; `τ = k_coh / (ν * f_peak)`, clamped to `[tau_min, tau_max]`. **ν = 0 (monochromatic) → `tau_max`** (guard div-by-zero). Narrow-band → small ν → longer τ. Docstring states plainly: *this is a coherence-time proxy, NOT the Deterministic Predictable Zone.*
  - `dpz_horizon(components, *, aperture, energetic_fraction=0.9) -> float` — the **rigorous** group-velocity DPZ temporal width: `cg_i = 0.5*g/ω_i` (deep water) over the band carrying the central `energetic_fraction` of variance; `τ_DPZ = aperture * (1/cg_min − 1/cg_max)` (spread in group-arrival times across the measurement aperture; Naaijen 2018). "Predictable zone" naming reserved for this aperture-based path.
  - The synthesizer uses `coherence_horizon` by default and `dpz_horizon` when an `aperture` is supplied. Honest ceiling on both via the clamp: realistic short-crested ~tens of s to ~1–2 min; never the vendor "4–5 min".
- `directional_spread(headings, mean_heading, s) -> weights`: a cos-2s spreading function; energy-preserving (weights sum to 1).
- `synthesize_directional_forecast(hs, tp, *, mean_heading, spread_s, n_freq, n_dir, aperture=None, seed, ...) -> WaveForecast`: JONSWAP × directional spread → a 2-D (freq × direction) phased component set with `a_{ij} = sqrt(2·S_i·dω_i·w_j)` (`Σ_j w_j = 1` ⇒ variance preserved, no double-counting); horizon from `dpz_horizon` if `aperture` else `coherence_horizon`; seeded phases. **Reuses `wave_source`'s ω-grid** (`edges = linspace(0.4wp, 3.2wp, n_freq+1)`, midpoints) so the `n_dir=1` case matches long-crested amplitudes.
- `surface_elevation(forecast, x, y, t) -> float|array`: LWT η = Σ aᵢ cos(ωᵢ t + φᵢ − kᵢ (d̂ᵢ·(p − p_ref))). The free surface the reconstruct engine implicitly transfers — exposed for the demo + validation.
- `WaveFieldSource` protocol (`read() -> WaveForecast`) + `SyntheticWaveField` stub (from a sea state) + a documented seam where a real radar/lidar reconstruction or HOS evolution plugs in (**partner/licensed — out of scope**).

## Plan
1. `predictable_zone_horizon` (+ bandwidth proxy and group-velocity option) + tests.
2. `directional_spread` + `synthesize_directional_forecast` + tests (energy preservation, mean direction, long-crested reduction).
3. `surface_elevation` + tests (single-component closed form; consistency with the components at the reference location).
4. `WaveFieldSource` protocol + `SyntheticWaveField` stub + test.
5. Optional: workflow `sea.spread_s`/`aperture` wiring so the router can request a directional, physics-horizon forecast.

## Acceptance Criteria
- [ ] `coherence_horizon`: a narrow-band spectrum yields a **strictly longer** horizon than a broadband one, **using spectra whose τ lands inside `[tau_min, tau_max]`** (not on the clamp rails); monochromatic (ν=0) → `tau_max` (no div-by-zero); both clamped to the realistic band (never 4–5 min).
- [ ] `dpz_horizon`: equals `aperture*(1/cg_min − 1/cg_max)` over the energetic band to <1e-9 for a constructed two-component case (`cg = 0.5g/ω` verified).
- [ ] `directional_spread` weights sum to 1; `synthesize_directional_forecast` preserves variance (`4√m0 ≈ Hs`) across the 2-D set and recovers the mean heading. `n_dir=1` reproduces the long-crested `wave_source` result as **amplitude/heading/variance equivalence on the shared ω-grid** (phases equal only under matched seeding — not asserted).
- [ ] `surface_elevation`: single-component η matches `a·cos(ωt + φ − k·Δx)` to <1e-9; the value at `phase_reference_location` equals `Σ aᵢ cos(ωᵢ t + φᵢ)`.
- [ ] `SyntheticWaveField.read()` returns a valid `WaveForecast` that flows through `reconstruct_motion` unchanged.
- [ ] Existing #1358/#1359/#1367/#1360 tests still green; pure-numerical.

## Deferred / out of scope (documented — the partner boundary)
Real phase-resolved reconstruction from X-band radar / wave lidar; HOS-Ocean nonlinear evolution; data assimilation of a live sensed field. These are the licensed research front-end; #1357 provides the interface + LWT-grade synthesis/propagation only.

## Open questions (recommendations)
1. **Location:** `motion_forecast/wave_forecast.py` (**recommended** — cohesion with `WaveForecast`/`reconstruct`) vs a new top-level `wave_forecast/` package (epic's original framing)?
2. **Predictable-zone default:** bandwidth-coherence proxy as default + group-velocity DPZ optional (**recommended**), or require an aperture always?
3. **Directional model:** cos-2s spreading (**recommended**, standard) vs a configurable spreading function now?
