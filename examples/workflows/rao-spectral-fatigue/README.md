# Spectral Fatigue from a Stress RAO

Estimates frequency-domain fatigue damage and life of an offshore detail (riser, hull, joint) from a **stress transfer function (RAO)** `H(f)` and a **parametric wave spectrum**, rather than a precomputed stress PSD.

The stress response PSD is formed by the standard linear transfer relation `S_stress(f) = |H(f)|² · S_wave(f)` (MPa²/Hz), where `S_wave(f)` is generated from `(Hs, Tp)` by the in-repo DNV-RP-C205 wave spectra (`jonswap` default, `pierson_moskowitz`, `bretschneider`) and `H(f)` is interpolated onto the spectral grid. The resulting stress PSD is handed to the tested `spectral_fatigue` damage chain (spectral moments → `dirlik` / `narrow_band` / `wirsching_light` / `benasciutti_tovo` → DNV-RP-C203 S-N → damage accumulated over sea states by `occurrence_fraction`).

This generalises the fixed-gain screening already baked into the spectral-fatigue atlas (`parametric.generate` maps the wave spectrum to stress with a constant `stress_gain_MPa_per_m`); here the gain is the frequency-dependent RAO.

Each location reports per-location fatigue life and the workflow reports a top-level `screening_status` (pass/fail vs design life × DFF) with the governing location. The keel joint exceeds the required life of 75.0 years; the splash-zone hot spot falls below that criterion and governs the failure.

The stress RAOs in `input.yml` are **illustrative** — a real project supplies its own `H(f)` from a coupled hydrodynamic + structural model.

Run with `uv run python -m digitalmodel examples/workflows/rao-spectral-fatigue/input.yml`.

The independent arithmetic check evaluates the JONSWAP shape at 100 equally spaced angular frequencies from 0.02 to 2.0 rad/s and normalizes its trapezoidal area to `Hs^2/16`. Conversion to Hz multiplies the spectrum by `2*pi`. Linear interpolation of the supplied RAO, clamped at its endpoints, gives `S_stress = |H|^2 S_wave`. Trapezoidal Hz moments and the analytic Dirlik range integral described in the [stress-PSD example](../spectral-fatigue/README.md) give the following lives after occurrence weighting.

| Location | Fatigue life (years) | Required life (years) | Result |
|---|---:|---:|---|
| Splash-zone hot spot | 66.2 | 75.0 | Fail |
| Keel joint | 567.7 | 75.0 | Pass |

Table 1. Independently calculated lives for the unchanged illustrative RAOs and sea states, using the workflow's discrete wave grid.

The workflow regression pins the unrounded calculations to a relative tolerance of `1e-6` for numerical rounding. The splash-zone detail requires reassessment with qualified RAOs and revised stress demand or detail design before the life criterion can be met. These checks establish arithmetic agreement on the specified grid; convergence and material qualification are not established.

Reference: DNV-RP-C205 (wave spectra); DNV-RP-F204 / DNV-RP-C203 Sec. 4 (frequency-domain fatigue from a stress transfer function).
