# Spectral Fatigue from a Stress RAO

Estimates frequency-domain fatigue damage and life of an offshore detail (riser, hull, joint) from a **stress transfer function (RAO)** `H(f)` and a **parametric wave spectrum**, rather than a precomputed stress PSD.

The stress response PSD is formed by the standard linear transfer relation `S_stress(f) = |H(f)|² · S_wave(f)` (MPa²/Hz), where `S_wave(f)` is generated from `(Hs, Tp)` by the in-repo DNV-RP-C205 wave spectra (`jonswap` default, `pierson_moskowitz`, `bretschneider`) and `H(f)` is interpolated onto the spectral grid. The resulting stress PSD is handed to the tested `spectral_fatigue` damage chain (spectral moments → `dirlik` / `narrow_band` / `wirsching_light` / `benasciutti_tovo` → DNV-RP-C203 S-N → damage accumulated over sea states by `occurrence_fraction`).

This generalises the fixed-gain screening already baked into the spectral-fatigue atlas (`parametric.generate` maps the wave spectrum to stress with a constant `stress_gain_MPa_per_m`); here the gain is the frequency-dependent RAO.

Each location reports per-location fatigue life and the workflow reports a top-level `screening_status` (pass/fail vs design life × DFF) with the governing location. The example keeps a riser keel joint healthy and drives the splash-zone hot spot to a fatigue-governed failure.

The stress RAOs in `input.yml` are **illustrative** — a real project supplies its own `H(f)` from a coupled hydrodynamic + structural model.

Run with `uv run python -m digitalmodel examples/workflows/rao-spectral-fatigue/input.yml`.

Reference: DNV-RP-C205 (wave spectra); DNV-RP-F204 / DNV-RP-C203 Sec. 4 (frequency-domain fatigue from a stress transfer function).
