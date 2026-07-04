# Vessel Seakeeping Operability

Screens a vessel's seakeeping operability from its **motion RAOs** and a **sea state scatter**, without a time-domain simulation.

For each degree of freedom (heave, roll, pitch, ...), the motion response spectrum is formed from the DOF's motion RAO and a parametric wave spectrum (`jonswap` default, `pm`, `bretschneider`) by the standard linear transfer `S_response(ω) = |RAO(ω)|² · S_wave(ω)`. The significant motion amplitude `s₁/₃ = 2·√m0` is compared against the DOF's operability criterion across the sea-state scatter, weighted by occurrence probability, giving a per-DOF **operability percentage**. The vessel reports a top-level `screening_status` (pass/fail): it passes only if every DOF meets its `required_operability_pct`, with the governing (lowest-operability) DOF named.

This wraps the tested `digitalmodel.hydrodynamics.seakeeping` engine (`operability_analysis`, `compute_response_spectrum`, `spectral_moments`, `significant_amplitude`) rather than reimplementing it.

The motion RAOs and criteria in `input.yml` are **illustrative** — a real project supplies its own RAOs from a diffraction model. The example keeps heave and pitch operable and drives a roll resonance to fail operability.

Run with `uv run python -m digitalmodel examples/workflows/vessel-seakeeping/input.yml`.

Reference: PNA Vol III (Motions in Waves); DNV-RP-C205 (wave spectra).
