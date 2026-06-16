# Spectral (Frequency-Domain) Fatigue
Estimates fatigue damage and life of an offshore detail (riser, hull, joint) directly from a stress response power spectral density (PSD) — no time-domain simulation or rainflow count.
Damage estimator: `dirlik` (default), `narrow_band`, `wirsching_light`, or `benasciutti_tovo`, against a DNV-RP-C203 S-N curve (`slope` m, `log_intercept` log10 a). Wraps the tested `digitalmodel.fatigue.spectral_fatigue` library.
Each location accumulates damage across its sea states weighted by `occurrence_fraction`; the workflow reports per-location fatigue life and a top-level `screening_status` (pass/fail vs design life × DFF) with the governing location.
The example keeps a riser keel joint healthy and drives the splash-zone hot spot to a fatigue-governed failure.
Run with `uv run python -m digitalmodel examples/workflows/spectral-fatigue/input.yml`.
