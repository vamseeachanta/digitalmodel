# Spectral (Frequency-Domain) Fatigue
Estimates fatigue damage and life of an offshore detail (riser, hull, joint) directly from a stress response power spectral density (PSD) — no time-domain simulation or rainflow count.
Damage estimator: `dirlik` (default), `narrow_band`, `wirsching_light`, or `benasciutti_tovo`, against a DNV-RP-C203 S-N curve (`slope` m, `log_intercept` log10 a). Wraps the tested `digitalmodel.fatigue.spectral_fatigue` library.
Each location accumulates damage across its sea states weighted by `occurrence_fraction`; the workflow reports per-location fatigue life and a top-level `screening_status` (pass/fail vs design life × DFF) with the governing location.
Both example locations fail the design life × DFF requirement of 25 yr × 3 = 75 yr. With Hz-consistent moments and Dirlik damage, the keel joint accumulates about 0.014225 damage/yr (70.30 yr life); the splash-zone hot spot accumulates about 0.24691 damage/yr (4.05 yr life) and governs the failure.
Run with `uv run python -m digitalmodel examples/workflows/spectral-fatigue/input.yml`.
