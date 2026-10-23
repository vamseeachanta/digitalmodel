# Spectral (Frequency-Domain) Fatigue
Estimates fatigue damage and life of an offshore detail (riser, hull, joint) directly from a stress response power spectral density (PSD) — no time-domain simulation or rainflow count.
Damage estimator: `dirlik` (default), `narrow_band`, `wirsching_light`, or `benasciutti_tovo`, against a DNV-RP-C203 S-N curve (`slope` m, `log_intercept` log10 a). Wraps the tested `digitalmodel.fatigue.spectral_fatigue` library.
Each location accumulates damage across its sea states weighted by `occurrence_fraction`; the workflow reports per-location fatigue life and a top-level `screening_status` (pass/fail vs design life × DFF) with the governing location.
Both example locations fail the required life of 75.0 years (design life multiplied by DFF). The splash-zone hot spot governs; the keel joint also falls below the criterion after correction of the Hz crossing rate and Dirlik coefficients. The illustrative PSD inputs remain unchanged.
Run with `uv run python -m digitalmodel examples/workflows/spectral-fatigue/input.yml`.

The independent arithmetic check integrates `m_n = integral(f^n S(f) df)` by the trapezoidal rule on the supplied Hz grid. For the cubic S-N slope, the analytic Dirlik range moment is `(2 sqrt(m0))^3 [6 D1 Q^3 + 3 sqrt(2 pi)/2 (D2 |R|^3 + D3)]`. Multiplying by `sqrt(m4/m2)`, dividing by the supplied S-N intercept in linear form, and converting seconds to a Julian year gives each full-occurrence annual damage. The splash-zone damage is weighted by the two supplied occurrence fractions; life is the reciprocal of accumulated annual damage.

| Location | Fatigue life (years) | Required life (years) | Result |
|---|---:|---:|---|
| Splash-zone hot spot | 4.1 | 75.0 | Fail |
| Keel joint | 70.3 | 75.0 | Fail |

Table 1. Independently calculated lives for the unchanged example inputs; these are arithmetic checks, not material qualification.

The workflow regression pins the unrounded calculations to a relative tolerance of `1e-6` to allow numerical rounding. A project assessment requires qualified stress spectra and S-N properties; both failing locations require reassessment and revised stress demand or detail design before the life criterion can be met.
