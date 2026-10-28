# OrcaWave offline worked example

## Summary

Explicit illustrative inputs give beam wind and current forces of **588.0 kN**
and **922.5 kN**, and head-wind surge of **-735.0 kN** in the declared body frame.
Independent hydrostatic, statistical and spectral checks supplement these drag
calculations. This example checks arithmetic and conventions; it does not
qualify a design or establish native solver agreement.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-22 | Add explicit inputs and hand calculations to distinguish numerical acceptance from unavailable qualification evidence. |

## Inputs and method

The following inputs are **assumed, based on previous experience** and selected
for the illustrative acceptance cases in issue #2748, sections 14, 15 and 19.
They are required explicitly in the new physical-value tests. None is a
coefficient calibrated for a particular hull.

| Quantity | Wind | Current |
| --- | --- | --- |
| Density, kg/m3 | 1.225 | 1025 |
| Speed, m/s | 20 | 1 |
| Beam drag coefficient | 0.8 | 1.2 |
| Projected area, m2 | 3000 | 1500 |
| Heading FROM, clockwise from bow | 90 degrees | 90 degrees |

The frame is +x forward, +y port, +z up. Head wind is FROM 0 degrees and
pushes in -x. The head-wind surge coefficient is explicitly 1.0.
The producer is `drift_forces.compute_wind_current_drift`.

[NASA, Drag Equation (2025)](https://www1.grc.nasa.gov/beginners-guide-to-aeronautics/drag-equation/)
defines force magnitude `F = rho * V² * Cd * A / 2`. Applying that equation:

- Beam wind: `1.225 * 20² * 0.8 * 3000 / 2 / 1000 = 588.0 kN`.
- Beam current: `1025 * 1² * 1.2 * 1500 / 2 / 1000 = 922.5 kN`.
- Head wind: `-1.225 * 20² * 1.0 * 3000 / 2 / 1000 = -735.0 kN`.

Yaw is an explicitly assumed lever-arm calculation, `Mz = rx * Fy`.
The retained package assumption is `rx = 0.1 * L`; with assumed `L = 250 m`,
`rx = 25 m`. This gives wind/current yaw moments of **14700.0 kN m** and
**23062.5 kN m**. The empirical arm has no confirmed published yaw-model
basis here: its physical validation is **NOT_EVALUATED**. The tests check
moment arithmetic only.

For hydrostatics, assume displaced volume `V = 10000 m3`, waterplane area
`Awp = 3000 m2`, transverse/longitudinal metacentric heights `GM = 2, 100 m`,
water density `rho = 1025 kg/m3`, and gravity `g = 9.81 m/s2`.
`hydro_coefficients.create_hydrostatic_restoring` reduces the hydrostatic
restoring equations about principal axes through the centre of flotation.
The governing source is [WAMIT 7.5 manual, section 3.1](https://www.wamit.com/manual7.x/html/images/wamit_v75manualch3.html).

- `C33 = rho*g*Awp = 1025*9.81*3000 = 30165750.0 N/m`.
- `C44 = rho*g*V*GMt = 1025*9.81*10000*2 = 201105000.0 N m/rad`.
- `C55 = rho*g*V*GMl = 1025*9.81*10000*100 = 10055250000.0 N m/rad`.

A second explicit-input case, `rho = 1000 kg/m3`, `g = 10 m/s2`, gives
`C33 = 30000000.0`, `C44 = 200000000.0`, `C55 = 10000000000.0` in the same
units. It detects an implementation that ignores the supplied physical inputs.
The relative assertion tolerance is the issue's numerical acceptance of
`1e-12`; it is not a physical uncertainty estimate. Coupling, including
`C35/C53`, is omitted. Asymmetric input matrices remain available for diagnosis
but excessive asymmetry is refused by `qa_gates.check_hydrodynamics`.

## Statistical and spectral calculations

The mathematical derivations are recorded in
[engineering-citations.md](engineering-citations.md), R1/R2/W2/W3, and the
corresponding test docstrings. Existing value tests already check the corrected
maximum and long-term formulas; they are retained.

| Producer | Independent calculation | Hand and program result, rounded |
| --- | --- | --- |
| `motion_statistics.compute_response_spectrum`, `short_term_statistics` | RAO=2, wave density=1/4 on omega=1,2 rad/s: response density=1; trapezoidal m0=(2-1)(1+1)/2=1; single=2 sqrt(m0), double=4 sqrt(m0) | RMS 1.0; single 2.0; double 4.0 |
| `motion_statistics.short_term_statistics` | Transform Rayleigh X to exponential Y=X²/(2 sigma²); E[max Y]=H_N; expanding sqrt about ln(N) gives sigma*(sqrt(2 ln N)+Euler/sqrt(2 ln N)) | For sigma=1, N=1000: most probable maximum 3.7, expected maximum 3.9; unrounded tests use relative 1e-5 |
| `motion_statistics.long_term_extreme` | Expected exceedances=R_seconds*sum[p_i/Tz_i*exp(-x²/(2 sigma_i²))]=1 | One cell, sigma=1,Tz=10 s,100 Julian years: 6.3; existing unrounded tests also cover a two-cell mixture |
| `motion_statistics.motion_sickness_incidence` | Arithmetic only: weighted acceleration 0.5 m/s2, exposure 2 h; dose=0.5 sqrt(7200); MSI=dose/3 | Dose 42.4; MSI 14.1%; licensed clause NOT_EVALUATED |
| `wave_spectrum.jonswap` | Angular spectrum ordinates from the closed form, without grid rescaling | Existing wide-grid ordinate assertions pass |
| `wave_spectrum.issc_spectrum` | For S=C omega^-5 exp(-B/omega^4), substitution u=B/omega^4 gives m_n=(C/4)B^((n-4)/4)Gamma(1-n/4); hence Tp/Tz=(5*pi/4)^(1/4) | Period ratio 1.4; unrounded test and absolute ordinates pass |

The JONSWAP source is **DNV-RP-C205, April 2014, sections 3.5.5.2–3.5.5.3**,
confirmed in the [public institutional copy](https://www.docenti.unina.it/webdocenti-be/allegati/materiale-didattico/584574).
The normalizing factor is approximate, rather than an exact finite-grid height
constraint. The ISSC period ratio cancels overall scale, so a separate absolute
ordinate test detects altered normalization. Test tolerances are numerical
acceptance choices and do not establish applicability of a sea-state model.

## Executable evidence and limits

Run `tests/orcawave/` with pytest and `-rA`, saving its output outside the
checkout. Then run:

```text
PYTHONPATH=src python scripts/validation/audit_orcawave_sections.py --pytest-log <scratch>/tests.log
```

For a baseline inventory, also pass `--expected-ref origin/main` before changing
the implementation or tracked tests. It verifies that those files equal the
requested ref; it does not switch branches. The caller must supply the log from
that tree. Missing, failing, skipped or expected-failing acceptance evidence
does not establish FIXED status.

The new value tests are in `tests/orcawave/test_audit_acceptance_values.py`.
Deliberate mutations of drag densities, unit conversions, hydrostatic density
and gravity, JONSWAP/ISSC shape constants and grid rescaling were detected.
Every mutation was reverted before the final suite. Default-contract tests
detect changed defaults; explicit-input cases detect incorrect use of supplied
values and intentionally remain independent of defaults.

Remaining qualification limits:

- ISO 2631-1:1997 Annex D clause confirmation is **NOT_EVALUATED**. The existing
  citation sidecar and computational-reference status remain; arithmetic is
  not a standard-conformance assertion. Sections 3 and 13 remain OPEN.
- The published two-peak parameterization is **NOT_EVALUATED**. The attempted
  institutional paper address returned HTTP 404. `heuristic_two_peak` remains
  an assumed screening model; its deprecated callable alias warns explicitly.
  The high-level published spectrum name still raises.
- Section 17 remains OPEN: four synthetic entries fail the proposed pitch-slope
  band at 25 s. Strict expected-failure tests retain the requested criterion.
  A fixed finite period is not universally the long-wave limit relative to each
  natural period; no cap is applied to force the criterion to pass. Displacement
  consistency and synthetic flags are tested for every entry.
- Native loading, native hydrostatics, irregular-frequency evidence and native
  comparisons remain **NOT_RUN**. No licensed solver is required.
- This example adds numerical evidence, not a replacement module report.
