# Single-tank roll-amplitude findings — issue #1911

## Outcome

The nominal amplitude-conditioned reduction **inverts at 2° roll**: the coupled
peak is 9.0643 for 70% fill versus 9.1024 for 50% fill. The 50% fill remains
lower at 4° and 8°. However, the 2° difference is only 0.42% and changes sign
within the existing damping fitter's 0.005 ζ resolution. The four-case study
therefore does **not** establish that the 50%-fill ranking survives every
amplitude, but it also does not resolve the 2° ordering decisively. It plainly
shows that the published fill-only damping interpretation is unsafe.

## Change and safety

`scripts/cfd/run_sloshing_response_sweep.py` now accepts
`--roll-amplitude-deg`, threads it into case generation, records it in each
case specification, uses it in moment reduction, and writes it to the collected
manifest metadata. The default remains 4.0°, so commands without the new option
retain the previous solver setup and manifest amplitude.

`scripts/cfd/reduce_sloshing_sdof.py` also accepts an optional `--output` path.
Its default remains `docs/api/cfd/sloshing-sdof.json`; the option allowed this
study to re-run the published curve fit without overwriting that artifact.

Both changes were made test-first. The seven focused CLI contract tests pass.

## Cases run

The added cases used the existing forced-response resonances:

- h/L = 0.50: 1.12136 s, equal to `resonant_period_cfd_s`; the independent
  free-decay `natural_period_cfd_s` is 1.11713 s (0.38% lower).
- h/L = 0.70: 1.08720 s, equal to `resonant_period_cfd_s`; the independent
  free-decay `natural_period_cfd_s` is 1.08897 s (0.16% higher).

These are the existing ratio-1.00 cases. Each solve used OpenFOAM ESI v2312 and
ran only after `pgrep -x interFoam` returned no process. Solves were serial.

| Fill h/L | Roll | Period (s) | Runtime (s) | Wall run-up amplitude (m) | Quadrature coefficient | Moment amplitude (N·m) |
|---:|---:|---:|---:|---:|---:|---:|
| 0.50 | 2° | 1.12136 | 183.0 | 0.285271 | 2.15650 | 2.15682 |
| 0.70 | 2° | 1.08720 | 175.0 | 0.338700 | 2.77499 | 2.77756 |
| 0.50 | 8° | 1.12136 | 198.4 | 0.363559 | 1.41385 | 1.81771 |
| 0.70 | 8° | 1.08720 | 189.2 | 0.408536 | 2.34572 | 3.53340 |

The run-up, quadrature, moment, and runtime values above are measured/reduced
directly from the four new CFD cases.

## Equivalent damping and coupled response

Re-running `scripts/cfd/reduce_sloshing_sdof.py` reproduced the published 4°
wall-run-up curve fits: ζ = 0.180 at h/L = 0.50 and ζ = 0.305 at h/L = 0.70.
The original raw 4° case tree was not present on this machine, so its A44/B44
contract rows could not be regenerated; the committed five-point response
manifest was available and is what determines these ζ fits.

Only resonance cases were requested at 2° and 8°, not new five-period response
curves. Their equivalent ζ values are therefore **inferred**, not independently
curve-fitted. At resonance, the SDOF response used by the reducer scales as
roll amplitude divided by ζ. Using each fill's 4° curve fit as the calibration:

`ζ(A) = ζ(4°) × (A / 4°) × [run-up(4°) / run-up(A)]`.

The coupled peaks are grid-sampled approximations using the same Den Hartog
reduction as `scripts/cfd/reduce_tld_two_peak.py`: mass ratio μ = 0.05, tuning
ratio = 1.0, and frequency-ratio grid 0.75–1.30.

| Fill h/L | Roll | ζ | ζ basis | Coupled peak response | Coupled regime |
|---:|---:|---:|:---|---:|:---|
| 0.50 | 2° | 0.1153 | inferred from resonant run-up scaling | 9.1024 | two-peak split |
| 0.70 | 2° | 0.1755 | inferred from resonant run-up scaling | **9.0643** | single peak |
| 0.50 | 4° | 0.1800 | measured five-period curve fit | **9.1537** | single peak |
| 0.70 | 4° | 0.3050 | measured five-period curve fit | 13.5225 | single peak |
| 0.50 | 8° | 0.3618 | inferred from resonant run-up scaling | **15.8752** | single peak |
| 0.70 | 8° | 0.5819 | inferred from resonant run-up scaling | 25.1613 | single peak |

The measured resonance response is strongly non-proportional to roll amplitude,
consistent with amplitude-dependent nonlinear free-surface loss. Nominally,
70% is slightly better at 2°, while 50% is better at 4° and 8°. But the
published 4° ζ values come from a fitter that searches in 0.005 increments.
Propagating only the resulting ±0.0025 quantization through the 2° scaling makes
`peak(50%) - peak(70%)` range from about -0.032 to +0.113. Thus the nominal
inversion is not robust at the method's own resolution. “70% fill is
over-damped” remains conditional on excitation amplitude and the stated coupled
model assumptions; a full frequency sweep (or an uncertainty-aware refined fit)
is needed to settle the close 2° ranking.

## Recommended wording for PR #1877

> At the tested 4° roll amplitude, 50% fill gives the lowest coupled peak
> response of the three fills (ζ ≈ 0.18), while 70% fill (ζ ≈ 0.305) lies on the
> over-damped side of this specific coupled model. These are
> amplitude-conditioned equivalent damping values, not fill-only properties.
> Resonance checks at 2° and 8° show nonlinear amplitude dependence. The nominal
> reduction reverses the 50%/70% ranking slightly at 2° and returns to 50%-best
> at 8°, but the 2° ordering is unresolved within the damping fit's numerical
> resolution. A full frequency sweep at each amplitude is required before
> claiming a general best fill.
