# Wave spectrum moments and deep-water power

The default upper angular frequency scales with peak period so that short-period
seas retain the tail needed for the second spectral moment and zero-crossing
period. The spectrum remains normalized to the specified significant height.
Energy period and incident deep-water power are available from the same samples.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-22 | Document the peak-scaled grid and period and power calculations to address tail truncation. |

## Equations and sources

The public [Orcina wave spectra documentation](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Waves,Wavespectra.htm)
(online edition, accessed 2026-10-22), sections **ISSC spectrum** and **JONSWAP
spectrum**, gives the two-parameter spectrum and peak enhancement. With
`wp = 2*pi/Tp` and conversion `S(w) = S(f)/(2*pi)`, the base shape is

```text
S(w) = (5/16)*Hs^2*wp^4*w^-5*exp(-(5/4)*(wp/w)^4).
```

JONSWAP multiplies this shape by
`gamma^exp(-(w/wp-1)^2/(2*sigma^2))`. The
[Orcina parameter documentation](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Environment,DataforJONSWAPandISSCspectra.htm)
(same online edition and access date), **Spectral width parameters**, supplies
sigma below and above the peak. The implementation retains its existing
peakedness default and normalizes the sampled shape numerically, rather than
using an approximate analytic JONSWAP normalization factor.

[WEC-Sim Theory](https://wec-sim.github.io/WEC-Sim/main/theory/theory.html)
(online edition, accessed 2026-10-22), **Irregular Waves**, defines moments in
ordinary frequency and `Hm0 = 4*sqrt(m0)`. For angular-frequency moments used by
this API, `m_n = integral(w^n*S(w) dw)`. Converting between conventions gives

```text
Tz = 2*pi*sqrt(m0/m2)
Te = integral((2*pi/w)*S(w) dw)/m0 = 2*pi*m_minus1/m0.
```

The first is the spectral zero-crossing period; the second follows directly by
energy-weighting the component period. WEC-Sim **Irregular Wave Power** gives
`P = rho*g*integral(S(f)*cg(f) df)`. Its **Regular Wave Power** group-velocity
equation in the deep-water limit gives `cg = g/(2*w)`, hence

```text
P = rho*g^2*m_minus1/2 = rho*g^2*Hs^2*Te/(64*pi).
```

This is incident power per crest width in W/m. It assumes linear waves and deep
water. Density and gravitational acceleration are required inputs. It does not
predict extracted power or apply finite-depth, directional or current effects.
These equations are derived from the opened public documentation; conformity to
DNV-RP-C205 is **NOT_EVALUATED**, because an authorized edition was not available.
No value from that standard is used as a new acceptance criterion.

## Grid error budget

The engineering numerical choices are an upper limit of `20*wp` and a default
of 1000 uniformly spaced samples, retaining the existing lower bound. Explicit
bounds and sample counts remain supported, including reproduction of the former
grid. The cutoff is justified by the following hand integral, rather than an
empirical period correction.

For Pierson-Moskowitz, set `A = (5/16)*Hs^2*wp^4`, `B = (5/4)*wp^4` and `u = B/w^4`. Then

```text
m_n = A/4 * B^((n-4)/4) * Gamma(1-n/4), n < 4
    = Hs^2/16 * wp^n * (5/4)^(n/4) * Gamma(1-n/4).
```

At `w > K*wp`, bounding the exponential by unity bounds the missing fraction of
m2 by `2*sqrt(5/4)/(sqrt(pi)*K^2)`. At the chosen cutoff this is less than 0.4 %
(rounded upward). The corresponding Tz increase is less than 0.2 % before
quadrature effects. The missing m0 fraction is bounded by `(5/4)/K^4`.
Numerical Hs normalization restores m0 but cannot restore the missing m2.

The regression budget of 0.5 % allows for that tail and quadrature. It is a
numerical test tolerance, not a design limit. The sample count resolves the
JONSWAP peak as well as the extended band for the tested periods and peakedness.
Tests cover the reported short-period range, a longer-period Pierson-Moskowitz case and two
enhanced JONSWAP shapes. Expected enhanced moments use an independent composite
Simpson integral after `y = wp/w`; its derivation and quadrature choices are
written in the test docstring. No expected value is obtained from this API.

A caller requesting fewer samples must establish convergence. Neither this
finite cutoff nor any other finite cutoff makes m4 converge for an ideal
`w^-5` tail: m4 diverges logarithmically. The returned m4 and spectral-width
parameter therefore describe the selected finite band. Their values can change
substantially when the upper bound changes.

## API and compatibility

- `WaveSpectra.jonswap`, `pierson_moskowitz`, `bretschneider` and `issc` accept
  `freq_max=None` to select the peak-scaled upper limit.
- `WaveParameters` resolves its optional upper bound at construction, so its
  serialized bounds, `frequency_array()` and `generate_spectrum()` agree.
- `energy_period_from_spectrum(omega, density)` returns seconds. A zero-energy
  spectrum has undefined energy period and returns NaN.
- `deep_water_wave_power(omega, density, water_density=..., gravity=...)` returns
  W/m; zero energy gives zero power. Supply finite positive fluid properties.
- `spectrum_statistics()` adds `m_minus1` and `Te_s`; existing keys remain.
- Arrays passed to moment and statistic methods must be matching, finite,
  non-negative spectra on a strictly increasing positive angular-frequency grid.
  These methods integrate the supplied band and do not extrapolate a tail.

ISSC retains its existing approximate mean-period formulation. This change fixes
its grid but does not recalibrate that formulation. The existing approximate
`WaveParameters.zero_crossing_period` property also remains; use the integrated
spectrum method when the actual spectral Tz is needed.

## Worked example

Run from the repository root:

```sh
PYTHONPATH=src python examples/domains/hydrodynamics/wave_spectra_tail.py
```

Inputs are synthetic: Hs = 4.0 m, Tp = 6.0 s, density = 1000.0 kg/m³ and
gravity = 10.0 m/s². They deliberately simplify the independent hand calculation;
they are not recommended environmental values. With `m0 = 1`, the formula above
provides m1, m2 and m(-1); the period and flux equations then give the answer.
All table values are rounded to one decimal place; tests compare unrounded values.

| Method | m0 (m²) | m1 (m²/s) | m2 (m²/s²) | Tz (s) | Te (s) | P (kW/m) |
| --- | --- | --- | --- | --- | --- | --- |
| Former fixed cutoff | 1.0 | 1.2 | 1.6 | 5.0 | 5.4 | 43.0 |
| Peak-scaled cutoff | 1.0 | 1.4 | 2.2 | 4.3 | 5.1 | 40.9 |
| Infinite Pierson-Moskowitz integral | 1.0 | 1.4 | 2.2 | 4.3 | 5.1 | 40.9 |

## Caller audit

Repository search covered direct imports, package re-exports and generator calls.
This is a source audit; it does not qualify downstream engineering results.

| Consumer | Frequency handling and implication |
| --- | --- |
| Hydrodynamics CLI and hull catalog | Construct default `WaveParameters`; inherit the wider grid. Catalog interpolation clamps RAOs beyond their supplied band, requiring engineering review of tail coverage. |
| RAO spectral fatigue workflow | Uses generator defaults; inherits the wider grid. Stress RAOs are clamped outside their input band, so results require review against available RAO coverage. |
| Parametric fatigue generation | Uses generator defaults with a frequency-independent stress gain; inherits the wider grid. Fourth-moment fatigue results and stored atlases require regeneration and review. |
| Scatter fatigue | Supplies all bounds and count from the response grid; retains its explicit band. |
| Seakeeping operability | Supplies bounds and count from the RAO frequencies; retains its explicit band. |
| Operation envelope | Supplies its own peak-scaled bounds and count through `envelope_numerics`; retains its explicit band. |
| Mooring workflow | Supplies its own bounds and count; retains its explicit band. |
| Durable wave-spectrum workflow | Requires an explicit configured grid; retains that grid. |
| Marine-analysis profiling helper | Supplies count but inherits the new upper bound; coarse resolutions require convergence checks. |
| Visualization and legacy scripts and examples | Some import `WaveSpectra` but use older spectrum interfaces; no active default-generator call was found in those paths. Repairing obsolete interfaces is separate work. |
| Existing spectrum tests | Grid-size assertions follow the new default. Finite-band moment snapshots explicitly request the former grid. Coverage and width assertions account for the extended tail. |

Later work includes finite-depth power, calibrated ISSC equivalence, RAO-band
policy, convergence requirements for user-specified coarse grids and requalification
of saved fatigue outputs. No licensed solver was started or queried.
