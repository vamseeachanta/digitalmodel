# Envelope calculation basis

Revision 1, audit [2747](https://github.com/vamseeachanta/digitalmodel/issues/2747).
This calculation is linear spectral screening. Standards compliance is not
established for the catalogue motion limits or operational alpha factors.
No licensed-standard clause is asserted for these screening inputs.

## Governing equations

**B1 — response and significant amplitude.**
`S_response(omega) = |RAO(omega)|^2 S_wave(omega)`;
`m0 = integral(S_response d omega)`; `s = 2 sqrt(m0)`.
The existing package functions `compute_response_spectrum`, `spectral_moments`
and `significant_amplitude` implement this linear, narrow-band model.
The wave normalization is `m0_wave = Hs^2/16`. These definitions give
`s = gain * Hs/2` for constant gain; this identity is the test oracle.

**B2 — forecast criterion.**
`Hs_forecast_limit = alpha * Hs_ref * C / s(Hs_ref)` follows from B1's linear
response scaling. It is the alpha-reduced forecast criterion, not the design
limit. For unit gain, `Hs_forecast_limit = 2 alpha C`: transit 5.4 m,
station keeping 3.6 m, heavy lift 2.55 m, offloading 4.5 m. Doubling the
governing heave gain gives 2.7 m for transit. A 20 m display cap is a numerical
screening bound, not a standards-derived allowable; capped results carry
`status="capped"`, `governing_dof="cap"` and a null governing heading.

**B3 — spectral truncation and independent integral.**
With `x = omega / omega_peak`, `omega_peak = 2 pi/Tp`, the unit-integral
Pierson–Moskowitz shape is `q(x) = 5 x^-5 exp(-1.25/x^4)`.
Its antiderivative is `F(x) = exp(-1.25/x^4)`, because differentiating F
gives q. Thus the fraction below `8 omega_peak` is
`F(8) = 0.9996948708`; the upper tail is `1-F(8) = 0.0003051292`.
For the existing JONSWAP shape multiply q by
`gamma^exp(-(x-1)^2/(2 sigma^2))`, with sigma 0.07 below the peak and 0.09 above.
The enhancement at x=8 underflows to unity in double precision. The lower
tail below x=0.01 underflows to zero. The recorded pre-normalization captured
fraction is `integral(q_enhanced, .01, 8) / (integral + 1-F(8))`.
The existing JONSWAP generator still normalizes the retained spectrum to
`Hs^2/16`; the returned captured fraction exposes the discarded variance.

The 4097-point grid over `.01 <= x <= 8` is a numerical integration choice,
checked by the independent low-pass test, not a standard requirement.
For gamma=1, gain one below x=1 and zero above, the significant response is
`0.5 sqrt(F(1)/F(8))` at Hs=1 m. B2 gives `10.0873 m` for transit,
with relative acceptance tolerance 1%. This test distinguishes a frequency-
dependent response from the constant-gain normalization identity.

The upstream `WaveSpectra.jonswap` implementation names DNV-RP-C205 section
3.5.2; its edition and clause text remain unverified here. Public corroboration
of the JONSWAP shape parameters, gamma=1 equivalence and Hs normalization is
provided by the publisher's [spectral-parameter documentation](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Environment%2CDataforJONSWAPandISSCspectra.htm)
and [random-sea documentation](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Environment%2CSettinguparandomsea.htm).
No solver execution is required or represented by these analytical tests.

**B4 — RAO coverage.**
`extrapolated_energy_fraction = 1 - integral(S_wave, RAO_min, RAO_max) / integral(S_wave)`.
The integral clips to the calculation grid and interpolates spectral density
at the boundary cells. Every required DOF shall cover at least 99% of retained
wave variance. This is the explicit numerical screening tolerance selected
for audit section 4, not a DNV limit. Endpoint continuation is used only in
the remaining fraction, which is reported as the maximum across required DOFs.
Wave-energy coverage alone does not bound response error for unknown large
out-of-band resonances; qualified RAOs remain necessary for design acceptance.

**B5 — headings and dimensional contracts.**
`distance = abs((available - requested + 180) mod 360 - 180)`.
Substitution is limited to half the smallest observed heading step, including
the wrap step; a single heading supports exact requests only. Symmetry is not
inferred. Frequencies shall be finite, distinct, positive rad/s; headings shall
be finite degrees and unique modulo 360. RAOs shall be finite non-negative
arrays of shape `(n_frequency, n_heading)`, with translation m/m and rotation
deg/m. Units are rejected rather than converted. Square arrays are rejected
because an unlabelled transpose cannot be distinguished from the expected axes.
The caller shall supply a non-square grid. Motion limits require m or deg by
DOF, alpha in `(0,1]`, positive finite Tp/Hs/reference cap, gamma at least 1,
and non-empty criteria and calculation grids.

## Screening inputs

| Operation | Alpha (1) | Heave (m) | Roll (deg) | Pitch (deg) |
|---|---:|---:|---:|---:|
| Transit | 0.90 | 3.0 | 6.0 | 2.5 |
| Station keeping | 0.90 | 2.0 | 4.0 | 2.0 |
| Heavy lift | 0.85 | 1.5 | 2.0 | 1.5 |
| Offloading | 0.90 | 2.5 | 4.0 | 2.0 |

Table B6. Retained unverified screening values; no verified standard edition,
clause or alpha-table provenance. The result's `alpha_source` and
`citation_sidecar` carry this limitation for catalogue and custom operations.
The maintainer shall qualify inputs before design use; screening metadata
does not authenticate a project-specific alpha or standards citation.
