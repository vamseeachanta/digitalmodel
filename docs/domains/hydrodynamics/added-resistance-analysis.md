# Added resistance in waves: screening analysis

## Summary

A synthetic tanker-form hull in head seas gives conditional reflection resistance
of 20.0 kN and 80.0 kN in two single wind seas.
The radiated-energy resistance is NOT_EVALUATED because its source equation and
heading validity could not be read. The numerical reflection result alone does
not represent total added resistance. Obtain the missing source and suitable
sectional motion/damping data before combining mechanisms or using the result
for design. No solver is run.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-22 | Initial sourced reflection screening and supplied-input workflow, with the radiated-energy source gap retained. |

## Inputs

All physical example inputs are **assumed, based on previous experience** and
chosen for transparent arithmetic. They are synthetic, uncalibrated inputs.
The bow length ends at the station with 95% of maximum waterline beam [1, G.1].

| Input | Value | Unit or basis |
| --- | --- | --- |
| Length between perpendiculars | 200.0 | m |
| Beam | 32.0 | m |
| Bow waterline length | 32.0 | m |
| Draught | 10.0 | m; recorded, unused by reflection formula |
| Water density | 1000.0 | kg/m³ |
| Gravity | 10.0 | m/s² |
| Calm speed | 10.0 | m/s |
| Incoming heading | 0.0 | degrees from bow; head seas |
| Propulsive efficiency | 0.5 | useful power / shaft power, held constant |
| Sea state A | Hs 1.0; Tp 4.0 | m; s |
| Sea state B | Hs 2.0; Tp 5.0 | m; s |
| Bow acceleration A; B | 0.1; 0.2 | m/s², stipulated over the speed bracket |
| Height basis | measured | synthetic measurement basis for the applicability gate |
| Short-wave confirmation | true | stipulated external assessment, not a prediction |
| Supplied heave and pitch RAOs | zero complex arrays | placeholders for the unevaluated force method only |

The calm-water curve supplies speeds 0.0, 2.0, 4.0, 6.0, 8.0 and 10.0 m/s,
with resistances 0.0, 4.0, 16.0, 36.0, 64.0 and 100.0 kN. Linear interpolation
is used between these samples; the code does not invent a hull curve. The
available shaft power at the calm speed is 2000.0 kW. Placeholder motion RAOs
do not qualify as a motion assessment; the bow-acceleration bound is separate
synthetic evidence. Real applications require consistent strip-theory RAOs.

## Methods and applicability

**Reflection:** STAWAVE-1 [1, Appendix G.1, equation G-1] gives
`R = rho*g*Hs²*B*sqrt(B/Lbow)/16`. This is an irregular-sea reflection
expression. It requires short head waves, limited heave and pitch, a bow
acceleration below 0.05 g and an incoming heading within ±45 degrees of the
bow. Section 6.3 limits Hs to `2.25*sqrt(Lpp/100)` for a measured spectrum
or `1.5*sqrt(Lpp/100)` for visual observations. Outside these gates the result
is NOT_EVALUATED, never a zero force. The implemented single wind-sea case
excludes swell and mixed directional seas. The code evaluates these method
gates; it does not certify compliance with the complete speed-trial procedure.
The source supplies no numerical short-wave wavelength boundary, so none is
invented: external confirmation is required. The synthetic confirmation here
illustrates conditional arithmetic and does not establish real-hull validity.

**Regular-wave table:** G-1 supplies no wavelength-dependent response.
A **constant spectral equivalent** is provided explicitly as `K = 8*C`, where
`C = rho*g*B*sqrt(B/Lbow)/16` and `R_regular = K*a²`. This follows from
`Hs²=16*m0` and `mean R=2*K*m0` [2, equation (19); repository wave_spectra.py].
It reproduces G-1 on spectral integration, but is not a frequency-resolved
short-wave reflection model. Using `Hs=2*a` in G-1 would give the wrong
variance conversion. Transfer is `R_regular/(rho*g*a²*B²/Lpp)`.

| Tp (s) | Wavelength (m) | Amplitude (m) | Wave frequency (rad/s) | Encounter frequency (rad/s) | Equivalent R (kN) | Transfer |
| --- | --- | --- | --- | --- | --- | --- |
| 4.0 | 101.9 | 1.0 | 0.8 | 1.4 | 160.0 | 3.1 |
| 4.0 | 3.4 | 1.0 | 4.3 | 23.0 | 160.0 | 3.1 |
| 4.0 | 1.0 | 1.0 | 7.9 | 69.5 | 160.0 | 3.1 |
| 5.0 | 159.2 | 1.0 | 0.6 | 1.0 | 160.0 | 3.1 |
| 5.0 | 5.3 | 1.0 | 3.5 | 15.4 | 160.0 | 3.1 |
| 5.0 | 1.6 | 1.0 | 6.3 | 45.8 | 160.0 | 3.1 |

Frequencies use deep-water `omega²=g*k`, `k=2*pi/lambda` [4] and the phase
Doppler shift `omega_e=omega+k*U*cos(beta)` for incoming headings from the bow.
Supplied RAOs use a common phase origin, `exp(i*omega_e*t)`, heave in m/m and
pitch in rad/m, with positive pitch raising the bow. Reader conventions must
be converted before supplying arrays; no reader or solver adapter is invoked.

**Radiated energy:** the force conversion, sectional damping correction and
heading limits from the 1972 method [5] remain unavailable. No force coefficient
is inferred. A separately labelled mechanical-work diagnostic accepts
sectional damping per unit length and complex vertical relative velocities:
`P = integral(b*abs(v)²/2 dx)` and `E_cycle = 2*pi*P/omega_e`.
The factor one-half is the exact cycle average of a squared sinusoid.
`v = a*(i*omega_e*(heave+x*pitch) - incident_velocity_rao)` follows by
rigid-body kinematics, with x forward from the common RAO origin. No
attenuation, effective-wave or forward-speed strip correction is supplied.
For the hand-check sections x = 0.0, 2.0, 4.0 m, damping = 2.0, 4.0, 2.0
N·s/m² and complex velocities = 1.0, 2.0i, -1.0 m/s, trapezoidal integration
gives 18.0 W and, at encounter frequency pi rad/s, 36.0 J per cycle. This
checks `E_cycle*omega_e/(2*pi)=P`; it does not validate the 1972 force method.

**Spectrum:** the existing repository two-parameter spectrum generator is reused
with explicit lower and upper cutoffs, avoiding its default band (#2613).
The existing finite band is normalized to Hs²/16; its omitted unnormalized
variance is disclosed separately using the analytic cumulative fraction
`F(w)=exp[-(5/4)*(wp/w)^4]` from the repository's documented two-parameter spectrum expression.
The lower and upper omissions sum to `F(w_min)+1-F(w_max)`. A frequency-dependent
force needs band and grid convergence in addition to this variance diagnostic.

| Tp (s) | Lower cutoff (rad/s) | Upper cutoff (rad/s) | Samples | Omitted variance (%) |
| --- | --- | --- | --- | --- |
| 4.0 | 0.8 | 7.9 | 2001 | 0.2 |
| 5.0 | 0.6 | 6.3 | 2001 | 0.2 |

The unrounded bands are `[wp/2,5*wp]`, using 2001 samples and `wp=2*pi/Tp`.
Mean force is `2*integral(S(omega)*R_regular(omega)/a² d(omega))` [2, (19)].
Integration is on absolute wave frequency, not encounter frequency. Unevaluated
rows, duplicate frequencies and response extrapolation are rejected. The
reflection response is constant, so this example tests the variance conversion;
it does not assess force sensitivity to a real frequency-dependent response.

**Propulsion:** added shaft power is `R_added*U/eta`; required efficiency is
held constant. The steady-state speed satisfies
`(R0(U)+R_added(U))*U/eta=R0(U0)*U0/eta`, following the constant-output
construction in [3, section 2.5.4, Figure 3] under this reduced assumption.
Bisection uses the supplied calm-water table and reevaluates added resistance
at each speed. The example stipulates reflection validity over the whole speed
bracket; full propulsion factors and changes in motion evidence are excluded.
Numerical controls are a speed tolerance of one hundred-millionth of a metre
per second and at most 100 iterations; these are numerical choices, not
physical acceptance criteria. Nonconvergence is NOT_EVALUATED.

## Results

| Hs (m) | Tp (s) | Direct reflection (kN) | Spectral equivalent (kN) | Radiated-energy resistance | Added shaft power at calm speed (kW) | Reduced speed (m/s) | Speed loss (m/s) |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1.0 | 4.0 | 20.0 | 20.0 | NOT_EVALUATED | 400.0 | 9.3 | 0.7 |
| 2.0 | 5.0 | 80.0 | 80.0 | NOT_EVALUATED | 1600.0 | 7.4 | 2.6 |

G-1 gives `C=1000*10*32/16=20000` N/m², hence reflection is 20.0 kN at
Hs 1.0 m and 80.0 kN at Hs 2.0 m. The equivalent coefficient is 160000.0 N/m².
At 10.0 m/s and efficiency 0.5 these forces add 400.0 kW and 1600.0 kW.
Independent quadratic solutions of the local linearly interpolated calm-water
power curves reproduce the iterated speeds in the tests. No numerical
comparison between two completed resistance methods is claimed.

## Later slices and way forward

- Obtain a readable public equation source for [5], including its damping
  definition, forward-speed correction, energy-to-force identity and heading
  range. Add the radiated-energy force calculation and a sourced hand example.
- Supply consistent sectional strip-theory damping and complex heave/pitch RAOs,
  with incident vertical velocities at the same sections and phase origin.
- Qualify the external short-wave and bow-acceleration evidence for each speed.
- A frequency-resolved short-wave method, directional spreading, swell, viscous
  effects, full propeller characteristics and validation against measurements
  remain outside this screening implementation. Do not add the reflected and
  radiated mechanisms without checking their definitions for overlap.

## Reproduce and verify

Run `PYTHONPATH=src python -m digitalmodel.hydrodynamics.added_resistance.example`
for the full input/result JSON, or append `--report` for this Markdown report.
The required runtime dependencies are those already used by hydrodynamics.
Tests under `tests/unit/hydrodynamics/added_resistance/` cover independent hand
calculations, validity boundaries, negative controls, finite-input validation,
nonconvergence and exact report reproduction. Existing spectrum regression
tests check that the reused module remains unchanged.

## Sources

1. [ITTC 7.5-04-01-01.1, Preparation, Conduct and Analysis of Speed/Power Trials](https://ittc.info/media/11972/75-04-01-011.pdf), 2024, revision 08, Appendix G.1, G-1 and Figure G-1; section 6.3. Public PDF read 2026-10-22; the equation and height limits were checked visually.
2. [ITTC 7.5-02-07-02.2, Prediction of Power Increase in Irregular Waves from Model Test](https://ittc.info/media/11890/75-02-07-022.pdf), 2024, revision 07, equation (19). Public PDF read 2026-10-22.
3. [ITTC 7.5-02-07-02.8, Calculation of the Weather Factor for Decrease of Ship Speed in Wind and Waves](https://ittc.info/media/11900/75-02-07-028.pdf), 2024, revision 02, section 2.5.4 and Figure 3. Public PDF read 2026-10-22.
4. Repository `hydrodynamics/capytaine/screening.py`, `finite_depth_wave_number` docstring: MIT Marine Hydrodynamics, 2005, Lecture 20, section 6.2.4, equation (6); deep-water limit used here. Repository `parametric_hull_analysis/forward_speed.py` supplies the phase-Doppler convention; the incoming-heading convention here reverses its sign.
5. [Analysis of the resistance increase in waves of a fast cargo ship, bibliography entry](https://ittc.info/media/11900/75-02-07-028.pdf#page=17), International Shipbuilding Progress 19, number 217, 1972, pages 285–293. Only the bibliography was confirmed in [3]; the underlying equation text was unavailable. A publisher access attempt returned HTTP 403; its identifier was not independently confirmed and is not cited. The open textbook route returned HTTP 503 and the institutional repository timed out. No numeric value is taken from these unavailable texts.
6. Repository `hydrodynamics/wave_spectra.py`, `WaveSpectra.pierson_moskowitz` and `_normalize_to_significant_height`: reused two-parameter spectrum shape and Hs normalization. The tail expression is an independent analytic integration of that documented formula; no new spectrum coefficients are introduced.
