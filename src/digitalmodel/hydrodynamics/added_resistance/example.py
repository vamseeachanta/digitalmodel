# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reproducible synthetic tanker-form screening analysis, with no solver calls.

All hull, sea-state, motion, fluid, efficiency and calm-water inputs below are
assumed, based on previous experience, and deliberately chosen for independent
hand arithmetic. They are not measured or calibrated data. Grid resolution and
iteration controls are illustrative numerical choices, not acceptance limits.
"""

import argparse
import json
from dataclasses import asdict
from math import pi

from .energy import radiated_energy_table
from .models import OperatingPoint, RegularWaves, Ship, Status
from .propulsion import CalmWaterCurve, added_shaft_power, speed_loss
from .reflection import ReflectionEvidence, stawave1, stawave1_equivalent_regular_table
from .spectra import integrate_regular_table, pm_spectrum


def calculate(*, short_waves_confirmed: bool = True) -> dict:
    """Run two conditional reflection estimates with an explicit source-gap column."""
    # Synthetic arithmetic inputs, not empirical coefficients or design limits.
    ship = Ship(200, 32, 32, 10, 1000, 10)
    point = OperatingPoint(10, 0)
    efficiency = 0.5
    speeds = (0, 2, 4, 6, 8, 10)
    curve = CalmWaterCurve(speeds, tuple(1000 * speed**2 for speed in speeds))
    states = []
    # Bow accelerations and short-wave confirmation are stipulated evidence
    # over the whole [0,10] m/s bracket, not computed motion predictions.
    for hs, tp, bow_acceleration in ((1, 4, 0.1), (2, 5, 0.2)):
        evidence = ReflectionEvidence(
            hs, bow_acceleration, "measured", short_waves_confirmed
        )
        direct = stawave1(ship, point, evidence)
        if direct.status != Status.EVALUATED:
            raise ValueError(f"NOT_EVALUATED reflection case: {direct.reason}")
        wp = 2 * pi / tp
        # Explicit illustration band [wp/2,5*wp], 2001 samples. Convergence
        # of a real frequency-dependent response needs a separate assessment.
        spectrum = pm_spectrum(
            hs, tp, omega_min=wp / 2, omega_max=5 * wp, n_points=2001
        )
        waves = RegularWaves(
            tuple(2 * pi * ship.gravity_m_s2 / w**2 for w in spectrum.omega_rad_s),
            tuple(1 for _ in spectrum.omega_rad_s),
        )
        rows = stawave1_equivalent_regular_table(ship, point, waves, evidence)
        # Use the same dispersion-reconstructed frequencies for both tables;
        # differences from generator coordinates are floating-point roundoff.
        omega = [row.omega_rad_s for row in rows]
        mean = integrate_regular_table(
            rows, omega, spectrum.density_m2_s, omega_min=omega[0], omega_max=omega[-1]
        )
        deferred = radiated_energy_table(
            ship,
            point,
            waves,
            heave_rao=[0j] * len(rows),
            pitch_rao_rad_per_m=[0j] * len(rows),
        )

        def added_at_speed(speed):
            prediction = stawave1(ship, OperatingPoint(speed, 0), evidence)
            if prediction.status != Status.EVALUATED:
                raise ValueError("NOT_EVALUATED reflection during speed iteration")
            return prediction.resistance_n

        reduced = speed_loss(
            curve,
            point.speed_m_s,
            added_at_speed,
            propulsive_efficiency=efficiency,
            speed_tolerance_m_s=1e-8,
            max_iterations=100,
        )
        if reduced.status != Status.EVALUATED:
            raise ValueError(reduced.reason)
        states.append(
            {
                "hs_m": hs,
                "tp_s": tp,
                "bow_acceleration_m_s2": bow_acceleration,
                "omega_min": spectrum.omega_min,
                "omega_max": spectrum.omega_max,
                "n_points": len(omega),
                "omitted_variance_fraction": spectrum.omitted_variance_fraction,
                "reflection_n": direct.resistance_n,
                "spectral_reflection_n": mean,
                "radiated_energy_status": deferred[0].status.value,
                "radiated_energy_n": None,
                "added_shaft_power_w": added_shaft_power(
                    mean, point.speed_m_s, propulsive_efficiency=efficiency
                ),
                "reduced_speed_m_s": reduced.speed_m_s,
                "speed_loss_m_s": reduced.speed_loss_m_s,
                "available_shaft_power_w": reduced.available_shaft_power_w,
                "regular_table": [
                    asdict(rows[i]) for i in (0, len(rows) // 2, len(rows) - 1)
                ],
            }
        )
    return {
        "ship": asdict(ship),
        "point": asdict(point),
        "propulsive_efficiency": efficiency,
        "calm_water_curve": asdict(curve),
        "sea_states": states,
    }


def render_report(result: dict) -> str:
    """Render the committed analysis in engineering register; figures use one decimal."""
    states = result["sea_states"]
    summary_rows = "\n".join(
        f"| {s['hs_m']:.1f} | {s['tp_s']:.1f} | {s['reflection_n'] / 1000:.1f} | "
        f"{s['spectral_reflection_n'] / 1000:.1f} | NOT_EVALUATED | "
        f"{s['added_shaft_power_w'] / 1000:.1f} | {s['reduced_speed_m_s']:.1f} | {s['speed_loss_m_s']:.1f} |"
        for s in states
    )
    band_rows = "\n".join(
        f"| {s['tp_s']:.1f} | {s['omega_min']:.1f} | {s['omega_max']:.1f} | {s['n_points']} | "
        f"{100 * s['omitted_variance_fraction']:.1f} |"
        for s in states
    )
    regular_rows = "\n".join(
        f"| {s['tp_s']:.1f} | {r['wavelength_m']:.1f} | {r['amplitude_m']:.1f} | "
        f"{r['omega_rad_s']:.1f} | {r['encounter_rad_s']:.1f} | {r['resistance_n'] / 1000:.1f} | {r['transfer']:.1f} |"
        for s in states
        for r in s["regular_table"]
    )
    return f"""# Added resistance in waves: screening analysis

## Summary

A synthetic tanker-form hull in head seas gives conditional reflection resistance
of {states[0]["reflection_n"] / 1000:.1f} kN and {states[1]["reflection_n"] / 1000:.1f} kN in two single wind seas.
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
{regular_rows}

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
{band_rows}

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
{summary_rows}

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
"""


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--report", action="store_true", help="Print the reproducible Markdown analysis"
    )
    args = parser.parse_args()
    result = calculate()
    if args.report:
        print(render_report(result), end="")
    else:
        print(json.dumps(result, indent=2, allow_nan=False))


if __name__ == "__main__":
    main()
