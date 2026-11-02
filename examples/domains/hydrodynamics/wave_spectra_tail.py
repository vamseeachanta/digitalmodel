# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Compare a truncated spectrum, the default grid, and an exact Pierson-Moskowitz integral.

Run with PYTHONPATH=src python examples/domains/hydrodynamics/wave_spectra_tail.py.
All inputs are synthetic, including density and gravity; no site is represented.
Equations: Orcina online help (accessed 2026), Waves: Wave spectra, ISSC;
WEC-Sim Theory (accessed 2026), Irregular Waves and Irregular Wave Power.
See docs/domains/hydrodynamics/wave-spectra.md for links and the hand integral.
"""

import math

from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra


def main():
    """Print rounded answers; compare unrounded moments in the regression tests."""
    # Synthetic inputs, deliberately simple for independent hand calculation.
    hs, tp, water_density, gravity = 4.0, 6.0, 1000.0, 10.0
    ws = WaveSpectra()
    wp = 2 * math.pi / tp

    def exact_moment(n):
        # u=(5/4)*(wp/w)^4 in the cited ISSC equation yields the gamma integral.
        return hs**2 / 16 * wp**n * (5 / 4)**(n / 4) * math.gamma(1 - n / 4)

    print(f"Synthetic inputs: Hs={hs:.1f} m, Tp={tp:.1f} s, "
          f"density={water_density:.1f} kg/m^3, gravity={gravity:.1f} m/s^2")
    print("Method: angular-frequency moments; Te=2*pi*m(-1)/m0; P=rho*g^2*m(-1)/2")
    print("case | m0 (m^2) | m1 (m^2/s) | m2 (m^2/s^2) | Tz (s) | Te (s) | P (kW/m)")
    # The old bounds and count reproduce the former numerical defaults.
    for label, options in [("fixed cutoff", {"freq_max": 2.0, "n_points": 100}),
                           ("peak-scaled cutoff", {})]:
        omega, density = ws.pierson_moskowitz(hs, tp, **options)
        stats = ws.spectrum_statistics(omega, density)
        power = ws.deep_water_wave_power(
            omega, density, water_density=water_density, gravity=gravity)
        print(f"{label} | {stats['m0']:.1f} | {stats['m1']:.1f} | {stats['m2']:.1f} | "
              f"{stats['Tz_s']:.1f} | {stats['Te_s']:.1f} | {power / 1000:.1f}")
    m0, m1, m2 = (exact_moment(n) for n in (0, 1, 2))
    tz = 2 * math.pi * math.sqrt(m0 / m2)
    te = 2 * math.pi * exact_moment(-1) / m0
    power = water_density * gravity**2 / 2 * exact_moment(-1)
    print(f"infinite Pierson-Moskowitz integral | {m0:.1f} | {m1:.1f} | {m2:.1f} | "
          f"{tz:.1f} | {te:.1f} | {power / 1000:.1f}")


if __name__ == "__main__":
    main()
