# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Normalized broad-band BT2 calculation; not a material assessment.

Source: Metals 11 (2021), 1333, Table 1, c=5; Eqs. (11), (13).
https://doi.org/10.3390/met11091333
"""

import math

from digitalmodel.fatigue.spectral_fatigue import (
    SpectralMoments,
    benasciutti_tovo_damage,
    narrow_band_damage,
)


def calculate():
    """Return narrow-band and BT2 results for the normalized rectangle."""
    # Analytical normalization: unit stress variance, unit second angular
    # moment, A=1, b=3. These inputs do not specify a material S-N curve.
    # Integrating a unit-area rectangle on [1,5] gives
    # Mj=(5**(j+1)-1)/(4*(j+1)); rescale frequency by sqrt(3/31).
    # Convert angular moments to the API's Hz moments: divide Mj by (2*pi)**j.
    moments = SpectralMoments(
        m0=1, m1=3 * math.sqrt(3 / 31) / math.tau, m2=1 / math.tau**2,
        m4=(781 / 5) * (3 / 31) ** 2 / math.tau**4,
    )
    return (
        narrow_band_damage(moments, sn_slope=3, sn_intercept=0),
        benasciutti_tovo_damage(moments, sn_slope=3, sn_intercept=0),
    )


if __name__ == "__main__":
    nb, bt = calculate()
    # Eq. (11): the dimensionless reduction is independent of rate units.
    correction = bt.damage_rate / nb.damage_rate
    print(f"Narrow-band damage rate: {nb.damage_rate:.1f} per second")
    print(f"BT2 damage rate: {bt.damage_rate:.1f} per second")
    print(f"Correction: {100 * correction:.1f} %")
    print(f"Reduction: {100 * (1 - correction):.1f} %")
    print(f"Equivalent range at peak rate: {bt.equivalent_stress_range:.1f}")
