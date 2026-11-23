# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic profile integration and fixed-gravity wave-helper contract."""

import math

import pytest


def test_diameter_average_is_an_integral_not_a_centre_value():
    from digitalmodel.subsea.on_bottom_stability.e305_simplified import diameter_averaged_current

    # Integral of ln(1+z), z=0..1, is 2*ln(2)-1. Reference is z=1.
    average = diameter_averaged_current(2.0, reference_height_m=1.0,
                                         diameter_m=1.0, roughness_m=1.0)
    assert average == pytest.approx(2 * (2*math.log(2)-1)/math.log(2))
    assert average != pytest.approx(2 * math.log(1.5)/math.log(2))
    assert diameter_averaged_current(0, reference_height_m=1, diameter_m=1, roughness_m=1) == 0
    with pytest.raises(ValueError):
        diameter_averaged_current(2, reference_height_m=0, diameter_m=1, roughness_m=1)


def test_g_is_not_silently_changed_by_the_spectral_helper():
    from digitalmodel.subsea.on_bottom_stability.e305_simplified import wave_from_sea_state
    from digitalmodel.subsea.pipeline.free_span.wave_velocity import compute_wave_velocity, _G

    with pytest.raises(ValueError, match='gravity'):
        wave_from_sea_state(Hs_m=2, Tp_s=4, depth_m=8, elevation_m=0.5, g=9.81)
    expected = compute_wave_velocity(2, 4, 8, 0.5)
    result = wave_from_sea_state(Hs_m=2, Tp_s=4, depth_m=8, elevation_m=0.5, g=_G)
    assert result == pytest.approx((expected.Uw_ms, expected.Tu_s))
