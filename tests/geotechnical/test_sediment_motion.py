# ABOUTME: Independent Shields threshold calculations and input-domain tests.
# ABOUTME: Synthetic examples require no default sediment or fluid properties.
"""Critical sediment motion tests, using independent hand calculations.

Basis: USACE, HEC-RAS 2D Sediment Technical Reference Manual, v6.4.1
(2023), section 2.14.1 equation 132 and section 2.13.1 shear velocity
definition following equation 111. All numeric inputs below are synthetic
arithmetic examples, not recommended material properties or design limits.
"""

import math

import pytest

from digitalmodel.geotechnical.scour import critical_sediment_motion


def inputs(**overrides):
    """Synthetic inputs chosen to make the hand calculation reproducible."""
    values = dict(
        sediment_d50_m=0.002,
        sediment_density_kg_m3=2000.0,
        water_density_kg_m3=1000.0,
        critical_shields_parameter=0.05,
        gravity_ms2=10.0,
    )
    return values | overrides


@pytest.mark.parametrize(
    "overrides, stress_pa, velocity_ms",
    [
        ({}, 1.0, math.sqrt(0.001)),
        ({"sediment_d50_m": 0.008}, 4.0, math.sqrt(0.004)),
        ({"sediment_density_kg_m3": 3000.0}, 2.0, math.sqrt(0.002)),
        ({"water_density_kg_m3": 1500.0}, 0.5, math.sqrt(1.0 / 3000.0)),
        ({"critical_shields_parameter": 0.1}, 2.0, math.sqrt(0.002)),
        ({"gravity_ms2": 5.0}, 0.5, math.sqrt(0.0005)),
    ],
)
def test_hand_calculated_thresholds(overrides, stress_pa, velocity_ms):
    """tau = theta*(rho_s-rho_w)*g*d; u_star = sqrt(tau/rho_w).

    Base: 0.05*(2000-1000)*10*0.002 = 1 Pa; sqrt(1/1000) m/s.
    Fourfold diameter gives 4 Pa and sqrt(4/1000) m/s.
    Density 3000 gives 2 Pa and sqrt(2/1000) m/s.
    Water density 1500 gives 0.5 Pa and sqrt(0.5/1500) m/s.
    Doubling theta gives 2 Pa and sqrt(2/1000) m/s.
    Halving gravity gives 0.5 Pa and sqrt(0.5/1000) m/s.
    These rearrange the cited equations independently of the implementation.
    """
    result = critical_sediment_motion(**inputs(**overrides))
    assert result.critical_shear_stress_pa == pytest.approx(stress_pa)
    assert result.critical_shear_velocity_ms == pytest.approx(velocity_ms)


@pytest.mark.parametrize("name", list(inputs()))
@pytest.mark.parametrize("value", [0.0, -1.0, math.nan, math.inf, -math.inf])
def test_rejects_nonpositive_and_nonfinite_inputs(name, value):
    """The cited noncohesive threshold requires positive finite quantities."""
    with pytest.raises(ValueError, match=name):
        critical_sediment_motion(**inputs(**{name: value}))


@pytest.mark.parametrize("density", [500.0, 1000.0])
def test_rejects_nonsettling_sediment(density):
    """rho_s must exceed rho_w for a positive submerged grain weight."""
    with pytest.raises(ValueError, match="sediment_density_kg_m3 must exceed"):
        critical_sediment_motion(**inputs(sediment_density_kg_m3=density))


@pytest.mark.parametrize("name", list(inputs()))
def test_all_physical_inputs_are_required(name):
    """No uncited critical Shields, gravity, grain size or density default."""
    values = inputs()
    del values[name]
    with pytest.raises(TypeError, match=name):
        critical_sediment_motion(**values)


@pytest.mark.parametrize("diameter", [1e308, 5e-324])
def test_rejects_unrepresentable_thresholds(diameter):
    """Extreme diameters overflow stress or underflow squared shear velocity."""
    with pytest.raises(ValueError, match="representable"):
        critical_sediment_motion(**inputs(sediment_d50_m=diameter))
