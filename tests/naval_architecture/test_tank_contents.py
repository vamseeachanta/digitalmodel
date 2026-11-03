"""Attitude and contents checks, with independent calculations below.

Dimensions, densities and expansion coefficients are synthetic inputs.
No material coefficient or engineering acceptance threshold is adopted.
"""

import math

import pytest

from digitalmodel.naval_architecture.tank_gauging import (
    centre_sounding,
    density_at_temperature,
    liquid_mass_kg,
)


def test_one_degree_trim_hand_check():
    """OpenStax Precalculus 2e (2021), §5.4: opposite = adjacent tan θ.

    Independent calculator value tan(1 degree) = 0.0174550649282176.
    At x = +2 m, h_g = 1.5 m: h_c = 1.5 - 2 * tan(1) =
    1.465089870143565 m. Positive trim means surface rises towards +x.
    """
    assert centre_sounding(1.5, 3, x_gauge_m=2, trim_deg=1) == pytest.approx(
        1.465089870143565
    )
    assert centre_sounding(1.5, 3, x_gauge_m=-2, trim_deg=1) == pytest.approx(
        1.534910129856435
    )


def test_trim_and_list_slopes_are_additive():
    """tan(45 degrees) = 1; h_c = 2 - (0.5)(1) - (-0.25)(1) = 1.75.

    OpenStax Precalculus 2e (2021), §5.4; 45 degrees tests the algebra
    only, without claiming that an inclined full-area tank is unclipped.
    """
    assert centre_sounding(
        2, 4, x_gauge_m=0.5, y_gauge_m=-0.25, trim_deg=45, list_deg=45
    ) == pytest.approx(1.75)
    assert centre_sounding(2, 4) == 2
    assert centre_sounding(2, 4, y_gauge_m=0.5, list_deg=-45) == pytest.approx(2.5)


@pytest.mark.parametrize(
    "kwargs",
    [
        {"trim_deg": 90},
        {"list_deg": -90},
        {"trim_deg": 180},
        {"x_gauge_m": math.inf},
        {"y_gauge_m": math.nan},
        {"trim_deg": math.nan},
        {"list_deg": math.inf},
        {"x_gauge_m": 3, "trim_deg": 45},
        {"y_gauge_m": -3, "list_deg": 45},
    ],
)
def test_correction_rejects_invalid_or_out_of_height_result(kwargs):
    with pytest.raises(ValueError):
        centre_sounding(1.5, 3, **kwargs)


def test_mass_and_temperature_hand_check():
    """OpenStax University Physics (2016), vol 1 §14.1 and vol 2 §1.3 eq 1.4.

    Synthetic rho15 = 800 kg/m³, beta = 0.001 /°C, T = 40 °C.
    Expansion factor = 1 + 0.001 (40 - 15) = 1.025 = 41/40.
    rho40 = 800 * 40/41 = 32000/41 kg/m³.
    A measured 60 m³ weighs 48000 kg at 15 °C, or 1920000/41 kg
    at 40 °C. These represent equal measured volumes, not a sealed batch.
    A sealed 60 m³ batch expands to 61.5 m³ and keeps mass 48000 kg.
    """
    rho40 = density_at_temperature(800, 15, 40, 0.001)
    assert rho40 == pytest.approx(32000 / 41)
    assert liquid_mass_kg(60, 800) == 48000
    assert liquid_mass_kg(60, rho40) == pytest.approx(1920000 / 41)
    assert liquid_mass_kg(61.5, rho40) == pytest.approx(48000)
    assert density_at_temperature(800, 15, 15, 0.001) == 800
    assert density_at_temperature(800, 15, 40, 0) == 800
    assert liquid_mass_kg(0, 800) == 0


@pytest.mark.parametrize(
    "volume,density",
    [
        (-1, 800),
        (math.nan, 800),
        (math.inf, 800),
        (1, 0),
        (1, -1),
        (1, math.inf),
        (True, 800),
        (None, 800),
    ],
)
def test_mass_invalid_inputs(volume, density):
    with pytest.raises(ValueError):
        liquid_mass_kg(volume, density)


@pytest.mark.parametrize(
    "args",
    [
        (0, 15, 40, 0.001),
        (800, math.inf, 40, 0.001),
        (800, 15, math.nan, 0.001),
        (800, 15, 40, math.inf),
        (800, 0, -1000, 0.001),
        (800, 0, -2000, 0.001),
    ],
)
def test_temperature_invalid_inputs(args):
    with pytest.raises(ValueError):
        density_at_temperature(*args)
