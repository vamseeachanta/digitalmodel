# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent elastic square-plate checks; no solver or legacy scenario imports."""

import pytest

from digitalmodel.structural.plate_capacity.elastic_buckling import (
    square_plate_elastic_buckling_stress,
)


def test_square_plate_hand_calculation():
    """MIT 2.080J (2013), Lecture 11, Eqs. 11.15-11.16; Lecture 4, Eq. 4.60.

    Synthetic inputs, not material properties: E=120000 MPa, nu=0,
    t=1 mm, b=100 mm. For a=b and m=1, k=(1+1)^2=4.
    By hand: D=120000*1^3/[12*(1-0^2)]=10000 N mm;
    Ncr=4*pi^2*10000/100^2=4*pi^2 N/mm;
    sigma=Ncr/t=39.47841760435743 MPa. Tolerance is numerical only.
    """
    assert square_plate_elastic_buckling_stress(
        youngs_modulus=120000.0, poisson_ratio=0.0, thickness=1.0, side_length=100.0
    ) == pytest.approx(39.47841760435743, rel=1e-12)


@pytest.mark.parametrize("poisson_ratio", [0.3, -0.3])
def test_nonzero_poisson_ratio_hand_calculation(poisson_ratio):
    """Same synthetic case: D=10000/0.91, sigma=4*pi^2/0.91 MPa.

    Independent hand result: 43.38287648830487 MPa. The sign is squared
    in MIT 2.080J (2013), Lecture 4, Eq. 4.60; auxetic inputs are supported.
    """
    assert square_plate_elastic_buckling_stress(
        youngs_modulus=120000.0,
        poisson_ratio=poisson_ratio,
        thickness=1.0,
        side_length=100.0,
    ) == pytest.approx(43.38287648830487, rel=1e-12)


@pytest.mark.parametrize("field", ["youngs_modulus", "thickness", "side_length"])
@pytest.mark.parametrize("invalid", [0.0, -1.0, float("inf"), float("nan")])
def test_rejects_nonpositive_or_nonfinite_dimensions_and_modulus(field, invalid):
    """Positive finite geometry and stiffness are required by the elastic model."""
    inputs = dict(
        youngs_modulus=120000.0, poisson_ratio=0.0, thickness=1.0, side_length=100.0
    )
    inputs[field] = invalid
    with pytest.raises(ValueError, match=field):
        square_plate_elastic_buckling_stress(**inputs)


@pytest.mark.parametrize("invalid", [-1.0, 0.5, 0.6, float("inf"), float("nan")])
def test_rejects_unstable_or_nonfinite_poisson_ratio(invalid):
    """Stable isotropic elasticity requires -1 < nu < 1/2 (source in module)."""
    with pytest.raises(ValueError, match="poisson_ratio"):
        square_plate_elastic_buckling_stress(
            youngs_modulus=120000.0,
            poisson_ratio=invalid,
            thickness=1.0,
            side_length=100.0,
        )


@pytest.mark.parametrize(
    ("youngs_modulus", "thickness", "side_length", "expected"),
    [
        (240000.0, 1.0, 100.0, 78.95683520871486),
        (120000.0, 2.0, 100.0, 157.91367041742972),
        (120000.0, 1.0, 200.0, 9.869604401089358),
        (120000.0, 0.001, 0.1, 39.47841760435743),
    ],
)
def test_modulus_and_geometry_scaling(youngs_modulus, thickness, side_length, expected):
    """MIT (2013), Lectures 4/11: sigma is proportional to E*(t/b)^2.

    From the worked 4*pi^2 MPa case above: double E gives 8*pi^2;
    double t gives 16*pi^2; double b gives pi^2; converting both lengths
    from mm to m leaves 4*pi^2. Inputs remain synthetic and elastic.
    """
    assert square_plate_elastic_buckling_stress(
        youngs_modulus=youngs_modulus,
        poisson_ratio=0.0,
        thickness=thickness,
        side_length=side_length,
    ) == pytest.approx(expected, rel=1e-12)


@pytest.mark.parametrize(
    ("youngs_modulus", "thickness", "side_length"),
    [(1e308, 1.0, 100.0), (120000.0, 1e200, 1.0), (120000.0, 1e-200, 1.0)],
)
def test_rejects_unrepresentable_intermediate_or_result(
    youngs_modulus, thickness, side_length
):
    """Synthetic floating-point stress tests, outside engineering usage."""
    with pytest.raises(ValueError, match="floating-point range"):
        square_plate_elastic_buckling_stress(
            youngs_modulus=youngs_modulus,
            poisson_ratio=0.0,
            thickness=thickness,
            side_length=side_length,
        )
