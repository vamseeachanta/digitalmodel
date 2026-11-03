# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Boundary contract and independent sine-mode plate buckling calculation."""

import math

import pytest

from digitalmodel.structural.structural_analysis.buckling import PlateBucklingAnalyzer
from digitalmodel.structural.structural_analysis.models import (
    MaterialProperties,
    PlateGeometry,
)


@pytest.fixture
def square_plate_case():
    """Synthetic inputs, chosen for this hand calculation rather than a grade.

    Square sides are 600 mm and thickness is 6 mm. Elastic properties are
    E = 200000 MPa and nu = 0.3. The unused strength and density inputs are
    yield = 250 MPa, ultimate = 400 MPa and density = 7800 kg/m^3.
    """
    material = MaterialProperties(
        yield_strength=250.0,
        ultimate_strength=400.0,
        youngs_modulus=200000.0,
        poissons_ratio=0.3,
        density=7800.0,
        name="Synthetic material",
    )
    plate = PlateGeometry(length=600.0, width=600.0, thickness=6.0)
    return material, plate


@pytest.mark.parametrize("boundary_conditions", ["clamped", "unknown", ""])
def test_unsupported_plate_boundary_is_refused(square_plate_case, boundary_conditions):
    material, plate = square_plate_case
    analyzer = PlateBucklingAnalyzer(material)

    with pytest.raises(ValueError, match="only 'simply_supported' is supported"):
        analyzer.elastic_buckling_stress(plate, boundary_conditions=boundary_conditions)


def test_explicit_simply_supported_matches_square_hand_solution(square_plate_case):
    """Derive the critical stress independently for uniaxial compression.

    For w = sin(m*pi*x/a)*sin(n*pi*y/b), the classical thin-plate equation
    D*del^4(w) + Nx*w_xx = 0 gives Nx = D*(qx^2 + qy^2)^2/qx^2,
    where qx = m*pi/a, qy = n*pi/b and sigma = Nx/t.
    Through-thickness integration gives D = E*t^3/[12*(1 - nu^2)].

    On a square, k = (m^2 + n^2)^2/m^2 >= 4*n^2 >= 4 for positive
    integer modes. Equality occurs at m = n = 1, giving k = 4. The
    calculation below substitutes these wave numbers into the eigenvalue
    equation instead of obtaining any expected value from the analyzer.
    """
    material, plate = square_plate_case
    flexural_rigidity = (
        material.youngs_modulus * plate.thickness**3
        / (12 * (1 - material.poissons_ratio**2))
    )
    # Fundamental sine mode: m = n = 1, as derived above.
    qx = math.pi / plate.length
    qy = math.pi / plate.width
    expected_stress = (
        flexural_rigidity * (qx**2 + qy**2)**2 / qx**2 / plate.thickness
    )
    analyzer = PlateBucklingAnalyzer(material)

    explicit = analyzer.elastic_buckling_stress(
        plate, boundary_conditions="simply_supported"
    )
    default = analyzer.elastic_buckling_stress(plate)

    # Relative tolerance allows floating-point evaluation of equivalent algebra.
    assert explicit == pytest.approx(expected_stress, rel=1e-12)
    assert default == pytest.approx(expected_stress, rel=1e-12)
    assert default == explicit
