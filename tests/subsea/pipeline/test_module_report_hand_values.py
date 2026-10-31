# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic hand calculations for the pipeline module report.

Basis A: integrate dA = r dr dtheta over radii 1..2 m and angles 0..2*pi.
Wall area = 3*pi m^2; bore/outside disks = pi and 4*pi m^2.
Integrating y^2 dA with y=r*sin(theta) gives I = 15*pi/4 m^4.
The radial separation is 1 m; the diameters are 4 m and 2 m.
Basis B: a uniform strain (1/100000 /K)*(+/-20 K) = +/-1/5000,
integrated over 1000 m, gives +/-1/5 m of unrestrained growth.
Basis C: arbitrary supplied capacity 100 MPa times arbitrary dimensionless
factors 4/5, 9/10 and 3/5 gives 80, 90 and 60 MPa. No burst oracle is used.
The 1e-12 relative tolerance allows floating-point rounding, not design error.
All numeric test inputs and expected values follow these written calculations.
"""

import importlib
import math

import pytest


@pytest.mark.parametrize(
    "dimensions",
    [
        {"outer_diameter": 4.0, "inner_diameter": 2.0},
        {"outer_diameter": 4.0, "wall_thickness": 1.0},
        {"inner_diameter": 2.0, "wall_thickness": 1.0},
        {"outer_diameter": 4.0, "inner_diameter": 2.0, "wall_thickness": 1.0},
    ],
    ids=["od-id", "od-thickness", "id-thickness", "all-dimensions"],
)
def test_public_geometry_routes_match_integrated_annulus(dimensions):
    """Normal public imports and every dimension route reproduce all fields."""
    geometry = importlib.import_module("digitalmodel.subsea.pipeline.calculations")
    expected = {
        "outer_diameter": 4.0,
        "inner_diameter": 2.0,
        "wall_thickness": 1.0,
        "A": 3 * math.pi,
        "Ai": math.pi,
        "Ao": 4 * math.pi,
        "I": 15 * math.pi / 4,
    }
    assert geometry.calculate_geometric_properties(**dimensions) == pytest.approx(
        expected, rel=1e-12, abs=0.0
    )
    assert geometry.calculate_section_properties(4.0, 2.0) == pytest.approx(
        {key: expected[key] for key in ("A", "Ai", "Ao", "I")},
        rel=1e-12,
        abs=0.0,
    )
    assert geometry.calculate_wall_thickness(4.0, 2.0) == 1.0
    assert geometry.calculate_diameter_from_thickness(4.0, 1.0, "outer") == 2.0
    assert geometry.calculate_diameter_from_thickness(2.0, 1.0, "inner") == 4.0
    assert geometry.validate_pipe_geometry(4.0, 2.0, 1.0) is True


@pytest.mark.parametrize("temperature, expected", [(20.0, 1 / 5), (-20.0, -1 / 5)])
def test_public_free_growth_matches_integrated_uniform_strain(temperature, expected):
    """Heating and cooling integrate the signed strain over the free length."""
    from digitalmodel.subsea.pipeline.thermal_expansion import free_thermal_expansion

    growth = free_thermal_expansion(1000.0, 1 / 100000, temperature)
    assert growth == pytest.approx(expected, rel=1e-12, abs=0.0)
    assert growth / 1000 == pytest.approx(expected / 1000, rel=1e-12, abs=0.0)


def test_public_pressure_factors_match_supplied_arithmetic():
    """The three products test wiring only; capacity and factors are synthetic."""
    from digitalmodel.subsea.pipeline.calculations import APISTD2RDCalculations

    result = APISTD2RDCalculations.calculate_design_pressures(
        100.0,
        {
            "internalPressure": {
                "design": 4 / 5,
                "incidentalPressure": 9 / 10,
                "hydroStaticTest": 3 / 5,
            }
        },
    )
    assert result == pytest.approx(
        {"design": 80.0, "accidental": 90.0, "test": 60.0},
        rel=1e-12,
        abs=0.0,
    )
