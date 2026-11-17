"""Illustrative example regressions for existing plate-capacity methods.

Expected values follow hand formulas and limiting identities. No source-case
inputs or cached results are used. Strict expected failures expose existing
API limitations; their implementations are deliberately retained.
"""

from math import pi, sqrt

import pytest

from digitalmodel.common.units import length_in_metres
from digitalmodel.structural.plate_capacity.classical_elastic_buckling import (
    rectangular_plate_elastic_buckling_stress,
)
from digitalmodel.structural.plate_capacity.elastic_buckling import (
    square_plate_elastic_buckling_stress,
)
from digitalmodel.structural.plate_capacity.perry_robertson import (
    perry_robertson_strength_ratio,
)
from digitalmodel.structural.structural_analysis.buckling import (
    ColumnBucklingAnalyzer,
    PlateBucklingAnalyzer,
)
from digitalmodel.structural.structural_analysis.girder_web_frame import (
    web_shear_buckling_coefficient,
)
from digitalmodel.structural.structural_analysis.models import (
    MaterialProperties,
    PlateGeometry,
    StressState,
)


@pytest.fixture
def material():
    """Illustrative example: stresses in MPa, density in kg/m³."""
    return MaterialProperties(
        yield_strength=300.0, ultimate_strength=450.0,
        youngs_modulus=200000.0, poissons_ratio=0.25, density=7800.0,
    )


@pytest.mark.parametrize("aspect", [0.5, 1.0, 2.0, 4.0])
def test_existing_elastic_plate_pattern(material, aspect):
    """One half-wave for a short plate; k=4 for a long plate."""
    plate = PlateGeometry(length=800.0 * aspect, width=800.0, thickness=16.0)
    coefficient = (aspect + 1 / aspect) ** 2 if aspect < 1 else 4.0
    # Hand formula: sigma_E = k*pi²*E*(t/b)²/[12*(1-nu²)].
    expected = coefficient * pi**2 * 200000 / (12 * (1 - 0.25**2)) * 0.02**2
    analyzer = PlateBucklingAnalyzer(material)
    assert analyzer.elastic_buckling_stress(plate) == pytest.approx(expected, rel=1e-13)
    assert analyzer.reduced_slenderness(plate) == pytest.approx(sqrt(300 / expected))


def test_square_and_long_plate_share_rigidity_normalization(material):
    """Independent API agreement at k=4, with dimensionless thickness ratio."""
    expected = square_plate_elastic_buckling_stress(
        youngs_modulus=200000.0, poisson_ratio=0.25,
        thickness=0.016, side_length=0.8,
    )
    assert PlateBucklingAnalyzer(material).elastic_buckling_stress(
        PlateGeometry(2400.0, 800.0, 16.0)
    ) == pytest.approx(expected)


def test_existing_rectangular_integer_mode_minimum():
    """Illustrative example: E in MPa, lengths in m; a/b=2 gives k=4 at m=2."""
    expected = 4 * pi**2 * 200000 / (12 * (1 - 0.25**2)) * (0.016 / 0.8)**2
    assert rectangular_plate_elastic_buckling_stress(
        youngs_modulus=200000.0, poisson_ratio=0.25,
        thickness=0.016, length=2 * 0.8, width=0.8,
    ) == pytest.approx(expected, rel=1e-13)


@pytest.mark.parametrize(
    ("elastic_stress", "expected"),
    [(60.0, 60.0), (150.0, 150.0), (300.0, 225.0), (750.0, 270.0)],
)
def test_existing_inelastic_branches(material, elastic_stress, expected):
    """Illustrative example: fy=300 MPa; branch junction at fy/2."""
    assert PlateBucklingAnalyzer(material).johnson_ostenfeld(
        elastic_stress
    ) == pytest.approx(expected)


@pytest.mark.parametrize("aspect", [0.5, 1.0, 2.0])
def test_existing_shear_coefficient_branches(aspect):
    ratio = 1 / aspect
    expected = 5.34 + 4 * ratio**2 if aspect >= 1 else 4 + 5.34 * ratio**2
    assert web_shear_buckling_coefficient(800.0, 800.0 * aspect) == expected


@pytest.mark.parametrize("slenderness", [0.4, 0.7, 1.0, 1.5])
def test_existing_quadratic_reduction_root_identity(material, slenderness):
    """Quadratic-root identity in the middle range; no large-lambda extension."""
    imperfection = 0.21 * (slenderness - 0.2)
    s = 1 + imperfection + slenderness**2
    expected = (s - sqrt(s**2 - 4 * slenderness**2)) / (2 * slenderness**2)
    assert perry_robertson_strength_ratio(
        reduced_slenderness=slenderness, imperfection_parameter=imperfection,
    ) == pytest.approx(expected, rel=1e-13)
    assert ColumnBucklingAnalyzer(material).reduction_factor(
        slenderness, buckling_curve="a",
    ) == pytest.approx(expected, rel=1e-13)


@pytest.mark.parametrize(
    ("sx", "sy", "tau", "expected"),
    [(12.0, 0.0, 0.0, 12.0), (12.0, 12.0, 0.0, 12.0),
     (0.0, 0.0, 4.0, sqrt(48.0)), (12.0, 4.0, 2.0, sqrt(124.0))],
)
def test_existing_plane_stress_invariant(sx, sy, tau, expected):
    assert StressState(sigma_x=sx, sigma_y=sy, tau_xy=tau).von_mises() == pytest.approx(expected)


@pytest.mark.parametrize("sx,tau", [(0.0, 0.0), (12.0, 0.0), (0.0, 6.0), (12.0, 6.0)])
def test_existing_longitudinal_and_shear_interaction(material, sx, tau):
    """Hand-computed Johnson-Ostenfeld capacities and linear/quadratic usage."""
    factor = 1.2
    rigidity = pi**2 * 200000 / (12 * (1 - 0.25**2)) * 0.02**2
    elastic_x = 4 * rigidity
    elastic_tau = (5.34 + 4 / 9) * rigidity
    critical_x = 300 * (1 - 300 / (4 * elastic_x))
    critical_tau = 300 * (1 - 300 / (4 * elastic_tau))
    expected = sx * factor / critical_x + (tau * factor / critical_tau) ** 2
    result = PlateBucklingAnalyzer(material).check_plate_buckling(
        PlateGeometry(2400.0, 800.0, 16.0), sx, tau=tau, gamma_m=factor,
    )
    assert result.critical_stress == pytest.approx(critical_x)
    assert result.utilization == pytest.approx(expected)
    assert result.passes == (expected <= 1)
    if expected:
        assert result.safety_factor == pytest.approx(1 / expected)
    else:
        assert result.safety_factor == float("inf")


def test_existing_analyzer_rejects_unimplemented_boundary(material):
    with pytest.raises(ValueError, match="boundary_conditions"):
        PlateBucklingAnalyzer(material).elastic_buckling_stress(
            PlateGeometry(2400.0, 800.0, 16.0), boundary_conditions="clamped",
        )


@pytest.mark.parametrize("field", ["youngs_modulus", "thickness", "side_length"])
@pytest.mark.parametrize("value", [0.0, -2.0, float("nan"), float("inf")])
def test_existing_square_solver_input_validation(field, value):
    inputs = dict(youngs_modulus=200000.0, poisson_ratio=0.25, thickness=0.016, side_length=0.8)
    inputs[field] = value
    with pytest.raises(ValueError, match=field):
        square_plate_elastic_buckling_stress(**inputs)


@pytest.mark.parametrize("value", [-1.0, 0.5, float("nan"), float("inf")])
def test_existing_square_solver_poisson_validation(value):
    with pytest.raises(ValueError, match="poisson_ratio"):
        square_plate_elastic_buckling_stress(
            youngs_modulus=200000.0, poisson_ratio=value,
            thickness=0.016, side_length=0.8,
        )


@pytest.mark.parametrize("value", [-2.0, float("nan"), float("inf")])
def test_existing_reduction_solver_input_validation(value):
    with pytest.raises(ValueError, match="reduced_slenderness"):
        perry_robertson_strength_ratio(reduced_slenderness=value, imperfection_parameter=0.1)


def test_existing_length_boundary_converter():
    """Illustrative example: 5 international feet = 1.524 m exactly."""
    assert length_in_metres(5.0, "ft") == 1.524
    with pytest.raises(ValueError, match="unit"):
        length_in_metres(5.0, "unspecified")


@pytest.mark.xfail(strict=True, reason="Existing combined-load API ignores sigma_y; tracked for method review")
def test_transverse_only_loading_requires_nonzero_usage(material):
    result = PlateBucklingAnalyzer(material).check_plate_buckling(
        PlateGeometry(2400.0, 800.0, 16.0), 0.0, sigma_y=12.0,
    )
    assert result.utilization > 0


@pytest.mark.xfail(strict=True, reason="Existing combined-load API accepts a zero material factor; tracked for method review")
def test_combined_api_requires_positive_material_factor(material):
    with pytest.raises(ValueError):
        PlateBucklingAnalyzer(material).check_plate_buckling(
            PlateGeometry(2400.0, 800.0, 16.0), 12.0, gamma_m=0.0,
        )
