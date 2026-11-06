"""Prevent silent method replacement and preserve inherited calculation results.

Equation sources are the existing ASMEThicknessCalc method docstrings in
src/digitalmodel/ansys/pressure_vessel.py: UG-27, simplified UG-37 and UG-99.
Their standard edition is not recorded; these are compatibility regressions,
not independent qualification against a particular ASME edition. All geometry,
stress and pressure inputs below are synthetic, not material-table values.
"""

import ast
import inspect
from collections import Counter

import pytest

from digitalmodel.ansys.pressure_vessel import ASMEThicknessCalc


@pytest.mark.parametrize(
    "method_name",
    [
        "calculate_mawp",
        "check_nozzle_reinforcement",
        "calculate_hydrotest_pressure",
    ],
)
def test_calculation_method_has_one_definition(method_name):
    """Inspect source: runtime lookup alone hides earlier duplicate methods."""
    tree = ast.parse(inspect.getsource(ASMEThicknessCalc))
    definitions = Counter(
        node.name
        for node in tree.body[0].body
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
    )
    assert definitions[method_name] == 1, f"{method_name} is silently shadowed"


def test_mawp_matches_independent_arithmetic():
    """Inherited UG-27 inverse, with synthetic inputs (mm and MPa).

    Source: calculate_mawp docstring, edition unspecified in repository.
    Net wall = 12 - 2 = 10 mm. Numerator = 106 * 1 * 10 = 1060.
    Denominator = 100 + 0.6 * 10 = 106; pressure = 1060 / 106 = 10 MPa.
    The coefficient is inherited from that documented equation, not a new value.
    """
    result = ASMEThicknessCalc().calculate_mawp(
        wall_thickness_mm=12.0,
        inner_radius_mm=100.0,
        allowable_stress_mpa=106.0,
        joint_efficiency=1.0,
        corrosion_allowance_mm=2.0,
    )
    assert result == pytest.approx(10.0)


@pytest.mark.parametrize("pad_mm, expected_available_mm2", [(0.0, 459.0), (4.0, 619.0)])
def test_simplified_nozzle_area_matches_independent_arithmetic(
    pad_mm, expected_available_mm2
):
    """Preserve the inherited simplified area model, not a full UG-37 check.

    Source: check_nozzle_reinforcement implementation and UG-27 docstring,
    edition unspecified in repository. Synthetic inputs use mm and MPa.
    Net shell = 22 - 2 = 20; net nozzle = 5 - 2 = 3; bore = 50 - 2*5 = 40.
    Required shell = 10*100/(106 - 0.6*10) = 10; nozzle = 10*20/100 = 2.
    Required area = 40*10 = 400; shell excess = (20 - 10)*40 = 400.
    Nozzle excess = 2*(3 - 2)*min(2.5*20, 2.5*3 + 22) = 59.
    Available area = 459 without a pad; a 4 mm pad adds 4*40 = 160 => 619.
    All equation coefficients are inherited from the cited implementation.
    """
    result = ASMEThicknessCalc().check_nozzle_reinforcement(
        shell_inner_radius_mm=100.0,
        shell_thickness_mm=22.0,
        nozzle_outer_diameter_mm=50.0,
        nozzle_thickness_mm=5.0,
        design_pressure_mpa=10.0,
        allowable_stress_mpa=106.0,
        joint_efficiency=1.0,
        corrosion_allowance_mm=2.0,
        reinforcement_pad_thickness_mm=pad_mm,
    )
    assert result["area_required_mm2"] == pytest.approx(400.0)
    assert result["area_available_mm2"] == pytest.approx(expected_available_mm2)
    assert result["is_adequate"] is True


def test_hydrotest_matches_independent_arithmetic():
    """Inherited stress-ratio equation with an explicit synthetic test factor.

    Source: calculate_hydrotest_pressure docstring (UG-99; edition unspecified).
    Factor 1.5 is a test input, not an asserted code requirement.
    Pressure = 1.5 * 10 * (120 / 100) = 18 MPa.
    """
    result = ASMEThicknessCalc().calculate_hydrotest_pressure(
        design_pressure_mpa=10.0,
        test_factor=1.5,
        allowable_design_mpa=100.0,
        allowable_test_mpa=120.0,
    )
    assert result == pytest.approx(18.0)
