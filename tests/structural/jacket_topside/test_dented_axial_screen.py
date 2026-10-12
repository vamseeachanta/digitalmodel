# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Cho (1987), Volume I: equations 6.13 to 6.17 and section 6.3.1.

Cases are synthetic hand checks of the opened equations unless a specimen
locator is given. Study-envelope tests exercise the extrema on page 193;
continuous interpolation is an explicit implementation assumption.
"""

from dataclasses import replace
import importlib
import math

import pytest


@pytest.fixture
def api():
    """Load the proposed calculation; its absence is the expected red state."""
    try:
        return importlib.import_module(
            "digitalmodel.structural.jacket_topside.dented_axial_screen"
        )
    except ModuleNotFoundError as exc:
        pytest.fail(f"The dented axial calculation is not implemented: {exc}")


@pytest.fixture
def case(api):
    """Cho (1987) section 6.3.1, page 193: synthetic study-grid inputs."""
    return api.DentedAxialInput(
        midwall_diameter_to_thickness=40.0,
        reduced_slenderness=1.0,
        dent_depth_ratio=0.1,
        neutral_axis_bow_ratio=0.005,
        elastic_to_yield_ratio=600.0,
        axial_demand_ratio=0.2,
        simply_supported=True,
        sharp_midspan_dent=True,
        bow_at_plastic_neutral_axis=True,
        has_applied_bending=False,
        has_hydrostatic_pressure=False,
    )


def test_hand_calculation(api, case):
    """Cho (1987) equations 6.13 to 6.17, page 194: synthetic hand check.

    1. sigma_cr/sigma_y = 1/(1 squared) = 1, notation page 8.
    2. Overall imperfection = 22.2*(0.005*1)^0.7 = 0.5440414350.
    3. Dent factor = 1+1.26*0.1^1.3*40^0.6 = 1.5775725438.
    4. Pressure factor = exp(0) = 1 (zero hydrostatic pressure).
    5. Product imperfection = 0.858264830595756.
    6. A = [1+(1+0.858264830595756)*1]/2 = 1.429132415297878.
    7. Capacity/yield = A-sqrt(A squared-1) = 0.4081429636.
    8. Demand/capacity = 0.2/0.4081429636 = 0.4900243734.
    Absolute tolerance 1e-10 covers rounding to ten decimal places only.
    """
    result = api.screen_dented_axial(case)
    assert result.capacity_ratio == pytest.approx(0.4081429636, abs=1e-10)
    assert result.utilization == pytest.approx(0.4900243734, abs=1e-10)
    assert result.verdict == "within_predicted_capacity"
    assert result.applicability_flags == ()
    assert result.validation_status == "hand_calculation_only"
    assert result.engineering_acceptance_established is False


@pytest.mark.parametrize(
    "demand, verdict", [(0.0, "within_predicted_capacity"),
                        (0.5, "exceeds_predicted_capacity")],
)
def test_demand_comparison(api, case, demand, verdict):
    """Cho (1987) equation 6.17, page 194: capacity 0.4081429636.

    Synthetic demands 0 and 0.5 give utilization 0 and 1.2250609335,
    respectively. Neither comparison establishes structural acceptance.
    """
    result = api.screen_dented_axial(replace(case, axial_demand_ratio=demand))
    assert result.verdict == verdict
    assert result.utilization == pytest.approx(demand / 0.4081429636, abs=1e-9)
    assert result.engineering_acceptance_established is False


@pytest.mark.parametrize(
    "field, lower, upper",
    [
        ("midwall_diameter_to_thickness", 20.0, 60.0),
        ("reduced_slenderness", 0.25, 1.5),
        ("dent_depth_ratio", 0.0, 0.15),
        ("neutral_axis_bow_ratio", 0.0005, 0.02),
    ],
)
def test_study_envelope_boundaries(api, case, field, lower, upper):
    """Cho (1987) section 6.3.1, page 193: listed grid extrema.

    Each endpoint is inside the implemented study envelope. The adjacent
    floating-point value outside is excluded; negative dent depth is invalid.
    These checks establish the guard, not experimental validity between points.
    """
    for endpoint in (lower, upper):
        result = api.screen_dented_axial(replace(case, **{field: endpoint}))
        assert result.applicability_flags == ()
        assert result.capacity_ratio is not None
    for value in (math.nextafter(lower, -math.inf),
                  math.nextafter(upper, math.inf)):
        if value < 0:
            with pytest.raises(ValueError):
                api.screen_dented_axial(replace(case, **{field: value}))
        else:
            result = api.screen_dented_axial(replace(case, **{field: value}))
            assert field in result.applicability_flags
            assert result.capacity_ratio is None
            assert result.utilization is None
            assert result.verdict == "outside_applicability"


@pytest.mark.parametrize("ratio", [599.0, 601.0])
def test_material_study_constraint(api, case, ratio):
    """Cho (1987) section 6.3.1, page 193: E/yield held at 600.

    Synthetic values on each side have no established fit in this study.
    """
    result = api.screen_dented_axial(replace(case, elastic_to_yield_ratio=ratio))
    assert "elastic_to_yield_ratio" in result.applicability_flags
    assert result.capacity_ratio is None


@pytest.mark.parametrize(
    "field, unsupported",
    [
        ("simply_supported", False),
        ("sharp_midspan_dent", False),
        ("bow_at_plastic_neutral_axis", False),
        ("has_applied_bending", True),
        ("has_hydrostatic_pressure", True),
    ],
)
def test_unsupported_conditions(api, case, field, unsupported):
    """Cho (1987) section 6.4.2.1, page 197; axial subset of 6.17.

    End restraints, dent position/shape, and bow reference must match the
    fitted method. Applied bending and pressure are excluded from this subset.
    """
    result = api.screen_dented_axial(replace(case, **{field: unsupported}))
    assert field in result.applicability_flags
    assert result.capacity_ratio is None


@pytest.mark.parametrize(
    "field", ["midwall_diameter_to_thickness", "reduced_slenderness",
              "dent_depth_ratio", "neutral_axis_bow_ratio",
              "elastic_to_yield_ratio", "axial_demand_ratio"],
)
@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf, -1.0])
def test_invalid_numeric_inputs(api, case, field, value):
    """Cho (1987) notation pages 3, 7, 8: finite nonnegative ratios.

    Nonfinite or negative values do not define the cited physical quantities.
    """
    with pytest.raises(ValueError):
        api.screen_dented_axial(replace(case, **{field: value}))


@pytest.mark.parametrize("field", ["midwall_diameter_to_thickness",
                                   "reduced_slenderness",
                                   "elastic_to_yield_ratio"])
def test_zero_denominators(api, case, field):
    """Cho (1987) notation page 8 and section 6.3.1: positive ratios."""
    with pytest.raises(ValueError):
        api.screen_dented_axial(replace(case, **{field: 0.0}))


@pytest.mark.parametrize("value", [True, "40"])
def test_numeric_types(api, case, value):
    """Cho (1987) notation page 3: D/t is numerical, not a condition."""
    with pytest.raises(TypeError):
        api.screen_dented_axial(
            replace(case, midwall_diameter_to_thickness=value)
        )


def test_condition_types(api, case):
    """Cho (1987) section 6.4.2.1: support condition requires confirmation."""
    with pytest.raises(TypeError):
        api.screen_dented_axial(replace(case, simply_supported=1))


def test_citations(api, case):
    """Cho (1987) equations 6.13 to 6.17, page 194: complete locators."""
    citations = api.screen_dented_axial(case).citations
    assert {item.locator for item in citations} == {
        "Notation, pages 3, 7, 8", "Section 6.3.1, page 193",
        "Equation 6.13, page 194", "Equation 6.14, page 194",
        "Equation 6.15, page 194", "Equation 6.16, page 194 (zero pressure)",
        "Equation 6.17, page 194", "Section 6.4.2.1, page 197",
    }
    assert all(item.year == 1987 for item in citations)
    assert all(item.url == "https://theses.gla.ac.uk/77600/1/10997383.pdf"
               for item in citations)


def test_published_b1_remains_unvalidated(api, case):
    """Cho (1987) Tables 4.1/4.3, pages 113/122, specimen B1.

    Opened measured collapse is 43.1 kilonewtons and stress/yield is 0.45.
    E/yield = 212000/516 = 410.8527131783, unlike the fitted 600.
    D/t=41.4, reduced slenderness=1.25, dent=0.062, bow=0.0023.
    This test requires exclusion, not a tuned match to the measured capacity.
    """
    specimen = replace(
        case, midwall_diameter_to_thickness=41.4, reduced_slenderness=1.25,
        dent_depth_ratio=0.062, neutral_axis_bow_ratio=0.0023,
        elastic_to_yield_ratio=212000 / 516,
    )
    result = api.screen_dented_axial(specimen)
    assert result.applicability_flags == ("elastic_to_yield_ratio",)
    assert result.capacity_ratio is None
    assert result.engineering_acceptance_established is False
