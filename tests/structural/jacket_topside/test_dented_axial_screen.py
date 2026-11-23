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
from pathlib import Path

import pytest


# Cho (1987), Volume I, Table 4.1, printed page 113, B1 row:
# compressive static yield stress, newtons per square millimetre.
B1_COMPRESSIVE_YIELD = 516.0
# Table 4.3, printed page 122, footnote to reduced slenderness:
# mean tensile-test elastic modulus, newtons per square millimetre.
B1_ELASTIC_MODULUS = 212000.0
# Table 4.3, printed page 122, B1 row, respective column meanings:
B1_DIAMETER_TO_THICKNESS = 41.4
B1_REDUCED_SLENDERNESS = 1.25
B1_DENT_DEPTH_RATIO = 0.062  # Dent depth divided by diameter.
B1_BOW_RATIO = 0.0023  # Bow divided by length.
B1_COLLAPSE_LOAD_KILONEWTONS = 43.1  # Axial compressive collapse load.
B1_COLLAPSE_STRESS_TO_YIELD = 0.45  # Collapse stress/compressive yield stress.
METHOD_NOTE = (Path(__file__).resolve().parents[3]
               / "docs/domains/asset-integrity/jacket-dented-axial-method.md")


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


@pytest.mark.parametrize("ratio", [599.0, 601.0, 600 * 0.9999, 600 * 1.0001])
def test_material_study_constraint(api, case, ratio):
    """Cho (1987) section 6.3.1, page 193: E/yield held at 600.

    Synthetic values on each side have no established fit in this study.
    """
    result = api.screen_dented_axial(replace(case, elastic_to_yield_ratio=ratio))
    assert "elastic_to_yield_ratio" in result.applicability_flags
    assert result.capacity_ratio is None
    assert result.utilization is None
    assert result.verdict == "outside_applicability"


def test_material_ratio_computed_roundoff(api, case):
    """Synthetic modulus/yield division differs from 600 by one double ULP."""
    ratio = 180.00000000000003 / 0.3
    assert ratio != 600.0
    assert abs(ratio - 600.0) == math.ulp(600.0)
    result = api.screen_dented_axial(replace(case, elastic_to_yield_ratio=ratio))
    assert result.applicability_flags == ()
    assert result.capacity_ratio == api.screen_dented_axial(case).capacity_ratio
    assert "study value 600" in result.applicability_text
    assert "floating-point round-off only" in result.applicability_text


@pytest.mark.parametrize("direction", [-1, 1])
def test_material_roundoff_boundary(api, case, direction):
    """Four double ULPs at 600 cover round-off; the next value is excluded."""
    tolerance = api.MATERIAL_RATIO_ROUNDOFF_TOLERANCE
    assert tolerance == 4 * math.ulp(600.0)
    boundary = 600.0 + direction * tolerance
    assert api.screen_dented_axial(
        replace(case, elastic_to_yield_ratio=boundary)
    ).applicability_flags == ()
    outside = math.nextafter(boundary, direction * math.inf)
    result = api.screen_dented_axial(replace(case, elastic_to_yield_ratio=outside))
    assert result.applicability_flags == ("elastic_to_yield_ratio",)
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
    """Cho (1987), Volume I, Tables 4.1/4.3, printed pages 113/122, B1.

    Table 4.1 compressive yield column: 516 newtons per square millimetre.
    Table 4.3 footnote: modulus 212000 newtons per square millimetre.
    B1 columns: diameter/thickness 41.4, reduced slenderness 1.25,
    dent depth/diameter 0.062, bow/length 0.0023, axial collapse load
    43.1 kilonewtons, collapse stress/compressive yield stress 0.45.
    E/yield = 212000/516 = 410.8527131783, unlike the fitted 600.
    This test requires exclusion, not a tuned match to the measured capacity.
    """
    specimen = replace(
        case, midwall_diameter_to_thickness=B1_DIAMETER_TO_THICKNESS,
        reduced_slenderness=B1_REDUCED_SLENDERNESS,
        dent_depth_ratio=B1_DENT_DEPTH_RATIO, neutral_axis_bow_ratio=B1_BOW_RATIO,
        elastic_to_yield_ratio=B1_ELASTIC_MODULUS / B1_COMPRESSIVE_YIELD,
    )
    result = api.screen_dented_axial(specimen)
    assert result.applicability_flags == ("elastic_to_yield_ratio",)
    assert result.capacity_ratio is None
    assert result.engineering_acceptance_established is False



def test_b1_table_4_1_quoted_value():
    """Pin Table 4.1, printed page 113, B1 compressive static yield column."""
    assert B1_COMPRESSIVE_YIELD == 516.0
    note = METHOD_NOTE.read_text()
    assert "Table 4.1, printed page 113, row B1" in note
    assert "compressive static yield stress is 516 newtons per square millimetre" in note


def test_b1_table_4_3_quoted_values():
    """Pin Table 4.3, printed page 122, B1 columns and modulus footnote.

    Columns: diameter/thickness, reduced slenderness, dent depth/diameter,
    bow/length, axial collapse load, collapse stress/compressive yield stress.
    The modulus is the mean tensile-test value in the table footnote.
    """
    assert (B1_DIAMETER_TO_THICKNESS, B1_REDUCED_SLENDERNESS,
            B1_DENT_DEPTH_RATIO, B1_BOW_RATIO, B1_COLLAPSE_LOAD_KILONEWTONS,
            B1_COLLAPSE_STRESS_TO_YIELD, B1_ELASTIC_MODULUS) == (
                41.4, 1.25, 0.062, 0.0023, 43.1, 0.45, 212000.0)
    note = METHOD_NOTE.read_text()
    assert "Table 4.3, printed page 122, row B1" in note
    for quote in ("diameter/thickness 41.4", "reduced slenderness 1.25",
                  "dent depth/diameter 0.062", "bow/length 0.0023",
                  "axial compressive collapse load 43.1 kilonewtons",
                  "collapse stress/compressive yield stress 0.45",
                  "modulus of 212000 newtons per square millimetre"):
        assert quote in note


def test_method_note_recheck_gap():
    """Re-check reuse remains unverified; no existing-code defect is asserted."""
    note = METHOD_NOTE.read_text()
    assert "not verified against API RP 2A-WSD" in note
    assert "reuse for the corroded-member re-check remains a gap" in note
    assert "inelastic column denominator absent" not in note
    assert "absent from the unconditional bending allowable" not in note
