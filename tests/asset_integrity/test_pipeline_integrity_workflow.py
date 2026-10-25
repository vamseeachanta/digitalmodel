"""Pressure-stage tests with independent arithmetic, no grade-table defaults.

Equation source: 49 CFR §192.105(a), 2025 annual edition, design formula
for steel pipe: P = (2 S t / D) F E T.
https://www.govinfo.gov/content/pkg/CFR-2025-title49-vol3/xml/CFR-2025-title49-vol3-sec192-105.xml

All input numbers below are synthetic test data, not regulatory factors or
material specifications. Expected numbers are hand calculations shown below.
"""

from dataclasses import asdict, replace
import json

import pytest

from digitalmodel.asset_integrity.pipeline_integrity_workflow import (
    PressureDesignInput,
    assess_pressure_design,
)


@pytest.fixture
def case():
    return PressureDesignInput(
        outer_diameter_mm=500.0,
        nominal_wall_thickness_mm=10.0,
        pressure_mpa=4.0,
        yield_strength_mpa=200.0,
        design_factor=0.5,
        joint_factor=1.0,
        temperature_factor=1.0,
        strength_reference="Synthetic strength for hand calculation",
        factors_reference="Synthetic factors for hand calculation",
        formula_applicable=True,
    )


@pytest.mark.parametrize(
    "changes,expected_pressure,expected_thickness,expected_utilisation,meets",
    [
        ({}, 4.0, 10.0, 1.0, True),
        ({"pressure_mpa": 6.0}, 4.0, 15.0, 1.5, False),
        (
            {"joint_factor": 0.8, "temperature_factor": 0.5},
            1.6, 25.0, 2.5, False,
        ),
    ],
)
def test_pressure_stage_hand_calculation(
    case, changes, expected_pressure, expected_thickness, expected_utilisation,
    meets,
):
    """Independent substitutions in §192.105(a), 2025 edition.

    Base: (2*200*10/500)*0.5*1*1 = 4 MPa;
    t_req = 4*500/(2*200*0.5*1*1) = 10 mm; U = 4/4 = 1.
    Higher demand: t_req = 6*500/200 = 15 mm; U = 6/4 = 1.5.
    Reduced factors: P = 4*0.8*0.5 = 1.6 MPa;
    t_req = 4*500/(200*0.8*0.5) = 25 mm; U = 4/1.6 = 2.5.
    """
    result = assess_pressure_design(replace(case, **changes))
    assert result.pressure_status == "EVALUATED"
    assert result.design_pressure_mpa == pytest.approx(expected_pressure)
    assert result.required_wall_thickness_mm == pytest.approx(expected_thickness)
    assert result.utilisation_ratio == pytest.approx(expected_utilisation)
    assert result.meets_pressure_design is meets
    assert result.ffs_status == "NOT_EVALUATED"
    assert result.integrity_status == "NOT_EVALUATED"


def test_zero_gauge_pressure(case):
    """Zero demand gives t_req = 0*500/200 = 0 and U = 0/4 = 0."""
    result = assess_pressure_design(replace(case, pressure_mpa=0.0))
    assert result.required_wall_thickness_mm == 0.0
    assert result.utilisation_ratio == 0.0
    assert result.meets_pressure_design is True
    assert result.integrity_status == "NOT_EVALUATED"


def test_unconfirmed_applicability_withholds_pressure_verdict(case):
    result = assess_pressure_design(replace(case, formula_applicable=False))
    assert result.pressure_status == "NOT_EVALUATED"
    assert result.design_pressure_mpa is None
    assert result.required_wall_thickness_mm is None
    assert result.utilisation_ratio is None
    assert result.meets_pressure_design is None
    assert result.ffs_status == "NOT_EVALUATED"
    assert result.integrity_status == "NOT_EVALUATED"
    assert any("applicability" in reason for reason in result.limitations)


def test_result_retains_basis_and_serialises_without_nonfinite_values(case):
    result = assess_pressure_design(case)
    assert result.inputs == case
    assert "192.105(a)" in result.equation_reference
    assert "2025" in result.equation_reference
    assert result.limitations
    assert json.loads(json.dumps(asdict(result), allow_nan=False))["inputs"] == (
        asdict(case)
    )


@pytest.mark.parametrize(
    "field",
    [
        "outer_diameter_mm", "nominal_wall_thickness_mm", "yield_strength_mpa",
        "design_factor", "joint_factor", "temperature_factor",
    ],
)
@pytest.mark.parametrize("value", [0.0, -1.0, float("nan"), float("inf")])
def test_rejects_invalid_positive_inputs(case, field, value):
    with pytest.raises(ValueError, match=field):
        replace(case, **{field: value})


@pytest.mark.parametrize("value", [-1.0, float("nan"), float("inf")])
def test_rejects_invalid_pressure(case, value):
    with pytest.raises(ValueError, match="pressure_mpa"):
        replace(case, pressure_mpa=value)


@pytest.mark.parametrize("thickness", [250.0, 300.0])
def test_rejects_geometry_without_a_bore(case, thickness):
    with pytest.raises(ValueError, match="bore"):
        replace(case, nominal_wall_thickness_mm=thickness)


@pytest.mark.parametrize("field", ["strength_reference", "factors_reference"])
@pytest.mark.parametrize("value", ["", "  ", None])
def test_requires_traceable_input_basis(case, field, value):
    with pytest.raises(ValueError, match=field):
        replace(case, **{field: value})


def test_applicability_requires_boolean(case):
    with pytest.raises(ValueError, match="formula_applicable"):
        replace(case, formula_applicable="yes")


@pytest.mark.parametrize("field", ["design_factor", "joint_factor", "temperature_factor"])
@pytest.mark.parametrize("value", [1.0000000000000002, 72.0])
def test_rejects_factors_above_one(case, field, value):
    with pytest.raises(ValueError, match=field):
        replace(case, **{field: value})


@pytest.mark.parametrize("field", ["design_factor", "joint_factor", "temperature_factor"])
@pytest.mark.parametrize("value", [0.5, 1.0])
def test_accepts_fractional_factors_and_unit_boundary(case, field, value):
    result = assess_pressure_design(replace(case, **{field: value}))
    assert result.pressure_status == "EVALUATED"
    assert getattr(result.inputs, field) == value


def test_large_strength_has_finite_calculated_capacity(case):
    """P = (10/500)*2*1e308*0.5 = 2e306 MPa, despite 2*S overflowing."""
    result = assess_pressure_design(replace(case, yield_strength_mpa=1e308))
    assert result.design_pressure_mpa == pytest.approx(2e306)
    assert result.utilisation_ratio == pytest.approx(2e-306, abs=0.0)
    assert result.required_wall_thickness_mm == pytest.approx(2e-305, abs=0.0)
    assert result.meets_pressure_design is True
    json.dumps(asdict(result), allow_nan=False)


def test_rejects_capacity_underflow_to_zero(case):
    with pytest.raises(ValueError, match="design pressure"):
        assess_pressure_design(replace(case, yield_strength_mpa=5e-324))


def test_rejects_nonfinite_calculated_utilisation(case):
    with pytest.raises(ValueError, match="thickness and utilisation"):
        assess_pressure_design(replace(case, yield_strength_mpa=1e-307))


@pytest.mark.parametrize("value", [True, None, "200"])
def test_rejects_non_numeric_strength(case, value):
    with pytest.raises(ValueError, match="yield_strength_mpa"):
        replace(case, yield_strength_mpa=value)
