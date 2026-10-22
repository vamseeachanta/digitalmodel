"""Part 9 Level 1 readiness tests; no published curve values are assumed.

The software contract withholds acceptance until the standard's assumptions
and independent curve validation are available. ASME's public 2021 contents
identifies sections 9.2.2, 9.3, 9.4.2, 9.4.3 and Annexes 9B/9C; it supplies
no numeric curve points. All dimensions below are synthetic test inputs.
"""

import json

import pytest

from digitalmodel.asset_integrity.assessment.part9_level1 import (
    Part9Level1Case,
    Part9Level1Result,
    screen_part9_level1,
)


@pytest.mark.parametrize("geometry", ["plate", "cylinder", "sphere"])
@pytest.mark.parametrize("reference_flaw", ["quarter_thickness", "full_thickness"])
def test_missing_curve_basis_withholds_acceptance(geometry, reference_flaw):
    result = screen_part9_level1(
        Part9Level1Case(geometry=geometry, reference_flaw=reference_flaw)
    )
    payload = result.to_dict()
    assert payload["status"] == "NOT_EVALUATED"
    assert payload["acceptable"] is None
    assert payload["allowable_flaw_length_mm"] is None
    assert payload["inputs"]["geometry"] == geometry
    assert payload["inputs"]["reference_flaw"] == reference_flaw
    assert "thickness_mm" in payload["missing_inputs"]
    assert "level1_applicability_and_assumptions" in payload["missing_evidence"]
    assert "validated_reference_flaw_curves" in payload["missing_evidence"]
    assert json.loads(json.dumps(payload, allow_nan=False)) == payload


def complete_case(**changes):
    """Synthetic dimensions and loads for software tests, not a design case."""
    values = dict(
        geometry="cylinder", reference_flaw="quarter_thickness",
        thickness_mm=20.0, flaw_depth_mm=5.0, flaw_length_mm=40.0,
        inside_radius_mm=500.0, membrane_stress_mpa=100.0,
        bending_stress_mpa=0.0, assessment_temperature_c=20.0,
        yield_strength_mpa=300.0, ultimate_strength_mpa=450.0,
        fracture_toughness_mpa_sqrt_m=80.0,
        flaw_orientation="axial", flaw_position="surface",
        material_location="base", pwht=False, cyclic_service=False,
        toughness_basis="synthetic test input",
    )
    return Part9Level1Case(**(values | changes))


@pytest.mark.parametrize("geometry", ["plate", "cylinder", "sphere"])
def test_complete_intake_still_requires_qualified_curves(geometry):
    payload = screen_part9_level1(complete_case(geometry=geometry)).to_dict()
    assert payload["missing_inputs"] == []
    assert payload["missing_evidence"]
    assert payload["acceptable"] is None


@pytest.mark.parametrize("field", [
    "flaw_orientation", "flaw_position", "material_location", "pwht",
    "cyclic_service", "toughness_basis",
])
def test_part9_context_fields_are_required(field):
    payload = screen_part9_level1(complete_case(**{field: None})).to_dict()
    assert payload["missing_inputs"] == [field]
    assert payload["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize("field,bad", [
    ("flaw_orientation", ""), ("flaw_orientation", "  "),
    ("flaw_orientation", 1), ("toughness_basis", ""),
    ("toughness_basis", "  "), ("toughness_basis", True),
    ("flaw_position", "unknown"), ("material_location", "unknown"),
    ("pwht", 0), ("pwht", "false"),
    ("cyclic_service", 1), ("cyclic_service", "false"),
])
def test_invalid_part9_context_is_rejected(field, bad):
    with pytest.raises(ValueError, match=field):
        screen_part9_level1(complete_case(**{field: bad}))


def test_false_service_and_heat_treatment_flags_are_supplied():
    payload = screen_part9_level1(complete_case()).to_dict()
    assert payload["missing_inputs"] == []
    assert payload["inputs"]["pwht"] is False
    assert payload["inputs"]["cyclic_service"] is False


@pytest.mark.parametrize("flaw_position", ["surface", "embedded"])
def test_non_through_wall_depth_must_be_below_thickness(flaw_position):
    with pytest.raises(ValueError, match="flaw_depth_mm"):
        screen_part9_level1(complete_case(
            flaw_position=flaw_position, flaw_depth_mm=20.0,
        ))


def test_through_wall_depth_must_match_thickness():
    with pytest.raises(ValueError, match="flaw_depth_mm"):
        screen_part9_level1(complete_case(flaw_position="through_wall"))


@pytest.mark.parametrize("geometry", ["plate", "cylinder", "sphere"])
def test_radius_required_only_for_curved_geometry(geometry):
    payload = screen_part9_level1(
        complete_case(geometry=geometry, inside_radius_mm=None)
    ).to_dict()
    expected = [] if geometry == "plate" else ["inside_radius_mm"]
    assert payload["missing_inputs"] == expected


@pytest.mark.parametrize("field", [
    "thickness_mm", "flaw_depth_mm", "flaw_length_mm", "inside_radius_mm",
    "membrane_stress_mpa", "bending_stress_mpa", "assessment_temperature_c",
    "yield_strength_mpa", "ultimate_strength_mpa", "fracture_toughness_mpa_sqrt_m",
])
@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -float("inf"), True, "20"])
def test_non_numeric_and_non_finite_values_are_rejected(field, bad):
    with pytest.raises(ValueError, match=field):
        screen_part9_level1(complete_case(**{field: bad}))


@pytest.mark.parametrize("field", [
    "thickness_mm", "flaw_depth_mm", "flaw_length_mm", "inside_radius_mm",
    "yield_strength_mpa", "ultimate_strength_mpa", "fracture_toughness_mpa_sqrt_m",
])
@pytest.mark.parametrize("bad", [0.0, -1.0])
def test_positive_quantities_reject_zero_and_negative_values(field, bad):
    with pytest.raises(ValueError, match=field):
        screen_part9_level1(complete_case(**{field: bad}))


@pytest.mark.parametrize("changes,field", [
    ({"geometry": "elbow"}, "geometry"),
    ({"reference_flaw": "surface"}, "reference_flaw"),
    ({"flaw_depth_mm": 21.0}, "flaw_depth_mm"),
    ({"ultimate_strength_mpa": 250.0}, "ultimate_strength_mpa"),
])
def test_invalid_case_is_rejected(changes, field):
    with pytest.raises(ValueError, match=field):
        screen_part9_level1(complete_case(**changes))


def test_signed_loads_and_temperature_are_retained_without_a_verdict():
    payload = screen_part9_level1(complete_case(
        membrane_stress_mpa=-100.0, bending_stress_mpa=0.0,
        assessment_temperature_c=-20.0,
    )).to_dict()
    assert payload["missing_inputs"] == []
    assert payload["inputs"]["bending_stress_mpa"] == 0.0
    assert payload["inputs"]["assessment_temperature_c"] == -20.0
    assert payload["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize("reference_flaw", ["quarter_thickness", "full_thickness"])
def test_full_depth_is_a_geometric_input_not_acceptance(reference_flaw):
    payload = screen_part9_level1(complete_case(
        reference_flaw=reference_flaw, flaw_depth_mm=20.0,
        flaw_position="through_wall",
    )).to_dict()
    assert payload["missing_inputs"] == []
    assert payload["acceptable"] is None


def test_ultimate_equal_to_yield_does_not_establish_acceptance():
    payload = screen_part9_level1(complete_case(
        ultimate_strength_mpa=300.0,
    )).to_dict()
    assert payload["missing_inputs"] == []
    assert payload["status"] == "NOT_EVALUATED"
    assert payload["acceptable"] is None


@pytest.mark.parametrize("temperature", [-300.0, -273.151])
def test_temperature_below_absolute_zero_is_rejected(temperature):
    with pytest.raises(ValueError, match="assessment_temperature_c"):
        screen_part9_level1(complete_case(assessment_temperature_c=temperature))


def test_absolute_zero_is_a_domain_boundary_without_acceptance():
    payload = screen_part9_level1(complete_case(
        assessment_temperature_c=-273.15,
    )).to_dict()
    assert payload["inputs"]["assessment_temperature_c"] == -273.15
    assert payload["acceptable"] is None


def test_oversized_integer_is_reported_as_an_invalid_input():
    with pytest.raises(ValueError, match="thickness_mm"):
        screen_part9_level1(complete_case(thickness_mm=10**400))


@pytest.mark.parametrize("scalar", ["float32", "float64", "int64"])
def test_numpy_real_scalars_produce_strict_json(scalar):
    import numpy as np

    payload = screen_part9_level1(complete_case(
        thickness_mm=getattr(np, scalar)(20),
    )).to_dict()
    assert payload["inputs"]["thickness_mm"] == 20.0
    assert json.loads(json.dumps(payload, allow_nan=False)) == payload


@pytest.mark.parametrize("scalar", ["int", "int64"])
def test_integer_input_precision_is_retained_in_json(scalar):
    import numpy as np

    value = 2**53 + 1
    supplied = np.int64(value) if scalar == "int64" else value
    payload = screen_part9_level1(complete_case(thickness_mm=supplied)).to_dict()
    assert payload["inputs"]["thickness_mm"] == value
    assert json.loads(json.dumps(payload))["inputs"]["thickness_mm"] == value


@pytest.mark.parametrize("field", [
    "flaw_length_mm", "membrane_stress_mpa", "assessment_temperature_c",
])
@pytest.mark.parametrize("sign", [-1, 1])
def test_nonzero_inputs_cannot_underflow_to_zero(field, sign):
    from fractions import Fraction

    with pytest.raises(ValueError, match=field):
        screen_part9_level1(complete_case(**{field: Fraction(sign, 10**400)}))


@pytest.mark.parametrize("changes,field", [
    ({"thickness_mm": float("nan")}, "thickness_mm"),
    ({"thickness_mm": 10**400}, "thickness_mm"),
    ({"assessment_temperature_c": -300.0}, "assessment_temperature_c"),
    ({"pwht": "false"}, "pwht"),
])
def test_direct_result_construction_validates_case(changes, field):
    with pytest.raises(ValueError, match=field):
        Part9Level1Result(case=complete_case(**changes), missing_inputs=())


def test_direct_result_cannot_hide_or_invent_missing_inputs():
    for case, missing in (
        (complete_case(thickness_mm=None), ()),
        (complete_case(), ("thickness_mm",)),
    ):
        with pytest.raises(ValueError, match="missing_inputs"):
            Part9Level1Result(case=case, missing_inputs=missing)


@pytest.mark.parametrize("geometry", ["plate", "cylinder", "sphere"])
@pytest.mark.parametrize("reference_flaw", ["quarter_thickness", "full_thickness"])
def test_explicit_crack_route_escalates_without_metal_loss_inference(
    geometry, reference_flaw,
):
    from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter

    case = complete_case(geometry=geometry, reference_flaw=reference_flaw)
    payload = FFSRouter.route_crack_like_flaw(case)
    assert payload["assessment_type"] == "CRACK_LIKE"
    assert payload["level_reached"] is None
    assert payload["level_attempted"] == 1
    assert payload["level1"]["status"] == "NOT_EVALUATED"
    assert payload["decision"]["verdict"] == "ESCALATE"
    assert payload["decision"]["next_step"] == "PART9_LEVEL2"
    assert "validated_reference_flaw_curves" in payload["decision"]["missing_evidence"]
    assert "NOT_EVALUATED" in payload["decision"]["governing_criterion"]
    assert "rsf" not in payload["decision"]
    assert json.loads(json.dumps(payload, allow_nan=False)) == payload


def test_empty_crack_intake_does_not_claim_a_completed_assessment():
    from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter

    payload = FFSRouter.route_crack_like_flaw(
        Part9Level1Case(geometry="plate", reference_flaw="quarter_thickness")
    )
    assert payload["level_reached"] is None
    assert payload["level_attempted"] == 1
    assert payload["decision"]["missing_inputs"]
    assert payload["decision"]["verdict"] == "ESCALATE"


def test_decision_preserves_missing_data_and_evidence_separately():
    from digitalmodel.asset_integrity.assessment.ffs_decision import decide_part9_level1

    result = screen_part9_level1(complete_case(fracture_toughness_mpa_sqrt_m=None))
    decision = decide_part9_level1(result)
    assert decision["verdict"] == "ESCALATE"
    assert decision["missing_inputs"] == ["fracture_toughness_mpa_sqrt_m"]
    assert decision["missing_evidence"] == result.to_dict()["missing_evidence"]
    assert "NOT_EVALUATED" in decision["governing_criterion"]


def test_runtime_type_hints_resolve_for_both_crack_adapters():
    from typing import get_type_hints

    from digitalmodel.asset_integrity.assessment.ffs_decision import decide_part9_level1
    from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter

    assert get_type_hints(decide_part9_level1)["result"] is Part9Level1Result
    assert get_type_hints(FFSRouter.route_crack_like_flaw)["case"] is Part9Level1Case


@pytest.mark.parametrize("missing", [False, True])
def test_decision_requests_missing_inputs_only_when_present(missing):
    from digitalmodel.asset_integrity.assessment.ffs_decision import decide_part9_level1

    case = complete_case(thickness_mm=None) if missing else complete_case()
    decision = decide_part9_level1(screen_part9_level1(case))
    criterion = decision["governing_criterion"]
    assert ("Complete the missing inputs" in criterion) is missing
    if missing:
        assert "thickness_mm" in criterion
    assert "qualified reference-flaw curves" in criterion
    assert "qualified Part 9 Level 2 assessment" in criterion


def test_returned_payload_mutation_does_not_change_future_assessments():
    result = screen_part9_level1(complete_case())
    payload = result.to_dict()
    payload["inputs"]["thickness_mm"] = 0.0
    payload["missing_evidence"].clear()
    fresh = result.to_dict()
    assert fresh["inputs"]["thickness_mm"] == 20.0
    assert fresh["missing_evidence"]


def test_worked_example_prints_inputs_method_and_unevaluated_answer(capsys):
    """The expected answer follows the evidence gate, with no curve calculation.

    Synthetic input: wall 20 mm, measured depth 5 mm, total length 40 mm,
    inside radius 500 mm. Even a complete intake has no qualified screening
    curves. Therefore the only supported answer is NOT_EVALUATED/ESCALATE,
    with no allowable flaw size. No published numeric answer is asserted.
    """
    import runpy
    from pathlib import Path

    example = Path(__file__).resolve().parents[2] / (
        "examples/asset_integrity/example_part9_level1.py"
    )
    runpy.run_path(str(example), run_name="__main__")
    payload = json.loads(capsys.readouterr().out)
    assert payload["input_basis"] == "synthetic demonstration"
    assert payload["level1"]["inputs"]["flaw_length_mm"] == 40.0
    assert payload["level1"]["missing_inputs"] == []
    assert payload["level1"]["status"] == "NOT_EVALUATED"
    assert payload["level1"]["acceptable"] is None
    assert payload["level1"]["allowable_flaw_length_mm"] is None
    assert payload["decision"]["verdict"] == "ESCALATE"
    assert payload["level1"]["method_reference"]
