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
    )
    return Part9Level1Case(**(values | changes))


@pytest.mark.parametrize("geometry", ["plate", "cylinder", "sphere"])
def test_complete_intake_still_requires_qualified_curves(geometry):
    payload = screen_part9_level1(complete_case(geometry=geometry)).to_dict()
    assert payload["missing_inputs"] == []
    assert payload["missing_evidence"]
    assert payload["acceptable"] is None


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


@pytest.mark.parametrize("changes", [
    {"geometry": "elbow"}, {"reference_flaw": "surface"},
    {"flaw_depth_mm": 21.0}, {"ultimate_strength_mpa": 250.0},
])
def test_invalid_case_is_rejected(changes):
    with pytest.raises(ValueError):
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


def test_full_depth_is_a_geometric_input_not_acceptance():
    payload = screen_part9_level1(complete_case(
        reference_flaw="full_thickness", flaw_depth_mm=20.0,
    )).to_dict()
    assert payload["missing_inputs"] == []
    assert payload["acceptable"] is None


@pytest.mark.parametrize("geometry", ["plate", "cylinder", "sphere"])
@pytest.mark.parametrize("reference_flaw", ["quarter_thickness", "full_thickness"])
def test_explicit_crack_route_escalates_without_metal_loss_inference(
    geometry, reference_flaw,
):
    from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter

    case = complete_case(geometry=geometry, reference_flaw=reference_flaw)
    payload = FFSRouter.route_crack_like_flaw(case)
    assert payload["assessment_type"] == "CRACK_LIKE"
    assert payload["level_reached"] == 1
    assert payload["level1"]["status"] == "NOT_EVALUATED"
    assert payload["decision"]["verdict"] == "ESCALATE"
    assert payload["decision"]["next_step"] == "PART9_LEVEL2"
    assert "validated_reference_flaw_curves" in payload["decision"]["missing_evidence"]
    assert "NOT_EVALUATED" in payload["decision"]["governing_criterion"]
    assert "rsf" not in payload["decision"]
    assert json.loads(json.dumps(payload, allow_nan=False)) == payload


def test_decision_preserves_missing_data_and_evidence_separately():
    from digitalmodel.asset_integrity.assessment.ffs_decision import decide_part9_level1

    result = screen_part9_level1(complete_case(fracture_toughness_mpa_sqrt_m=None))
    decision = decide_part9_level1(result)
    assert decision["verdict"] == "ESCALATE"
    assert decision["missing_inputs"] == ["fracture_toughness_mpa_sqrt_m"]
    assert decision["missing_evidence"] == result.to_dict()["missing_evidence"]
    assert "NOT_EVALUATED" in decision["governing_criterion"]


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
