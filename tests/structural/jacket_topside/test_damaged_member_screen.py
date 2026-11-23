"""Independent geometry/load checks; no platform qualification is asserted."""
import copy
import importlib
import json
import math
import subprocess
import sys
from pathlib import Path

import pytest


@pytest.fixture
def api():
    try:
        return importlib.import_module(
            "digitalmodel.structural.jacket_topside.damaged_member_screen"
        )
    except ModuleNotFoundError as exc:
        pytest.fail(f"Damaged-member workflow is missing: {exc}")


@pytest.fixture
def case():
    """Synthetic section and actions; not measured engineering inputs."""
    return {
        "id": "member-1", "flooded": False, "severed": False,
        "uniform_corrosion": True,
        "section": {"outer_diameter": 10.0, "wall_thickness": 1.0,
                    "yield_strength": 50.0, "elastic_modulus": 30000.0,
                    "effective_length": 100.0},
        "remaining_thickness": 0.5,
        "actions": {"axial_force": 30.0, "bending_moment": 20.0,
                    "Cm": 1.0, "is_tension": False},
    }


def test_uniform_corrosion_preserves_actions(api, case):
    """Hand integration of annulus and elementary beam stress relations.

    Synthetic OD=10 in; remaining t=0.5 in gives ID=9 in.
    A=pi*(100-81)/4=19*pi/4 in².
    I=pi*(10000-6561)/64=3439*pi/64 in⁴, S=I/5.
    For N=30 kip and M=20 kip-in: fa=120/(19*pi) ksi,
    fb=6400/(3439*pi) ksi. D/t=20. These are algebraic hand
    calculations, independent of the production section/check functions.
    """
    original = copy.deepcopy(case)
    result = api.screen_member(case)
    diagnostic = result["corrosion"]
    assert diagnostic["area_in2"] == pytest.approx(19 * math.pi / 4)
    assert diagnostic["section_modulus_in3"] == pytest.approx(3439 * math.pi / 320)
    assert diagnostic["axial_stress_ksi"] == pytest.approx(120 / (19 * math.pi))
    assert diagnostic["bending_stress_ksi"] == pytest.approx(6400 / (3439 * math.pi))
    assert diagnostic["diameter_to_thickness"] == 20
    assert diagnostic["status"] == "NOT_EVALUATED"
    # r² = I/A = (D² + d²)/16, so KL/r = 400/sqrt(181).
    assert diagnostic["slenderness_ratio"] == pytest.approx(400 / math.sqrt(181))
    assert case == original
    assert result["decision"]["verdict"] == "ESCALATE"
    assert result["engineering_acceptance_established"] is False
    assert result["decision"]["margin"] is None
    json.dumps(result, allow_nan=False)


def test_dent_prediction_keeps_qualification_gap(api, case):
    """Existing hand case: thesis (1987) eqs 6.13–6.17, p.194.

    Bow term=22.2*(0.005*1)^0.7=0.5440414350;
    dent term=1+1.26*0.1^1.3*40^0.6=1.5775725438;
    product=0.858264830595756. Half sum=1.429132415297878;
    lower root=0.4081429636; demand/root=0.4900243734.
    Decimal tolerance covers hand rounding only.
    """
    case.pop("remaining_thickness")
    case["uniform_corrosion"] = False
    case["dent"] = {
        "midwall_diameter_to_thickness": 40.0, "reduced_slenderness": 1.0,
        "dent_depth_ratio": 0.1, "neutral_axis_bow_ratio": 0.005,
        "elastic_to_yield_ratio": 600.0, "axial_demand_ratio": 0.2,
        "simply_supported": True, "sharp_midspan_dent": True,
        "bow_at_plastic_neutral_axis": True, "has_applied_bending": False,
        "has_hydrostatic_pressure": False,
    }
    result = api.screen_member(case)
    assert result["dent"]["capacity_ratio"] == pytest.approx(0.4081429636, abs=1e-10)
    assert result["dent"]["utilization"] == pytest.approx(0.4900243734, abs=1e-10)
    assert result["decision"]["verdict"] == "ESCALATE"
    assert "EXPERIMENTAL_VALIDATION_MISSING" in result["flags"]
    case["dent"]["has_applied_bending"] = True
    outside = api.screen_member(case)
    assert outside["dent"]["capacity_ratio"] is None
    assert "has_applied_bending" in outside["dent"]["applicability_flags"]


@pytest.mark.parametrize("flag", ["flooded", "severed"])
def test_system_damage_handoff_takes_precedence(api, case, flag):
    """Task routing requirement; no invented API assessment threshold."""
    case[flag] = True
    case["dent"] = {"invalid": "not calculated for a system handoff"}
    result = api.screen_member(case)
    assert result["dent"] is None
    assert result["corrosion"] is None
    assert result["handoff"]["requested_assessment"] == "Level 3 engineering review"
    assert result["handoff"]["solver_executed"] is False
    assert flag.upper() in result["flags"]
    assert result["decision"]["verdict"] == "ESCALATE"


@pytest.mark.parametrize("thickness", [0, -0.1, 1.1, float("nan"), float("inf"), True])
def test_reject_invalid_remaining_wall(api, case, thickness):
    case["remaining_thickness"] = thickness
    with pytest.raises((ValueError, TypeError)):
        api.screen_member(case)


@pytest.mark.parametrize("field", ["flooded", "severed", "uniform_corrosion"])
def test_require_explicit_conditions(api, case, field):
    case.pop(field)
    with pytest.raises(ValueError, match=field):
        api.screen_member(case)


def test_localized_corrosion_not_treated_as_uniform(api, case):
    case["uniform_corrosion"] = False
    result = api.screen_member(case)
    assert result["corrosion"] is None
    assert "LOCALIZED_CORROSION" in result["flags"]


@pytest.mark.parametrize("field,value", [("outer_diameter", 0), ("wall_thickness", 5),
                                         ("elastic_modulus", False), ("effective_length", -1)])
def test_reject_invalid_section(api, case, field, value):
    case["section"][field] = value
    with pytest.raises((ValueError, TypeError)):
        api.screen_member(case)


def test_combined_damage_not_independent_acceptance(api, case):
    dent_case = {
        "midwall_diameter_to_thickness": 40., "reduced_slenderness": 1.,
        "dent_depth_ratio": .1, "neutral_axis_bow_ratio": .005,
        "elastic_to_yield_ratio": 600., "axial_demand_ratio": .2,
        "simply_supported": True, "sharp_midspan_dent": True,
        "bow_at_plastic_neutral_axis": True, "has_applied_bending": False,
        "has_hydrostatic_pressure": False,
    }
    case["dent"] = dent_case
    result = api.screen_member(case)
    assert "COMBINED_DAMAGE_NOT_QUALIFIED" in result["flags"]
    assert result["dent"] is None
    assert result["decision"]["verdict"] == "ESCALATE"


def test_workflow_report_and_duplicate_ids(api, case):
    result = api.screen_campaign({"members": [case]})
    assert result["verdict"] == "ESCALATE"
    report = result["report_html"]
    assert "Revision history" in report
    assert "NOT_EVALUATED" in report
    assert "corroded" in report.lower()
    assert "677aa.pdf" in report
    assert "Level 3" in report
    assert "RSR=" not in report  # no member prediction presented as a system ratio
    with pytest.raises(ValueError, match="unique"):
        api.screen_campaign({"members": [case, case]})
    with pytest.raises(ValueError, match="nonempty"):
        api.screen_campaign({"members": []})


def test_worked_example_cli(api, tmp_path):
    """The shipped example executes offline and produces strict JSON and HTML."""
    root = Path(__file__).resolve().parents[3]
    example = root / "examples/workflows/jacket-damaged-member-screen/input.yml"
    output = tmp_path / "result.json"
    completed = subprocess.run(
        [sys.executable, "-m", api.__name__, str(example), "--output", str(output)],
        capture_output=True, text=True, check=False,
    )
    assert completed.returncode == 0, completed.stderr
    result = json.loads(output.read_text(encoding="utf-8"))
    assert len(result["members"]) == 3
    assert result["members"][0]["dent"]["capacity_ratio"] == pytest.approx(.4081429636)
    assert output.with_suffix(".html").read_text(encoding="utf-8") == result["report_html"]


def test_report_escapes_ids_and_preserves_summary(api, case):
    case["id"] = '<script>alert("x")</script>'
    report = api.screen_campaign({"members": [case]})["report_html"]
    assert '<script>alert("x")</script>' not in report
    assert "&lt;script&gt;" in report
    case["id"] = "Summary"
    report = api.screen_campaign({"members": [case]})["report_html"]
    assert "Assessment" in report
    assert "Member 1: Summary" in report


@pytest.mark.parametrize("field,value", [("axial_force", -1), ("bending_moment", float("nan")),
                                         ("Cm", 0), ("is_tension", "false")])
def test_invalid_actions_cannot_produce_diagnostics(api, case, field, value):
    case["actions"][field] = value
    with pytest.raises(ValueError):
        api.screen_member(case)


@pytest.mark.parametrize("is_tension", [False, True])
def test_corrosion_never_publishes_unqualified_strength_checks(api, case, is_tension):
    case["actions"]["is_tension"] = is_tension
    corrosion = api.screen_member(case)["corrosion"]

    def check_keys(value):
        if isinstance(value, dict):
            assert not {"allowable_stress", "unity_check", "passes"}.intersection(value)
            for item in value.values():
                check_keys(item)
        elif isinstance(value, (list, tuple)):
            for item in value:
                check_keys(item)

    check_keys(corrosion)
    assert "diagnostic_checks" not in corrosion


def test_campaign_headline_unites_all_member_flags_in_order(api, case):
    dent = {"id": "dent", "flooded": False, "severed": False,
            "uniform_corrosion": False, "dent": {
                "midwall_diameter_to_thickness": 40., "reduced_slenderness": 1.,
                "dent_depth_ratio": .1, "neutral_axis_bow_ratio": .005,
                "elastic_to_yield_ratio": 600., "axial_demand_ratio": .2,
                "simply_supported": True, "sharp_midspan_dent": True,
                "bow_at_plastic_neutral_axis": True, "has_applied_bending": False,
                "has_hydrostatic_pressure": False,
            }}
    severed = dict(case, id="severed", severed=True)
    result = api.screen_campaign({"members": [dent, severed, case]})
    headline = result["report_html"].split("<h3>Summary</h3>")[0]
    expected = "EXPERIMENTAL_VALIDATION_MISSING, SEVERED, MEMBER_CHECKS_NOT_QUALIFIED"
    assert expected in headline
    assert headline.count("EXPERIMENTAL_VALIDATION_MISSING") == 1
    assert result["members"][0]["flags"] == ["EXPERIMENTAL_VALIDATION_MISSING"]


def test_report_keeps_thickness_and_ratio_precision(api, case):
    case["remaining_thickness"] = .375
    dent = dict(case, id="dent", uniform_corrosion=False, dent={
        "midwall_diameter_to_thickness": 40., "reduced_slenderness": 1.,
        "dent_depth_ratio": .1, "neutral_axis_bow_ratio": .005,
        "elastic_to_yield_ratio": 600., "axial_demand_ratio": .4081429636 * 1.04,
        "simply_supported": True, "sharp_midspan_dent": True,
        "bow_at_plastic_neutral_axis": True, "has_applied_bending": False,
        "has_hydrostatic_pressure": False,
    })
    dent.pop("remaining_thickness")
    report = api.screen_campaign({"members": [case, dent]})["report_html"]
    assert "<td>remaining_thickness_in</td><td>0.375</td>" in report
    assert "<td>capacity_ratio</td><td>0.408</td>" in report
    assert "<td>utilization</td><td>1.040</td>" in report
    assert "<td>diameter_to_thickness</td><td>26.667</td>" in report


def test_applicability_has_one_ordered_explanation_per_flag(api, case):
    case["severed"] = True
    case["flooded"] = True
    result = api.screen_member(case)
    applicability = result["decision"]["applicability"]
    assert applicability["flags"] == result["flags"]
    assert len(applicability["notes"]) == len(result["flags"])
    for flag, note in zip(result["flags"], applicability["notes"]):
        assert note.startswith(flag + ": ")
        assert len(note) > len(flag) + 2


@pytest.mark.parametrize("with_dent", [False, True])
def test_uniform_corrosion_requires_remaining_wall(api, case, with_dent):
    case.pop("remaining_thickness")
    if with_dent:
        case["dent"] = {
            "midwall_diameter_to_thickness": 40., "reduced_slenderness": 1.,
            "dent_depth_ratio": .1, "neutral_axis_bow_ratio": .005,
            "elastic_to_yield_ratio": 600., "axial_demand_ratio": .2,
            "simply_supported": True, "sharp_midspan_dent": True,
            "bow_at_plastic_neutral_axis": True, "has_applied_bending": False,
            "has_hydrostatic_pressure": False,
        }
    result = api.screen_member(case)
    assert "CORROSION_INPUT_MISSING" in result["flags"]
    assert result["corrosion"] is None
    assert result["decision"]["verdict"] == "ESCALATE"
    if with_dent:
        assert result["dent"]["capacity_ratio"] is not None
        assert "COMBINED_DAMAGE_NOT_QUALIFIED" in result["flags"]


@pytest.mark.parametrize("key", ["section", "actions"])
@pytest.mark.parametrize("value", ["missing", None, [], 1])
def test_required_blocks_raise_named_value_error(api, case, key, value):
    if value == "missing":
        case.pop(key)
    else:
        case[key] = value
    with pytest.raises(ValueError, match=key):
        api.screen_member(case)


@pytest.mark.parametrize("config", [None, [], "members", 1])
def test_campaign_requires_top_level_mapping(api, config):
    with pytest.raises(ValueError, match="config.*mapping"):
        api.screen_campaign(config)


def test_empty_yaml_reports_named_config_error(api, tmp_path):
    source = tmp_path / "empty.yml"
    source.write_text("", encoding="utf-8")
    output = tmp_path / "result.json"
    completed = subprocess.run(
        [sys.executable, "-m", api.__name__, str(source), "--output", str(output)],
        capture_output=True, text=True, check=False,
    )
    assert completed.returncode != 0
    assert "ValueError: config must be a mapping" in completed.stderr
    assert not output.exists()


def test_corrosion_locator_is_secondary_and_consistent(api):
    root = Path(__file__).resolve().parents[3]
    locator = api.CORROSION_SOURCE["locator"]
    assert "ISO 19902:2007 A13.8" in locator
    assert "secondary citation" in locator
    for path in ("docs/domains/asset-integrity/jacket-damaged-member-screen.md",
                 "examples/workflows/jacket-damaged-member-screen/input.yml"):
        assert locator in (root / path).read_text(encoding="utf-8")


def test_revision_date_does_not_follow_execution_date(api, case, monkeypatch):
    from datetime import date

    class LaterDate(date):
        @classmethod
        def today(cls):
            return cls(2099, 1, 1)

    monkeypatch.setattr(api, "date", LaterDate, raising=False)
    report = api.screen_campaign({"members": [case]})["report_html"]
    revision = report.split("<h3>Revision history</h3>")[1].split("</table>")[0]
    assert "2026-10-20" in revision
    assert "2026-10-22" in revision
    assert "Initial damaged/corroded member screening" in revision
    assert "2099-01-01" not in revision
