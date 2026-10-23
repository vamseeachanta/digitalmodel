"""Flexible-pipe evidence screen; no licensed analysis software is loaded."""

import json
from pathlib import Path

import pytest
from pydantic import ValidationError

from digitalmodel.asset_integrity.flexible_pipe import (
    FlexiblePipeEvidence,
    screen_flexible_pipe,
)


def test_missing_evidence_never_implies_permission_to_operate():
    result = screen_flexible_pipe({})
    assert result["decision"]["verdict"] == "ESCALATE"
    assert len(result["failure_modes"]) == 5
    assert all(row["status"] == "NOT_EVALUATED" for row in result["failure_modes"])
    assert all(row["missing_inputs"] for row in result["failure_modes"])
    assert all(
        row["citation"] and row["next_action"] for row in result["failure_modes"]
    )
    assert result["remaining_capacity"] is None
    assert result["remaining_fatigue_life_years"] is None
    json.dumps(result, allow_nan=False)


@pytest.mark.parametrize(
    "broken,expected_intact,expected_fraction",
    [
        (0, 40, 0.0),
        (2, 38, 0.05),
        (40, 0, 1.0),
    ],
)
def test_wire_inventory_is_not_structural_capacity(
    broken, expected_intact, expected_fraction
):
    """Independent count arithmetic: N=40; b=0,2,40.

    Intact counts are 40-0=40, 40-2=38, 40-40=0; broken fractions
    are 0/40=0, 2/40=0.05, 40/40=1. These are invented unit-test
    counts, not measurements or API acceptance values. Capacity cannot
    follow from counts (2013 regulator field-experience report, sec. 5.5).
    """
    result = screen_flexible_pipe(
        {"total_armour_wires": 40, "broken_armour_wires": broken}
    )
    assert result["wire_inventory"]["intact_wire_count"] == expected_intact
    assert result["wire_inventory"]["broken_fraction"] == expected_fraction
    assert result["remaining_capacity"] is None
    assert result["decision"]["verdict"] == "ESCALATE"


@pytest.mark.parametrize(
    "broken,status",
    [(1, "BELOW_INPUT_LIMIT"), (2, "AT_INPUT_LIMIT"), (3, "ABOVE_INPUT_LIMIT")],
)
def test_explicit_input_limit_is_only_a_count_comparison(broken, status):
    """Hand calculation: b/40 is 0.025, 0.05, 0.075 versus an invented
    test-only input limit 0.05. This checks comparison, not an API limit.
    """
    result = screen_flexible_pipe(
        {
            "total_armour_wires": 40,
            "broken_armour_wires": broken,
            "allowable_broken_fraction": 0.05,
            "allowable_fraction_reference": "Test-only count criterion, section A",
        }
    )
    assert result["wire_inventory"]["input_limit_comparison"] == status
    assert result["decision"]["verdict"] == "ESCALATE"
    assert result["remaining_capacity"] is None


def test_unknown_count_is_not_zero():
    result = screen_flexible_pipe({"total_armour_wires": 40})
    assert result["wire_inventory"]["broken_fraction"] is None
    assert result["wire_inventory"]["intact_wire_count"] is None
    assert result["wire_inventory"]["input_limit_comparison"] == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "field,value",
    [
        ("total_armour_wires", 0),
        ("total_armour_wires", True),
        ("broken_armour_wires", -1),
        ("broken_armour_wires", 1.5),
        ("end_fitting_age_years", float("nan")),
        ("end_fitting_age_years", -1),
        ("design_fatigue_life_years", float("inf")),
        ("design_fatigue_life_years", 0),
        ("flooded_life_factor", 0),
        ("flooded_life_factor", 1.1),
        ("allowable_broken_fraction", -0.1),
        ("allowable_broken_fraction", True),
        ("outer_sheath_breach_location", "typo"),
        ("annulus_test", "passed"),
        ("vent_status", "unblocked"),
        ("carcass_collapse_indicated", "false"),
        ("load_history_available", 1),
        ("unknown_input", 2),
    ],
)
def test_invalid_inputs_are_rejected(field, value):
    with pytest.raises(ValidationError):
        screen_flexible_pipe({field: value})


@pytest.mark.parametrize(
    "evidence",
    [
        {"total_armour_wires": 4, "broken_armour_wires": 5},
        {"allowable_broken_fraction": 0.05},
        {"allowable_broken_fraction": 0.05, "allowable_fraction_reference": "  "},
        {"allowable_fraction_reference": "unpaired"},
        {"flooded_life_factor": 0.5},
        {"flooded_factor_reference": "unpaired"},
    ],
)
def test_inconsistent_inputs_are_rejected(evidence):
    with pytest.raises(ValidationError):
        FlexiblePipeEvidence.model_validate(evidence)


def test_clean_observations_do_not_qualify_a_method():
    result = screen_flexible_pipe(
        {
            "outer_sheath_breach_location": "none",
            "annulus_test": "dry",
            "vent_status": "clear",
            "total_armour_wires": 40,
            "broken_armour_wires": 0,
            "end_fitting_age_years": 0,
            "load_history_available": True,
            "carcass_collapse_indicated": False,
            "pressure_sheath_damage_indicated": False,
        }
    )
    assert result["decision"]["verdict"] == "ESCALATE"
    assert all(row["status"] == "NOT_EVALUATED" for row in result["failure_modes"])
    assert all(row["evidence"] for row in result["failure_modes"])


def test_flooding_factor_does_not_create_remaining_life():
    result = screen_flexible_pipe(
        {
            "annulus_test": "flooded",
            "design_fatigue_life_years": 20,
            "flooded_life_factor": 0.5,
            "flooded_factor_reference": "Test-only sensitivity assumption, section B",
        }
    )
    assert result["remaining_fatigue_life_years"] is None
    assert result["evidence"]["flooded_life_factor"] == 0.5
    assert any("environment" in gap.lower() for gap in result["limitations"])


def test_published_incident_evidence_requires_assessment():
    """Regulator field-experience report (2013), Fig. 3-7, p.25:
    submerged sheath damage and a vacuum test indicating leakage.
    Flooding and capacity were not quantified in that caption: neither
    is inferred here. This validates evidence routing only, not a
    published numerical capacity or repair/replacement verdict.
    """
    result = screen_flexible_pipe(
        {
            "outer_sheath_breach_location": "submerged",
            "annulus_test": "leak_indicated",
        }
    )
    row = result["failure_modes"][0]
    assert row["evidence"]["outer_sheath_breach_location"] == "submerged"
    assert row["evidence"]["annulus_test"] == "leak_indicated"
    assert result["evidence"]["annulus_test"] != "flooded"
    assert row["verdict"] == "ESCALATE"
    assert result["remaining_capacity"] is None


def test_report_escapes_inputs_and_persists_only_on_request(tmp_path):
    from digitalmodel.asset_integrity.flexible_pipe_workflow import router

    cfg = {
        "basename": "flexible_pipe_integrity_screen",
        "flexible_pipe_assessment": {
            "component_id": "<script>alert(1)</script>",
            "evidence": {"vent_status": "blocked"},
            "output_dir": "reports",
        },
    }
    cfg["_config_dir_path"] = str(tmp_path)
    result = router(cfg)["flexible_pipe_integrity_screen"]
    report = result["report_html"]
    assert "<script>" not in report
    assert "&lt;script&gt;" in report
    assert "NOT_EVALUATED" in report
    assert "blocked" in report
    assert "Remaining capacity" in report
    assert Path(result["report_path"]).read_text(encoding="utf-8") == report
    assert report.index("Revision history") < report.index("Inspection evidence")


def test_router_without_output_dir_does_not_write(tmp_path, monkeypatch):
    from digitalmodel.asset_integrity.flexible_pipe_workflow import router

    monkeypatch.chdir(tmp_path)
    cfg = router({"basename": "flexible_pipe_integrity_screen"})
    assert cfg["flexible_pipe_integrity_screen"]["decision"]["verdict"] == "ESCALATE"
    assert not list(tmp_path.iterdir())


def test_registered_workflow_runs_with_licensed_imports_blocked(monkeypatch):
    import sys

    # Engine imports legacy adapters. None sentinels prevent loading any
    # licensed API; this route uses only the offline evidence screen.
    monkeypatch.setitem(sys.modules, "OrcFxAPI", None)
    from digitalmodel.workflow_api import run_workflow

    envelope = run_workflow(
        "flexible-pipe-integrity-screen",
        params={
            "flexible_pipe_assessment": {
                "evidence": {
                    "total_armour_wires": 40,
                    "broken_armour_wires": 2,
                }
            },
        },
        verify_reproducible=True,
    )
    assert envelope.status == "ok", envelope.warnings
    result = envelope.result["value"]
    assert result["wire_inventory"]["broken_fraction"] == 0.05
    assert result["decision"]["verdict"] == "ESCALATE"
    assert result["remaining_capacity"] is None
    assert envelope.determinism["reproducible"] is True


def test_default_registered_workflow_exposes_unknowns(monkeypatch):
    import sys

    monkeypatch.setitem(sys.modules, "OrcFxAPI", None)
    from digitalmodel.workflow_api import run_workflow

    envelope = run_workflow("flexible-pipe-integrity-screen")
    assert envelope.status == "ok", envelope.warnings
    result = envelope.result["value"]
    assert result["evidence"]["annulus_test"] == "unknown"
    assert result["wire_inventory"]["broken_fraction"] is None


def test_documented_worked_example_executes(monkeypatch):
    """The guide's independent arithmetic is 40-2=38 and 2/40=0.05."""
    import sys
    from html.parser import HTMLParser

    class ExampleParser(HTMLParser):
        active = False
        code = ""

        def handle_starttag(self, tag, attrs):
            if tag == "pre" and dict(attrs).get("id") == "worked-example":
                self.active = True

        def handle_endtag(self, tag):
            if tag == "pre":
                self.active = False

        def handle_data(self, data):
            if self.active:
                self.code += data

    monkeypatch.setitem(sys.modules, "OrcFxAPI", None)
    doc = (
        Path(__file__).resolve().parents[2]
        / "docs/domains/asset-integrity/flexible-pipe-integrity-screen.html"
    )
    parser = ExampleParser()
    parser.feed(doc.read_text(encoding="utf-8"))
    assert parser.code.strip()
    exec(compile(parser.code, "documented-worked-example", "exec"), {})


def test_report_formats_fraction_as_percentage():
    """Independent arithmetic: one broken wire of three is 33.3 % at report precision."""
    from digitalmodel.asset_integrity.flexible_pipe_workflow import router

    result = router(
        {
            "basename": "flexible_pipe_integrity_screen",
            "flexible_pipe_assessment": {
                "evidence": {
                    "total_armour_wires": 3,
                    "broken_armour_wires": 1,
                }
            },
        }
    )["flexible_pipe_integrity_screen"]
    assert "33.3 %" in result["report_html"]
    assert "0.333333" not in result["report_html"]


def test_unavailable_load_history_is_missing_evidence():
    result = screen_flexible_pipe(
        {
            "end_fitting_age_years": 10,
            "load_history_available": False,
        }
    )
    row = next(
        row for row in result["failure_modes"] if row["mode"] == "end_fitting_fatigue"
    )
    assert row["missing_inputs"] == ["load_history_available"]
