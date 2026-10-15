"""Regression contracts for the reviewed S-lay input correction; no solver."""
import json
from copy import deepcopy
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import _NoAliasDumper
from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.lines_builder import LinesBuilder
from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import screen_case

ROOT = Path(__file__).resolve().parents[3]
CAMPAIGN = ROOT / "docs/domains/pipeline-installation/slay-campaign"
KEY = "ConnectionxBendingStiffness, ConnectionyBendingStiffness"


def case_spec(case_id):
    import importlib.util
    spec = importlib.util.spec_from_file_location(
        "prepare_revision", ROOT / "scripts/orcaflex/prepare_slay_campaign.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    table = yaml.safe_load((CAMPAIGN / "case-table.yml").read_text(encoding="utf-8"))
    case = next(row for row in table["cases"] if row["id"] == case_id)
    return module.case_spec(table["basis"], case)


def assert_end_form(line):
    assert line["IncludeTorsion"] == "No"
    assert line[KEY] == [[], [0, None]]
    assert not any("TwistingStiffness" in key for key in line)


def test_saved_end_form_and_compact_rows():
    line = LinesBuilder(case_spec("d0-t0"), BuilderContext()).build()["Lines"][0]
    assert_end_form(line)
    assert "- []\n" in yaml.dump(line, Dumper=_NoAliasDumper)
    assert "- [0, ~]\n" in yaml.dump(line, Dumper=_NoAliasDumper)
    bad = deepcopy(line)
    bad["IncludeTorsion"] = "Yes"
    with pytest.raises(AssertionError):
        assert_end_form(bad)
    bad = deepcopy(line)
    bad[KEY] = [[], [0, 0, 0, 0]]
    with pytest.raises(AssertionError):
        assert_end_form(bad)


@pytest.mark.parametrize("case_id", [f"d{d}-t{t}" for d in range(2) for t in range(3)])
def test_production_span_closes_weight_only_length(case_id):
    screen = screen_case(CAMPAIGN, case_id)
    assert 0 <= screen["current"]["excess_m"] <= screen["lead_m"]
    line = LinesBuilder(case_spec(case_id), BuilderContext()).build()["Lines"][0]
    top, bottom = next(v for k, v in line.items() if k.startswith("Connection, "))
    assert top[1] - bottom[1] == pytest.approx(screen["current"]["taut_anchor_span_m"])
    # Moving the anchor inward by more than the lead reproduces the old defect.
    excess = screen["pipe_length_m"] - screen["current"]["taut_length_m"]
    with pytest.raises(AssertionError):
        assert 0 <= excess + 2 * screen["lead_m"] <= screen["lead_m"]
    combined = screen["combined"]
    assert abs(combined["excess_m"]) <= screen["length_tolerance_m"]
    assert abs(combined["angle_mismatch_deg"]) <= screen["angle_tolerance_deg"]
    assert "arithmetic reproduction" in screen["tolerance_meaning"]


@pytest.mark.parametrize("case_id", ["d0-t0", "d1-t0"])
def test_probe_manifests_separate_prior_qa_from_current_inputs(case_id):
    manifest = json.loads((CAMPAIGN / f"probes/{case_id}-anchor-span.json").read_text(encoding="utf-8"))
    assert manifest["base_case"] == case_id
    assert manifest["current_inputs_native_status"] == "not_run"
    assert manifest["qa_pass_criteria"]["maximum_overbend_strain"] == 0.00254
    assert manifest["screen"]["combined"]["anchor_span_m"] > 0
    if case_id == "d0-t0":
        assert "statics not converged" in manifest["native_status"]
        assert manifest["previous_qa"]["error_code"] == 27
        assert manifest["previous_qa"]["required_readings_available"] is False
        assert manifest["previous_qa"]["load_warnings"] == 0


@pytest.mark.parametrize("fault", ["wire", "travel", "tension", "strain", "nonfinite"])
def test_static_acceptance_rejects_false_convergence(fault):
    from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import assert_static_acceptance
    values = dict(wire_length=10.0, displacement=0.0, lead=10.0,
                  grounded_tension=[1.0], overbend_strain=0.002)
    assert_static_acceptance(**values)
    key, bad = {"wire": ("wire_length", 0.0), "travel": ("displacement", 11.0),
                "tension": ("grounded_tension", [-1.0]), "strain": ("overbend_strain", 0.003),
                "nonfinite": ("wire_length", float("nan"))}[fault]
    values[key] = bad
    with pytest.raises(ValueError):
        assert_static_acceptance(**values)
