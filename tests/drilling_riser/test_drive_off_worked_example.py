"""Offline evidence checks for the worked example recorded on issue 162.

The reproduction remains blocked: no case-specific defaults or screening
results can stand in for the reported coupled drive-off time histories.
"""
from __future__ import annotations

import ast
import inspect
import json
import math
from pathlib import Path

import pytest

from digitalmodel.drilling_riser import drift_off
from digitalmodel.drilling_riser.envelope import EnvelopeCriteria, RiserSection
from digitalmodel.drilling_riser.telemetry_inputs import DPState
from digitalmodel.subsea.mooring_analysis.models import EnvironmentalConditions

_FIXTURE = Path(__file__).parent / "fixtures" / "drive_off_worked_example_2017.json"
_UNITS = {"kg", "kg m2", "m/s", "m", "s", "MN", "degrees", "MN m",
          "Beaufort", "not stated", "not applicable"}
_CITATION = (
    "Liu, Chen, Ji and Fu, Mechanika 23(2), 2017, "
    "doi 10.5755/j01.mech.23.2.13780"
)


def _read_case():
    return json.loads(_FIXTURE.read_text(encoding="utf-8"))


def _validate_entry(entry):
    assert {"value", "unit", "note_section", "caveat", "note_wording"} <= entry.keys()
    assert entry["unit"] in _UNITS
    assert entry["note_section"] in {"3", "4", "5", "8"}
    assert isinstance(entry["caveat"], str) and entry["caveat"].strip()
    assert isinstance(entry["note_wording"], str) and entry["note_wording"].strip()
    value = entry["value"]
    if entry["unit"] == "not applicable":
        assert isinstance(value, str) and value.strip()
    else:
        assert type(value) in (float, int)  # bool is not a recorded measurement
        assert math.isfinite(value) and value > 0


def _recorded_arguments(case):
    inputs = case["inputs"]
    assert inputs["riser_length"]["unit"] == "m"
    assert inputs["top_tension"]["unit"] == "MN"
    # SI prefix conversion is explicit: one MN = 1_000_000 N.
    return {
        "length_m": inputs["riser_length"]["value"],
        "tension_n": inputs["top_tension"]["value"] * 1_000_000,
    }


def test_fixture_loads_and_preserves_source_provenance():
    case = _read_case()
    assert case["source"] == _CITATION
    assert case["source_of_values"] == "research note on the issue, 2026-10-11"
    assert case["licence_as_recorded_in_note"] == "CC BY 4.0"
    assert case["research_note_url"].endswith("#issuecomment-6115195050")
    for group in ("inputs", "reported_results"):
        assert case[group]
        for entry in case[group].values():
            _validate_entry(entry)


@pytest.mark.parametrize("bad_value", [0, -1, float("inf"), float("nan"), True])
def test_numeric_integrity_rejects_invalid_values(bad_value):
    entry = dict(_read_case()["inputs"]["vessel_mass"], value=bad_value)
    with pytest.raises(AssertionError):
        _validate_entry(entry)


def test_integrity_rejects_unrecognised_units():
    entry = dict(_read_case()["inputs"]["vessel_mass"], unit="unknown unit")
    with pytest.raises(AssertionError):
        _validate_entry(entry)


@pytest.mark.parametrize("input_name", ["riser_length", "top_tension"])
def test_mapping_rejects_incompatible_units(input_name):
    case = _read_case()
    case["inputs"][input_name]["unit"] = "kg"
    with pytest.raises(AssertionError):
        _recorded_arguments(case)


def test_signature_has_no_unaccounted_inputs_or_silent_defaults():
    case = _read_case()
    missing = [entry["parameter"] for entry in case["inputs_not_recorded_in_note"]]
    assert len(missing) == len(set(missing))
    supplied = set(_recorded_arguments(case))
    missing_top = {name for name in missing if "." not in name}
    parameters = set(inspect.signature(drift_off.drift_off_screen).parameters)
    assert supplied.isdisjoint(missing_top)
    # Include defaulted parameters: the generic defaults are not case evidence.
    assert supplied | missing_top == parameters
    assert case["module_argument_sources"] == {
        "length_m": "riser_length", "tension_n": "top_tension",
        "condition.current_speed": "surface_current_speed",
    }
    for entry in case["inputs_not_recorded_in_note"]:
        assert entry["caveat"].strip() and entry["note_section"].strip()


def test_nested_input_gaps_match_actual_dataclass_signatures():
    case = _read_case()
    missing = {entry["parameter"] for entry in case["inputs_not_recorded_in_note"]}
    supplied = set(case["module_argument_sources"])
    for prefix, cls in (("dp_state", DPState), ("condition", EnvironmentalConditions),
                        ("section", RiserSection), ("criteria", EnvelopeCriteria)):
        gaps = {name.removeprefix(prefix + ".") for name in missing
                if name.startswith(prefix + ".")}
        recorded = {name.removeprefix(prefix + ".") for name in supplied
                    if name.startswith(prefix + ".")}
        assert gaps.isdisjoint(recorded)
        # Wave Hs/Tp semantics and structural OD remain unresolved; surface
        # current is supplied, but no complete condition can be constructed.
        assert gaps | recorded == set(inspect.signature(cls).parameters)


def test_config_gaps_cover_consumed_keys_without_loading_generic_config():
    missing = {entry["parameter"] for entry in
               _read_case()["inputs_not_recorded_in_note"]}
    source = ast.parse(inspect.getsource(drift_off.drift_off_screen))
    source_env = ast.parse(inspect.getsource(drift_off.environmental_force_n))
    for tree in (source, source_env):
        for node in ast.walk(tree):
            if (isinstance(node, ast.Subscript) and isinstance(node.value, ast.Name)
                    and isinstance(node.slice, ast.Constant)):
                if node.value.id == "config":
                    assert "config." + node.slice.value in missing
                elif node.value.id == "v":
                    assert "config.vessel." + node.slice.value in missing
            if (isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
                    and isinstance(node.func.value, ast.Name)
                    and node.func.value.id == "config" and node.func.attr == "get"):
                assert "config." + node.args[0].value in missing


def test_skip_reason_exposes_every_gap_and_reported_target():
    case = _read_case()
    reason = _skip_reason(case)
    for gap in case["inputs_not_recorded_in_note"]:
        assert gap["parameter"] in reason
    assert "coupled drive-off" in reason and "riser-force sign" in reason
    assert "50 s" in reason


def _skip_reason(case):
    names = ", ".join(entry["parameter"] for entry in
                      case["inputs_not_recorded_in_note"])
    return (
        "Missing case inputs: " + names + "; modelling gap: scalar quasi-static "
        "drift-off screening does not compute coupled drive-off upper flex-joint "
        "time history (reported limit at 50 s), riser-force sign change, or "
        "coupled versus uncoupled offsets. No missing input is defaulted."
    )


# Loading evidence here makes the collected skip reason list exact nested gaps.
# An absent or malformed fixture is a collection failure, not a silent skip.
@pytest.mark.skip(reason=_skip_reason(_read_case()))
def test_reproduce_drive_off_worked_example():
    case = _read_case()
    available_arguments = _recorded_arguments(case)
    partial_condition = {
        "current_speed": case["inputs"]["surface_current_speed"]["value"],
    }
    reported_time = case["reported_results"]["upper_flexjoint_limit_time"]
    # Deliberately incomplete: no DPState, full condition, section, criteria or
    # config is constructed. A future compatible time-domain model and complete
    # inputs must precede choosing a comparison tolerance from source evidence.
    pytest.fail(
        f"Reproduction remains blocked with arguments {available_arguments}, "
        f"partial condition {partial_condition}; "
        f"reported coupled limit event: {reported_time['value']} {reported_time['unit']}"
    )
