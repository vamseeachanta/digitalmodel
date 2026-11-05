# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent boundary controls for the illustrative seakeeping report."""

import copy
import csv
import importlib.util
from pathlib import Path

import pytest
import yaml


@pytest.fixture
def control(tmp_path, monkeypatch):
    """Supply literal coordinates and arithmetic, without the spectral engine."""
    script = Path(__file__).resolve().parents[2] / "scripts/reports/build_vessel_seakeeping_module_report.py"
    spec = importlib.util.spec_from_file_location("seakeeping_contract", script)
    build = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(build)
    inputs = {
        "vessel": "illustrative", "output_dir": "results",
        "required_operability_pct": 95.0,
        "sea_states": [
            {"hs": 1.0, "tp": 6.0, "probability": 0.25},
            {"hs": 2.0, "tp": 8.0, "probability": 0.75},
        ],
        "dofs": [
            {"name": "heave", "unit": "m", "criterion": 2.0},
            {"name": "roll", "unit": "deg", "criterion": 3.0},
        ],
    }
    source = tmp_path / "input.yml"
    source.write_text(yaml.safe_dump({"vessel_seakeeping": inputs}), encoding="utf-8")
    monkeypatch.setattr(build, "EXAMPLE", str(source))
    return build, source, inputs


def install_output(monkeypatch, tmp_path, damage=lambda rows, summaries, settings: None):
    """Replace only the method under evaluation with independently known output."""
    from digitalmodel.vessel_seakeeping import workflow

    held = {}

    def router(cfg):
        settings = cfg["vessel_seakeeping"]
        rows = [
            {"dof": dof["name"], "unit": dof["unit"], **state,
             "criterion": dof["criterion"], "significant_amplitude": 1.0, "operable": True}
            for dof in settings["dofs"] for state in settings["sea_states"]
        ]
        summaries = [
            {"dof": dof["name"], "unit": dof["unit"], "criterion": dof["criterion"],
             "operability_pct": 100.0, "required_operability_pct": 95.0, "passes": True}
            for dof in settings["dofs"]
        ]
        damage(rows, summaries, settings)
        path = tmp_path / "returned.csv"
        with path.open("w", encoding="utf-8", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=[
                "dof", "unit", "hs", "tp", "probability", "criterion",
                "significant_amplitude", "operable",
            ])
            writer.writeheader()
            writer.writerows(rows)
        result = {"results_csv": str(path), "dofs": summaries,
                  "screening_status": settings.get("returned_status", "pass")}
        held.update(result=result)
        return {"vessel_seakeeping": result}

    monkeypatch.setattr(workflow, "router", router)
    return held


def test_complete_literal_control_and_independent_inputs(control, tmp_path, monkeypatch):
    build, source, inputs = control
    held = install_output(monkeypatch, tmp_path)
    actual = build.calculate(tmp_path / "calculation")
    expected = copy.deepcopy(inputs)
    expected.pop("vessel")
    expected.pop("output_dir")
    assert actual["inputs"] == expected
    assert len(actual["rows"]) == 4
    assert [row["dof"] for row in actual["rows"]] == ["heave", "heave", "roll", "roll"]
    assert all(row["significant_amplitude"] == 1.0 for row in actual["rows"])
    assert actual["status"] == "pass"
    held["result"]["dofs"][0]["dof"] = "changed after return"
    assert actual["summary"][0]["dof"] == "heave"
    assert yaml.safe_load(source.read_text())["vessel_seakeeping"] == inputs


@pytest.mark.parametrize("damage", [
    "missing_row", "surplus_row", "missing_summary", "surplus_summary",
    "reordered_rows", "reordered_summary", "replaced_label", "mutated_input_labels",
    "changed_coordinate", "changed_unit", "changed_criterion", "changed_weight",
    "nan_amplitude", "infinite_amplitude", "negative_amplitude", "nested_amplitude",
    "failed_amplitude", "invalid_boolean", "stale_summary", "nan_summary",
    "infinite_summary", "changed_requirement", "stale_summary_flag", "stale_status",
    "nested_summary", "boolean_summary", "missing_field", "surplus_field",
])
def test_invalid_output_cannot_qualify(control, tmp_path, monkeypatch, damage):
    build, source, inputs = control
    original_bytes = source.read_bytes()

    def corrupt(rows, summaries, settings):
        if damage == "missing_row":
            rows.pop()
        elif damage == "surplus_row":
            rows.append(copy.deepcopy(rows[-1]))
        elif damage == "missing_summary":
            summaries.pop()
        elif damage == "surplus_summary":
            summaries.append(copy.deepcopy(summaries[-1]))
        elif damage == "reordered_rows":
            rows.reverse()
        elif damage == "reordered_summary":
            summaries.reverse()
        elif damage == "replaced_label":
            summaries[0]["dof"] = "unexpected"
        elif damage == "mutated_input_labels":
            settings["dofs"][:] = [{**dof, "name": "unexpected"} for dof in settings["dofs"]]
            for row in rows + summaries:
                row["dof"] = "unexpected"
        elif damage == "changed_coordinate":
            rows[0]["tp"] = 9.0
        elif damage == "changed_unit":
            rows[0]["unit"] = "unexpected"
        elif damage == "changed_criterion":
            rows[0]["criterion"] = 9.0
        elif damage == "changed_weight":
            rows[0]["probability"] = 0.5
        elif damage.endswith("amplitude"):
            rows[0]["significant_amplitude"] = {
                "nan_amplitude": float("nan"), "infinite_amplitude": float("inf"),
                "negative_amplitude": -1.0, "nested_amplitude": [1.0], "failed_amplitude": 5.0,
            }[damage]
        elif damage == "invalid_boolean":
            rows[0]["operable"] = "unknown"
        elif damage == "stale_summary":
            summaries[0]["operability_pct"] = 25.0
        elif damage == "nan_summary":
            summaries[0]["operability_pct"] = float("nan")
        elif damage == "infinite_summary":
            summaries[0]["operability_pct"] = float("inf")
        elif damage == "changed_requirement":
            summaries[0]["required_operability_pct"] = 0.0
        elif damage == "stale_summary_flag":
            summaries[0]["passes"] = False
        elif damage == "stale_status":
            settings["returned_status"] = "fail"
        elif damage == "nested_summary":
            summaries[0]["operability_pct"] = [100.0]
        elif damage == "boolean_summary":
            summaries[0]["operability_pct"] = True
        elif damage == "missing_field":
            rows[0].pop("criterion")
        elif damage == "surplus_field":
            summaries[0]["extra"] = 1.0

    install_output(monkeypatch, tmp_path, corrupt)
    with pytest.raises(ValueError, match="Report output"):
        build.calculate(tmp_path / "calculation")
    assert source.read_bytes() == original_bytes
    assert yaml.safe_load(original_bytes)["vessel_seakeeping"] == inputs


def test_valid_failed_screening_follows_current_evidence(control, tmp_path, monkeypatch):
    """Retain a real criterion failure and derive all visible dispositions from it."""
    build, _, _ = control

    def fail_screen(rows, summaries, settings):
        rows[0]["significant_amplitude"] = 5.0
        rows[0]["operable"] = False
        summaries[0]["operability_pct"] = 75.0
        summaries[0]["passes"] = False
        settings["returned_status"] = "fail"

    install_output(monkeypatch, tmp_path, fail_screen)
    actual = build.calculate(tmp_path / "calculation")
    assert actual["status"] == "fail"
    assert actual["summary"][0]["operability_pct"] == 75.0
    assert "The criterion is exceeded." in build.result_row(actual["rows"][0])
    assert "The requirement is not met." in build.summary_row(actual["summary"][0])
    assert "The criterion is met." in build.result_row(actual["rows"][1])
    assert "The requirement is met." in build.summary_row(actual["summary"][1])
