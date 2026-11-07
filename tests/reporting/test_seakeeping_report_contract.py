# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent boundary controls for the illustrative seakeeping report."""

import copy
import csv
import importlib.util
import math
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


def test_literal_occurrence_threshold_equality(control):
    """Accept an independently specified 95.0 percent boundary result."""
    build, _, inputs = control
    inputs["dofs"] = inputs["dofs"][:1]
    inputs["sea_states"][0]["probability"] = 0.57
    inputs["sea_states"][1]["probability"] = 0.03
    dof = inputs["dofs"][0]
    rows = [
        {"dof": "heave", "unit": "m", **state, "criterion": 2.0,
         "significant_amplitude": amplitude, "operable": operable}
        for state, amplitude, operable in zip(
            inputs["sea_states"], [1.0, 3.0], [True, False], strict=True,
        )
    ]
    # The first state contributes 57 of 60 occurrence units: exactly 95 percent.
    summaries = [{
        "dof": dof["name"], "unit": dof["unit"], "criterion": 2.0,
        "operability_pct": 95.0, "required_operability_pct": 95.0, "passes": True,
    }]
    build.validate_output(inputs, rows, summaries, "pass")


def test_spectral_engine_occurrence_threshold_equality(control):
    """Retain a passing threshold result produced by the actual spectral engine."""
    import numpy as np

    from digitalmodel.hydrodynamics.seakeeping import operability_analysis

    build, _, inputs = control
    inputs["dofs"] = [{"name": "heave", "unit": "m", "criterion": 0.75}]
    inputs["sea_states"][0]["probability"] = 0.57
    inputs["sea_states"][1]["probability"] = 0.03
    result = operability_analysis(
        np.linspace(0.1, 3.0, 100), np.ones(100),
        inputs["sea_states"], {"heave": 0.75},
    )
    assert [row["operable"] for row in result["sea_state_results"]] == [True, False]
    assert result["operability_pct"] == 95.0
    rows = [
        {"dof": "heave", "unit": "m", "criterion": 0.75,
         **{key: value for key, value in row.items() if key != "m0"}}
        for row in result["sea_state_results"]
    ]
    summaries = [{
        "dof": "heave", "unit": "m", "criterion": 0.75,
        "operability_pct": result["operability_pct"], "required_operability_pct": 95.0,
        "passes": result["operability_pct"] >= 95.0,
    }]
    assert summaries[0]["passes"] is True
    build.validate_output(inputs, rows, summaries, "pass")


@pytest.mark.parametrize("source", ["literal", "spectral"])
def test_ordered_occurrence_accumulation_retains_failed_threshold(control, source):
    """Preserve the four-state failed boundary across Python sum algorithms."""
    build, _, inputs = control
    inputs["dofs"] = [{"name": "heave", "unit": "m", "criterion": 0.75}]
    inputs["sea_states"] = [
        {"hs": hs, "tp": 6.0, "probability": probability}
        for hs, probability in zip(
            [1.0, 1.0, 2.0, 1.0], [0.03, 0.81, 0.05, 0.11], strict=True,
        )
    ]
    # Ordered binary additions yield the float immediately below 95 percent.
    expected = math.nextafter(95.0, -math.inf)
    rows = [
        {"dof": "heave", "unit": "m", "criterion": 0.75, **state,
         "significant_amplitude": state["hs"] / 2.0, "operable": index != 2}
        for index, state in enumerate(inputs["sea_states"])
    ]
    operability = expected
    if source == "spectral":
        import numpy as np

        from digitalmodel.hydrodynamics.seakeeping import operability_analysis

        result = operability_analysis(
            np.linspace(0.1, 3.0, 100), np.ones(100),
            inputs["sea_states"], {"heave": 0.75},
        )
        rows = [
            {"dof": "heave", "unit": "m", "criterion": 0.75,
             **{key: value for key, value in row.items() if key != "m0"}}
            for row in result["sea_state_results"]
        ]
        operability = result["operability_pct"]
    assert [row["operable"] for row in rows] == [True, True, False, True]
    assert operability == expected
    summaries = [{
        "dof": "heave", "unit": "m", "criterion": 0.75,
        "operability_pct": operability, "required_operability_pct": 95.0,
        "passes": operability >= 95.0,
    }]
    assert summaries[0]["passes"] is False
    build.validate_output(inputs, rows, summaries, "fail")
    assert "The requirement is not met." in build.summary_row(summaries[0])
    summaries[0]["passes"] = True
    with pytest.raises(ValueError, match="stale summary"):
        build.validate_output(inputs, rows, summaries, "pass")
