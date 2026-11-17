"""Registry and pressure-window contracts for the offline swab/surge example."""

import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.well.workflow import run_swab_surge


ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = Path("examples/workflows/swab-surge")
SUMMARY = Path("results/swab_surge/swab_surge_summary.json")


def test_swab_surge_registry_row():
    registry = yaml.safe_load((ROOT / "docs/registry/workflows.yaml").read_text("utf-8"))
    (row,) = (item for item in registry["workflows"] if item["id"] == "swab-surge")

    assert row["runtime"] == "offline"
    assert row["basename"] == "swab_surge"
    assert row["input"] == (EXAMPLE / "input.yml").as_posix()
    assert row["outputs"] == [(EXAMPLE / SUMMARY).as_posix()]
    assert row["test"] == (
        "tests/workflows/test_durable_workflows.py::test_workflow_registry[swab-surge]"
    )


@pytest.mark.parametrize(
    "window, status, flags, reason_words",
    [
        pytest.param("example", "pass", (False, False), ("within",), id="example"),
        pytest.param({"pore_pressure_emw_ppg": 12, "fracture_emw_ppg": 13},
                     "fail", (True, False), ("kick",), id="kick"),
        pytest.param({"pore_pressure_emw_ppg": 11.5, "fracture_emw_ppg": 12},
                     "fail", (False, True), ("loss",), id="loss"),
        pytest.param({"pore_pressure_emw_ppg": 11.9, "fracture_emw_ppg": 12.1},
                     "fail", (True, True), ("kick", "loss"), id="both"),
        pytest.param(None, "warning", None, ("not evaluated",), id="no-window"),
        pytest.param({"fracture_emw_ppg": 13}, "warning", None,
                     ("not evaluated",), id="no-pore-bound"),
        pytest.param({"pore_pressure_emw_ppg": 11.5}, "warning", None,
                     ("not evaluated",), id="no-fracture-bound"),
    ],
)
def test_pressure_window_summary(tmp_path, window, status, flags, reason_words):
    cfg = _example_config(tmp_path)
    if window is None:
        del cfg["swab_surge"]["window"]
    elif window != "example":
        cfg["swab_surge"]["window"] = window

    output = run_swab_surge(cfg)
    returned = output["swab_surge"]
    saved = json.loads((tmp_path / SUMMARY).read_text("utf-8"))
    assert output["screening_status"] == saved["screening_status"] == status
    assert output["screening_reason"] == saved["screening_reason"]
    assert all(word in saved["screening_reason"].lower() for word in reason_words)
    assert returned["summary"] == {**saved, "summary_json": str(tmp_path / SUMMARY)}
    assert "summary_json" not in saved
    assert json.dumps(str(tmp_path))[1:-1] not in json.dumps(saved)
    result = saved["result"]
    assert result == returned["result"]
    if flags is None:
        assert set(result) == {"at_trip_speed"}
    else:
        assert (result["kick_at_trip_speed"], result["loss_at_trip_speed"]) == flags


@pytest.mark.parametrize(
    "section, field, value",
    [("window", "pore_pressure_emw_ppg", float("nan")),
     ("window", "fracture_emw_ppg", float("inf")),
     ("trip", "trip_speed_ft_min", float("nan"))],
)
def test_nonfinite_pressure_comparison_is_rejected(tmp_path, section, field, value):
    cfg = _example_config(tmp_path)
    cfg["swab_surge"][section][field] = value
    with pytest.raises(ValueError, match="finite"):
        run_swab_surge(cfg)
    assert not (tmp_path / SUMMARY).exists()


def _example_config(tmp_path):
    cfg = yaml.safe_load((ROOT / EXAMPLE / "input.yml").read_text("utf-8"))
    cfg["_config_dir_path"] = tmp_path
    return cfg
