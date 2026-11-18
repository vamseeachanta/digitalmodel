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
    saved = json.loads(
        (tmp_path / SUMMARY).read_text("utf-8"),
        parse_constant=lambda value: pytest.fail(f"Non-finite JSON number: {value}"),
    )
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
    "section, field, value, with_window",
    [("window", "pore_pressure_emw_ppg", float("nan"), True),
     ("window", "fracture_emw_ppg", float("inf"), True),
     ("trip", "trip_speed_ft_min", float("nan"), True),
     ("trip", "trip_speed_ft_min", float("nan"), False),
     ("trip", "trip_speed_ft_min", float("inf"), False),
     ("trip", "trip_speed_ft_min", float("-inf"), False),
     ("section", "mud_weight", float("nan"), True),
     ("section", "mud_weight", float("inf"), True),
     ("section", "mud_weight", float("nan"), False),
     ("section", "mud_weight", float("inf"), False)],
)
def test_nonfinite_pressure_comparison_is_rejected(
    tmp_path, section, field, value, with_window
):
    cfg = _example_config(tmp_path)
    if not with_window:
        del cfg["swab_surge"]["window"]
    cfg["swab_surge"][section][field] = value
    with pytest.raises(ValueError, match="finite"):
        run_swab_surge(cfg)
    assert not (tmp_path / SUMMARY).exists()


@pytest.mark.parametrize(
    "field",
    ["pipe_od", "pipe_id", "hole_diameter", "mud_weight", "tvd", "plastic_viscosity",
     "yield_point", "clinging_constant"],
)
@pytest.mark.parametrize("value", [float("nan"), float("inf"), float("-inf")])
@pytest.mark.parametrize("with_window", [True, False])
def test_nonfinite_section_input_is_rejected(tmp_path, field, value, with_window):
    cfg = _example_config(tmp_path)
    cfg["swab_surge"]["section"]["pipe_config"] = "closed"
    cfg["swab_surge"]["section"][field] = value
    if not with_window:
        del cfg["swab_surge"]["window"]

    with pytest.raises(ValueError, match="finite"):
        run_swab_surge(cfg)
    assert not (tmp_path / "results").exists()


@pytest.mark.parametrize(
    "block, field", [("section", "bit_depth"), ("trip", "pull_speed_ft_min")]
)
@pytest.mark.parametrize("value", [float("nan"), float("inf"), float("-inf")])
@pytest.mark.parametrize("with_window", [True, False])
def test_nonfinite_extra_summary_input_is_rejected(
    tmp_path, block, field, value, with_window
):
    cfg = _example_config(tmp_path)
    cfg["swab_surge"][block][field] = value
    if not with_window:
        del cfg["swab_surge"]["window"]

    with pytest.raises(ValueError, match="JSON compliant"):
        run_swab_surge(cfg)
    assert not (tmp_path / "results").exists()


def test_finite_extra_summary_inputs_are_preserved(tmp_path):
    cfg = _example_config(tmp_path)
    cfg["swab_surge"]["section"]["bit_depth"] = 12000.0
    cfg["swab_surge"]["trip"]["pull_speed_ft_min"] = 90.0

    output = run_swab_surge(cfg)
    saved = json.loads(
        (tmp_path / SUMMARY).read_text("utf-8"),
        parse_constant=lambda value: pytest.fail(f"Non-finite JSON number: {value}"),
    )
    assert saved["screening_status"] == "pass"
    for block in ("section", "trip"):
        assert saved[block] == cfg["swab_surge"][block]
        assert output["swab_surge"]["summary"][block] == saved[block]


@pytest.mark.parametrize("field", ["pore_pressure_emw_ppg", "fracture_emw_ppg"])
@pytest.mark.parametrize("value", [float("nan"), float("inf"), float("-inf")])
def test_partial_window_ignores_supplied_nonfinite_limit(tmp_path, field, value):
    cfg = _example_config(tmp_path)
    cfg["swab_surge"]["window"] = {field: value}

    output = run_swab_surge(cfg)
    saved = json.loads(
        (tmp_path / SUMMARY).read_text("utf-8"),
        parse_constant=lambda value: pytest.fail(f"Non-finite JSON number: {value}"),
    )
    assert output["screening_status"] == saved["screening_status"] == "warning"
    assert "not evaluated" in saved["screening_reason"]
    assert set(saved["result"]) == {"at_trip_speed"}
    assert "window" not in saved


@pytest.mark.parametrize("pore, fracture", [(12.0, 12.0), (13.0, 11.5)])
def test_unordered_pressure_window_is_rejected(tmp_path, pore, fracture):
    cfg = _example_config(tmp_path)
    cfg["swab_surge"]["window"] = {
        "pore_pressure_emw_ppg": pore,
        "fracture_emw_ppg": fracture,
    }
    with pytest.raises(ValueError, match="pore-pressure limit.*below.*fracture limit"):
        run_swab_surge(cfg)
    assert not (tmp_path / SUMMARY).exists()


def _example_config(tmp_path):
    cfg = yaml.safe_load((ROOT / EXAMPLE / "input.yml").read_text("utf-8"))
    cfg["_config_dir_path"] = tmp_path
    return cfg
