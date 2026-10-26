# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent force-balance checks and chart-data reproducibility."""

import csv
import hashlib
import json
from dataclasses import replace
from pathlib import Path

import pytest

from digitalmodel.asset_integrity.pressure_vessel_metal_loss import (
    ShellInputs,
    evaluate_shell_loss,
)
from scripts.reporting.build_pressure_vessel_metal_loss import build

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / "examples/workflows/pressure-vessel-metal-loss"
INPUTS = ShellInputs(
    pressure_mpa=2.0,
    inner_radius_mm=600.0,
    nominal_thickness_mm=16.0,
    assumed_allowable_stress_mpa=100.0,
    joint_efficiency=1.0,
)


@pytest.mark.parametrize("loss, hoop, longitudinal", [(0.0, 75.0, 37.5), (8.0, 150.0, 75.0)])
def test_membrane_stress_from_independent_force_balance(loss, hoop, longitudinal):
    """Unit-length cut: pressure force = 2*2*600 = 2400 N.

    At t=16 mm, two cut walls have area 32 mm2: 2400/32=75 MPa.
    At t=8 mm, area is 16 mm2: 2400/16=150 MPa.
    Closed-end force / wall area is (2*pi*600**2)/(2*pi*600*t),
    giving 37.5 and 75 MPa independently of the function under test.
    Basis: first-principles equilibrium, derived in the calculation docstring.
    """
    result = evaluate_shell_loss(INPUTS, loss)
    assert result.hoop_stress_mpa == pytest.approx(hoop)
    assert result.longitudinal_stress_mpa == pytest.approx(longitudinal)
    assert result.remaining_thickness_mm == 16.0 - loss


def test_thickness_comparison_calls_existing_implementation(monkeypatch):
    """Check delegation and a signed difference without validating a standard."""
    calls = []

    def thickness(self, **kwargs):
        calls.append(kwargs)
        return {"t_required_mm": 12.5}

    monkeypatch.setattr(
        "digitalmodel.asset_integrity.pressure_vessel_metal_loss."
        "ASMEThicknessCalc.ug27_cylindrical_shell",
        thickness,
    )
    result = evaluate_shell_loss(INPUTS, 4.0)
    assert result.implementation_required_thickness_mm == 12.5
    assert result.thickness_difference_mm == -0.5
    assert calls == [{
        "pressure_mpa": 2.0,
        "inner_radius_mm": 600.0,
        "allowable_stress_mpa": 100.0,
        "joint_efficiency": 1.0,
        "corrosion_allowance_mm": 0.0,
    }]


@pytest.mark.parametrize("loss", [-1.0, 16.0, 17.0, float("nan"), float("inf"), True])
def test_invalid_loss_is_rejected(loss):
    with pytest.raises(ValueError):
        evaluate_shell_loss(INPUTS, loss)


@pytest.mark.parametrize("field, value", [
    ("pressure_mpa", -1.0),
    ("pressure_mpa", float("nan")),
    ("pressure_mpa", float("inf")),
    ("pressure_mpa", True),
    ("inner_radius_mm", 0.0),
    ("inner_radius_mm", "600"),
    ("nominal_thickness_mm", -1.0),
    ("assumed_allowable_stress_mpa", 0.0),
    ("joint_efficiency", 0.0),
    ("joint_efficiency", 1.1),
])
def test_invalid_inputs_are_rejected(field, value):
    with pytest.raises(ValueError):
        evaluate_shell_loss(replace(INPUTS, **{field: value}), 0.0)


def test_zero_pressure_has_zero_demand():
    result = evaluate_shell_loss(replace(INPUTS, pressure_mpa=0.0), 8.0)
    assert result.hoop_stress_mpa == 0.0
    assert result.longitudinal_stress_mpa == 0.0
    assert result.implementation_required_thickness_mm == 0.0


@pytest.mark.parametrize("allowable", [1.0, 1.2])
def test_invalid_implementation_thickness_is_rejected(allowable):
    with pytest.raises(ValueError, match="implementation"):
        evaluate_shell_loss(
            replace(INPUTS, assumed_allowable_stress_mpa=allowable), 0.0
        )


def test_committed_chart_data_is_reproducible_and_traceable(tmp_path):
    paths = build(ROOT, tmp_path)
    for path in paths:
        assert path.read_bytes() == (WORKFLOW / path.name).read_bytes()
    manifest = json.loads((tmp_path / "manifest.json").read_text())
    for source, digest in manifest["source_sha256"].items():
        assert hashlib.sha256((ROOT / source).read_bytes()).hexdigest() == digest
    for filename, record in manifest["charts"].items():
        path = tmp_path / filename
        assert hashlib.sha256(path.read_bytes()).hexdigest() == record["sha256"]
        with path.open(newline="") as stream:
            rows = list(csv.DictReader(stream))
        assert rows == record["rows"]
        assert len(rows) == 17
        for index, row in enumerate(rows):
            loss = index / 2
            assert float(row["metal_loss_mm"]) == loss
            if filename == "membrane-stress.csv":
                # Independent equilibrium arithmetic; CSV precision is 0.1 MPa.
                hoop = 1200.0 / (16.0 - loss)
                assert row["hoop_stress_mpa"] == f"{hoop:.1f}"
                assert row["longitudinal_stress_mpa"] == f"{hoop / 2:.1f}"
            else:
                assert float(row["remaining_thickness_mm"]) == 16.0 - loss
        if filename == "remaining-thickness.csv":
            assert float(rows[0]["thickness_difference_mm"]) > 0
            assert float(rows[-1]["thickness_difference_mm"]) < 0
            assert len({row["implementation_required_thickness_mm"] for row in rows}) == 1


def test_builder_rejects_unlabelled_inputs(tmp_path):
    input_file = tmp_path / "examples/workflows/pressure-vessel-metal-loss/input.json"
    input_file.parent.mkdir(parents=True)
    config = json.loads((WORKFLOW / "input.json").read_text())
    config["basis"] = "unconfirmed"
    input_file.write_text(json.dumps(config))
    with pytest.raises(ValueError, match="ASSUMED"):
        build(tmp_path, tmp_path / "output")
