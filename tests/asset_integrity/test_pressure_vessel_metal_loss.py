# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent force-balance checks and chart-data reproducibility."""

import csv
import hashlib
import json
from dataclasses import fields, replace
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
)


@pytest.mark.parametrize("loss, hoop, longitudinal", [
    (0.0, 75.0, 37.5), (4.0, 100.0, 50.0), (8.0, 150.0, 75.0),
])
def test_membrane_stress_from_independent_force_balance(loss, hoop, longitudinal):
    """Unit-length cut: pressure force = 2*2*600 = 2400 N.

    At t=16 mm, two cut walls have area 32 mm2: 2400/32=75 MPa.
    At t=12 mm, area is 24 mm2: 2400/24=100 MPa.
    At t=8 mm, area is 16 mm2: 2400/16=150 MPa.
    Closed-end force / wall area is (2*pi*600**2)/(2*pi*600*t),
    giving 37.5, 50 and 75 MPa independently of the function under test.
    Basis: MIT Pressure Vessels (2001), Stresses, equations (2) and (3), p. 3.
    """
    result = evaluate_shell_loss(INPUTS, loss)
    assert result.hoop_stress_mpa == pytest.approx(hoop)
    assert result.longitudinal_stress_mpa == pytest.approx(longitudinal)
    assert result.remaining_thickness_mm == 16.0 - loss


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
    ("nominal_thickness_mm", 0.0),
    ("nominal_thickness_mm", float("nan")),
    ("inner_radius_mm", float("inf")),
])
def test_invalid_inputs_are_rejected(field, value):
    with pytest.raises(ValueError):
        evaluate_shell_loss(replace(INPUTS, **{field: value}), 0.0)


def test_zero_pressure_has_zero_demand():
    result = evaluate_shell_loss(replace(INPUTS, pressure_mpa=0.0), 8.0)
    assert result.hoop_stress_mpa == 0.0
    assert result.longitudinal_stress_mpa == 0.0


def test_committed_chart_data_is_reproducible_and_traceable(tmp_path):
    paths = build(ROOT, tmp_path)
    assert {path.name for path in paths} == {
        "membrane-stress.csv", "remaining-thickness.csv", "manifest.json",
    }
    rebuild = (
        "PYTHONPATH=src uv run python scripts/reporting/"
        "build_pressure_vessel_metal_loss.py --output "
        "examples/workflows/pressure-vessel-metal-loss"
    )
    manifest = json.loads((tmp_path / "manifest.json").read_text(encoding="utf-8"))
    committed = json.loads((WORKFLOW / "manifest.json").read_text(encoding="utf-8"))
    assert manifest == committed, f"Manifest changed; rebuild with: {rebuild}"
    for source, digest in manifest["source_sha256"].items():
        assert hashlib.sha256((ROOT / source).read_bytes()).hexdigest() == digest, rebuild
    for filename, record in manifest["charts"].items():
        path = tmp_path / filename
        assert hashlib.sha256(path.read_bytes()).hexdigest() == record["sha256"], rebuild
        assert hashlib.sha256((WORKFLOW / filename).read_bytes()).hexdigest() == record["sha256"], rebuild
        assert b"\r" not in path.read_bytes()
        with path.open(encoding="utf-8", newline="") as stream:
            rows = list(csv.DictReader(stream))
        assert rows == record["rows"]
        assert len(rows) == 17
        for index, row in enumerate(rows):
            loss = index / 2
            assert float(row["metal_loss_mm"]) == loss
            if filename == "membrane-stress.csv":
                # Independent equilibrium arithmetic; CSV precision is 0.1 MPa.
                hoop = 1200.0 / (16.0 - loss)
                assert float(row["hoop_stress_mpa"]) == pytest.approx(hoop, abs=0.05)
                assert float(row["longitudinal_stress_mpa"]) == pytest.approx(hoop / 2, abs=0.05)
            else:
                assert float(row["remaining_thickness_mm"]) == 16.0 - loss


def test_builder_rejects_unlabelled_inputs(tmp_path):
    input_file = tmp_path / "examples/workflows/pressure-vessel-metal-loss/input.json"
    input_file.parent.mkdir(parents=True)
    config = json.loads((WORKFLOW / "input.json").read_text(encoding="utf-8"))
    config["basis"] = "unconfirmed"
    input_file.write_text(json.dumps(config), encoding="utf-8", newline="\n")
    with pytest.raises(ValueError, match="ASSUMED"):
        build(tmp_path, tmp_path / "output")


def test_example_has_only_independently_supported_inputs_and_results():
    assert {field.name for field in fields(ShellInputs)} == {
        "pressure_mpa", "inner_radius_mm", "nominal_thickness_mm",
    }
    result = evaluate_shell_loss(INPUTS, 4.0)
    assert {field.name for field in fields(result)} == {
        "metal_loss_mm", "remaining_thickness_mm", "hoop_stress_mpa",
        "longitudinal_stress_mpa",
    }


def test_chart_and_manifest_exclude_unqualified_thickness_reference(tmp_path):
    build(ROOT, tmp_path)
    manifest = json.loads((tmp_path / "manifest.json").read_text(encoding="utf-8"))
    with (tmp_path / "remaining-thickness.csv").open(encoding="utf-8", newline="") as stream:
        reader = csv.DictReader(stream)
        assert reader.fieldnames == ["metal_loss_mm", "remaining_thickness_mm"]
    assert "thickness_basis" not in manifest
    assert set(manifest["inputs"]["shell"]) == {
        "pressure_mpa", "inner_radius_mm", "nominal_thickness_mm",
    }
    assert set(manifest["source_sha256"]) == {
        "examples/workflows/pressure-vessel-metal-loss/input.json",
        "src/digitalmodel/asset_integrity/pressure_vessel_metal_loss.py",
        "scripts/reporting/build_pressure_vessel_metal_loss.py",
    }
