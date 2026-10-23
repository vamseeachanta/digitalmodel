# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Literal arithmetic and offline case contracts for the actuator disk."""

import importlib.util
import json
import math
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
MODULE = ROOT / "src/digitalmodel/solvers/openfoam/kcs_actuator_disk.py"
spec = importlib.util.spec_from_file_location("kcs_actuator_disk", MODULE)
disk = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = disk
spec.loader.exec_module(disk)


def test_thrust_units_and_rpm_conversion():
    # 1000 * (600/60)^2 * 0.2^4 * 0.25 = 40 N.
    assert disk.thrust(600, density=1000, diameter=0.2, kt=0.25) == pytest.approx(40)
    assert disk.thrust(0) == 0


def test_controller_sign_correction_and_integral():
    controller = disk.RpmController(kp=2, ki=0.5, rpm_max=1000)
    # Error = 100 - 30 - 40 = 30 N; integral = 60 N s.
    rpm = controller.update(600, resistance=100, applied_thrust=40, dt=2, towing=30)
    assert rpm == pytest.approx(690)
    assert controller.integral == pytest.approx(60)
    assert controller.update(600, resistance=50, applied_thrust=40, dt=1, towing=30) < 600


def test_saturation_has_no_integral_windup():
    controller = disk.RpmController(kp=10, ki=1, rpm_max=700)
    assert controller.update(600, resistance=100, applied_thrust=0, dt=1) == 700
    assert controller.integral == 0
    assert controller.update(700, resistance=0, applied_thrust=20, dt=1) == 480


def test_controller_closes_synthetic_balance():
    controller = disk.RpmController(kp=3, ki=0)
    rpm = 300
    for _ in range(100):
        rpm = controller.update(rpm, resistance=70, applied_thrust=disk.thrust(
            rpm, density=1000, diameter=0.2, kt=0.25), dt=1, towing=30)
    assert rpm == pytest.approx(600, abs=0.01)


@pytest.mark.parametrize("value", [-1, math.inf, math.nan])
def test_thrust_rejects_invalid_rpm(value):
    with pytest.raises(ValueError):
        disk.thrust(value)


@pytest.mark.parametrize("kwargs", [{"dt": 0}, {"resistance": math.nan}, {"towing": -1}])
def test_controller_rejects_invalid_inputs(kwargs):
    values = dict(resistance=70, applied_thrust=40, dt=1, towing=30)
    values.update(kwargs)
    with pytest.raises(ValueError):
        disk.RpmController().update(600, **values)


def test_force_readback_signed_full_hull(tmp_path):
    path = tmp_path / "force.dat"
    path.write_text("# Time total pressure viscous\n0.1 (90 1 2) (80 1 1) (10 0 1)\n")
    assert disk.read_resistance(path) == [(0.1, 90)]


def test_source_is_total_newtons_in_density_weighted_equation():
    text = disk.source_dictionary(40)
    assert "volumeMode absolute" in text
    assert "cellZone propeller" in text
    assert "U ((40 0 0) 0)" in text


@pytest.fixture(scope="module")
def built_case(tmp_path_factory):
    return disk.build_case(tmp_path_factory.mktemp("disk") / "case")


def test_builder_uses_public_geometry_and_transient_vof(built_case):
    manifest = json.loads((built_case / "manifest.json").read_text())
    assert manifest["geometry"]["kind"] == "public_workshop_surface_grid"
    assert manifest["geometry"]["stand_in"] is False
    assert manifest["validation_status"] == "not_established"
    assert "Euler" in (built_case / "system/fvSchemes").read_text()
    assert "interFoam" in (built_case / "system/controlDict").read_text()
    assert "rho rho;" in (built_case / "system/controlDict").read_text()
    assert "cylinderToCell" in (built_case / "system/topoSetDict").read_text()
    assert (built_case / "constant/triSurface/hull.stl").stat().st_size > 1000


def test_builder_manifest_digests_match(built_case):
    import hashlib
    manifest = json.loads((built_case / "manifest.json").read_text())
    for name, digest in manifest["files"].items():
        assert hashlib.sha256((built_case / name).read_bytes()).hexdigest() == digest


def test_builder_refuses_existing_destination(built_case):
    with pytest.raises(FileExistsError):
        disk.build_case(built_case)


def test_published_reference_has_traceable_values():
    ref = disk.load_reference()
    assert ref["efd"]["n_rps"] == 9.5
    assert ref["efd"]["kt"] == 0.17
    assert ref["efd"]["ct"] == 0.003966
    assert ref["conditions"]["towing_n"] == 30.3
    assert ref["scatter"] is None
    assert ref["citation"]["cells"]["kt"] == "Sheet1!C10"


def test_comparison_uses_measured_coefficients_without_inventing_scatter():
    result = disk.compare_point([dict(time_s=1, rpm=600, thrust_n=60,
                                      resistance_n=90, towing_n=30.3)])
    assert result["quantities"]["rpm"]["reference"] == 570
    assert result["quantities"]["rpm"]["difference_percent"] == 5.3
    assert result["validation_status"] == "not_established"
    assert result["force_imbalance_n"] == pytest.approx(-0.3)


def test_comparison_rejects_nonfinite_or_empty_history():
    with pytest.raises(ValueError):
        disk.compare_point([])
    with pytest.raises(ValueError):
        disk.compare_point([dict(time_s=1, rpm=math.nan, thrust_n=60,
                                 resistance_n=90, towing_n=30.3)])


@pytest.mark.parametrize("row", ["0.1 (90 1 2)\n", "0.1 (nan 1 2) (80 1 1) (10 0 1)\n", ""])
def test_force_reader_rejects_incomplete_or_nonfinite_rows(tmp_path, row):
    path = tmp_path / "force.dat"
    path.write_text(row)
    with pytest.raises(ValueError):
        disk.read_resistance(path)


def test_smoke_rejects_changed_case_input(built_case):
    path = built_case / "constant/fvOptions"
    original = path.read_text()
    try:
        path.write_text(original + "// modified input\n")
        with pytest.raises(ValueError, match="input digest mismatch"):
            disk.smoke(built_case)
    finally:
        path.write_text(original)


def test_completed_smoke_rejects_changed_checkpoint(tmp_path):
    import hashlib
    manifest = tmp_path / "manifest.json"
    manifest.write_text("{}")
    checkpoint = tmp_path / "field"
    checkpoint.write_text("changed")
    (tmp_path / "smoke_state.json").write_text(json.dumps({
        "manifest_sha256": hashlib.sha256(manifest.read_bytes()).hexdigest(),
        "checkpoint_sha256": {"field": "0" * 64}, "tools": {}
    }))
    with pytest.raises(ValueError, match="checkpoint digest mismatch"):
        disk.smoke(tmp_path)


def test_check_cli_reports_diagnostic_status(tmp_path):
    history = tmp_path / "history.csv"
    history.write_text("time_s,rpm,thrust_n,resistance_n,towing_n\n1,600,60,90,30.3\n")
    output = tmp_path / "comparison.json"
    assert disk.main(["check", str(history), "--output", str(output)]) == 0
    assert json.loads(output.read_text())["validation_status"] == "not_established"


def test_script_readback_without_optional_package_dependencies(tmp_path):
    import subprocess
    history = tmp_path / "history.csv"
    history.write_text("time_s,rpm,thrust_n,resistance_n,towing_n\n1,600,60,90,30.3\n")
    result = subprocess.run([sys.executable, str(ROOT / "scripts/cfd/kcs_self_propulsion.py"),
                             "check", str(history)], capture_output=True, text=True, check=True)
    assert json.loads(result.stdout)["quantities"]["rpm"]["reference"] == 570
