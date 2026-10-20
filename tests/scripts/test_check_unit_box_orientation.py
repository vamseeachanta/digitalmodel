# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline regression for the repaired fixtures in issue 898."""

import importlib.util
import json
import os
from pathlib import Path
import subprocess

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/validation/check_unit_box_orientation.py"
CANONICAL = ROOT / "examples/hydrodynamics/diffraction/unit_box_rao/unit_box.gdf"


@pytest.fixture
def checker():
    spec = importlib.util.spec_from_file_location("box_check", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def five_panel_fixtures():
    """Census tracked GDF headers, independent of names and mesh validity."""
    env = {key: value for key, value in os.environ.items()
           if key not in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE")}
    tracked = subprocess.check_output(
        ["git", "ls-files", "-z", "--", "*.gdf"], cwd=ROOT, env=env,
    ).decode().split("\0")
    five_panel = []
    for path in filter(None, tracked):
        lines = (ROOT / path).read_text(encoding="utf-8").splitlines()
        if len(lines) >= 4 and lines[3].split()[:1] == ["5"]:
            five_panel.append(path)
    return five_panel


FIXTURES = five_panel_fixtures()


def test_fixture_inventory_covers_tracked_five_panel_meshes():
    assert "examples/workflows/orcawave-control-surface/control_surface.gdf" in FIXTURES
    assert len(FIXTURES) >= 12


@pytest.mark.parametrize("path", FIXTURES)
def test_repaired_fixture(checker, path):
    result = checker.check_mesh(ROOT / path)
    assert result["orientation"] == "PASS"
    # Exact geometry from canonical GDF, not a solver tolerance.
    assert result["axis_volumes_m3"] == [1, 1, 1]
    assert result["native_hydrostatics"] == "NOT_EVALUATED"


@pytest.mark.parametrize("flipped", [(0,), (1, 2, 3, 4), (0, 1, 2, 3, 4)])
def test_historical_faults_and_uniform_inversion(checker, tmp_path, flipped):
    lines = CANONICAL.read_text(encoding="utf-8").splitlines()
    # GDF layout: four header lines, four vertices per panel (canonical fixture).
    for panel in flipped:
        start = 4 + panel * 4
        lines[start:start + 4] = reversed(lines[start:start + 4])
    path = tmp_path / "fault.gdf"
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    with pytest.raises(ValueError, match="outward"):
        checker.check_mesh(path)


def test_duplicate_face_rejected(checker, tmp_path):
    lines = CANONICAL.read_text(encoding="utf-8").splitlines()
    lines[-4:] = lines[-8:-4]
    path = tmp_path / "duplicate.gdf"
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    with pytest.raises(ValueError, match="faces"):
        checker.check_mesh(path)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), None, True])
def test_invalid_native_values_fail_closed(checker, value):
    result = checker.check_mesh(CANONICAL)
    observed = native_record(result)
    observed["k33_n_per_m"] = value
    with pytest.raises(ValueError):
        checker.compare_native(result, observed)


def native_record(result):
    # Density/gravity: canonical spec.yml environment; geometry: canonical GDF.
    return {"mesh_sha256": result["mesh_sha256"], "density_kg_m3": 1025,
            "gravity_m_s2": 9.80665, "k33_n_per_m": 1025 * 9.80665,
            "volume_m3": 1, "waterplane_area_m2": 1, "cob_z_m": -0.5}


def test_native_screen_and_negative_stiffness(checker):
    result = checker.check_mesh(CANONICAL)
    observed = native_record(result)
    assert checker.compare_native(result, observed)["status"] == "PASS_CONDITIONAL"
    observed["k33_n_per_m"] *= -1
    assert checker.compare_native(result, observed)["status"] == "FAIL"


def test_digest_mismatch_rejected(checker):
    result = checker.check_mesh(CANONICAL)
    observed = native_record(result)
    observed["mesh_sha256"] = "different"
    with pytest.raises(ValueError, match="digest"):
        checker.compare_native(result, observed)


def test_cli_no_native_data(checker, capsys):
    assert checker.main([str(CANONICAL)]) == 0
    assert json.loads(capsys.readouterr().out)["native_hydrostatics"] == "NOT_EVALUATED"


@pytest.mark.parametrize("key,value", [("density_kg_m3", 1000),
                                       ("gravity_m_s2", 9.81)])
def test_native_settings_must_match_spec(checker, key, value):
    result = checker.check_mesh(CANONICAL)
    observed = native_record(result)
    observed[key] = value
    observed["k33_n_per_m"] = observed["density_kg_m3"] * observed["gravity_m_s2"]
    screen = checker.compare_native(result, observed)
    assert screen["status"] == "FAIL"
    assert screen["expected"]["k33_n_per_m"] == 1025 * 9.80665


def test_native_reference_matches_canonical_spec(checker):
    import yaml

    environment = yaml.safe_load(CANONICAL.with_name("spec.yml").read_text())["environment"]
    result = checker.check_mesh(CANONICAL)
    expected = checker.compare_native(result, native_record(result))["expected"]
    assert expected["k33_n_per_m"] == environment["water_density"] * environment["gravity"]


@pytest.mark.parametrize("fault,reason", [("crossed", "crosses"),
                                         ("lid", "lid"),
                                         ("line_count", "five")])
def test_malformed_faces_rejected(checker, tmp_path, fault, reason):
    lines = CANONICAL.read_text(encoding="utf-8").splitlines()
    if fault == "crossed":
        lines[5], lines[6] = lines[6], lines[5]
    elif fault == "lid":
        lines[4:8] = [line.rsplit(maxsplit=1)[0] + " 0" for line in lines[4:8]]
    else:
        lines.pop()
    path = tmp_path / "fault.gdf"
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    with pytest.raises(ValueError, match=reason):
        checker.check_mesh(path)


def test_cli_native_failure_exits_one(checker, tmp_path, capsys):
    observed = native_record(checker.check_mesh(CANONICAL))
    observed["k33_n_per_m"] *= -1
    path = tmp_path / "observations.json"
    path.write_text(json.dumps(observed), encoding="utf-8")
    assert checker.main([str(CANONICAL), "--observations", str(path)]) == 1
    assert json.loads(capsys.readouterr().out)["native_hydrostatics"]["status"] == "FAIL"


def test_mesh_digest_ignores_checkout_line_endings(checker, tmp_path):
    raw = CANONICAL.read_bytes().replace(b"\r\n", b"\n")
    lf, crlf = tmp_path / "lf.gdf", tmp_path / "crlf.gdf"
    lf.write_bytes(raw)
    crlf.write_bytes(raw.replace(b"\n", b"\r\n"))
    assert checker.check_mesh(lf)["mesh_sha256"] == checker.check_mesh(crlf)["mesh_sha256"]
