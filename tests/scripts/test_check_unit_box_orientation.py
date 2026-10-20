"""Offline regression for the repaired fixtures in issue 898."""

import importlib.util
import json
from pathlib import Path

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


# Inventory from the issue 898 repair; the hull-library mesh is a different box.
FIXTURES = [
    "examples/hydrodynamics/diffraction/unit_box_rao/unit_box.gdf",
    "tests/hydrodynamics/diffraction/fixtures/acceptance_610/unit_box.gdf",
    "tests/hydrodynamics/bemrosetta/fixtures/sample_box.gdf",
    *[f"examples/workflows/{workflow}/{name}.gdf" for workflow, name in [
        ("orcawave-control-surface", "unit_box"),
        ("orcawave-diffraction-solve", "unit_box"),
        ("orcawave-field-points", "unit_box"),
        ("orcawave-mean-drift", "unit_box"),
        ("orcawave-input-prep", "sample_box"),
        ("aqwa-diffraction-deck-prep", "sample_box"),
    ]],
    *[f"docs/domains/orcawave/L01_aqwa_benchmark/benchmark_results/{solver}/"
      "unit_box_clean.gdf" for solver in ("aqwa", "orcawave")],
]


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
