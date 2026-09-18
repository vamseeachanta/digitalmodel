"""Example geometry tests: these do not validate an API 579 assessment."""
import hashlib
import json
from dataclasses import replace
from pathlib import Path

import pytest

from digitalmodel.asset_integrity.assessment import example_vessel_data as ev


def test_import_uses_this_worktree():
    root = Path(__file__).resolve().parents[2]
    assert Path(ev.__file__).resolve().is_relative_to(root / "src")


@pytest.mark.parametrize("area", ev.AREAS)
def test_surface_exact_centre_sound_edges_and_symmetry(area):
    assert ev.thickness(area, 0, 0) == pytest.approx(area.minimum_mm)
    half = area.axial_extent_mm / 2
    assert ev.thickness(area, half, 0) == 16
    assert ev.thickness(area, half + 10, 0) == 16
    assert ev.thickness(area, half / 3, 10) == ev.thickness(area, -half / 3, -10)
    assert abs(ev.thickness(area, half - 0.001, 0) - 16) < 1e-6


@pytest.mark.parametrize("bad", [0, -1, float("nan"), float("inf")])
def test_invalid_pitch_rejected(bad):
    with pytest.raises(ValueError):
        ev.sample(ev.AREAS[0], bad)


@pytest.mark.parametrize("field,value", [
    ("minimum_mm", 0.5), ("minimum_mm", 17),
    ("axial_extent_mm", -1), ("theta_deg", float("nan")),
    ("area_id", "../escape"), ("centre_x_mm", -1000),
])
def test_invalid_area_rejected(field, value):
    with pytest.raises(ValueError):
        ev.sample(replace(ev.AREAS[0], **{field: value}), 25)


def test_named_dimensions_only():
    with pytest.raises(TypeError):
        ev.Area("A", 1100, 90, 200, 160, 14, 0)


def test_grid_preserves_current_and_once_only_deductions():
    grid = ev.sample(ev.AREAS[0], 25)
    centre = next(r for r in grid["rows"] if r["local_x_mm"] == r["local_s_mm"] == 0)
    assert centre["current_mm"] == 14
    assert centre["assessed_mm"] == pytest.approx(13.3)
    assert centre["global_x_mm"] == 1100
    assert centre["theta_deg"] == 90
    for row in grid["rows"]:
        assert row["current_mm"] - row["assessed_mm"] == pytest.approx(0.7)
    assert 80 in grid["s_mm"] and -80 in grid["s_mm"]
    assert grid["actual_pitch_s_mm"] <= 25
    wrap = ev.sample(replace(ev.AREAS[0], theta_deg=0), 25)
    assert any(r["theta_deg"] > 350 for r in wrap["rows"])


def test_profiles_against_manual_fixture():
    rows = [dict(local_x_mm=x, local_s_mm=s, assessed_mm=t)
            for x, s, t in [(0, 0, 10), (0, 1, 8), (2, 0, 7), (2, 1, 9)]]
    assert ev.critical_profiles(rows) == {"axial": [[0, 8], [2, 7]],
                                          "circumferential": [[0, 7], [1, 8]]}


@pytest.mark.parametrize("area", ev.AREAS)
def test_sampling_uses_independent_analytic_volume(area):
    study = ev.sampling_study(area)
    assert len(study["resolutions"]) == 3
    finest = study["resolutions"][-1]
    assert finest["developed_loss_error_fraction"] < 0.01
    assert study["fea_mesh_convergence"] == "NOT EVALUATED"


def test_export_deterministic_complete_and_readable(tmp_path):
    first = ev.write_package(tmp_path, "first")
    second = ev.write_package(tmp_path, "second")
    assert sorted(p.name for p in first.iterdir()) == sorted(p.name for p in second.iterdir())
    for path in first.iterdir():
        assert path.read_bytes() == (second / path.name).read_bytes()
    manifest = json.loads((first / "manifest.json").read_text())
    assert manifest["example_data"] is True
    assert set(manifest["files"]) == {p.name for p in first.iterdir()} - {"manifest.json"}
    for name, digest in manifest["files"].items():
        assert hashlib.sha256((first / name).read_bytes()).hexdigest() == digest
    assert b"\r\n" not in (first / "area-a-grid.csv").read_bytes()
    html = (first / "report.html").read_text(encoding="utf-8")
    assert "EXAMPLE DATA" in html and "NOT EVALUATED" in html
    assert "code-qualified PASS" not in html
    profiles = json.loads((first / "area-b-profiles.json").read_text())
    assert profiles["target_pitch_mm"] == 6.25
    assert "developed-surface loss" in html
    assert "Removed volume" not in html


def test_export_refuses_existing_and_traversal(tmp_path):
    (tmp_path / "held").mkdir()
    (tmp_path / "held" / "keep.txt").write_text("preserve")
    with pytest.raises(FileExistsError):
        ev.write_package(tmp_path, "held")
    with pytest.raises(ValueError):
        ev.write_package(tmp_path, "../escaped")
    assert (tmp_path / "held" / "keep.txt").read_text() == "preserve"


def test_export_rejects_duplicate_ids_and_invalid_before_write(tmp_path):
    with pytest.raises(ValueError):
        ev.write_package(tmp_path, "bad", areas=(ev.AREAS[0], ev.AREAS[0]))
    assert not (tmp_path / "bad").exists()


def test_sampling_failure_reaches_report(tmp_path, monkeypatch):
    original = ev.sampling_study
    def failed(area):
        study = original(area)
        study["status"] = "PROVISIONAL"
        return study
    monkeypatch.setattr(ev, "sampling_study", failed)
    package = ev.write_package(tmp_path, "provisional")
    assert "PROVISIONAL" in (package / "report.html").read_text(encoding="utf-8")


def test_coarse_sampling_detects_under_resolution():
    study = ev.sampling_study(ev.AREAS[1], pitches=(100, 80, 60))
    assert study["status"] == "PROVISIONAL"


def test_json_precision_and_nonfinite_rejection():
    assert ev.json_bytes({"x": -0.00000001, "y": 1.12345678}).decode() == (
        '{\n  "x": 0.0,\n  "y": 1.123457\n}\n')
    with pytest.raises(ValueError):
        ev.json_bytes({"x": float("nan")})


def test_export_rejects_overlapping_footprints(tmp_path):
    overlap = replace(ev.AREAS[1], centre_x_mm=1100, theta_deg=90)
    with pytest.raises(ValueError, match="overlap"):
        ev.write_package(tmp_path, "overlap", areas=(ev.AREAS[0], overlap, *ev.AREAS[2:]))
    assert not (tmp_path / "overlap").exists()


def test_closed_form_integral_known_values():
    assert ev.analytic_volume(ev.AREAS[0]) == 16000
    assert ev.analytic_volume(ev.AREAS[1]) == 57000


def test_assumed_material_is_explicit_and_new_revision_preserves_previous(tmp_path):
    first = ev.write_package(tmp_path, "v1")
    before = (first / "manifest.json").read_bytes()
    second = ev.write_package(tmp_path, "v2", version="v2")
    basis = json.loads((second / "assumptions.json").read_text())
    manifest = json.loads((second / "manifest.json").read_text())
    material = basis["material"]
    assert basis["version"] == manifest["version"] == "v2"
    assert material["source_type"] == "user-authorized example assumption"
    assert material["code_qualified"] is False
    assert material["yield_mpa"] / material["elastic_modulus_mpa"] == pytest.approx(0.00123076923)
    assert material["screening_stress_mpa"] < material["yield_mpa"] < material["tensile_mpa"]
    assert material["tensile_use"] == "information only; not a constitutive point or failure criterion"
    html = (second / "report.html").read_text(encoding="utf-8")
    for term in ("195000", "240.000", "450.000", "120.000", "not a code-qualified allowable"):
        assert term in html
    assert (first / "manifest.json").read_bytes() == before


def test_invalid_revision_rejected_before_output(tmp_path):
    with pytest.raises(ValueError, match="version"):
        ev.write_package(tmp_path, "bad", version="")
    assert not (tmp_path / "bad").exists()
