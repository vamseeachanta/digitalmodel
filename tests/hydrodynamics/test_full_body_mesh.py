# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Geometry-only checks, including incomplete and reversed surface controls."""

import hashlib
import importlib.util
import json
from pathlib import Path

import numpy as np
import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/benchmark/full_body_mesh.py"
SPEC = importlib.util.spec_from_file_location("full_body_mesh", SCRIPT)
mesh = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(mesh)


@pytest.mark.parametrize("level", ["coarse", "medium", "fine"])
def test_full_mesh_geometry_and_determinism(level, tmp_path):
    data = mesh.gdf_bytes(level)
    assert data == mesh.gdf_bytes(level)
    path = tmp_path / f"{level}.gdf"
    path.write_bytes(data)
    parsed = mesh.read_gdf(path)
    assert parsed["symmetry"] == (0, 0)
    assert parsed["ulen"] == 1.0
    assert parsed["gravity"] == 9.80665
    target = dict(coarse=1464, medium=2928, fine=5856)[level]
    assert abs(len(parsed["panels"]) - target) / target < 0.05
    report = mesh.check_df2_buoyancy(path, 119052930)
    assert report["decision"] == "PASS"
    assert report["volume_m3"] == pytest.approx(116149.2, rel=1e-8)
    assert report["waterplane_area_m2"] == pytest.approx(116149.2 / 11)
    assert report["centre_of_buoyancy_m"] == pytest.approx([115.5, 0, -5.5], abs=1e-8)
    assert report["relative_error"] < 1e-8
    panels = parsed["panels"]
    assert np.min(panels[:, :, 0]) == 0
    assert np.max(panels[:, :, 0]) == 231
    assert np.max(panels[:, :, 1]) == pytest.approx(24.5)
    assert np.min(panels[:, :, 2]) == -11
    # End sections and the entire hull are symmetric fore/aft and port/starboard.
    vertices = set(map(tuple, panels.reshape(-1, 3).round(8)))
    assert np.allclose(
        sorted(vertices),
        sorted((231 - x, y, z) for x, y, z in vertices),
        rtol=0,
        atol=2e-8,
    )
    assert vertices == {(x, -y, z) for x, y, z in vertices}
    manifest = json.loads(
        (ROOT / "examples/hydrodynamics/full-body-meshes.json").read_text()
    )
    assert hashlib.sha256(data).hexdigest() == manifest["meshes"][level]["sha256"]


def test_half_mesh_fails_without_implicit_reflection(tmp_path):
    panels = mesh.panels_for_level("coarse")
    half = panels[np.min(panels[:, :, 1], axis=1) >= -1e-9]
    path = tmp_path / "half.gdf"
    path.write_bytes(mesh.encode_gdf(half, symmetry=(0, 1)))
    report = mesh.check_df2_buoyancy(path, 119052930)
    assert report["decision"] == "FAIL"
    assert report["volume_m3"] == pytest.approx(58074.6)
    assert report["waterplane_area_m2"] == pytest.approx(116149.2 / 22)
    assert report["relative_error"] == pytest.approx(0.5)
    assert 11 < report["centre_of_buoyancy_m"][1] < 12.5


@pytest.mark.parametrize(
    "fault", ["missing", "reversed", "one_reversed", "duplicate", "nan", "bad_header"]
)
def test_unusable_mesh_is_not_evaluated(tmp_path, fault):
    panels = mesh.panels_for_level("coarse")
    if fault == "missing":
        panels = panels[1:]
    elif fault == "reversed":
        panels = panels[:, ::-1]
    elif fault == "one_reversed":
        panels[0] = panels[0, ::-1]
    elif fault == "duplicate":
        panels = np.concatenate([panels, panels[:1]])
    elif fault == "nan":
        panels[0, 0, 0] = np.nan
    path = tmp_path / "invalid.gdf"
    path.write_bytes(mesh.encode_gdf(panels) if fault != "bad_header" else b"invalid")
    report = mesh.check_df2_buoyancy(path, 119052930)
    assert report["decision"] == "NOT_EVALUATED"
    assert report["reason"]


@pytest.mark.parametrize(
    "mass,rho,tolerance",
    [
        (0, 1025, 0.01),
        (True, 1025, 0.01),
        (119052930, 0, 0.01),
        (119052930, 1025, -1),
        (119052930, 1025, 1),
    ],
)
def test_invalid_df2_parameters(tmp_path, mass, rho, tolerance):
    path = tmp_path / "valid.gdf"
    path.write_bytes(mesh.gdf_bytes("coarse"))
    assert (
        mesh.check_df2_buoyancy(path, mass, rho, tolerance)["decision"]
        == "NOT_EVALUATED"
    )


def test_analytic_box_and_scaled_units(tmp_path):
    # Outward wet faces of a 2 x 2 x 1 box; independent of the hull generator.
    panels = np.array(
        [
            [[0, -1, 0], [0, 1, 0], [0, 1, -1], [0, -1, -1]],
            [[2, -1, -1], [2, 1, -1], [2, 1, 0], [2, -1, 0]],
            [[0, -1, -1], [2, -1, -1], [2, -1, 0], [0, -1, 0]],
            [[0, 1, 0], [2, 1, 0], [2, 1, -1], [0, 1, -1]],
            [[0, -1, -1], [0, 1, -1], [2, 1, -1], [2, -1, -1]],
        ],
        dtype=float,
    )
    path = tmp_path / "box.gdf"
    data = mesh.encode_gdf(panels / 2).decode().splitlines()
    data[1] = "2.0 9.80665"
    path.write_text("\n".join(data) + "\n")
    report = mesh.check_df2_buoyancy(path, 4100)
    assert report["decision"] == "PASS"
    assert report["volume_m3"] == pytest.approx(4)
    assert report["waterplane_area_m2"] == pytest.approx(4)
    assert report["centre_of_buoyancy_m"] == pytest.approx([1, 0, -0.5])
