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
    assert report["mass_kg"] == pytest.approx(119052930)
    assert report["density_kg_m3"] == pytest.approx(1025)
    # Supplied criteria: 1 / 100 = 0.01; 1 / 1000 = 0.001.
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
        (ROOT / "examples/hydrodynamics/full-body-meshes.json").read_text(
            encoding="utf-8"
        )
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


def test_df2_records_supplied_criteria_and_input_basis(tmp_path):
    path = tmp_path / "full.gdf"
    path.write_bytes(mesh.gdf_bytes("coarse"))
    report = mesh.check_df2_buoyancy(path, 119052930)
    assert report["decision"] == "PASS"
    assert report["mass_relative_error_limit"] == pytest.approx(0.01)
    assert report["transverse_beam_fraction_limit"] == pytest.approx(0.001)
    # Hand calculation: centre limit = 49 m * 0.001 = 0.049 m.
    assert report["transverse_centre_limit_m"] == pytest.approx(0.049)
    assert "supplied investigation criteria" in report["criteria_basis"]
    assert "assumed, based on previous experience" in report["input_basis"]


def test_generated_manifest_labels_unsupported_inputs(tmp_path):
    manifest = mesh.generate(tmp_path / "generated")
    assert "assumed, based on previous experience" in manifest["basis"]
    retained = json.loads(
        (ROOT / "examples/hydrodynamics/full-body-meshes.json").read_text(
            encoding="utf-8"
        )
    )
    assert manifest["basis"] == retained["basis"]
    for level in ("coarse", "medium", "fine"):
        assert (
            manifest["meshes"][level]["sha256"] == retained["meshes"][level]["sha256"]
        ), "Rebuild with python scripts/benchmark/full_body_mesh.py <output>"


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


def test_analytic_box(tmp_path):
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
    path.write_bytes(mesh.encode_gdf(panels))
    report = mesh.check_df2_buoyancy(path, 4100)
    assert report["decision"] == "PASS"
    assert report["volume_m3"] == pytest.approx(4)
    assert report["waterplane_area_m2"] == pytest.approx(4)
    assert report["centre_of_buoyancy_m"] == pytest.approx([1, 0, -0.5])


@pytest.mark.parametrize("sign", [-1, 1])
def test_asymmetric_equal_volume_mesh_fails_df2(tmp_path, sign):
    panels = mesh.panels_for_level("coarse")
    # A transverse shear preserves volume and area but shifts buoyancy off centre.
    panels[:, :, 1] += sign * 0.004 * mesh.BEAM * panels[:, :, 0] / mesh.LENGTH
    path = tmp_path / "asymmetric.gdf"
    path.write_bytes(mesh.encode_gdf(panels))
    report = mesh.check_df2_buoyancy(path, mesh.MASS)
    assert report["volume_m3"] == pytest.approx(mesh.VOLUME)
    assert report["relative_error"] < 1e-8
    assert report["centre_of_buoyancy_m"][1] == pytest.approx(sign * 0.098)
    assert report["decision"] == "FAIL"


@pytest.mark.parametrize("fraction,decision", [(0.0009, "PASS"), (0.0011, "FAIL")])
@pytest.mark.parametrize("sign", [-1, 1])
def test_df2_transverse_centre_tolerance(tmp_path, fraction, decision, sign):
    panels = mesh.panels_for_level("coarse")
    panels[:, :, 1] += sign * fraction * mesh.BEAM
    path = tmp_path / "offset.gdf"
    path.write_bytes(mesh.encode_gdf(panels))
    report = mesh.check_df2_buoyancy(path, mesh.MASS)
    assert report["relative_error"] < 1e-8
    assert report["decision"] == decision


@pytest.mark.parametrize("ulen", [0.5, 2.0])
def test_nonunit_ulen_cannot_be_interpreted_as_coordinate_scale(tmp_path, ulen):
    path = tmp_path / "nonunit.gdf"
    lines = mesh.gdf_bytes("coarse").decode().splitlines()
    lines[1] = f"{ulen} 9.80665"
    path.write_text("\n".join(lines) + "\n", encoding="utf-8", newline="\n")
    with pytest.raises(ValueError, match="ULEN.*1.0"):
        mesh.read_gdf(path)
    report = mesh.check_df2_buoyancy(path, mesh.MASS)
    assert report["decision"] == "NOT_EVALUATED"


@pytest.mark.parametrize("flags", [(0, 0), (1, 0), (0, 1), (1, 1)])
@pytest.mark.parametrize("declared", ["none", "xz", "yz", "xz+yz", "unknown"])
def test_campaign_symmetry_requires_full_body_without_optimization(
    tmp_path, flags, declared
):
    path = tmp_path / "symmetry.gdf"
    path.write_bytes(mesh.encode_gdf(mesh.panels_for_level("coarse"), flags))
    if flags == (0, 0) and declared == "none":
        assert mesh.check_symmetry(path, declared) == (0, 0)
    else:
        with pytest.raises(ValueError, match="full-body.*symmetry.*none"):
            mesh.check_symmetry(path, declared)
