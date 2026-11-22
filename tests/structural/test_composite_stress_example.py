# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic perfect-bond CFT stress results, independent of the hand oracle."""

import csv
import hashlib
import importlib.util
import inspect
import json
import math
from importlib.metadata import version
from pathlib import Path
from unittest.mock import Mock

import matplotlib.figure
import matplotlib.pyplot as plt
import numpy as np
import pytest
from sectionproperties.analysis import Section
from sectionproperties.post.stress_post import StressPost
from sectionproperties.pre.geometry import CompoundGeometry

SCRIPT = (
    Path(__file__).resolve().parents[2]
    / "scripts/integrations/composite_stress_example.py"
)


def load_example():
    spec = importlib.util.spec_from_file_location("composite_stress_example", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize("sides,area", [(32, 16.0), (64, 4.0)])
@pytest.mark.parametrize("strain,curvature", [(1e-4, 1e-6), (1e-4, -1e-6), (0.0, 0.0)])
def test_library_stress_outputs(sides, area, strain, curvature, tmp_path, monkeypatch):
    module = load_example()
    calls, plots = [], []
    calculate, plot = Section.calculate_stress, StressPost.plot_stress

    def capture_stress(section, *args, **kwargs):
        bound = inspect.signature(calculate).bind(section, *args, **kwargs)
        bound.apply_defaults()
        post = calculate(section, *args, **kwargs)
        calls.append((section, bound.arguments, post))
        return post

    def capture_plot(post, *args, **kwargs):
        ax = plot(post, *args, **kwargs)
        plots.append((post, args, kwargs, ax))
        return ax

    monkeypatch.setattr(Section, "calculate_stress", capture_stress)
    monkeypatch.setattr(StressPost, "plot_stress", capture_plot)
    for name in ("calculate_warping_properties", "calculate_plastic_properties"):
        monkeypatch.setattr(Section, name, Mock(side_effect=AssertionError(name)))
    before = plt.get_fignums()
    output = tmp_path / "results"
    receipt = module.run_example(
        output, epsilon0=strain, kappa=curvature, n=sides, mesh_area=area
    )
    assert plt.get_fignums() == before
    assert len(calls) == len(plots) == 1
    section, actions, post = calls[0]
    stiffness = json.loads((output / "sectionproperties_cft_results.json").read_text())
    assert actions["n"] == pytest.approx(strain * stiffness["EA_N"])
    assert actions["mxx"] == pytest.approx(curvature * stiffness["EIxx_Nmm2"])
    assert all(actions[key] == 0 for key in ("vx", "vy", "myy", "m11", "m22", "mzz"))
    assert plots[0][0] is post
    assert plots[0][2]["stress"] == "zz"
    assert plots[0][2]["colorbar_label"] == "Normal stress (MPa)"
    assert plots[0][2]["render"] is False
    assert plots[0][3].get_xlabel() == "x (mm)"
    assert plots[0][3].get_ylabel() == "y (mm)"

    with (output / "composite_stress.csv").open(newline="") as stream:
        rows = list(csv.DictReader(stream))
    assert list(rows[0]) == ["material", "node_id", "x_mm", "y_mm", "sigma_zz_MPa"]
    exported = {(row["material"], int(row["node_id"])): row for row in rows}
    assert len(exported) == len(rows)
    expected_keys = set()
    # Independent membership from raw mesh region attributes and connectivity.
    attributes = np.asarray(section.mesh["triangle_attributes"]).reshape(-1)
    stresses = {item["material"]: item["sig_zz"] for item in post.get_stress()}
    for region, material in enumerate(section.materials):
        nodes = set(section.mesh_elements[attributes == region].ravel())
        expected_keys.update((material.name, int(node)) for node in nodes)
        for node in nodes:
            row = exported[(material.name, int(node))]
            assert [float(row["x_mm"]), float(row["y_mm"])] == pytest.approx(
                section.mesh_nodes[node]
            )
            assert float(row["sigma_zz_MPa"]) == stresses[material.name][node]
    assert set(exported) == expected_keys  # Also retains true zero values.

    tolerance = (2 * math.pi / sides) ** 2 / 3 + 10 * math.sqrt(math.ulp(1.0))
    strains = []
    for material, y, modulus in (
        ("Steel", 40.0, 210_000.0),
        ("Steel", 32.0, 210_000.0),
        ("Concrete", 32.0, 35_000.0),
    ):
        target = [
            row
            for row in rows
            if row["material"] == material
            and abs(float(row["x_mm"])) <= 1e-8
            and abs(float(row["y_mm"]) - y) <= 1e-8
        ]
        assert len(target) == 1  # No nearest-point fallback or missing interface side.
        stress = float(target[0]["sigma_zz_MPa"])
        expected = modulus * (strain + curvature * y)
        assert stress == pytest.approx(expected, rel=tolerance, abs=1e-6)
        if y == 32.0:
            strains.append(stress / modulus)
    assert strains[0] == pytest.approx(strains[1], rel=tolerance, abs=1e-6 / 35_000)

    saved = json.loads((output / "composite_stress_receipt.json").read_text())
    assert saved == receipt
    assert receipt["sectionproperties_version"] == version("sectionproperties")
    assert receipt["inputs"] == {
        "outer_diameter_mm": 80.0,
        "wall_thickness_mm": 8.0,
        "steel_E_MPa": 210_000.0,
        "concrete_E_MPa": 35_000.0,
        "epsilon0": strain,
        "kappa_per_mm": curvature,
    }
    assert receipt["mesh"] == {"n": sides, "maximum_triangle_area_mm2": area}
    assert receipt["stiffness"] == {
        key: stiffness[key] for key in ("EA_N", "EIxx_Nmm2", "Ixx_steel_mm4")
    }
    assert receipt["actions"] == {"N_N": actions["n"], "Mxx_Nmm": actions["mxx"]}
    assert receipt["tolerance"] == {
        "relative_stress": tolerance,
        "absolute_stress_MPa": 1e-6,
        "coordinate_mm": 1e-8,
    }
    assert receipt["units"] == {
        "length": "mm",
        "stress": "MPa",
        "force": "N",
        "moment": "N mm",
    }
    assert "synthetic" in " ".join(receipt["assumptions"]).lower()
    names = {
        "sectionproperties_cft_results.json",
        "composite_stress.png",
        "composite_stress.csv",
        "composite_stress_receipt.json",
    }
    assert set(receipt["outputs"]) == {p.name for p in output.iterdir()} == names
    assert set(receipt["sha256"]) == names - {"composite_stress_receipt.json"}
    for name, digest in receipt["sha256"].items():
        assert hashlib.sha256((output / name).read_bytes()).hexdigest() == digest
    png = (output / "composite_stress.png").read_bytes()
    assert png.startswith(b"\x89PNG\r\n\x1a\n") and len(png) > 1000


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
@pytest.mark.parametrize("field", ["epsilon0", "kappa"])
def test_nonfinite_actions_rejected_before_output(field, value, tmp_path, monkeypatch):
    module = load_example()
    guard = Mock(side_effect=AssertionError("Invalid actions must not mesh"))
    monkeypatch.setattr(CompoundGeometry, "create_mesh", guard)
    with pytest.raises(ValueError, match="finite"):
        module.run_example(tmp_path / "absent", **{field: value})
    guard.assert_not_called()
    assert list(tmp_path.iterdir()) == []


def test_import_is_inert(tmp_path, monkeypatch, capsys):
    monkeypatch.chdir(tmp_path)
    guards = []
    for target, name in (
        (CompoundGeometry, "create_mesh"),
        (Section, "calculate_stress"),
        (Path, "mkdir"),
        (Path, "write_text"),
        (Path, "open"),
        (matplotlib.figure.Figure, "savefig"),
    ):
        guard = Mock(side_effect=AssertionError(f"Import must not call {name}"))
        monkeypatch.setattr(target, name, guard)
        guards.append(guard)
    assert callable(load_example().run_example)
    for guard in guards:
        guard.assert_not_called()
    assert list(tmp_path.iterdir()) == []
    assert capsys.readouterr().out == ""
