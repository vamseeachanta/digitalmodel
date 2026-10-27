# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent checks of the saved synthetic package example.

Numerical expectations follow the hand calculation in the example README:
a cube has six square faces and eight corners. No solver is used.
"""

from __future__ import annotations

import json
import os
import runpy
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

EXAMPLE = (
    Path(__file__).resolve().parents[3]
    / "examples/domains/hydrodynamics/synthetic_package"
)


def _inputs(tmp_path: Path) -> Path:
    source = tmp_path / "source"
    shutil.copytree(EXAMPLE / "input", source)
    return source / "spec.yml"


def _emitted_meshes(node):
    """Inspect YAML independently of the converter's input asset iterator."""
    if isinstance(node, dict):
        for key, value in node.items():
            if key.endswith("MeshFileName"):
                yield value
            else:
                yield from _emitted_meshes(value)
    elif isinstance(node, list):
        for value in node:
            yield from _emitted_meshes(value)


def _assert_local_meshes(package: Path) -> None:
    references = []
    for path in package.glob("*.yml"):
        for document in yaml.safe_load_all(path.read_text()):
            for reference in _emitted_meshes(document):
                relative = Path(reference)
                assert not relative.is_absolute()
                resolved = (path.parent / relative).resolve()
                assert resolved.is_relative_to(package.resolve())
                assert resolved.is_file()
                references.append(reference)
    assert set(references) == {"box.gdf"}
    assert (package / "box.gdf").read_bytes() == (
        EXAMPLE / "input/meshes/box.gdf"
    ).read_bytes()


@pytest.mark.parametrize("output_format", ["single", "modular"])
def test_synthetic_package_example(tmp_path, monkeypatch, output_format):
    spec = _inputs(tmp_path)
    unrelated = tmp_path / "unrelated"
    unrelated.mkdir()
    monkeypatch.chdir(unrelated)
    build_package = runpy.run_path(str(EXAMPLE / "package_example.py"))["build_package"]
    output = tmp_path / "package"

    result = build_package(spec, output, output_format=output_format)

    assert result.is_file()
    assert result.parent == output
    _assert_local_meshes(output)
    # Relocation must not depend on the source tree or invocation directory.
    relocated = tmp_path / "relocated"
    shutil.copytree(output, relocated)
    _assert_local_meshes(relocated)
    quality = json.loads((output / "mesh_quality_report.json").read_text())
    body, = quality
    assert body["mesh"] == "box.gdf"
    assert body["status"] == "WARNING"
    assert not body["blocking"]
    assert body["report"]["num_panels"] == 6
    assert body["report"]["num_nodes"] == 8
    assert body["report"]["is_watertight"] is True
    assert body["report"]["normals_consistent"] is True
    assert body["report"]["panel_count_ok"] is False
    assert not list(unrelated.iterdir())


def test_package_example_missing_mesh(tmp_path, monkeypatch):
    spec = _inputs(tmp_path)
    text = spec.read_text().replace("meshes/box.gdf", "meshes/absent.gdf")
    spec.write_text(text)
    monkeypatch.chdir(tmp_path)
    build_package = runpy.run_path(str(EXAMPLE / "package_example.py"))["build_package"]
    output = tmp_path / "package"

    with pytest.raises(FileNotFoundError, match="Mesh pre-flight validation failed") as error:
        build_package(spec, output)

    assert "absent.gdf" in str(error.value)
    assert "spec file directory" in str(error.value)
    assert not output.exists()


@pytest.mark.parametrize("missing", [False, True])
def test_saved_example_command(tmp_path, missing):
    spec = _inputs(tmp_path)
    if missing:
        spec.write_text(spec.read_text().replace("meshes/box.gdf", "meshes/absent.gdf"))
    unrelated = tmp_path / "unrelated"
    unrelated.mkdir()
    output = tmp_path / "package"
    completed = subprocess.run(
        [sys.executable, str(EXAMPLE / "package_example.py"),
         "--spec", str(spec), "--output-dir", str(output)],
        cwd=unrelated, capture_output=True, text=True, check=False,
        env={**os.environ, "PYTHONPATH": str(EXAMPLE.parents[3] / "src")},
    )

    if missing:
        assert completed.returncode != 0
        assert "absent.gdf" in completed.stderr
        assert "Mesh pre-flight validation failed" in completed.stderr
        assert "Package generated" not in completed.stdout
        assert not output.exists()
    else:
        assert completed.returncode == 0, completed.stderr
        assert "Package generated" in completed.stdout
        assert "solver validation has not been run" in completed.stdout
        assert "WARNING" in completed.stdout
        _assert_local_meshes(output)
    assert not list(unrelated.iterdir())
