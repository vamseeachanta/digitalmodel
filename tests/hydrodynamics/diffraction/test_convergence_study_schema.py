# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic study cases; expected products are enumerated independently.

All grid coordinates below are synthetic inputs, not engineering defaults.
The sample has two mesh variants, two frequency grids and two heading grids:
hand multiplication gives eight cases, with headings varying fastest.
"""

from __future__ import annotations

import importlib.util
import subprocess
import sys
from copy import deepcopy
from pathlib import Path

import pytest
import yaml

# Load the pure module without executing the legacy package initializer.
SOURCE = (
    Path(__file__).resolve().parents[3]
    / "src/digitalmodel/hydrodynamics/diffraction/convergence_study_schema.py"
)
SPEC = importlib.util.spec_from_file_location("convergence_study_schema", SOURCE)
assert SPEC is not None and SPEC.loader is not None
schema = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = schema
SPEC.loader.exec_module(schema)

STUDY_YAML = """
mesh_variants:
  - {label: coarse, mesh_file: synthetic/coarse.gdf}
  - {label: fine, mesh_file: synthetic/fine.gdf}
frequency_grids:
  - {label: sparse, frequencies: [0.5, 1.0]}
  - {label: dense, frequencies: [0.5, 0.75, 1.0]}
heading_grids:
  - {label: axial, headings: [0.0, 180.0]}
  - {label: quarter, headings: [0.0, 90.0, 180.0, 270.0]}
"""


def raw_study() -> dict:
    """Return a fresh copy of the synthetic YAML input."""
    return yaml.safe_load(STUDY_YAML)


def test_study_schema_cartesian_count_and_order() -> None:
    study = schema.ConvergenceStudy.from_yaml(STUDY_YAML)
    expected = [
        ("coarse", "sparse", "axial"),
        ("coarse", "sparse", "quarter"),
        ("coarse", "dense", "axial"),
        ("coarse", "dense", "quarter"),
        ("fine", "sparse", "axial"),
        ("fine", "sparse", "quarter"),
        ("fine", "dense", "axial"),
        ("fine", "dense", "quarter"),
    ]
    first = schema.expand_study(study, max_cases=len(expected))
    second = schema.expand_study(study, max_cases=len(expected))
    assert first == second
    assert len(first) == len(expected)
    assert [
        (c.mesh_label, c.frequency_label, c.heading_label) for c in first
    ] == expected
    assert first[0].mesh_file == "synthetic/coarse.gdf"
    assert first[0].frequencies == (0.5, 1.0)
    assert first[0].headings == (0.0, 180.0)
    assert first[-1].mesh_file == "synthetic/fine.gdf"
    assert first[-1].frequencies == (0.5, 0.75, 1.0)
    assert first[-1].headings == (0.0, 90.0, 180.0, 270.0)


@pytest.mark.parametrize("axis", ["mesh_variants", "frequency_grids", "heading_grids"])
@pytest.mark.parametrize(
    "bad_axis", ["empty", "duplicate", "padded_duplicate", "blank_label"]
)
def test_study_schema_rejects_duplicates_and_bad_grids(axis, bad_axis) -> None:
    raw = raw_study()
    if bad_axis == "empty":
        raw[axis] = []
        message = axis
    elif bad_axis == "duplicate":
        raw[axis].append(deepcopy(raw[axis][0]))
        message = f"{axis}.*unique labels"
    elif bad_axis == "padded_duplicate":
        raw[axis][1]["label"] = f" {raw[axis][0]['label']} "
        message = f"{axis}.*unique labels"
    else:
        raw[axis][0]["label"] = "   "
        message = "label"
    with pytest.raises(ValueError, match=message):
        schema.ConvergenceStudy.model_validate(raw)


@pytest.mark.parametrize(
    "axis, field, values",
    [
        ("frequency_grids", "frequencies", []),
        ("frequency_grids", "frequencies", [0.0]),
        ("frequency_grids", "frequencies", [-0.5]),
        ("frequency_grids", "frequencies", [float("nan")]),
        ("frequency_grids", "frequencies", [float("inf")]),
        ("frequency_grids", "frequencies", [float("-inf")]),
        ("frequency_grids", "frequencies", [True]),
        ("frequency_grids", "frequencies", ["0.5"]),
        ("frequency_grids", "frequencies", [0.5, 0.5]),
        ("heading_grids", "headings", []),
        ("heading_grids", "headings", [-0.1]),
        ("heading_grids", "headings", [360.0]),
        ("heading_grids", "headings", [float("nan")]),
        ("heading_grids", "headings", [float("inf")]),
        ("heading_grids", "headings", [float("-inf")]),
        ("heading_grids", "headings", [False]),
        ("heading_grids", "headings", ["90.0"]),
        ("heading_grids", "headings", [0.0, 0.0]),
    ],
)
def test_study_schema_invalid_coordinates(axis, field, values) -> None:
    raw = raw_study()
    raw[axis][0][field] = values
    with pytest.raises(ValueError, match=field):
        schema.ConvergenceStudy.model_validate(raw)


@pytest.mark.parametrize("axis", ["mesh_variants", "frequency_grids", "heading_grids"])
def test_study_schema_rejects_missing_axes(axis) -> None:
    raw = raw_study()
    del raw[axis]
    with pytest.raises(ValueError, match=axis):
        schema.ConvergenceStudy.model_validate(raw)


@pytest.mark.parametrize("text", ["", "[]", "null", "mesh_variants: ["])
def test_study_schema_bad_yaml(text) -> None:
    with pytest.raises(ValueError):
        schema.ConvergenceStudy.from_yaml(text)


@pytest.mark.parametrize("axis", ["mesh_variants", "frequency_grids", "heading_grids"])
def test_study_schema_rejects_repeated_yaml_axes(axis) -> None:
    text = STUDY_YAML + yaml.safe_dump({axis: raw_study()[axis]})
    with pytest.raises(ValueError, match="invalid study YAML") as error:
        schema.ConvergenceStudy.from_yaml(text)
    assert "duplicate mapping key" in str(error.value.__cause__)


@pytest.mark.parametrize(
    "original, replacement",
    [
        ("label: coarse", "label: coarse, label: alternate"),
        ("mesh_file: synthetic/coarse.gdf",
         "mesh_file: synthetic/coarse.gdf, mesh_file: synthetic/alternate.gdf"),
        ("label: sparse", "label: sparse, label: alternate"),
        ("frequencies: [0.5, 1.0]",
         "frequencies: [0.5, 1.0], frequencies: [2.0]"),
        ("label: axial", "label: axial, label: alternate"),
        ("headings: [0.0, 180.0]", "headings: [0.0, 180.0], headings: [90.0]"),
    ],
)
def test_study_schema_rejects_repeated_nested_yaml_keys(original, replacement) -> None:
    text = STUDY_YAML.replace(original, replacement)
    with pytest.raises(ValueError, match="invalid study YAML") as error:
        schema.ConvergenceStudy.from_yaml(text)
    assert "duplicate mapping key" in str(error.value.__cause__)


def test_study_schema_accepts_noncolliding_yaml_merges() -> None:
    text = STUDY_YAML.replace(
        "- {label: coarse, mesh_file: synthetic/coarse.gdf}",
        "- {<<: &mesh {mesh_file: synthetic/coarse.gdf}, label: coarse}",
    ).replace("mesh_file: synthetic/fine.gdf", "<<: *mesh")
    study = schema.ConvergenceStudy.from_yaml(text)
    assert [mesh.label for mesh in study.mesh_variants] == ["coarse", "fine"]
    assert [mesh.mesh_file for mesh in study.mesh_variants] == [
        "synthetic/coarse.gdf", "synthetic/coarse.gdf",
    ]


def test_study_schema_rejects_colliding_yaml_merges() -> None:
    text = STUDY_YAML.replace(
        "{label: coarse, mesh_file: synthetic/coarse.gdf}",
        "{<<: {label: alternate}, label: coarse, mesh_file: synthetic/coarse.gdf}",
    )
    with pytest.raises(ValueError, match="invalid study YAML") as error:
        schema.ConvergenceStudy.from_yaml(text)
    assert "duplicate mapping key" in str(error.value.__cause__)


def test_study_schema_rejects_unsafe_yaml_tags() -> None:
    with pytest.raises(ValueError, match="invalid study YAML"):
        schema.ConvergenceStudy.from_yaml("!!python/tuple [1, 2]")


@pytest.mark.parametrize("mesh_file", ["", "   ", None])
def test_study_schema_requires_mesh_reference(mesh_file) -> None:
    raw = raw_study()
    raw["mesh_variants"][0]["mesh_file"] = mesh_file
    with pytest.raises(ValueError, match="mesh_file"):
        schema.ConvergenceStudy.model_validate(raw)


def test_study_schema_rejects_unknown_fields() -> None:
    raw = raw_study()
    raw["max_cases"] = 8  # The hand-enumerated sample count belongs to the caller.
    with pytest.raises(ValueError, match="max_cases"):
        schema.ConvergenceStudy.model_validate(raw)


@pytest.mark.parametrize("max_cases", [0, -1, True, 8.0, "8", None])
def test_study_schema_requires_positive_integer_cap(max_cases) -> None:
    study = schema.ConvergenceStudy.from_yaml(STUDY_YAML)
    with pytest.raises(ValueError, match="max_cases.*positive integer"):
        schema.expand_study(study, max_cases=max_cases)


def test_study_schema_requires_explicit_cap() -> None:
    study = schema.ConvergenceStudy.from_yaml(STUDY_YAML)
    with pytest.raises(TypeError, match="max_cases"):
        schema.expand_study(study)


def test_study_schema_cap_before_expansion(monkeypatch) -> None:
    study = schema.ConvergenceStudy.from_yaml(STUDY_YAML)

    def forbidden(*args, **kwargs):
        pytest.fail("product construction or descriptor allocation preceded cap check")

    monkeypatch.setattr(schema, "product", forbidden)
    monkeypatch.setattr(schema, "StudyCase", forbidden)
    # Eight cases were enumerated above; seven cannot hold the product.
    with pytest.raises(ValueError, match="8 cases.*max_cases=7"):
        schema.expand_study(study, max_cases=7)


def test_study_schema_preserves_input_and_freezes_cases() -> None:
    raw = raw_study()
    original = deepcopy(raw)
    study = schema.ConvergenceStudy.model_validate(raw)
    cases = schema.expand_study(study, max_cases=8)
    assert raw == original
    raw["frequency_grids"][0]["frequencies"].append(2.0)
    assert cases[0].frequencies == (0.5, 1.0)
    assert study.frequency_grids[0].frequencies == (0.5, 1.0)
    assert original == raw_study()
    with pytest.raises((AttributeError, ValueError)):
        cases[0].mesh_label = "changed"
    with pytest.raises(ValueError):
        study.mesh_variants = ()


def test_study_schema_labels_are_scoped_to_each_axis() -> None:
    raw = raw_study()
    for axis in raw:
        raw[axis][0]["label"] = "shared"
    study = schema.ConvergenceStudy.model_validate(raw)
    assert len(schema.expand_study(study, max_cases=8)) == 8


def _run_isolated_study(source: Path, directory: Path) -> subprocess.CompletedProcess:
    """Import and expand the study in a fresh isolated process."""
    script = """
import builtins
import importlib.util
import sys
from dataclasses import dataclass
from itertools import product
from pathlib import Path
from typing import Annotated, Self
import yaml
from pydantic import (
    BaseModel, ConfigDict, Field, StringConstraints, field_validator, model_validator,
)
# Preload dependency plugin discovery before target-module filesystem guards.
class DependencyWarmup(BaseModel):
    value: int

source, text = sys.argv[1:]
spec = importlib.util.spec_from_file_location('pure_study', source)
module = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = module
def forbidden(*args, **kwargs):
    raise AssertionError('filesystem operation during planning')
builtins.open = forbidden
Path.open = forbidden
Path.exists = forbidden
Path.mkdir = forbidden
spec.loader.exec_module(module)
study = module.ConvergenceStudy.from_yaml(text)
assert len(module.expand_study(study, max_cases=8)) == 8
assert not any(name.startswith('digitalmodel.') for name in sys.modules)
assert not any(name.startswith('OrcFxAPI') for name in sys.modules)
"""
    result = subprocess.run(
        [sys.executable, "-I", "-B", "-c", script, source.as_posix(), STUDY_YAML],
        cwd=directory,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    return result


def test_study_schema_import_and_expansion_are_solver_free(tmp_path) -> None:
    """A fresh process must not import runners or perform filesystem operations."""
    result = _run_isolated_study(SOURCE, tmp_path)
    assert result.returncode == 0, result.stderr
    assert list(tmp_path.iterdir()) == []


@pytest.mark.parametrize(
    "operation",
    [
        "open(__file__, encoding='utf-8')",
        "Path(__file__).open(encoding='utf-8')",
        "Path(__file__).exists()",
        "Path('unexpected').mkdir()",
    ],
)
def test_study_schema_guard_detects_import_time_filesystem_access(
    tmp_path, operation,
) -> None:
    probe = tmp_path / "probe_schema.py"
    probe.write_text(
        SOURCE.read_text(encoding="utf-8")
        + "\nfrom pathlib import Path\n" + operation + "\n",
        encoding="utf-8",
        newline="\n",
    )
    result = _run_isolated_study(probe, tmp_path)
    assert result.returncode != 0
    assert "filesystem operation during planning" in result.stderr
    assert operation in result.stderr


def test_study_schema_preserves_unsorted_coordinates() -> None:
    raw = raw_study()
    # Synthetic reversal of the sample grids must not trigger sorting.
    raw["frequency_grids"][0]["frequencies"] = [1.0, 0.5]
    raw["heading_grids"][0]["headings"] = [180.0, 0.0]
    study = schema.ConvergenceStudy.model_validate(raw)
    cases = schema.expand_study(study, max_cases=8)
    assert cases[0].frequencies == (1.0, 0.5)
    assert cases[0].headings == (180.0, 0.0)
