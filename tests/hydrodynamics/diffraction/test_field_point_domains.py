# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Coordinate-domain regressions for field-point input and offline emission."""

import json
import math
import os
from pathlib import Path
import subprocess
import sys

import numpy as np
import pytest
from pydantic import ValidationError
import yaml

from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    DiffractionSpec,
    FieldPointSpec,
    OutputSpec,
)
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend


@pytest.fixture
def authored_groups():
    """Synthetic geometry, authored here as the independent input oracle.

    Coordinates are chosen to exercise signs and zero, with no physical range
    assumption. Names deliberately oppose alphabetical order. Expected output
    is the literal rows below concatenated in authored order, without arithmetic.
    """
    return [
        {"name": "z_group", "points": [[-2, 0, 0.5], [2, -1, 0]]},
        {"name": "a_group", "points": [[0, 2, -0.5], [0, -0.0, 0]]},
    ]


def _validate_group(payload, entrypoint):
    if entrypoint == "construction":
        return FieldPointSpec(**payload)
    # Pydantic's JSON parser accepts NaN and Infinity tokens; validation must
    # reject them even on that path. json.dumps preserves these test tokens.
    return FieldPointSpec.model_validate_json(json.dumps(payload))


@pytest.mark.parametrize("entrypoint", ["construction", "json"])
# Axis indices follow the schema's (x, y, z) coordinate tuple.
@pytest.mark.parametrize("axis", [0, 1, 2], ids=["x", "y", "z"])
@pytest.mark.parametrize("coordinate", [math.nan, math.inf, -math.inf])
def test_field_points_reject_nonfinite_coordinates(
    authored_groups, entrypoint, axis, coordinate
):
    payload = authored_groups[0]
    payload["points"][-1][axis] = coordinate

    with pytest.raises(ValidationError) as error:
        _validate_group(payload, entrypoint)

    assert error.value.errors()[0]["loc"] == (
        "points", len(payload["points"]) - 1, axis
    )


@pytest.mark.parametrize("entrypoint", ["construction", "json"])
@pytest.mark.parametrize("axis", [0, 1, 2], ids=["x", "y", "z"])
@pytest.mark.parametrize("coordinate", [True, False])
def test_field_points_reject_boolean_coordinates(
    authored_groups, entrypoint, axis, coordinate
):
    payload = authored_groups[0]
    payload["points"][-1][axis] = coordinate

    with pytest.raises(ValidationError) as error:
        _validate_group(payload, entrypoint)

    assert error.value.errors()[0]["loc"] == (
        "points", len(payload["points"]) - 1, axis
    )


@pytest.mark.parametrize("nested", [False, True], ids=["direct", "nested"])
@pytest.mark.parametrize("axis", [0, 1, 2], ids=["x", "y", "z"])
@pytest.mark.parametrize("coordinate", [np.bool_(True), np.bool_(False)])
def test_field_points_reject_numpy_boolean_coordinates(
    authored_groups, nested, axis, coordinate
):
    payload = authored_groups[1]
    payload["points"][-1][axis] = coordinate

    with pytest.raises(ValidationError) as error:
        if nested:
            OutputSpec(field_points=authored_groups)
        else:
            FieldPointSpec(**payload)

    prefix = ("field_points", 1) if nested else ()
    assert error.value.errors()[0]["loc"] == prefix + (
        "points", len(payload["points"]) - 1, axis
    )


@pytest.mark.parametrize("nested", [False, True], ids=["direct", "nested"])
def test_field_points_accept_numpy_numeric_coordinates(nested):
    payload = {
        "name": "numeric_group",
        "points": [(np.float64(-2), np.int64(0), np.float64(0.5))],
    }
    if nested:
        group = OutputSpec(field_points=[payload]).field_points[0]
    else:
        group = FieldPointSpec(**payload)

    assert group.points == [(-2.0, 0.0, 0.5)]


@pytest.mark.parametrize("entrypoint", ["construction", "json"])
@pytest.mark.parametrize("mode", ["single", "modular"])
def test_field_points_preserve_finite_geometry(
    authored_groups, entrypoint, mode, tmp_path
):
    payload = {"field_points": authored_groups}
    if entrypoint == "construction":
        outputs = OutputSpec(**payload)
    else:
        outputs = OutputSpec.model_validate_json(json.dumps(payload))
    assert [group.name for group in outputs.field_points] == [
        group["name"] for group in authored_groups
    ]
    expected = [point for group in authored_groups for point in group["points"]]
    actual = [list(point) for group in outputs.field_points for point in group.points]
    assert actual == expected
    assert [math.copysign(1, value) for row in actual for value in row] == [
        math.copysign(1, value) for row in expected for value in row
    ]

    spec = DiffractionSpec.from_yaml(
        Path(__file__).parent / "fixtures/acceptance_610/spec.yml"
    )
    spec.outputs = outputs
    backend = OrcaWaveBackend()
    if mode == "single":
        generated = backend.generate_single(spec, tmp_path)
    else:
        backend.generate_modular(spec, tmp_path)
        generated = tmp_path / "08_outputs.yml"
    data = yaml.safe_load(generated.read_text(encoding="utf-8"))
    assert data["FieldPointX, FieldPointY, FieldPointZ"] == expected
    assert data["DetectAndSkipFieldPointsInsideBodies"] is True


@pytest.mark.parametrize("entrypoint", ["construction", "json"])
# Python's float_info defines representation limits, not engineering limits.
@pytest.mark.parametrize(
    "coordinate", [sys.float_info.max, -sys.float_info.max, sys.float_info.min]
)
def test_field_points_accept_extreme_finite_coordinates(
    authored_groups, entrypoint, coordinate
):
    payload = authored_groups[0]
    payload["points"][-1] = [coordinate] * len(payload["points"][-1])
    group = _validate_group(payload, entrypoint)
    assert list(group.points[-1]) == payload["points"][-1]


def test_field_point_yaml_round_trip_without_utf8_mode(authored_groups, tmp_path):
    spec = DiffractionSpec.from_yaml(
        Path(__file__).parent / "fixtures/acceptance_610/spec.yml"
    )
    spec.outputs = OutputSpec(field_points=authored_groups)
    spec.metadata.description = "\u03b1"
    source = tmp_path / "input.yml"
    source.write_text(
        yaml.safe_dump(spec.model_dump(mode="json"), allow_unicode=True),
        encoding="utf-8", newline="\n",
    )
    output = tmp_path / "output.yml"
    environment = dict(
        os.environ, LC_ALL="C", PYTHONCOERCECLOCALE="0",
        PYTHONPATH=(Path(__file__).resolve().parents[3] / "src").as_posix(),
    )
    environment.pop("PYTHONUTF8", None)
    result = subprocess.run(
        [sys.executable, "-X", "utf8=0", "-c", """
import sys
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
spec = DiffractionSpec.from_yaml(sys.argv[1])
assert spec.metadata.description == chr(945)
spec.to_yaml(sys.argv[2])
reloaded = DiffractionSpec.from_yaml(sys.argv[2])
assert reloaded.outputs == spec.outputs
assert reloaded.metadata.description == chr(945)
""", source.as_posix(), output.as_posix()],
        env=environment, capture_output=True, text=True, encoding="utf-8",
    )
    assert result.returncode == 0, result.stderr
    assert b"\r\n" not in output.read_bytes()
