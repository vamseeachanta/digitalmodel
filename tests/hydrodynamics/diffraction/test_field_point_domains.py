# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Coordinate-domain regressions for field-point input and offline emission."""

import json
import math
from pathlib import Path
import sys

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
    data = yaml.safe_load(generated.read_text())
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
