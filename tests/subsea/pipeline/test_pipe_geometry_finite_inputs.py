# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Finite geometry checks and independent annulus hand calculations.

Synthetic dimensions are declared below in metres, with no design-code limits.
Hand calculation basis: in polar coordinates dA = r dr dtheta, so
A = integral(0..2*pi) integral(ri..ro) r dr dtheta = pi*(ro**2-ri**2).
About a centroidal diameter, y = r*sin(theta), hence
I = integral(0..2*pi) integral(ri..ro) r**3*sin(theta)**2 dr dtheta
  = pi*(ro**4-ri**4)/4. Thickness is ro-ri; diameters are twice the radii.
Thus ri=1, ro=2 gives (OD, ID, t)=(4, 2, 1), A=3*pi, I=15*pi/4;
ri=3, ro=5 gives (OD, ID, t)=(10, 6, 2), A=16*pi, I=136*pi.
Internal/external disk areas follow the same area integral with lower radius 0.
These written integrals are the source of every geometric expected value here.
The relative comparison tolerance 1e-12 allows binary64 rounding only.

Load the standard-library-only geometry source directly: the existing package
initializer imports a missing legacy module. Package import repair is separate
from these function regressions; no dependency or calculation is mocked.
"""

import importlib.util
import math
from pathlib import Path

import pytest


_SOURCE = (
    Path(__file__).resolve().parents[3]
    / "src/digitalmodel/subsea/pipeline/calculations/pipe_properties.py"
)
_SPEC = importlib.util.spec_from_file_location("pipe_geometry_under_test", _SOURCE)
geometry = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(geometry)

# The first synthetic annulus from the written integral above supplies all
# ordinary inputs. Only the selected argument changes in each rejection case.
_DIMENSIONS = dict(outer_diameter=4.0, inner_diameter=2.0, wall_thickness=1.0)
_PAIRS = (
    ("outer_diameter", "inner_diameter"),
    ("outer_diameter", "wall_thickness"),
    ("inner_diameter", "wall_thickness"),
)
_NONFINITE = [
    pytest.param(float("nan"), id="nan"),
    pytest.param(float("inf"), id="positive-infinity"),
    pytest.param(float("-inf"), id="negative-infinity"),
]


def _calculation_routes():
    """Enumerate each supplied argument of each supported calculation route."""
    routes = []
    for name in ("calculate_section_properties", "calculate_wall_thickness"):
        inputs = {key: _DIMENSIONS[key] for key in _PAIRS[0]}
        for argument in inputs:
            routes.append(pytest.param(name, inputs, argument, id=f"{name}-{argument}"))
    for diameter_type, diameter in (("outer", 4.0), ("inner", 2.0)):
        inputs = dict(
            known_diameter=diameter, wall_thickness=1.0, diameter_type=diameter_type
        )
        for argument in ("known_diameter", "wall_thickness"):
            routes.append(
                pytest.param(
                    "calculate_diameter_from_thickness",
                    inputs,
                    argument,
                    id=f"diameter-{diameter_type}-{argument}",
                )
            )
    for pair in (*_PAIRS, tuple(_DIMENSIONS)):
        inputs = {key: _DIMENSIONS[key] for key in pair}
        for argument in inputs:
            routes.append(
                pytest.param(
                    "calculate_geometric_properties",
                    inputs,
                    argument,
                    id=f"wrapper-{'-'.join(pair)}-{argument}",
                )
            )
    return routes


@pytest.mark.parametrize("value", _NONFINITE)
@pytest.mark.parametrize("function_name, inputs, argument", _calculation_routes())
def test_nonfinite_pipe_geometry_is_refused(function_name, inputs, argument, value):
    """Every supplied nonfinite dimension raises before calculation."""
    inputs = {**inputs, argument: value}
    with pytest.raises(ValueError):
        getattr(geometry, function_name)(**inputs)


@pytest.mark.parametrize("value", _NONFINITE)
@pytest.mark.parametrize("argument", tuple(_DIMENSIONS))
def test_validator_returns_false_for_nonfinite_geometry(argument, value):
    """The validator retains its Boolean rejection convention."""
    assert geometry.validate_pipe_geometry(**{**_DIMENSIONS, argument: value}) is False


@pytest.mark.parametrize("pair", _PAIRS, ids=("od-id", "od-thickness", "id-thickness"))
@pytest.mark.parametrize(
    "outer, inner, thickness, wall_area, inner_area, outer_area, inertia",
    [
        (4.0, 2.0, 1.0, 3 * math.pi, math.pi, 4 * math.pi, 15 * math.pi / 4),
        (10.0, 6.0, 2.0, 16 * math.pi, 9 * math.pi, 25 * math.pi, 136 * math.pi),
    ],
    ids=("radii-one-two", "radii-three-five"),
)
def test_dimension_pairs_match_annulus_hand_values(
    pair, outer, inner, thickness, wall_area, inner_area, outer_area, inertia
):
    """Each pair recovers dimensions, areas and inertia from the written integrals."""
    dimensions = dict(
        outer_diameter=outer, inner_diameter=inner, wall_thickness=thickness
    )
    result = geometry.calculate_geometric_properties(
        **{key: dimensions[key] for key in pair}
    )
    assert result == pytest.approx(
        {**dimensions, "A": wall_area, "Ai": inner_area, "Ao": outer_area, "I": inertia},
        rel=1e-12,
        abs=0.0,
    )
    assert geometry.validate_pipe_geometry(**dimensions) is True


@pytest.mark.parametrize("inner", [0.0, -2.0, 4.0, 6.0])
@pytest.mark.parametrize(
    "function_name",
    ["calculate_section_properties", "calculate_wall_thickness", "calculate_geometric_properties"],
)
def test_positive_bore_smaller_than_outer_diameter_is_required(function_name, inner):
    """Zero/negative, equal and reversed bores remain invalid for OD=4 m."""
    with pytest.raises(ValueError):
        getattr(geometry, function_name)(outer_diameter=4.0, inner_diameter=inner)


@pytest.mark.parametrize("thickness", [2.0, 3.0])
def test_outer_diameter_and_thickness_require_positive_bore(thickness):
    """At OD=4 m, thickness 2 m closes the bore; 3 m exceeds the radius."""
    with pytest.raises(ValueError):
        geometry.calculate_diameter_from_thickness(4.0, thickness, "outer")
    with pytest.raises(ValueError):
        geometry.calculate_geometric_properties(
            outer_diameter=4.0, wall_thickness=thickness
        )


@pytest.mark.parametrize("supplied_thickness", [0.0, -1.0, 2.0])
def test_finite_three_input_precedence_is_preserved(supplied_thickness):
    """Both diameters still determine thickness, even if a finite third disagrees."""
    result = geometry.calculate_geometric_properties(
        outer_diameter=4.0, inner_diameter=2.0, wall_thickness=supplied_thickness
    )
    assert result["wall_thickness"] == 1.0
    assert result["A"] == pytest.approx(3 * math.pi, rel=1e-12)
    assert result["I"] == pytest.approx(15 * math.pi / 4, rel=1e-12)


def test_validator_consistency_and_tolerance_convention_is_preserved():
    """Synthetic 2 m thickness disagrees with the 1 m radial difference by 1 m."""
    inconsistent = {**_DIMENSIONS, "wall_thickness": 2.0}
    assert geometry.validate_pipe_geometry(**inconsistent) is False
    assert geometry.validate_pipe_geometry(**inconsistent, tolerance=1.0) is True
