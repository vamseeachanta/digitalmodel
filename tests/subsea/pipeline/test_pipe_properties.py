# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent annulus examples; synthetic dimensions, all lengths in metres.

Source: Engineering Statics: Open and Interactive, online edition dated
2026-09-26, section 10.2.4, equations 10.2.9 and 10.2.10, and section 10.4.
https://engineeringstatics.org/MOI-common-shapes.html#MOI-circles
https://engineeringstatics.org/Chapter_10-moment-of-inertia-of-composite-shapes.html
Removing a concentric circle gives A=pi(D²-d²)/4 and I=pi(D⁴-d⁴)/64.
The derived polar moment is J=2I. Section modulus is I/(D/2).
The latter follows from Mechanics of Materials, fall 1999 course-text edition,
section 4.2, equation 4.2.7: https://eng.libretexts.org/@go/page/44541
Tolerances below allow floating-point arithmetic, not dimensional uncertainty.
"""

import math
from importlib import import_module, util
from pathlib import Path

import pytest

# The package initializer imports a removed module. Load the unchanged source
# directly so its arithmetic can be tested independently of that baseline defect.
_SOURCE = (
    Path(__file__).resolve().parents[3]
    / "src/digitalmodel/subsea/pipeline/calculations/pipe_properties.py"
)
_SPEC = util.spec_from_file_location("section_properties_test_target", _SOURCE)
_MODULE = util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(_MODULE)
calculate_diameter_from_thickness = _MODULE.calculate_diameter_from_thickness
calculate_geometric_properties = _MODULE.calculate_geometric_properties
calculate_section_properties = _MODULE.calculate_section_properties
calculate_wall_thickness = _MODULE.calculate_wall_thickness
validate_pipe_geometry = _MODULE.validate_pipe_geometry


@pytest.mark.xfail(
    strict=True,
    raises=ModuleNotFoundError,
    reason="Stale package import; see linked issue in the reference document",
)
def test_section_properties_public_import():
    module = import_module("digitalmodel.subsea.pipeline.calculations.pipe_properties")
    assert callable(module.calculate_section_properties)


@pytest.mark.parametrize(
    "outer,inner,expected,modulus,polar",
    [
        (2.0, 1.8, (0.19, 0.81, 1.0, 0.085975), 0.085975, 0.17195),
        (4.0, 2.0, (3.0, 1.0, 4.0, 3.75), 1.875, 7.5),
        pytest.param(
            6.0,
            0.0,
            (9.0, 0.0, 9.0, 20.25),
            6.75,
            40.5,
            marks=pytest.mark.xfail(
                strict=True,
                raises=ValueError,
                reason="Solid section limit is rejected; see reference document",
            ),
        ),
    ],
    ids=["thin-annulus", "thick-annulus", "solid-limit"],
)
def test_section_properties_hand_annuli(outer, inner, expected, modulus, polar):
    """Thin: (4-3.24)/4=.19; (16-10.4976)/64=.085975.

    Thick: (16-4)/4=3; (256-16)/64=3.75.
    Solid: (36-0)/4=9; (1296-0)/64=20.25.
    Each coefficient multiplies pi; internal/external areas are pi*d²/4,
    pi*D²/4. These are independent pencil-and-paper reductions of the source.
    Derived Z/pi=(.085975,1.875,6.75), J/pi=(.17195,7.5,40.5);
    the API returns I, so Z and J are checked as derived quantities only.
    Relative tolerance 1e-12 covers cancellation in the thin annulus.
    """
    result = calculate_section_properties(outer, inner)
    assert set(result) == {"A", "Ai", "Ao", "I"}
    assert result == pytest.approx(
        dict(zip(("A", "Ai", "Ao", "I"), [v * math.pi for v in expected])),
        rel=1e-12,
        abs=1e-14,
    )
    assert result["I"] / (outer / 2.0) == pytest.approx(modulus * math.pi, rel=1e-12)
    assert 2.0 * result["I"] == pytest.approx(polar * math.pi, rel=1e-12)


@pytest.mark.parametrize(
    "kind,known,expected", [("outer", 4.0, 2.0), ("inner", 2.0, 4.0)]
)
def test_diameter_and_thickness_hand_value_and_round_trip(kind, known, expected):
    """Synthetic D=4, d=2: t=(4-2)/2=1; D=d+2t, d=D-2t.

    Absolute tolerance 1e-12 permits only floating-point round-off.
    """
    thickness = calculate_wall_thickness(4.0, 2.0)
    assert thickness == pytest.approx(1.0, abs=1e-12)
    assert calculate_diameter_from_thickness(known, thickness, kind) == pytest.approx(
        expected, abs=1e-12
    )


@pytest.mark.parametrize(
    "inputs",
    [
        {"outer_diameter": 4.0, "inner_diameter": 2.0},
        {"outer_diameter": 4.0, "wall_thickness": 1.0},
        {"inner_diameter": 2.0, "wall_thickness": 1.0},
    ],
)
def test_geometric_properties_all_parameter_pairs(inputs):
    """D=4,d=2,t=1; A=3pi,Ai=pi,Ao=4pi,I=3.75pi; see hand annulus above."""
    assert calculate_geometric_properties(**inputs) == pytest.approx(
        {
            "outer_diameter": 4.0,
            "inner_diameter": 2.0,
            "wall_thickness": 1.0,
            "A": 3.0 * math.pi,
            "Ai": math.pi,
            "Ao": 4.0 * math.pi,
            "I": 3.75 * math.pi,
        },
        rel=1e-12,
    )


@pytest.mark.parametrize(
    "outer,inner,thickness",
    [
        pytest.param(
            0.0,
            -2.0,
            1.0,
            marks=pytest.mark.xfail(
                strict=True, reason="Nonpositive geometry is accepted"
            ),
        ),
        pytest.param(
            -2.0,
            -4.0,
            1.0,
            marks=pytest.mark.xfail(
                strict=True, reason="Nonpositive geometry is accepted"
            ),
        ),
        pytest.param(
            2.0,
            0.0,
            1.0,
            marks=pytest.mark.xfail(
                strict=True, reason="Nonpositive geometry is accepted"
            ),
        ),
        pytest.param(
            2.0,
            -2.0,
            2.0,
            marks=pytest.mark.xfail(
                strict=True, reason="Nonpositive geometry is accepted"
            ),
        ),
        pytest.param(
            2.0,
            2.0,
            0.0,
            marks=pytest.mark.xfail(strict=True, reason="Equal diameters are accepted"),
        ),
        pytest.param(
            2.0,
            4.0,
            -1.0,
            marks=pytest.mark.xfail(
                strict=True, reason="Reversed diameters are accepted"
            ),
        ),
        (4.0, 2.0, 0.0),
        (4.0, 2.0, -1.0),
    ],
)
def test_validate_pipe_geometry_refuses_nonphysical_dimensions(outer, inner, thickness):
    """Synthetic invalid cases exercise positivity and diameter ordering independently."""
    assert validate_pipe_geometry(outer, inner, thickness) is False


@pytest.mark.parametrize("bad", [math.nan, math.inf, -math.inf])
@pytest.mark.parametrize("index", [0, 1, 2])
def test_validate_pipe_geometry_refuses_nonfinite(bad, index):
    """Every input position must be finite; finite reference geometry is (4,2,1)."""
    values = [4.0, 2.0, 1.0]
    values[index] = bad
    assert validate_pipe_geometry(*values) is False


def test_validate_pipe_geometry_consistency_and_explicit_tolerance():
    """t=1 exactly; synthetic tolerance .001 accepts .0005 and refuses .002 error."""
    assert validate_pipe_geometry(4.0, 2.0, 1.0) is True
    assert validate_pipe_geometry(4.0, 2.0, 1.0005, tolerance=0.001) is True
    assert validate_pipe_geometry(4.0, 2.0, 1.002, tolerance=0.001) is False


@pytest.mark.parametrize(
    "inputs", [{}, {"outer_diameter": 4.0}, {"inner_diameter": 2.0}]
)
def test_geometric_properties_requires_two_parameters(inputs):
    with pytest.raises(ValueError, match="At least two"):
        calculate_geometric_properties(**inputs)


@pytest.mark.parametrize(
    "known,thickness,kind",
    [
        (0.0, 1.0, "outer"),
        (4.0, 0.0, "outer"),
        (4.0, 3.0, "outer"),
        (4.0, 1.0, "radius"),
    ],
)
def test_diameter_from_thickness_refuses_invalid_inputs(known, thickness, kind):
    with pytest.raises(ValueError):
        calculate_diameter_from_thickness(known, thickness, kind)
