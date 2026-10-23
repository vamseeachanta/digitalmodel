# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent MIT 2.080J (2013) plate checks; lengths mm, stresses MPa."""

import ast
from pathlib import Path

import pytest

from digitalmodel.structural.plate_capacity.elastic_buckling import (
    plate_buckling_coefficient,
    plate_elastic_buckling_stress,
)


@pytest.mark.parametrize("ratio", [1.0, 2.0, 3.0])
def test_integer_aspect_ratios(ratio):
    """MIT Lecture 11 (2013), Eqs. 11.15-17, synthetic mm/MPa inputs.

    a/b=1,2,3 select m=1,2,3: k=(m/(a/b)+(a/b)/m)^2=4.
    E=120000 MPa, nu=0, t=1 mm, b=100 mm gives
    sigma=4*pi^2*120000/(12*10000)=4*pi^2=39.47841760435743 MPa.
    """
    assert plate_buckling_coefficient(length=100 * ratio, width=100) == 4
    assert plate_elastic_buckling_stress(
        youngs_modulus=120000,
        poisson_ratio=0,
        thickness=1,
        width=100,
        length=100 * ratio,
    ) == pytest.approx(39.47841760435743, rel=1e-12)


@pytest.mark.parametrize(
    ("ratio", "expected"),
    [(0.5, 6.25), (1.5, 4.340277777777778), (2.5, 4.134444444444444)],
)
def test_discrete_mode_minimum(ratio, expected):
    """MIT (2013) Eq. 11.16; a,b in mm, coefficient dimensionless.

    By hand: r=0.5 -> m=1 -> (2+0.5)^2=6.25;
    r=1.5 -> m=2 -> (4/3+3/4)^2=625/144;
    r=2.5 -> m=3 -> (6/5+5/6)^2=3721/900.
    These cases distinguish integer mode selection from the long-plate limit.
    """
    assert plate_buckling_coefficient(length=100 * ratio, width=100) == pytest.approx(
        expected
    )


def test_long_plate_limit():
    """MIT (2013) Eqs. 11.15-17: k=4, sigma=4*pi^2 MPa for this mm case."""
    assert plate_elastic_buckling_stress(
        youngs_modulus=120000,
        poisson_ratio=0,
        thickness=1,
        width=100,
    ) == pytest.approx(39.47841760435743, rel=1e-12)


def test_fractional_aspect_ratio_stress():
    """MIT (2013), Eqs. 11.15-16; synthetic MPa/mm case with a/b=0.5.

    a=50 mm, b=100 mm -> m=1, k=(2+0.5)^2=6.25.
    E=120000 MPa, nu=0, t=1 mm -> sigma=6.25*pi² MPa
    =61.68502750680849 MPa. This checks that stress uses the finite-plate k.
    """
    assert plate_elastic_buckling_stress(
        youngs_modulus=120000,
        poisson_ratio=0,
        thickness=1,
        width=100,
        length=50,
    ) == pytest.approx(61.68502750680849, rel=1e-12)


@pytest.mark.parametrize("field", ["youngs_modulus", "thickness", "width", "length"])
@pytest.mark.parametrize("invalid", [0, -1, float("nan"), float("inf"), -float("inf")])
def test_invalid_positive_inputs(field, invalid):
    """Finite positive E (MPa) and lengths (mm) are required."""
    inputs = dict(
        youngs_modulus=120000, poisson_ratio=0, thickness=1, width=100, length=200
    )
    inputs[field] = invalid
    with pytest.raises(ValueError, match=field):
        plate_elastic_buckling_stress(**inputs)


@pytest.mark.parametrize("invalid", [-1, -1.1, 0.5, 0.6, float("nan"), float("inf")])
def test_invalid_poisson_ratio(invalid):
    """MIT Lecture 4 (2013), Eqs. 4.14/4.16; nu dimensionless, E in MPa."""
    with pytest.raises(ValueError, match="poisson_ratio"):
        plate_elastic_buckling_stress(
            youngs_modulus=120000,
            poisson_ratio=invalid,
            thickness=1,
            width=100,
        )


@pytest.mark.parametrize("length,width", [(1e308, 1e-308), (1e-308, 1e308)])
def test_unrepresentable_ratio(length, width):
    """Synthetic mm lengths must fail cleanly outside floating-point range."""
    with pytest.raises(ValueError, match="floating-point range"):
        plate_buckling_coefficient(length=length, width=width)


def test_calculations_public_function_inventory():
    """The calculations folder has no public functions; units are inapplicable.

    AST inventory avoids importing historical scenario scripts. A new public
    function requires an explicit value assertion or NOT_EVALUATED entry.
    """
    root = (
        Path(__file__).resolve().parents[3]
        / "src/digitalmodel/structural/plate_capacity/calculations"
    )
    public = []
    for path in sorted(root.rglob("*.py")):
        tree = ast.parse(path.read_text())
        for node in ast.walk(tree):
            if isinstance(
                node, (ast.FunctionDef, ast.AsyncFunctionDef)
            ) and not node.name.startswith("_"):
                public.append(f"{path.relative_to(root)}:{node.name}")
    assert public == []
