# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand checks of the simply supported rectangular plate solution.

Basis: MIT 2.080J Structural Mechanics (2013), Lecture 11, pp. 11-3--11-4,
Eqs. 11.15--11.17. Expected coefficients below are rational hand calculations,
not evaluations of the implementation. Stress inputs are synthetic.
"""

import csv
import subprocess
import sys
from math import sqrt
from pathlib import Path

import pytest

from digitalmodel.structural.plate_capacity.classical_elastic_buckling import (
    governing_half_waves,
    plate_buckling_coefficient,
    rectangular_plate_elastic_buckling_stress,
)
from digitalmodel.structural.plate_capacity.elastic_buckling import (
    square_plate_elastic_buckling_stress,
)


@pytest.mark.parametrize(
    ("ratio", "mode", "expected"),
    [
        (0.5, 1, 25 / 4),  # (2 + 1/2)^2
        (1.0, 1, 4.0),  # (1 + 1)^2
        (1.41, 1, 892874161 / 198810000),  # (100/141 + 141/100)^2
        (1.45, 2, 5958481 / 1345600),  # (40/29 + 29/40)^2
        (1.5, 2, 625 / 144),  # (4/3 + 3/4)^2
        (2.0, 2, 4.0),
        (1000.0, 1000, 4.0),  # integer aspect ratio divides into squares
    ],
)
def test_hand_calculated_minimum(ratio, mode, expected):
    assert governing_half_waves(ratio) == mode
    assert plate_buckling_coefficient(ratio) == pytest.approx(expected, rel=1e-12)


@pytest.mark.parametrize("mode, expected", [(1, 25 / 4), (2, 4), (3, 169 / 36)])
def test_specified_mode_hand_calculation(mode, expected):
    """For a/b=2, m=1,2,3: (m/2+2/m)^2; Lecture 11 Table 11.1."""
    assert plate_buckling_coefficient(2.0, half_waves=mode) == pytest.approx(expected)


@pytest.mark.parametrize("mode", [1, 2, 10])
def test_mode_transition(mode):
    """Equality of adjacent branches gives r^2=m(m+1), by subtraction."""
    transition = sqrt(mode * (mode + 1))
    assert governing_half_waves(transition * (1 - 1e-8)) == mode
    assert governing_half_waves(transition * (1 + 1e-8)) == mode + 1
    left = plate_buckling_coefficient(transition, half_waves=mode)
    right = plate_buckling_coefficient(transition, half_waves=mode + 1)
    # At r=sqrt(m*(m+1)), each branch gives (2*m+1)^2/[m*(m+1)].
    assert left == pytest.approx((2 * mode + 1) ** 2 / (mode * (mode + 1)))
    assert left == pytest.approx(right, rel=1e-12)
    assert plate_buckling_coefficient(transition) == pytest.approx(left, rel=1e-12)


def test_short_plate_representable_coefficient():
    """For r=1e-154, m=1: k=(1e154+1e-154)^2 remains representable."""
    assert governing_half_waves(1e-154) == 1
    assert plate_buckling_coefficient(1e-154) == pytest.approx(1e308)


def test_long_plate_limit():
    """r=1000.5: m=1001, k=4+(4003/4006002)^2, tends to 4."""
    coefficient = plate_buckling_coefficient(1000.5)
    assert governing_half_waves(1000.5) == 1001
    assert coefficient == pytest.approx(4 + (4003 / 4006002) ** 2, rel=1e-14)
    assert coefficient > 4.0
    assert coefficient == pytest.approx(4.0, abs=1e-6)


@pytest.mark.parametrize("ratio", [0.5, 1.0, 1.41, 2.0])
@pytest.mark.parametrize("poisson_ratio", [0.0, 0.3, -0.3])
def test_stress_hand_calculation(ratio, poisson_ratio):
    """E=120000, t=1, b=100: sigma=k*pi^2/(1-nu^2) MPa.

    Hand k=6.25 at r=0.5 gives 61.68502750680849 MPa for nu=0;
    k=4 at r=1,2 gives 39.47841760435743 MPa. For |nu|=0.3,
    divide those hand results by 0.91. No yield correction is applied.
    """
    expected = 61.68502750680849 if ratio == 0.5 else 39.47841760435743
    if ratio == 1.41:
        # sigma=(892874161/198810000)*pi^2; use the independent 4*pi^2
        # hand result as the normalization, not the square-plate function.
        expected = 39.47841760435743 * (892874161 / 795240000)
    if poisson_ratio:
        expected /= 0.91
    assert rectangular_plate_elastic_buckling_stress(
        youngs_modulus=120000.0,
        poisson_ratio=poisson_ratio,
        thickness=1.0,
        length=ratio * 100.0,
        width=100.0,
    ) == pytest.approx(expected, rel=1e-12)


def test_square_function_compatibility_and_length_unit_scaling():
    reference = square_plate_elastic_buckling_stress(
        youngs_modulus=120000.0, poisson_ratio=0.3, thickness=1.0, side_length=100.0
    )
    for scale in (1.0, 0.001):
        assert rectangular_plate_elastic_buckling_stress(
            youngs_modulus=120000.0,
            poisson_ratio=0.3,
            thickness=scale,
            length=100.0 * scale,
            width=100.0 * scale,
        ) == pytest.approx(reference, rel=1e-12)


@pytest.mark.parametrize("invalid", [0, -1, float("inf"), float("nan"), 1e-200])
def test_rejects_invalid_or_unrepresentable_coefficient(invalid):
    with pytest.raises(ValueError):
        plate_buckling_coefficient(invalid)


@pytest.mark.parametrize("invalid", [0, -1, float("inf"), float("nan")])
def test_rejects_invalid_mode_selection_ratio(invalid):
    with pytest.raises(ValueError, match="aspect_ratio"):
        governing_half_waves(invalid)


@pytest.mark.parametrize("invalid", [0, -1, 1.5, True, "2", 10**400])
def test_rejects_invalid_or_unrepresentable_mode(invalid):
    with pytest.raises(ValueError):
        plate_buckling_coefficient(1.0, half_waves=invalid)


@pytest.mark.parametrize("field", ["length", "width", "thickness", "youngs_modulus"])
@pytest.mark.parametrize("invalid", [0, -1, float("inf"), float("nan")])
def test_rejects_invalid_stress_inputs(field, invalid):
    inputs = dict(
        youngs_modulus=120000.0,
        poisson_ratio=0.3,
        thickness=1.0,
        length=100.0,
        width=100.0,
    )
    inputs[field] = invalid
    with pytest.raises(ValueError, match=field):
        rectangular_plate_elastic_buckling_stress(**inputs)


@pytest.mark.parametrize("invalid", [-1.0, 0.5, float("inf"), float("nan")])
def test_rejects_invalid_poisson_ratio(invalid):
    with pytest.raises(ValueError, match="poisson_ratio"):
        rectangular_plate_elastic_buckling_stress(
            youngs_modulus=120000.0,
            poisson_ratio=invalid,
            thickness=1.0,
            length=100.0,
            width=100.0,
        )


def test_rejects_overflowing_rectangular_stress():
    """Square stress is representable; the short-plate multiplier overflows."""
    with pytest.raises(ValueError, match="floating-point range"):
        rectangular_plate_elastic_buckling_stress(
            youngs_modulus=1e305,
            poisson_ratio=0.0,
            thickness=1.0,
            length=1e-10,
            width=100.0,
        )


def test_csv_reproduces_chart_data(tmp_path):
    root = Path(__file__).resolve().parents[3]
    generated = tmp_path / "coefficients.csv"
    subprocess.run(
        [
            sys.executable,
            str(root / "scripts/studies/generate_classical_plate_buckling.py"),
            "--output",
            str(generated),
        ],
        check=True,
        cwd=root,
    )
    committed = (
        root / "docs/domains/plate-buckling/data/classical-elastic-coefficients.csv"
    )
    assert generated.read_bytes() == committed.read_bytes()
    with generated.open(newline="", encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 198
    ratios = [float(row["aspect_ratio"]) for row in rows]
    assert ratios == sorted(set(ratios))
    for row in rows:
        ratio, mode, coefficient = (
            float(row["aspect_ratio"]), int(row["half_waves"]), float(row["k"])
        )
        assert mode == governing_half_waves(ratio)
        assert coefficient == pytest.approx(
            plate_buckling_coefficient(ratio), rel=1e-14
        )
    by_ratio = {float(row["aspect_ratio"]): float(row["k"]) for row in rows}
    for ratio, expected in [
        (0.5, 6.25), (1.0, 4), (1.41, 892874161 / 198810000), (2, 4)
    ]:
        assert by_ratio[ratio] == pytest.approx(expected, rel=1e-12)
