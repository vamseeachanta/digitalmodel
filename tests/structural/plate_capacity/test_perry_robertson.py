# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent first-yield column checks; derivation is recorded in the module."""

import csv
from math import pi, sqrt
from pathlib import Path
import subprocess
import sys

import pytest

from digitalmodel.structural.plate_capacity.perry_robertson import (
    perry_robertson_strength_ratio,
    perry_robertson_stress,
)


def test_hand_worked_first_yield_case():
    """Synthetic pinned strut: E=200000 MPa, fy=250 MPa, r=10 mm.

    Choose L/r=pi*sqrt(E/fy), so Euler stress equals fy. With e0=2 mm
    and c=25 mm, eta=e0*c/r^2=0.5. At mean stress 125 MPa, P/PE=0.5;
    total bow=2/(1-0.5)=4 mm. Bending stress=125*4*25/100=125 MPa.
    Maximum stress=125+125=250 MPa: first yield, q=0.5.
    This is a hand equilibrium calculation, independent of the quadratic root.
    """
    assert perry_robertson_strength_ratio(
        reduced_slenderness=1.0, imperfection_parameter=0.5
    ) == pytest.approx(0.5, rel=1e-14)
    assert perry_robertson_stress(
        youngs_modulus=200000.0,
        yield_strength=250.0,
        slenderness_ratio=pi * sqrt(800.0),
        imperfection_parameter=0.5,
    ) == pytest.approx(125.0, rel=1e-14)


@pytest.mark.parametrize("slenderness", [1e4, 1e6, 1e8])
def test_euler_limit_at_high_slenderness(slenderness):
    """At fixed material and c/r, fixed bow/length gives eta proportional to L/r.

    Here eta=0.002*lambda_bar; eta/lambda_bar^2 tends to zero.
    Euler stress divided by fy is 1/lambda_bar^2. The independent
    asymptotic relative deficit is bounded by 0.003/lambda_bar here.
    """
    ratio = perry_robertson_strength_ratio(
        reduced_slenderness=slenderness,
        imperfection_parameter=0.002 * slenderness,
    )
    assert 0 < 1 - ratio * slenderness**2 < 0.003 / slenderness


@pytest.mark.parametrize("slenderness", [1e-4, 1e-6, 1e-8])
def test_squash_limit_as_length_and_initial_bow_vanish(slenderness):
    """Eta proportional to length vanishes: q approaches the squash ratio 1."""
    ratio = perry_robertson_strength_ratio(
        reduced_slenderness=slenderness,
        imperfection_parameter=0.2 * slenderness,
    )
    assert 0 < 1 - ratio < 0.21 * slenderness


@pytest.mark.parametrize(
    ("slenderness", "expected"), [(0.0, 1.0), (0.5, 1.0), (1.0, 1.0), (2.0, 0.25)]
)
def test_perfect_column_is_limited_by_squash_or_euler(slenderness, expected):
    """With no initial bow, first yield or Euler bifurcation governs."""
    assert perry_robertson_strength_ratio(
        reduced_slenderness=slenderness, imperfection_parameter=0.0
    ) == expected


def test_fixed_nonzero_imperfection_has_a_reduced_short_column_limit():
    """At lambda_bar=0, q*(1+eta)=1; fixed eta does not vanish with length."""
    assert perry_robertson_strength_ratio(
        reduced_slenderness=0.0, imperfection_parameter=0.5
    ) == pytest.approx(2 / 3)


def test_large_slenderness_does_not_cancel_the_smaller_root():
    """For lambda_bar=1e8 and eta=0.5, Euler ratio is 1e-16."""
    assert perry_robertson_strength_ratio(
        reduced_slenderness=1e8, imperfection_parameter=0.5
    ) == pytest.approx(1e-16, rel=1e-14, abs=0.0)


def test_strength_falls_with_initial_bow_and_stays_below_both_limits():
    """Increasing bow raises bending stress at the same axial force."""
    ratios = [
        perry_robertson_strength_ratio(
            reduced_slenderness=1.5, imperfection_parameter=eta
        )
        for eta in (0.0, 0.1, 0.5, 1.0)
    ]
    assert 0 < ratios[-1] < ratios[-2] < ratios[-3] < ratios[0] <= 1 / 1.5**2


@pytest.mark.parametrize("field", ["reduced_slenderness", "imperfection_parameter"])
@pytest.mark.parametrize("invalid", [-1.0, float("inf"), float("nan")])
def test_rejects_invalid_dimensionless_inputs(field, invalid):
    """Negative or nonfinite slenderness and imperfection are outside the model."""
    inputs = dict(reduced_slenderness=1.0, imperfection_parameter=0.2)
    inputs[field] = invalid
    with pytest.raises(ValueError, match=field):
        perry_robertson_strength_ratio(**inputs)


@pytest.mark.parametrize("field", ["youngs_modulus", "yield_strength"])
@pytest.mark.parametrize("invalid", [0.0, -1.0, float("inf"), float("nan")])
def test_rejects_invalid_material_inputs(field, invalid):
    """Elastic modulus and compressive yield stress must be finite and positive."""
    inputs = dict(
        youngs_modulus=200000.0,
        yield_strength=250.0,
        slenderness_ratio=100.0,
        imperfection_parameter=0.2,
    )
    inputs[field] = invalid
    with pytest.raises(ValueError, match=field):
        perry_robertson_stress(**inputs)


@pytest.mark.parametrize("invalid", [-1.0, float("inf"), float("nan")])
def test_rejects_invalid_geometric_slenderness(invalid):
    """L/r is nonnegative, with zero permitted only as a limiting value."""
    with pytest.raises(ValueError, match="slenderness_ratio"):
        perry_robertson_stress(
            youngs_modulus=200000.0,
            yield_strength=250.0,
            slenderness_ratio=invalid,
            imperfection_parameter=0.2,
        )


def test_rejects_unrepresentable_slenderness_squared():
    """An intermediate outside the floating-point range raises a clear error."""
    with pytest.raises(ValueError, match="floating-point range"):
        perry_robertson_strength_ratio(
            reduced_slenderness=1e200, imperfection_parameter=0.2
        )


def test_csv_is_reproducible_and_satisfies_independent_equilibrium(tmp_path):
    """Every CSV row satisfies first-yield equilibrium, without calling the API."""
    root = Path(__file__).resolve().parents[3]
    output = tmp_path / "curve.csv"
    subprocess.run(
        [sys.executable, str(root / "scripts/studies/perry_robertson_columns.py"),
         "--output", str(output)],
        check=True,
        cwd=root,
    )
    committed = root / "examples/workflows/perry-robertson/strength_curve.csv"
    assert output.read_bytes() == committed.read_bytes()
    rows = list(csv.DictReader(output.read_text().splitlines()))
    assert len(rows) == 61
    assert [float(row["slenderness_ratio"]) for row in rows] == list(range(0, 301, 5))
    for row in rows:
        slenderness = float(row["slenderness_ratio"])
        eta = float(row["imperfection_parameter"])
        ratio = float(row["strength_ratio"])
        assert eta == pytest.approx(0.001 * 2.0 * slenderness, abs=1e-12)
        assert float(row["critical_stress_MPa"]) == pytest.approx(250.0 * ratio)
        if slenderness == 0:
            assert ratio == 1.0
            continue
        reduced_squared = 250.0 / 200000.0 * (slenderness / pi)**2
        assert float(row["reduced_slenderness"])**2 == pytest.approx(reduced_squared)
        # sigma_max/fy = q + q*eta/(1 - sigma/sigma_E) = 1.
        assert ratio + ratio * eta / (1 - ratio * reduced_squared) == pytest.approx(
            1.0, abs=2e-9
        )
