# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Execute the documented block against independent equilibrium and invariants."""

import json
import math
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest


# Numerical comparison budget: a few floating-point operations on exact decimal
# inputs. These bounds cover binary rounding, not thin-wall modelling error.
REL_TOL = 1e-12
ABS_TOL_PSI = 1e-8


@pytest.fixture(scope="module")
def documented_results(tmp_path_factory):
    """Run the sole Python fence verbatim, outside the source checkout."""
    root = Path(__file__).resolve().parents[3]
    document = (
        root
        / "src/digitalmodel/well/tubulars/docs"
        / "worked-tubular-stress-invariants.md"
    )
    blocks = re.findall(
        r"^```python\n(.*?)^```$", document.read_text(encoding="utf-8"), re.M | re.S
    )
    assert len(blocks) == 1, "The worked example must contain one Python block"
    directory = tmp_path_factory.mktemp("documented-tubular")
    environment = dict(os.environ)
    environment.update(
        PYTHONPATH=str(root / "src"),
        PYTHONDONTWRITEBYTECODE="1",
        TMPDIR=str(directory),
    )
    completed = subprocess.run(
        [sys.executable, "-c", blocks[0]],
        cwd=directory,
        env=environment,
        capture_output=True,
        text=True,
        check=True,
        timeout=30,
    )
    assert completed.stderr == ""
    assert list(directory.iterdir()) == [], "The example must not create files"
    results = json.loads(completed.stdout)
    assert results["stress_unit"] == "psi"
    assert set(results["cases"]) == {"internal", "reversed", "pure_shear"}
    return results["cases"]


def equivalent_from_principals(first, second, third=0.0):
    """Use principal differences, as derived in the document's invariant section."""
    return math.sqrt(
        ((first - second) ** 2 + (second - third) ** 2 + (third - first) ** 2)
        / 2.0
    )


def test_documented_tubular_stresses(documented_results):
    """Check half-cylinder equilibrium and the independently derived eigenvalues."""
    case = documented_results["internal"]
    assert case["differential_pressure_psi"] == 400.0
    assert case["axial_psi"] == 6000.0
    assert case["shear_psi"] == 2000.0
    # Inputs from the document's assumption table: D = 10 in, t = 0.2 in,
    # L = 1 in. Pressure resultant = 400 * 10 * 1 = 4000 lbf; two cut walls.
    expected_hoop = 4000.0 / 0.4
    assert case["hoop_psi"] == pytest.approx(
        expected_hoop, rel=REL_TOL, abs=ABS_TOL_PSI
    )
    assert 0.4 * case["hoop_psi"] == pytest.approx(4000.0, rel=REL_TOL)
    # Hand calculation: stress matrix [[6000, 2000], [2000, 10000]] psi
    # has eigenvalues 8000 +/- sqrt(8,000,000) psi, plus zero radial stress.
    radius = math.sqrt(8_000_000.0)
    expected = equivalent_from_principals(8000.0 + radius, 8000.0 - radius)
    assert case["vme_psi"] == pytest.approx(
        expected, rel=REL_TOL, abs=ABS_TOL_PSI
    )


def test_documented_pure_shear(documented_results):
    """Pure shear has principals +tau, -tau, zero and equivalent sqrt(3)*|tau|."""
    case = documented_results["pure_shear"]
    assert case["differential_pressure_psi"] == 0.0
    assert case["hoop_psi"] == 0.0
    assert case["axial_psi"] == 0.0
    assert case["shear_psi"] == 1500.0
    # The invariant gives ((2*tau)^2 + tau^2 + tau^2)/2 = 3*tau^2;
    # the coefficient is derived, not a material or conversion constant.
    expected = math.sqrt(3.0) * 1500.0
    assert case["vme_psi"] == pytest.approx(
        expected, rel=REL_TOL, abs=ABS_TOL_PSI
    )


def test_documented_reversed_pressure(documented_results):
    """Reverse pressure only: compressive hoop and a changed stress invariant."""
    case = documented_results["reversed"]
    assert case["differential_pressure_psi"] == -400.0
    assert case["axial_psi"] == 6000.0
    assert case["shear_psi"] == 2000.0
    assert case["hoop_psi"] == pytest.approx(
        -4000.0 / 0.4, rel=REL_TOL, abs=ABS_TOL_PSI
    )
    # [[6000, 2000], [2000, -10000]] has eigenvalues
    # -2000 +/- sqrt(68,000,000) psi. Axial and shear loads are unchanged.
    radius = math.sqrt(68_000_000.0)
    expected = equivalent_from_principals(-2000.0 + radius, -2000.0 - radius)
    assert case["vme_psi"] == pytest.approx(
        expected, rel=REL_TOL, abs=ABS_TOL_PSI
    )
    assert case["vme_psi"] > documented_results["internal"]["vme_psi"]
