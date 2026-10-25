# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Execute the documented example and check independent static balances."""

import json
import os
from pathlib import Path
import re
import subprocess
import sys
from decimal import Decimal

import pytest


REPO_ROOT = Path(__file__).resolve().parents[3]
DOCUMENT = (
    REPO_ROOT
    / "src/digitalmodel/subsea/capping_stack/docs/worked-static-containment.md"
)


@pytest.fixture(scope="module")
def documented_results(tmp_path_factory):
    """Run the sole Python fence verbatim in a separate temporary directory."""
    blocks = re.findall(r"^```python\n(.*?)^```$", DOCUMENT.read_text(), re.M | re.S)
    assert len(blocks) == 1, "The example must contain exactly one Python block"
    workdir = tmp_path_factory.mktemp("documented-static-containment")
    env = os.environ.copy()
    env["PYTHONPATH"] = str(REPO_ROOT / "src")
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    completed = subprocess.run(
        [sys.executable, "-c", blocks[0]],
        cwd=workdir,
        env=env,
        capture_output=True,
        text=True,
        timeout=30,
        check=True,
    )
    assert completed.stdout == ""
    assert completed.stderr == ""
    assert {path.name for path in workdir.iterdir()} == {"static-containment.json"}
    return json.loads((workdir / "static-containment.json").read_text())


def assert_static_balance(result, reservoir_pressure):
    """Integrate the assumed column with decimal arithmetic, outside the model."""
    # Assumptions: document Inputs, cases A-D. Gravity: NIST standard acceleration
    # of gravity, https://physics.nist.gov/cgi-bin/cuu/Value?gn (exact convention).
    # Constant-density integral: (500 kg/m^3)(2000 m)(9.80665 m/s^2).
    head = Decimal("500") * Decimal("2000") * Decimal("9.80665")
    raw_pressure = Decimal(reservoir_pressure) - head
    pressure = max(raw_pressure, Decimal("0"))
    rating = Decimal("50000000")
    utilisation = pressure / rating
    # Relative tolerance 1e-12 allows binary roundoff, not input uncertainty;
    # at the stated rating it permits 0.00005 Pa. Zero has no absolute allowance.
    assert result["hydrostatic_head"] == pytest.approx(float(head), rel=1e-12, abs=0)
    assert result["siwhp"] == pytest.approx(float(pressure), rel=1e-12, abs=0)
    assert result["direct_siwhp"] == pytest.approx(float(pressure), rel=1e-12, abs=0)
    assert result["utilisation"] == pytest.approx(float(utilisation), rel=1e-12, abs=0)
    assert result["is_contained"] is (pressure <= rating)


def test_documented_static_containment(documented_results):
    """Check head, wellhead pressure, utilisation and disposition from the fence."""
    assert set(documented_results) == {
        "within_rating", "at_rating", "above_rating", "overbalanced"
    }
    assert_static_balance(documented_results["within_rating"], "49806650")


def test_documented_containment_boundaries(documented_results):
    """Preserve inclusive equality, exceedance and exact zero clamping."""
    assert_static_balance(documented_results["at_rating"], "59806650")
    assert_static_balance(documented_results["above_rating"], "64806650")
    assert_static_balance(documented_results["overbalanced"], "5000000")
    # These exact policy checks are deliberately separate from roundoff tolerance.
    assert documented_results["at_rating"]["utilisation"] == 1.0
    assert documented_results["at_rating"]["is_contained"] is True
    assert documented_results["above_rating"]["utilisation"] > 1.0
    assert documented_results["above_rating"]["is_contained"] is False
    assert documented_results["overbalanced"]["siwhp"] == 0.0
    assert documented_results["overbalanced"]["direct_siwhp"] == 0.0
    assert documented_results["overbalanced"]["utilisation"] == 0.0
    assert documented_results["overbalanced"]["is_contained"] is True
