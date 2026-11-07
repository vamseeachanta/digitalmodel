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
    blocks = re.findall(
        r"^```python\n(.*?)^```$", DOCUMENT.read_text(encoding="utf-8"), re.M | re.S
    )
    assert len(blocks) == 1, "The example must contain exactly one Python block"
    workdir = tmp_path_factory.mktemp("documented-static-containment")
    env = os.environ.copy()
    env["PYTHONPATH"] = (REPO_ROOT / "src").as_posix()
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    completed = subprocess.run(
        [sys.executable, "-c", blocks[0]],
        cwd=workdir,
        env=env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        timeout=30,
        check=True,
    )
    assert completed.stdout == ""
    assert completed.stderr == ""
    assert {path.name for path in workdir.iterdir()} == {"static-containment.json"}
    output = workdir / "static-containment.json"
    assert b"\r" not in output.read_bytes(), "The result must use LF newlines"
    return json.loads(output.read_text(encoding="utf-8"))


def reference_inputs():
    """Convert the cited public inputs independently, without model constants."""
    # AAPG, Development Geology Reference Manual (1992), Wellsite math:
    # Hydrostatic pressure and Mud weight examples: 11.5 ppg, 10000 ft, 6292 psi.
    # OSRL, Capping Stacks, online specification: 15000 psi rating.
    # NIST SP 811 (2008), Appendix B.8: inch, foot, pound and standard gravity;
    # NIST Handbook 133 (2026), Appendix E: US gallon = 231 cubic inches.
    inch = Decimal("0.0254")
    pound = Decimal("0.45359237")
    gravity = Decimal("9.80665")
    psi = pound * gravity / inch**2
    density = Decimal("11.5") * pound / (Decimal("231") * inch**3)
    depth = Decimal("10000") * Decimal("0.3048")
    head = density * gravity * depth
    return head, Decimal("15000") * psi, Decimal("6292") * psi


def assert_static_balance(result, reservoir_pressure, expected_contained):
    """Integrate the cited column with decimal arithmetic, outside the model."""
    head, rating, _ = reference_inputs()
    raw_pressure = reservoir_pressure - head
    pressure = max(raw_pressure, Decimal("0"))
    utilisation = pressure / rating
    # Relative tolerance 1e-12 allows binary roundoff, not input uncertainty;
    # Zero has no absolute allowance; this does not relax the inclusive decision.
    assert result["hydrostatic_head"] == pytest.approx(float(head), rel=1e-12, abs=0)
    assert result["siwhp"] == pytest.approx(float(pressure), rel=1e-12, abs=0)
    assert result["direct_siwhp"] == pytest.approx(float(pressure), rel=1e-12, abs=0)
    assert result["utilisation"] == pytest.approx(float(utilisation), rel=1e-12, abs=0)
    assert result["is_contained"] is expected_contained
    assert result["rated_working_pressure"] == pytest.approx(float(rating), rel=1e-12)
    assert result["reservoir_pressure"] == pytest.approx(float(reservoir_pressure), rel=1e-12)


def test_documented_static_containment(documented_results):
    """Check head, wellhead pressure, utilisation and disposition from the fence."""
    assert set(documented_results) == {
        "within_rating", "at_rating", "above_rating", "overbalanced"
    }
    _, _, reservoir = reference_inputs()
    result = documented_results["within_rating"]
    assert_static_balance(result, reservoir, True)
    # Hand calculation: (11.5 * 0.45359237 / (231 * 0.0254**3))
    # * 9.80665 * (10000 * 0.3048) = 41189459.154 Pa.
    assert result["hydrostatic_head"] == pytest.approx(41_189_459.154, abs=0.001, rel=0)
    # 6292 * (0.45359237 * 9.80665 / 0.0254**2) - head = 2192353.735 Pa.
    assert result["siwhp"] == pytest.approx(2_192_353.735, abs=0.001, rel=0)
    # R = 15000 * (0.45359237 * 9.80665 / 0.0254**2); SIWHP/R = 0.0211982684.
    assert result["utilisation"] == pytest.approx(0.0211982684, abs=1e-10, rel=0)


def test_documented_containment_boundaries(documented_results):
    """Preserve inclusive equality, exceedance and exact zero clamping."""
    head, rating, _ = reference_inputs()
    # Derived arithmetic cases: p_res = H + R, H + 1.1R, and H/2 respectively.
    assert_static_balance(documented_results["at_rating"], head + rating, True)
    assert_static_balance(
        documented_results["above_rating"], head + Decimal("1.1") * rating, False
    )
    assert_static_balance(
        documented_results["overbalanced"], head / Decimal("2"), True
    )
    # These exact policy checks are deliberately separate from roundoff tolerance.
    assert documented_results["at_rating"]["utilisation"] == 1.0
    assert (
        documented_results["at_rating"]["siwhp"]
        == documented_results["at_rating"]["rated_working_pressure"]
    )
    assert documented_results["at_rating"]["is_contained"] is True
    assert documented_results["above_rating"]["utilisation"] > 1.0
    assert documented_results["above_rating"]["is_contained"] is False
    assert documented_results["overbalanced"]["siwhp"] == 0.0
    assert documented_results["overbalanced"]["direct_siwhp"] == 0.0
    assert documented_results["overbalanced"]["utilisation"] == 0.0
    assert documented_results["overbalanced"]["is_contained"] is True
