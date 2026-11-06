# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Execute the documented synthetic example against independent SI arithmetic."""

import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys

import pytest


REPO_ROOT = Path(__file__).resolve().parents[3]
DOCUMENT = (
    REPO_ROOT
    / "src/digitalmodel/structural/parachute/docs/worked-synthetic-canopy-drag.md"
)

# NIST SP 811, Appendix B.8 (foot and mile), footnote 23 (exact lbf),
# Chapter 5, Table 6 (hour); links and dimensional derivation are in DOCUMENT.
METRES_PER_FOOT = 0.3048
METRES_PER_MILE = 1609.344
SECONDS_PER_HOUR = 3600.0
NEWTONS_PER_LBF = 4.4482216152605

# Stated synthetic assumptions in DOCUMENT, independent of the executed block.
DIAMETER_FT = 5.0
DENSITY_SLUG_FT3 = 0.0023
DRAG_COEFFICIENT = 1.1
OPENING_MULTIPLIER = 1.25
BASE_SPEED_MPH = 40.0
CHANGED_SPEED_MPH = 60.0

# The production N/lbf factor is rounded: its relative error is below 5e-7.
# The lbf calculation and the speed ratio only incur floating-point roundoff;
# 1e-12 allows that error without accepting rounded engineering results.
CONVERSION_REL_TOL = 5e-7
ARITHMETIC_REL_TOL = 1e-12


@pytest.fixture(scope="module")
def documented_results(tmp_path_factory):
    """Run the sole Python fence verbatim in a separate process and directory."""
    blocks = re.findall(r"^```python\n(.*?)^```\s*$", DOCUMENT.read_text(), re.M | re.S)
    assert len(blocks) == 1, "The example must contain exactly one Python block"
    output_dir = tmp_path_factory.mktemp("documented-canopy")
    env = os.environ.copy()
    env["PYTHONPATH"] = str(REPO_ROOT / "src")
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    subprocess.run(
        [sys.executable, "-B", "-c", blocks[0], str(output_dir)],
        cwd=output_dir,
        env=env,
        check=True,
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert {path.name for path in output_dir.iterdir()} == {"results.json"}
    return json.loads((output_dir / "results.json").read_text())


def independent_force_n(speed_mph):
    """Apply the cited drag law in SI without calling production helpers."""
    # 1 slug = 1 lbf s^2/ft by F = ma. Convert density before using the SI law.
    density_kg_m3 = DENSITY_SLUG_FT3 * NEWTONS_PER_LBF / METRES_PER_FOOT**4
    radius_m = DIAMETER_FT * METRES_PER_FOOT / 2
    speed_m_s = speed_mph * METRES_PER_MILE / SECONDS_PER_HOUR
    # Circle area pi*r^2 and dynamic pressure rho*V^2/2 (NASA reference).
    return (
        density_kg_m3
        * speed_m_s**2
        / 2
        * math.pi
        * radius_m**2
        * DRAG_COEFFICIENT
        * OPENING_MULTIPLIER
    )


def test_documented_canopy_drag(documented_results):
    """Check both returned force units against the independent SI derivation."""
    assert len(documented_results) == 2
    for row, speed in zip(
        documented_results, (BASE_SPEED_MPH, CHANGED_SPEED_MPH), strict=True
    ):
        assert row["speed_mph"] == speed
        expected_n = independent_force_n(speed)
        assert row["force_lbs"] == pytest.approx(
            expected_n / NEWTONS_PER_LBF, rel=ARITHMETIC_REL_TOL, abs=0.0
        )
        assert row["force_n"] == pytest.approx(
            expected_n, rel=CONVERSION_REL_TOL, abs=0.0
        )
        assert row["independent_force_n"] == pytest.approx(
            expected_n, rel=ARITHMETIC_REL_TOL, abs=0.0
        )


def test_documented_canopy_speed_change(documented_results):
    """Check quadratic speed scaling with the other stated inputs fixed."""
    base, changed = documented_results
    expected_ratio = (CHANGED_SPEED_MPH / BASE_SPEED_MPH) ** 2
    for unit in ("force_lbs", "force_n"):
        assert changed[unit] / base[unit] == pytest.approx(
            expected_ratio, rel=ARITHMETIC_REL_TOL, abs=0.0
        )
