# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Execute the documented synthetic section and sling example verbatim."""

import json
import math
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest


REPO_ROOT = Path(__file__).resolve().parents[3]
DOCUMENT = (
    REPO_ROOT
    / "src/digitalmodel/structural/offshore_container/docs"
    / "worked-shs-sling-geometry.md"
)
# The inputs and hand values in derivations D1 and D2 are exact decimals.
# Relative tolerance 1e-12 permits floating-point arithmetic roundoff while
# remaining sensitive to a unit conversion or section-formula error. The angle
# uses an absolute tolerance of 1e-12 radians (and 1e-12 degrees when converted).
REL_TOL = 1e-12
ANGLE_ABS_TOL = 1e-12
HAND_SECTION = {"area_m2": 3.0e-2, "inertia_m4": 1.25e-4, "modulus_m3": 1.25e-3}


@pytest.fixture(scope="module")
def documented_results(tmp_path_factory):
    """Run the sole Python fence from an isolated temporary working directory."""
    blocks = re.findall(r"^```python\n(.*?)^```$", DOCUMENT.read_text(encoding="utf-8"), re.M | re.S)
    assert len(blocks) == 1, "The example must contain exactly one Python block"
    workdir = tmp_path_factory.mktemp("documented-shs")
    script = workdir / "example.py"
    script.write_text(blocks[0], encoding="utf-8")
    env = os.environ.copy()
    env["PYTHONPATH"] = str(REPO_ROOT / "src")
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    completed = subprocess.run(
        [sys.executable, "-B", str(script)],
        cwd=workdir,
        env=env,
        capture_output=True,
        text=True,
        check=True,
        timeout=30,
    )
    assert completed.stderr == ""
    assert set(workdir.iterdir()) == {script}, "The example requires no output files"
    return json.loads(completed.stdout)


def test_documented_shs_sling_geometry(documented_results):
    """Check the section hand integration and equal-leg right triangle."""
    for route in ("metres", "millimetres"):
        for quantity, expected in HAND_SECTION.items():
            assert documented_results[route][quantity] == pytest.approx(
                expected, rel=REL_TOL, abs=0.0
            )
    # D2: half spans 3 m and 4 m give a 5 m radius; height is also 5 m.
    # The isosceles right triangle has angle pi/4 rad = 45 degrees.
    assert documented_results["angle_rad"] == pytest.approx(
        math.pi / 4, rel=0.0, abs=ANGLE_ABS_TOL
    )
    assert documented_results["angle_deg"] == pytest.approx(
        45.0, rel=0.0, abs=ANGLE_ABS_TOL
    )


def test_documented_shs_unit_equivalence(documented_results):
    """Check both construction routes against SI inputs and hand properties."""
    expected = {"b_m": 0.2, "t_m": 0.05, **HAND_SECTION}  # assumptions A1; D1
    assert set(documented_results["metres"]) == set(expected)
    assert set(documented_results["millimetres"]) == set(expected)
    for quantity, value in expected.items():
        metres = documented_results["metres"][quantity]
        millimetres = documented_results["millimetres"][quantity]
        assert metres == pytest.approx(value, rel=REL_TOL, abs=0.0)
        assert millimetres == pytest.approx(value, rel=REL_TOL, abs=0.0)
        assert millimetres == pytest.approx(metres, rel=REL_TOL, abs=0.0)
