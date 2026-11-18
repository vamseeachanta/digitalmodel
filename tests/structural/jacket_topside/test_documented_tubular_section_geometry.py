# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Execute the documented annulus example against its independent integrals."""

import json
import math
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest


REPOSITORY = Path(__file__).resolve().parents[3]
DOCUMENT = (
    REPOSITORY
    / "src/digitalmodel/structural/jacket_topside/docs"
    / "worked-tubular-section-geometry.md"
)
# The document's numerical comparison policy explains this rounding allowance.
RELATIVE_TOLERANCE = 1e-12
UNITS = {
    "inner_diameter": "in",
    "cross_sectional_area": "in^2",
    "moment_of_inertia": "in^4",
    "section_modulus": "in^3",
    "radius_of_gyration": "in",
}


def _execute_example(directory: Path, scale: float) -> dict:
    """Run the sole Python fence verbatim, with a temporary working directory."""
    blocks = re.findall(
        r"^```python\n(.*?)^```$", DOCUMENT.read_text(encoding="utf-8"), re.M | re.S
    )
    assert len(blocks) == 1, "The example must contain exactly one Python block"
    directory.mkdir()
    environment = os.environ.copy()
    environment.update(
        PYTHONPATH=str(REPOSITORY / "src"),
        PYTHONDONTWRITEBYTECODE="1",
        TMPDIR=str(directory),
    )
    result = subprocess.run(
        [
            sys.executable,
            "-c",
            blocks[0],
            "--output-dir",
            str(directory / "results"),
            "--scale",
            str(scale),
        ],
        cwd=directory,
        env=environment,
        capture_output=True,
        text=True,
        check=True,
        timeout=30,  # Documented test-harness time budget, in seconds.
    )
    output = directory / "results/tubular-section-geometry.json"
    assert set(directory.rglob("*")) == {output.parent, output}
    properties = json.loads(output.read_text(encoding="utf-8"))
    assert json.loads(result.stdout) == properties
    assert {name: item["unit"] for name, item in properties.items()} == UNITS
    return {name: item["value"] for name, item in properties.items()}


def test_documented_tubular_geometry(tmp_path: Path) -> None:
    """Check every returned property against the worked radial integrals."""
    actual = _execute_example(tmp_path / "original", scale=1.0)
    # Independent hand calculation in the document: R = 6, a = 11/2 inches.
    # Integrating r dr dtheta and r^3 sin(theta)^2 dr dtheta gives
    # A = (23/4) pi and I = (6095/64) pi, without reading package properties.
    # S = I/6 and r_g = sqrt(I/A) = sqrt(265)/4 follow from these integrals.
    expected = {
        "inner_diameter": 11.0,
        "cross_sectional_area": (23.0 / 4.0) * math.pi,
        "moment_of_inertia": (6095.0 / 64.0) * math.pi,
        "section_modulus": (6095.0 / 384.0) * math.pi,
        "radius_of_gyration": math.sqrt(265.0) / 4.0,
    }
    assert actual == pytest.approx(expected, rel=RELATIVE_TOLERANCE, abs=0.0)


def test_documented_tubular_geometry_scaling(tmp_path: Path) -> None:
    """Check the dimensional powers under the assumed uniform length scale."""
    original = _execute_example(tmp_path / "original", scale=1.0)
    scale = 3.0  # Synthetic scale assumption in the document's inputs table.
    scaled = _execute_example(tmp_path / "scaled", scale=scale)
    # Powers follow from the integration measure and definitions in the document.
    powers = {
        "inner_diameter": 1,
        "cross_sectional_area": 2,
        "moment_of_inertia": 4,
        "section_modulus": 3,
        "radius_of_gyration": 1,
    }
    for name, power in powers.items():
        assert scaled[name] == pytest.approx(
            original[name] * scale**power, rel=RELATIVE_TOLERANCE, abs=0.0
        )
