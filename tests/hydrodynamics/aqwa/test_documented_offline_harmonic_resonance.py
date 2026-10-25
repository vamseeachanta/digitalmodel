# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Execute the documented public oscillator with independent expectations.

Basis: Calculus Volume 3 (2016), section 7.3, Example 7.20, and the
document's derived grids and numerical error budget. No expected result uses
the helper.
"""

import inspect
import json
import math
from pathlib import Path
import re
import subprocess
import sys

import pytest
from scipy.optimize import brentq


ROOT = Path(__file__).resolve().parents[3]
DOCUMENT = (
    ROOT / "src/digitalmodel/hydrodynamics/aqwa/docs"
    / "worked-offline-harmonic-resonance.md"
)


@pytest.fixture
def documented_block():
    """Keep the document as the sole executable example source."""
    blocks = re.findall(
        r"^```python\n(.*?)^```$",
        DOCUMENT.read_text(encoding="utf-8"),
        flags=re.MULTILINE | re.DOTALL,
    )
    assert len(blocks) == 1, "The example must contain one complete Python block"
    return blocks[0]


def run_documented_block(block, temporary_directory, *arguments):
    """Use an isolated interpreter and a temporary working/output directory."""
    return subprocess.run(
        [
            sys.executable, "-I", "-B", "-c", block,
            ROOT.as_posix(), (temporary_directory / "results").as_posix(),
            *arguments,
        ],
        cwd=temporary_directory,
        capture_output=True,
        text=True,
        encoding="utf-8",
        timeout=60,
        check=False,
    )


def test_documented_harmonic_resonance(documented_block, tmp_path):
    completed = run_documented_block(documented_block, tmp_path)
    assert completed.returncode == 0, completed.stderr
    report = json.loads(
        (tmp_path / "results" / "resonance.json").read_text(encoding="utf-8")
    )
    actual = report["actual"]

    # Calculus Volume 3 (2016), section 7.3, Example 7.20:
    # M=1 kg, K=49 N/m, B=14 N s/m; no separate added mass.
    # Hand calculation: omega=sqrt(49/1)=7; Bcrit=2*1*7=14;
    # T=2*pi/7; zeta=14/14=1. These literals do not use helper outputs.
    natural = 7.0
    expected = {
        "effective_mass_kg": 1.0,
        "natural_frequency_rad_s": 7.0,
        "natural_period_s": 2.0 * math.pi / 7.0,
        "critical_damping_ns_m": 14.0,
        "damping_at_natural_frequency": 14.0,
        "zeta_at_natural_frequency": 1.0,
    }
    # Derived error budget: root error plus reciprocal propagation and rounding.
    for name, value in expected.items():
        assert actual[name] == pytest.approx(value, rel=1e-12, abs=0.0)
    assert actual["critical_damping_ratio"] == pytest.approx(
        [1.0, 1.0, 1.0, 1.0], rel=1e-12, abs=0.0
    )
    # The analytic root is strictly between samples, so interpolation is used.
    assert report["omega_rad_s"] == [3.5, 5.25, 8.75, 10.5]
    assert 5.25 < natural < 8.75
    assert natural not in report["omega_rad_s"]
    assert sorted(path.name for path in tmp_path.iterdir()) == ["results"]
    assert sorted(path.name for path in (tmp_path / "results").iterdir()) == [
        "resonance.json"
    ]


def test_documented_resonance_outside_grid_refused(documented_block, tmp_path):
    # Derived exclusion grid: [5/4, 3/2, 2] * 7 rad/s exceeds the root.
    completed = run_documented_block(
        documented_block, tmp_path, "--omega", "8.75", "10.5", "14.0"
    )
    assert completed.returncode != 0
    assert (
        "ValueError: heave: expected one resonance inside frequency grid; found 0"
        in completed.stderr
    )
    assert list(tmp_path.iterdir()) == []


def test_documented_numerical_error_budget(documented_block, tmp_path):
    """Check the declared criterion against the installed solver contract."""
    completed = run_documented_block(documented_block, tmp_path)
    assert completed.returncode == 0, completed.stderr
    report = json.loads(
        (tmp_path / "results" / "resonance.json").read_text(encoding="utf-8")
    )
    comparison = report["comparison"]
    # SciPy brentq: |error| <= xtol + rtol*|root|; root is exactly 7 rad/s.
    parameters = inspect.signature(brentq).parameters
    root_error = parameters["xtol"].default / 7.0 + parameters["rtol"].default
    # Reciprocal propagation is rho/(1-rho); allow 32 rounding errors.
    budget = root_error / (1.0 - root_error) + 32.0 * math.ulp(1.0)
    tolerance = comparison["relative_tolerance"]
    assert math.isfinite(tolerance)
    assert 0.0 < budget <= tolerance <= 1e-12
    assert comparison["absolute_tolerance"] == 0.0
