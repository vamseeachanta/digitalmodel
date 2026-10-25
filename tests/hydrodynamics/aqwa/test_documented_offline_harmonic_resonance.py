# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Execute the documented oscillator; scalar expectations follow E4-E5.

The example's Inputs and derivation sections supply every assumed coefficient,
grid and numerical tolerance used below. No expected result uses the helper.
"""

import json
import math
from pathlib import Path
import re
import subprocess
import sys

import pytest


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
            str(ROOT), str(temporary_directory / "results"), *arguments,
        ],
        cwd=temporary_directory,
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )


def test_documented_harmonic_resonance(documented_block, tmp_path):
    completed = run_documented_block(documented_block, tmp_path)
    assert completed.returncode == 0, completed.stderr
    report = json.loads((tmp_path / "results" / "resonance.json").read_text())
    actual = report["actual"]

    # Independent hand calculation: M=3+2=5 kg, K=20 N/m, B=4 N s/m.
    # E4: omega=sqrt(K/M); E5: Bcrit=2*M*omega, T=2*pi/omega.
    natural = math.sqrt(20.0 / 5.0)
    critical = 2.0 * 5.0 * natural
    zeta = 4.0 / critical
    expected = {
        "effective_mass_kg": 5.0,
        "natural_frequency_rad_s": natural,
        "natural_period_s": 2.0 * math.pi / natural,
        "critical_damping_ns_m": critical,
        "damping_at_natural_frequency": 4.0,
        "zeta_at_natural_frequency": zeta,
    }
    # Chosen acceptance assumptions in the document, not empirical accuracy.
    for name, value in expected.items():
        assert actual[name] == pytest.approx(value, rel=1e-9, abs=1e-12)
    assert actual["critical_damping_ratio"] == pytest.approx(
        [zeta, zeta, zeta, zeta], rel=1e-9, abs=1e-12
    )
    # The analytic root is strictly between samples, so interpolation is used.
    assert report["omega_rad_s"] == [1.0, 1.5, 2.5, 3.0]
    assert 1.5 < natural < 2.5
    assert natural not in report["omega_rad_s"]
    assert sorted(path.name for path in tmp_path.iterdir()) == ["results"]
    assert sorted(path.name for path in (tmp_path / "results").iterdir()) == [
        "resonance.json"
    ]


def test_documented_resonance_outside_grid_refused(documented_block, tmp_path):
    # Assumed exclusion grid in the document: every sample exceeds 2 rad/s.
    completed = run_documented_block(
        documented_block, tmp_path, "--omega", "2.5", "3.0", "4.0"
    )
    assert completed.returncode != 0
    assert (
        "ValueError: heave: expected one resonance inside frequency grid; found 0"
        in completed.stderr
    )
    assert list(tmp_path.iterdir()) == []
