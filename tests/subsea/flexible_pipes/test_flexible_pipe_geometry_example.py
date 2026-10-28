# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise the geometry example against independent hand calculations."""

from dataclasses import replace
import os
from pathlib import Path
import runpy
import subprocess
import sys

import pytest

from digitalmodel.subsea.flexible_pipes.cross_section import FlexiblePipeSection


ROOT = Path(__file__).resolve().parents[3]
EXAMPLE = ROOT / "examples/domains/subsea/flexible_pipe_geometry.py"


def test_example_output_matches_hand_calculation():
    """Compare a fresh interpreter's output with the adjacent note's results."""
    # Source: flexible_pipe_geometry.md, Independent hand calculation.
    # Areas: (0.0011, 0.0028, 0.0036)*pi m^2.
    # Layer masses: (1.1, 5.6, 1.8)*pi kg/m; dry total: 8.5*pi kg/m.
    # Displacement: 800*0.01*pi = 8*pi; contents: 600*0.0025*pi = 1.5*pi.
    # Decimal literals below are those expressions rounded for display,
    # calculated independently of Layer and FlexiblePipeSection.
    expected = """Synthetic full-wall geometry only; design qualification remains open.
Bore diameter: 0.100 m
Outer diameter: 0.200 m
Layer 1 (pressure_sheath) annular area: 0.003455752 m^2
Layer 1 (pressure_sheath) dry mass: 3.455752 kg/m
Layer 2 (anti_wear) annular area: 0.008796459 m^2
Layer 2 (anti_wear) dry mass: 17.592919 kg/m
Layer 3 (outer_sheath) annular area: 0.011309734 m^2
Layer 3 (outer_sheath) dry mass: 5.654867 kg/m
Total dry mass: 26.703538 kg/m
Displaced mass (external fluid density 800 kg/m^3): 25.132741 kg/m
Content mass (bore fluid density 600 kg/m^3): 4.712389 kg/m
"""
    env = os.environ.copy()
    env["PYTHONPATH"] = str(ROOT / "src")
    result = subprocess.run(
        [sys.executable, str(EXAMPLE)],
        cwd=ROOT,
        env=env,
        capture_output=True,
        text=True,
        check=True,
    )
    assert result.stdout == expected
    assert result.stderr == ""


def test_example_section_rejects_layer_gap():
    """Reject the note's synthetic gap using the example's actual layer stack."""
    section = runpy.run_path(str(EXAMPLE))["build_section"]()
    # Source: flexible_pipe_geometry.md, Layer-gap check: 0.122 - 0.120 = 0.002 m.
    layers = list(section.layers)
    layers[1] = replace(layers[1], inner_diameter=0.122)
    with pytest.raises(ValueError, match="layers are not contiguous"):
        FlexiblePipeSection(layers)
