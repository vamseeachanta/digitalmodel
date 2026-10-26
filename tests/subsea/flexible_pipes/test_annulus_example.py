# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the synthetic case defined in the adjacent example README.

Independent hand calculation in polar coordinates, for each full solid annulus:
    A = integral(theta=0..2*pi) integral(r=ID/2..OD/2) r dr dtheta
      = pi * (OD**2 - ID**2) / 4
    m' = integral(A) rho dA = rho * A; section m' = sum(rho_i * A_i).

The README declares (ID, OD, rho) in (m, m, kg/m^3):
pressure sheath (0.200, 0.210, 950), outer sheath (0.210, 0.230, 1200).
No expected value is obtained from the example or cross-section API.
The relative tolerance of 1e-12 allows floating-point roundoff only;
zero absolute tolerance prevents small areas from passing on that basis alone.
"""

import json
import math
import os
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[3]
EXAMPLE = ROOT / "docs/reports/modules/flexible_pipes/examples/annulus.py"
NOT_EVALUATED = ["stiffness interaction", "API compliance", "design qualification"]


def _run(cwd, *args):
    env = os.environ.copy()
    env["PYTHONPATH"] = str(ROOT / "src")
    return subprocess.run(
        [sys.executable, str(EXAMPLE), *map(str, args)],
        cwd=cwd,
        env=env,
        check=True,
        capture_output=True,
        text=True,
    )


def _reject_nonfinite(value):
    raise ValueError(f"Non-finite JSON value: {value}")


def test_annulus_example_rebuilds_hand_values(tmp_path):
    result = _run(tmp_path)
    assert result.stderr == ""
    data = json.loads(result.stdout, parse_constant=_reject_nonfinite)
    assert set(data) == {"layers", "dry_mass_per_length_kg_per_m", "NOT_EVALUATED"}
    assert data["NOT_EVALUATED"] == NOT_EVALUATED
    # Independent bounds and densities: synthetic inputs in the README.
    inputs = [
        ("pressure_sheath", 0.200, 0.210, 950.0),
        ("outer_sheath", 0.210, 0.230, 1200.0),
    ]
    assert len(data["layers"]) == len(inputs)
    total_mass = 0.0  # Empty sum of the hand-calculated layer masses.
    for row, (kind, inner, outer, density) in zip(data["layers"], inputs):
        assert set(row) == {
            "layer_type", "annular_area_m2", "dry_mass_per_length_kg_per_m"
        }
        assert row["layer_type"] == kind
        area = math.pi * (outer**2 - inner**2) / 4.0
        mass = density * area
        assert row["annular_area_m2"] == pytest.approx(area, rel=1e-12, abs=0.0)
        assert row["dry_mass_per_length_kg_per_m"] == pytest.approx(
            mass, rel=1e-12, abs=0.0
        )
        total_mass += mass
    assert data["dry_mass_per_length_kg_per_m"] == pytest.approx(
        total_mass, rel=1e-12, abs=0.0
    )


def test_annulus_example_is_repeatable_without_implicit_output(tmp_path):
    first = _run(tmp_path)
    second = _run(tmp_path)
    assert first.stdout == second.stdout
    assert first.stderr == second.stderr == ""
    assert list(tmp_path.iterdir()) == []


def test_annulus_example_import_is_inert(tmp_path):
    # Execute a normal import with no command arguments or main-guard execution.
    code = (
        "import importlib.util, sys; sys.argv = []; "
        f"spec = importlib.util.spec_from_file_location('annulus_example', {str(EXAMPLE)!r}); "
        "module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module)"
    )
    result = subprocess.run(
        [sys.executable, "-c", code],
        cwd=tmp_path,
        check=True,
        capture_output=True,
        text=True,
    )
    assert result.stdout == result.stderr == ""
    assert list(tmp_path.iterdir()) == []


def test_annulus_example_writes_only_explicit_output(tmp_path):
    expected = _run(tmp_path).stdout
    output = tmp_path / "annulus.json"
    result = _run(tmp_path, "--output", output)
    assert result.stdout == result.stderr == ""
    assert output.read_text(encoding="utf-8") == expected
    assert list(tmp_path.iterdir()) == [output]
