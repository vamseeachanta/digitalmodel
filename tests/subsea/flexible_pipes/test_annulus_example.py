# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the public sheath case cited in the adjacent example README.

Independent hand calculation in polar coordinates, for each full solid annulus:
    A = integral(theta=0..2*pi) integral(r=ID/2..OD/2) r dr dtheta
      = pi * (OD**2 - ID**2) / 4
    m' = integral(A) rho dA = rho * A; sheath subtotal = sum(rho_i * A_i).

Basis: Calculus Volume 3, first edition (2016), sections 5.3 and 5.6.
Inputs: DOI 10.1007/s12204-019-2064-8, section 1, Table 1, page 358,
Fluid barrier and Outer sheath rows; mm are divided by 1000 to obtain m.
No expected value is obtained from the example or cross-section API.
The relative tolerance of 1e-12 allows floating-point roundoff only;
zero absolute tolerance prevents small areas from passing on that basis alone.
"""

import json
import os
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[3]
EXAMPLE = ROOT / "docs/reports/modules/flexible_pipes/examples/annulus.py"
NOT_EVALUATED = [
    "stiffness interaction", "API compliance", "design qualification",
    "complete-pipe dry mass",
]


def _run(cwd, *args):
    env = os.environ.copy()
    env["PYTHONPATH"] = (ROOT / "src").as_posix()
    arguments = [arg.as_posix() if isinstance(arg, Path) else str(arg) for arg in args]
    return subprocess.run(
        [sys.executable, EXAMPLE.as_posix(), *arguments],
        cwd=cwd,
        env=env,
        check=True,
        capture_output=True,
        text=True,
        encoding="utf-8",
    )


def _reject_nonfinite(value):
    raise ValueError(f"Non-finite JSON value: {value}")


def test_annulus_example_rebuilds_hand_values(tmp_path):
    result = _run(tmp_path)
    assert result.stderr == ""
    data = json.loads(result.stdout, parse_constant=_reject_nonfinite)
    # Table 1 hand calculation, SI units, using polar integration above:
    # A1 = pi*(0.0800**2 - 0.0702**2)/4; m1 = 1080*A1.
    # A2 = pi*(0.1115**2 - 0.1105**2)/4; m2 = 800*A2.
    # Selected-sheath subtotal = m1 + m2 (not a complete-pipe mass).
    expected = [
        ("pressure_sheath", 0.001156074680594508, 1.248560655042069),
        ("outer_sheath", 0.0001743583922742335, 0.1394867138193868),
    ]
    assert len(data["layers"]) == len(expected)
    for row, (kind, area, mass) in zip(data["layers"], expected):
        assert set(row) == {
            "layer_type", "annular_area_m2", "dry_mass_per_length_kg_per_m"
        }
        assert row["layer_type"] == kind
        assert row["annular_area_m2"] == pytest.approx(area, rel=1e-12, abs=0.0)
        assert row["dry_mass_per_length_kg_per_m"] == pytest.approx(
            mass, rel=1e-12, abs=0.0
        )
    assert data["dry_mass_per_length_kg_per_m"] == pytest.approx(
        1.388047368861455, rel=1e-12, abs=0.0
    )


def test_annulus_example_labels_selected_sheath_subtotal(tmp_path):
    data = json.loads(_run(tmp_path).stdout, parse_constant=_reject_nonfinite)
    assert set(data) == {
        "layers", "dry_mass_per_length_kg_per_m", "NOT_EVALUATED", "scope"
    }
    assert data["scope"] == "fluid barrier and outer sheath only"
    assert data["NOT_EVALUATED"] == NOT_EVALUATED


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
        f"spec = importlib.util.spec_from_file_location('annulus_example', {EXAMPLE.as_posix()!r}); "
        "module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module)"
    )
    result = subprocess.run(
        [sys.executable, "-c", code],
        cwd=tmp_path,
        check=True,
        capture_output=True,
        text=True,
        encoding="utf-8",
    )
    assert result.stdout == result.stderr == ""
    assert list(tmp_path.iterdir()) == []


def test_annulus_example_writes_only_explicit_output(tmp_path):
    expected = _run(tmp_path).stdout
    output = tmp_path / "annulus.json"
    result = _run(tmp_path, "--output", output)
    assert result.stdout == result.stderr == ""
    assert output.read_text(encoding="utf-8") == expected
    assert output.read_bytes() == expected.encode("utf-8")
    assert list(tmp_path.iterdir()) == [output]
