# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent bookkeeping checks; no measured resistance reference is used."""

import json
import os
import runpy
import subprocess
import sys
from decimal import Decimal
from pathlib import Path

import pytest

from digitalmodel.naval_architecture import holtrop_mennen as hm

ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = ROOT / "examples/naval_architecture/holtrop_power.py"
COMPONENTS = {
    "viscous_n",
    "appendage_n",
    "wave_n",
    "bulb_n",
    "transom_n",
    "correlation_n",
}


def run_example(cwd, *args):
    """Execute the saved example with an explicit source import path."""
    return subprocess.run(
        [sys.executable, "-B", str(EXAMPLE), *map(str, args)],
        cwd=cwd,
        env={**os.environ, "PYTHONPATH": str(ROOT / "src")},
        capture_output=True,
        text=True,
        check=False,
    )


def test_holtrop_example_power_balance(tmp_path):
    """Sum printed forces independently and hand-multiply by synthetic V=5 m/s."""
    output = tmp_path / "selected" / "results"
    result = run_example(tmp_path, "--output-dir", output)
    assert result.returncode == 0, result.stderr
    saved = output / "holtrop_power.json"
    data = json.loads(saved.read_text(), parse_float=Decimal)
    assert set(tmp_path.rglob("*")) == {output.parent, output, saved}
    assert data["synthetic"] is True
    assert set(data["components_n"]) == COMPONENTS
    assert all(value > 0 for value in data["components_n"].values())
    # Decimal addition/multiplication is independent of the package helpers.
    force_sum = sum(data["components_n"].values())
    speed = Decimal("5")  # Synthetic input table in the usage page.
    assert data["inputs"]["speed_ms"] == speed
    assert data["total_resistance_n"] == pytest.approx(force_sum)
    assert data["effective_power_w"] == pytest.approx(force_sum * speed)


def test_holtrop_example_component_accounting(monkeypatch):
    """Use invented forces with a hand sum, independent of regression equations.

    Synthetic stubs: S=2 m^2, (1+k1)=1.2, RF=100 N, RAPP=10 N,
    RW=20 N, RB=30 N, RTR=40 N, CA=0.01; V=5 m/s from the usage page.
    Density 1025 kg/m^3 is the existing holtrop_coefficients.RHO_SW assumption.
    RA = (1/2)*1025*5*5*2*0.01 = 256.25 N by hand.
    RT = 100*1.2 + 10 + 20 + 30 + 40 + 256.25 = 476.25 N.
    PE = 476.25*5 = 2381.25 W. These are bookkeeping expectations only.
    """
    stubs = {
        "wetted_surface_holtrop": 2.0,
        "form_factor_k1": 1.2,
        "frictional_resistance": 100.0,
        "appendage_resistance": 10.0,
        "wave_resistance": 20.0,
        "bulbous_bow_resistance": 30.0,
        "transom_resistance": 40.0,
        "correlation_allowance": 0.01,
    }
    for name, value in stubs.items():
        monkeypatch.setattr(hm, name, lambda *a, _value=value, **kw: _value)
    example = runpy.run_path(str(EXAMPLE))
    data = example["build_result"]()
    assert data["components_n"] == pytest.approx({
        "viscous_n": 120.0,
        "appendage_n": 10.0,
        "wave_n": 20.0,
        "bulb_n": 30.0,
        "transom_n": 40.0,
        "correlation_n": 256.25,
    })
    assert data["total_resistance_n"] == pytest.approx(476.25)
    assert data["effective_power_w"] == pytest.approx(2381.25)


def test_holtrop_example_requires_output_directory(tmp_path):
    """An omitted destination must leave the working directory empty."""
    result = run_example(tmp_path)
    assert result.returncode != 0
    assert "--output-dir" in result.stderr
    assert not list(tmp_path.iterdir())


def test_holtrop_example_import_has_no_output(tmp_path, monkeypatch):
    """Loading the example must not produce files."""
    monkeypatch.chdir(tmp_path)
    runpy.run_path(str(EXAMPLE))
    assert not list(tmp_path.iterdir())


def test_holtrop_example_preserves_existing_output(tmp_path):
    """A repeated run must preserve the caller's existing result file."""
    saved = tmp_path / "holtrop_power.json"
    original = "existing result\n"
    saved.write_text(original)
    result = run_example(tmp_path, "--output-dir", tmp_path)
    assert result.returncode != 0
    assert saved.read_text() == original
