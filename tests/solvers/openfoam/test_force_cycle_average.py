from __future__ import annotations

import math
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.force_cycle_average import (
    MIN_SEP,
    analyse,
    extrema,
    load_force,
)

MEAN = -50_000.0
AMPLITUDE = 300_000.0
TAU = 800.0
PERIOD = 1_500.0
PHASE = 0.35
VISCOUS = -174_000.0


def write_force(path: Path, rows: int = 4_000) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    t = np.arange(rows, dtype=float)
    pressure = MEAN + AMPLITUDE * np.exp(-t / TAU) * np.cos(
        2.0 * np.pi * t / PERIOD + PHASE
    )
    viscous = np.full_like(t, VISCOUS)
    total = pressure + viscous
    with path.open("w") as handle:
        handle.write("# OpenFOAM forces function object\n# Time total pressure viscous\n")
        for ti, ft, fp, fv in zip(t, total, pressure, viscous):
            handle.write(
                f"{ti:g} {ft:.12g} 0 0 {fp:.12g} 0 0 {fv:.12g} 0 0\n"
            )
    return path


@pytest.fixture
def force_file(tmp_path):
    return write_force(tmp_path / "force.dat")


def test_extrema_pruning_and_asymptotes(force_file):
    _, t, total, pressure, _ = load_force(force_file)
    _, ex = extrema(t, pressure, start=500.0, smooth=25)
    assert len(ex) >= 4
    assert all(b[0] - a[0] >= MIN_SEP for a, b in zip(ex, ex[1:]))
    assert all(a[2] != b[2] for a, b in zip(ex, ex[1:]))

    result = analyse(force_file, start=500.0, smooth=25)
    expected_total = MEAN + VISCOUS
    assert result["aitken_total"] == pytest.approx(expected_total, rel=0.02)
    assert result["fit_total"] == pytest.approx(expected_total, rel=0.02)
    assert result["fit"]["period"] == pytest.approx(PERIOD, rel=0.05)

    analytic = result["fit"]["t0"] + result["fit"]["tau"] * math.log(
        result["fit"]["amplitude"] / (0.01 * abs(result["fit_total"]))
    )
    assert result["iteration_amp_below_pct"] == pytest.approx(analytic)


def test_cycle_change_decreases_as_the_transient_decays(force_file):
    early = analyse(force_file, start=0.0, smooth=25)
    late = analyse(force_file, start=500.0, smooth=25)
    early_changes = [
        abs(a["total"] - b["total"]) / abs(b["total"]) * 100.0
        for a, b in zip(reversed(early["cycles"]), reversed(early["cycles"][:-1]))
    ]
    assert early_changes == sorted(early_changes, reverse=True)
    assert late["cycle_change_pct"] <= early_changes[0]


def test_short_history_reports_that_three_extrema_are_needed(tmp_path):
    result = analyse(write_force(tmp_path / "short.dat", rows=1_200), start=0, smooth=25)
    assert len(result["extrema"]) < 3
    assert "need 3" in result["note"]


@pytest.mark.parametrize("function_name", ["forces_hull", "forces"])
def test_load_force_accepts_case_directory_layout(tmp_path, function_name):
    expected = write_force(
        tmp_path / "postProcessing" / function_name / "0" / "force.dat", rows=10
    )
    loaded, t, total, pressure, viscous = load_force(tmp_path)
    assert loaded == expected
    assert len(t) == 10
    np.testing.assert_allclose(total, pressure + viscous)
