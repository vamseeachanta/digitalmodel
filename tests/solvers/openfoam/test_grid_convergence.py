"""Three-mesh Richardson / GCI: recovers a known order and extrapolant, reports
the medium-mesh GCI the programme's 1 % target is read from, and reduces a
forceCoeffs file the way the settling gate does."""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam import grid_convergence as gc


def synthetic(p: float, f_exact: float, c: float, h):
    return [f_exact + c * hi ** p for hi in h]


def test_recovers_order_and_extrapolant_for_a_clean_second_order_sequence():
    h = [1.0, 2.0, 4.0]
    f = synthetic(2.0, 4.0e-3, 1.0e-4, h)
    r = gc.grid_convergence(h, f)
    assert r.p == pytest.approx(2.0, rel=1e-6)
    assert r.f_ext == pytest.approx(4.0e-3, rel=1e-6)
    assert r.monotone and not r.oscillatory
    assert r.r21 == 2.0 and r.r32 == 2.0
    # GCI_fine = 1.25 * |e21| / (r^p - 1); e21 = 3e-4 / 4.1e-3
    assert r.gci_fine_pct == pytest.approx(1.25 * (3e-4 / 4.1e-3) / 3 * 100, rel=1e-6)
    assert r.asymptotic_ratio == pytest.approx(1.0, rel=0.05)


def test_unequal_ratios_are_solved_iteratively():
    h = [1.0, 1.5, 3.0]
    f = synthetic(1.7, 1.0, 0.2, h)
    r = gc.grid_convergence(h, f)
    assert r.p == pytest.approx(1.7, rel=1e-4)
    assert r.f_ext == pytest.approx(1.0, rel=1e-4)


def test_oscillatory_convergence_is_flagged():
    r = gc.grid_convergence([1, 2, 4], [1.00, 1.02, 0.99])
    assert r.oscillatory


def test_refuses_degenerate_inputs():
    with pytest.raises(gc.GridConvergenceError):
        gc.grid_convergence([2, 1, 4], [1, 2, 3])
    with pytest.raises(gc.GridConvergenceError):
        gc.grid_convergence([1, 2, 4], [1, 1, 3])
    with pytest.raises(gc.GridConvergenceError):
        gc.grid_convergence([1, 1.05, 4], [1, 2, 3])


def write_coeffs(path: Path, values):
    path.parent.mkdir(parents=True, exist_ok=True)
    lines = ["# forceCoeffs", "# Time  Cd  Cd(f)  Cd(r)  Cl"]
    for i, v in enumerate(values, 1):
        lines.append(f"{i}\t{v:.6e}\t0\t0\t0")
    path.write_text("\n".join(lines) + "\n")


def test_settled_mean_uses_the_last_window_and_reports_drift(tmp_path):
    f = tmp_path / "coefficient.dat"
    write_coeffs(f, [5e-3] * 400 + [4e-3] * 400)
    mean, drift, n = gc.settled_mean(f, window=400)
    assert mean == pytest.approx(4e-3)
    assert drift == pytest.approx(25.0)
    assert n == 800
    with pytest.raises(gc.GridConvergenceError):
        gc.settled_mean(f, window=500)


def test_gci_report_reads_cases_and_orders_fine_to_coarse(tmp_path):
    hs = {"c": 0.030, "b": 0.015, "a": 0.0075}
    for name, h in hs.items():
        case = tmp_path / name
        (case).mkdir()
        (case / "case_provenance.json").write_text(json.dumps(
            {"mesh": {"estimated_cells": int(1e6 * (0.0075 / h) ** 2),
                      "free_surface": {"in_plane_cell_size_m": h, "cells_per_wavelength": 1.35 / h}}}))
        write_coeffs(case / "postProcessing/forceCoeffs/0/coefficient.dat", [4.0e-3 + 2.0 * h ** 2] * 800)
    rep = gc.gci_report([tmp_path / "c", tmp_path / "a", tmp_path / "b"])
    assert [m["case"] for m in rep["meshes"]] == ["a", "b", "c"]
    assert rep["gci"]["p"] == pytest.approx(2.0, rel=1e-6)
    assert rep["gci"]["f_ext"] == pytest.approx(4.0e-3, rel=1e-6)
    assert rep["gci"]["gci_medium_pct"] > rep["gci"]["gci_fine_pct"] > 0
