"""Tests for benchmark_dof_tables — DOF amplitude/phase table builders.

ABOUTME: Tests for _compute_dof_amplitude_rows, _compute_dof_phase_rows,
and _build_solver_column_table.
"""
from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_dof_tables import (
    _build_solver_column_table,
    _compute_dof_amplitude_rows,
    _compute_dof_phase_rows,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import DOF

from tests.hydrodynamics.diffraction.conftest import (
    _make_solver_results,
    HEADINGS,
    N_HEAD,
)


def _build_three_solver_results():
    """Build a three-solver results dict inline."""
    return {
        "AQWA": _make_solver_results("AQWA", seed_offset=0),
        "OrcaWave": _make_solver_results("OrcaWave", seed_offset=0, magnitude_scale=1.02),
        "BEMRosetta": _make_solver_results(
            "BEMRosetta", seed_offset=0, magnitude_scale=1.01, heave_bias=0.15,
        ),
    }


# ---------------------------------------------------------------------------
# _compute_dof_amplitude_rows
# ---------------------------------------------------------------------------


class TestComputeDofAmplitudeRows:
    def test_returns_list(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_amplitude_rows(DOF.SURGE, [0, 1], results, names)
        assert isinstance(rows, list)

    def test_row_keys(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_amplitude_rows(DOF.HEAVE, [0], results, names)
        assert len(rows) > 0
        for row in rows:
            assert "heading" in row
            assert "solver" in row
            assert "peak_amp" in row
            assert "peak_period" in row
            assert "long_period_amp" in row

    def test_row_count_per_heading(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        h_indices = [0, 2]
        rows = _compute_dof_amplitude_rows(DOF.SURGE, h_indices, results, names)
        # 3 solvers * 2 headings = 6 rows
        assert len(rows) == 3 * 2

    def test_all_headings(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        h_indices = list(range(N_HEAD))
        rows = _compute_dof_amplitude_rows(DOF.ROLL, h_indices, results, names)
        assert len(rows) == 3 * N_HEAD

    def test_heading_values_match(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_amplitude_rows(DOF.SURGE, [0], results, names)
        for row in rows:
            assert row["heading"] == "0"

    def test_peak_amp_is_numeric_string(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_amplitude_rows(DOF.HEAVE, [0], results, names)
        for row in rows:
            float(row["peak_amp"])  # should not raise

    def test_peak_period_is_numeric_string(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_amplitude_rows(DOF.PITCH, [1], results, names)
        for row in rows:
            val = float(row["peak_period"])
            assert val > 0

    def test_empty_heading_indices(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_amplitude_rows(DOF.SURGE, [], results, names)
        assert rows == []


# ---------------------------------------------------------------------------
# _compute_dof_phase_rows
# ---------------------------------------------------------------------------


class TestComputeDofPhaseRows:
    def test_returns_list(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_phase_rows(DOF.HEAVE, [0], results, names)
        assert isinstance(rows, list)

    def test_row_keys(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_phase_rows(DOF.HEAVE, [0, 1], results, names)
        assert len(rows) > 0
        for row in rows:
            assert "heading" in row
            assert "solver" in row
            assert "phase_at_peak" in row
            assert "long_period_phase" in row

    def test_row_count(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_phase_rows(DOF.YAW, [0, 1, 2], results, names)
        assert len(rows) == 3 * 3

    def test_phase_values_in_range(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_phase_rows(DOF.SURGE, list(range(N_HEAD)), results, names)
        for row in rows:
            val = float(row["phase_at_peak"])
            assert -180.0 <= val <= 180.0

    def test_empty_heading_indices(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_phase_rows(DOF.SURGE, [], results, names)
        assert rows == []


# ---------------------------------------------------------------------------
# _build_solver_column_table
# ---------------------------------------------------------------------------


class TestBuildSolverColumnTable:
    def test_empty_rows_returns_no_data(self):
        html = _build_solver_column_table([], "amplitude", ["AQWA"])
        assert "No data" in html

    def test_amplitude_mode_has_table(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_amplitude_rows(DOF.SURGE, [0], results, names)
        html = _build_solver_column_table(rows, "amplitude", names)
        assert "<table" in html
        assert "</table>" in html

    def test_phase_mode_has_table(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_phase_rows(DOF.SURGE, [0], results, names)
        html = _build_solver_column_table(rows, "phase", names)
        assert "<table" in html

    def test_amplitude_table_contains_solver_names(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_amplitude_rows(DOF.HEAVE, [0, 1], results, names)
        html = _build_solver_column_table(rows, "amplitude", names)
        for name in names:
            assert name in html

    def test_phase_table_contains_heading(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_phase_rows(DOF.PITCH, [0], results, names)
        html = _build_solver_column_table(rows, "phase", names)
        assert "0&deg;" in html

    def test_amplitude_table_has_peak_header(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_amplitude_rows(DOF.SURGE, [0], results, names)
        html = _build_solver_column_table(rows, "amplitude", names)
        assert "Peak" in html

    def test_phase_table_has_at_peak_header(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_phase_rows(DOF.SURGE, [0], results, names)
        html = _build_solver_column_table(rows, "phase", names)
        assert "@Peak" in html

    def test_returns_string(self):
        html = _build_solver_column_table([], "amplitude", [])
        assert isinstance(html, str)

    def test_multiple_headings_in_table(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        rows = _compute_dof_amplitude_rows(DOF.HEAVE, [0, 2, 4], results, names)
        html = _build_solver_column_table(rows, "amplitude", names)
        assert "0&deg;" in html
        assert "90&deg;" in html
        assert "180&deg;" in html
