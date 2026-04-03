"""Tests for comparison_framework.py.

Covers:
- DiffractionComparator initialisation and vessel name check
- DeviationStatistics calculation
- RAO comparison (compare_raos)
- Matrix comparison (compare_added_mass, compare_damping)
- Full report generation (generate_report)
- Agreement assessment (_assess_agreement)
- Note generation (_generate_notes)
- JSON export (export_report)
- compare_diffraction_results convenience function

Uses conftest fixtures (mock_diffraction_results, three_solver_results).
"""
from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.comparison_framework import (
    ComparisonReport,
    DeviationStatistics,
    DiffractionComparator,
    MatrixComparison,
    RAOComparison,
    compare_diffraction_results,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DOF,
    DiffractionResults,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _clone_results(dr: DiffractionResults, new_name: str | None = None) -> DiffractionResults:
    """Shallow-copy a DiffractionResults, optionally changing the vessel_name."""
    import copy
    clone = copy.deepcopy(dr)
    if new_name:
        clone.vessel_name = new_name
        clone.raos.vessel_name = new_name
    return clone


# ---------------------------------------------------------------------------
# Tests: DiffractionComparator initialisation
# ---------------------------------------------------------------------------


class TestComparatorInit:

    def test_vessel_name_mismatch_raises(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        orcawave = _clone_results(mock_diffraction_results, new_name="OtherVessel")
        with pytest.raises(ValueError, match="Vessel name mismatch"):
            DiffractionComparator(aqwa, orcawave)

    def test_stores_results(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        assert comp.aqwa is aqwa
        assert comp.orcawave is ow

    def test_default_tolerance(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        assert comp.tolerance == pytest.approx(0.05)

    def test_custom_tolerance(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow, tolerance=0.10)
        assert comp.tolerance == pytest.approx(0.10)


# ---------------------------------------------------------------------------
# Tests: _calculate_deviation_stats
# ---------------------------------------------------------------------------


class TestDeviationStatistics:

    def test_identical_arrays_zero_error(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)

        a = np.array([1.0, 2.0, 3.0])
        stats = comp._calculate_deviation_stats(a, a, np.array([0.1, 0.2, 0.3]))
        assert stats.mean_error == pytest.approx(0.0)
        assert stats.max_error == pytest.approx(0.0)
        assert stats.rms_error == pytest.approx(0.0)
        assert stats.correlation == pytest.approx(1.0)

    def test_known_offset(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)

        a = np.array([1.0, 2.0, 3.0])
        b = a + 0.5  # uniform offset
        stats = comp._calculate_deviation_stats(a, b, np.array([0.1, 0.2, 0.3]))
        assert stats.mean_error == pytest.approx(0.5)
        assert stats.max_error == pytest.approx(0.5)
        assert stats.mean_abs_error == pytest.approx(0.5)
        assert stats.correlation == pytest.approx(1.0, abs=1e-10)

    def test_returns_deviation_statistics_type(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)

        # Need at least 2 elements for np.corrcoef to avoid DOF warning
        stats = comp._calculate_deviation_stats(
            np.array([1.0, 3.0]), np.array([2.0, 4.0]), np.array([0.1, 0.2])
        )
        assert isinstance(stats, DeviationStatistics)
        assert stats.mean_error == pytest.approx(1.0)


# ---------------------------------------------------------------------------
# Tests: compare_raos
# ---------------------------------------------------------------------------


class TestCompareRAOs:

    def test_returns_dict_of_rao_comparisons(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        result = comp.compare_raos()

        assert isinstance(result, dict)
        assert "surge" in result
        assert "yaw" in result
        assert isinstance(result["surge"], RAOComparison)

    def test_identical_raos_have_zero_diff(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        result = comp.compare_raos()

        for dof_name, rao_comp in result.items():
            np.testing.assert_allclose(
                rao_comp.magnitude_diff, 0.0, atol=1e-10,
                err_msg=f"{dof_name} magnitude diff should be zero"
            )

    def test_modified_raos_show_diff(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        # Modify OrcaWave heave magnitudes
        ow.raos.heave.magnitude = ow.raos.heave.magnitude + 0.1
        comp = DiffractionComparator(aqwa, ow)
        result = comp.compare_raos()

        heave_comp = result["heave"]
        assert heave_comp.statistics.mean_error == pytest.approx(0.1, abs=0.01)

    def test_phase_wrapping(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        # Set phases to test wrapping
        aqwa.raos.surge.phase[:, :] = 170.0
        ow.raos.surge.phase[:, :] = -170.0
        comp = DiffractionComparator(aqwa, ow)
        result = comp.compare_raos()

        # Phase diff should be about 20 degrees (wrapping from 170 to -170 = -340 → 20)
        surge_comp = result["surge"]
        max_phase = np.max(np.abs(surge_comp.phase_diff))
        assert max_phase < 30, f"Expected wrapped phase diff near 20, got {max_phase}"


# ---------------------------------------------------------------------------
# Tests: compare_added_mass
# ---------------------------------------------------------------------------


class TestCompareAddedMass:

    def test_returns_matrix_comparison(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        result = comp.compare_added_mass()

        assert isinstance(result, MatrixComparison)
        assert result.matrix_type == "added_mass"

    def test_identical_matrices_zero_deviation(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        result = comp.compare_added_mass()

        assert result.max_deviation_value == pytest.approx(0.0, abs=1e-10)

    def test_all_36_elements_compared(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        result = comp.compare_added_mass()

        assert len(result.element_statistics) == 36  # 6x6

    def test_frequency_count_mismatch_raises(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        # Remove one matrix from orcawave
        ow.added_mass.matrices = ow.added_mass.matrices[:-1]
        comp = DiffractionComparator(aqwa, ow)

        with pytest.raises(ValueError, match="Frequency count mismatch"):
            comp.compare_added_mass()


# ---------------------------------------------------------------------------
# Tests: compare_damping
# ---------------------------------------------------------------------------


class TestCompareDamping:

    def test_returns_matrix_comparison(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        result = comp.compare_damping()

        assert isinstance(result, MatrixComparison)
        assert result.matrix_type == "damping"

    def test_tracks_max_deviation(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        # Introduce a known difference
        ow.damping.matrices[3].matrix[2, 2] += 999.0
        comp = DiffractionComparator(aqwa, ow)
        result = comp.compare_damping()

        assert result.max_deviation_value >= 999.0
        assert result.max_deviation_element == (2, 2)


# ---------------------------------------------------------------------------
# Tests: generate_report
# ---------------------------------------------------------------------------


class TestGenerateReport:

    def test_returns_comparison_report(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        report = comp.generate_report()

        assert isinstance(report, ComparisonReport)
        assert report.vessel_name == "TestVessel"
        assert len(report.rao_comparisons) == 6  # 6 DOFs
        assert report.added_mass_comparison is not None
        assert report.damping_comparison is not None

    def test_identical_results_excellent_agreement(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        report = comp.generate_report()

        # Identical data → perfect correlation → EXCELLENT
        assert report.overall_agreement == "EXCELLENT"


# ---------------------------------------------------------------------------
# Tests: _assess_agreement
# ---------------------------------------------------------------------------


class TestAssessAgreement:

    def test_excellent_for_high_correlation(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        report = comp.generate_report()
        assert comp._assess_agreement(report) == "EXCELLENT"

    def test_poor_for_uncorrelated(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        # Randomise orcawave RAOs
        rng = np.random.default_rng(99)
        for dof_name in ("surge", "sway", "heave", "roll", "pitch", "yaw"):
            comp_obj = getattr(ow.raos, dof_name)
            comp_obj.magnitude = rng.uniform(0, 5, comp_obj.magnitude.shape)
        # Randomise matrices
        for i, m in enumerate(ow.added_mass.matrices):
            m.matrix = rng.uniform(0, 1000, (6, 6))
            m.matrix = (m.matrix + m.matrix.T) / 2
        for i, m in enumerate(ow.damping.matrices):
            m.matrix = rng.uniform(0, 1000, (6, 6))
            m.matrix = (m.matrix + m.matrix.T) / 2

        comp = DiffractionComparator(aqwa, ow)
        report = comp.generate_report()
        assert report.overall_agreement in ("FAIR", "POOR")


# ---------------------------------------------------------------------------
# Tests: _generate_notes
# ---------------------------------------------------------------------------


class TestGenerateNotes:

    def test_notes_are_list(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        report = comp.generate_report()
        assert isinstance(report.notes, list)

    def test_notes_include_matrix_deviations(self, mock_diffraction_results):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        report = comp.generate_report()
        # Should have notes about added mass and damping deviations
        text = " ".join(report.notes)
        assert "added mass" in text.lower() or "damping" in text.lower()


# ---------------------------------------------------------------------------
# Tests: export_report
# ---------------------------------------------------------------------------


class TestExportReport:
    """Test export_report JSON serialisation.

    NOTE: comparison_framework.export_report has a known bug where
    ``max_magnitude_diff_location`` and ``max_phase_diff_location``
    (numpy int64 tuples) are not JSON-serialisable.  These tests mark
    that as xfail to document the issue without blocking CI.
    """

    @pytest.mark.xfail(
        reason="Source bug: numpy int64 in diff_location tuples is not JSON-serialisable",
        raises=TypeError,
        strict=True,
    )
    def test_creates_json_file(self, mock_diffraction_results, tmp_path):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        output = tmp_path / "comparison.json"
        comp.export_report(output)

        assert output.exists()
        data = json.loads(output.read_text())
        assert "overall_agreement" in data

    @pytest.mark.xfail(
        reason="Source bug: numpy int64 in diff_location tuples is not JSON-serialisable",
        raises=(TypeError, RecursionError),
        strict=True,
    )
    def test_json_has_correlation_values(self, mock_diffraction_results, tmp_path):
        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)
        comp = DiffractionComparator(aqwa, ow)
        output = tmp_path / "comparison.json"
        comp.export_report(output)

        data = json.loads(output.read_text())
        surge = data["rao_comparisons"]["surge"]
        assert "correlation" in surge


# ---------------------------------------------------------------------------
# Tests: compare_diffraction_results convenience function
# ---------------------------------------------------------------------------


class TestConvenienceFunction:

    def test_returns_comparison_report(self, mock_diffraction_results, tmp_path):
        """compare_diffraction_results generates a report and calls export_report.

        NOTE: The source export_report method has a known serialization issue
        with numpy int64 types in max_*_diff_location tuples. We patch the
        export_report call to isolate the report-generation logic under test.
        """
        from unittest.mock import patch as _patch

        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)

        with _patch.object(DiffractionComparator, "export_report"):
            report = compare_diffraction_results(
                aqwa, ow, output_dir=tmp_path
            )
        assert isinstance(report, ComparisonReport)

    def test_creates_output_dir(self, mock_diffraction_results, tmp_path):
        """compare_diffraction_results creates the output directory."""
        from unittest.mock import patch as _patch

        aqwa = _clone_results(mock_diffraction_results)
        ow = _clone_results(mock_diffraction_results)

        out = tmp_path / "sub" / "dir"
        with _patch.object(DiffractionComparator, "export_report"):
            compare_diffraction_results(aqwa, ow, output_dir=out)
        assert out.exists()
