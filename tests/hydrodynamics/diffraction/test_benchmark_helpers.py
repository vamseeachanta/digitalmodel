"""Tests for benchmark_helpers — leaf constants and utility functions.

ABOUTME: Tests for _is_phase_at_negligible_amplitude, _parse_fdf_panels,
generate_dof_observations, and module-level constants (DOF_ORDER,
_AMPLITUDE_UNITS, _SOLVER_STYLES, _FILE_DESCRIPTIONS, _NEGLIGIBLE_AMPLITUDE_RATIO).
"""
from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_helpers import (
    DOF_ORDER,
    _AMPLITUDE_UNITS,
    _FILE_DESCRIPTIONS,
    _NEGLIGIBLE_AMPLITUDE_RATIO,
    _SOLVER_STYLES,
    _is_phase_at_negligible_amplitude,
    _parse_fdf_panels,
    generate_dof_observations,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import DOF


# ---------------------------------------------------------------------------
# Constants validation
# ---------------------------------------------------------------------------


class TestConstants:
    """Verify module-level constants have the expected shape and content."""

    def test_dof_order_length(self):
        assert len(DOF_ORDER) == 6

    def test_dof_order_starts_with_surge(self):
        assert DOF_ORDER[0] == DOF.SURGE

    def test_dof_order_ends_with_yaw(self):
        assert DOF_ORDER[-1] == DOF.YAW

    def test_dof_order_all_dofs_present(self):
        assert set(DOF_ORDER) == set(DOF)

    def test_amplitude_units_has_six_keys(self):
        assert len(_AMPLITUDE_UNITS) == 6

    def test_amplitude_units_keys_match_dof(self):
        assert set(_AMPLITUDE_UNITS.keys()) == set(DOF)

    def test_amplitude_units_translational_are_m_per_m(self):
        for dof in (DOF.SURGE, DOF.SWAY, DOF.HEAVE):
            assert _AMPLITUDE_UNITS[dof] == "m/m"

    def test_amplitude_units_rotational_are_deg_per_m(self):
        for dof in (DOF.ROLL, DOF.PITCH, DOF.YAW):
            assert _AMPLITUDE_UNITS[dof] == "deg/m"

    def test_solver_styles_has_four_entries(self):
        assert len(_SOLVER_STYLES) == 4

    def test_solver_styles_keys_are_dash_and_color(self):
        for idx, style in _SOLVER_STYLES.items():
            assert "dash" in style
            assert "color_base" in style

    def test_file_descriptions_has_known_keys(self):
        assert "OrcaWave (.owd)" in _FILE_DESCRIPTIONS
        assert "OrcaWave (spec.yml)" in _FILE_DESCRIPTIONS
        assert "AQWA" in _FILE_DESCRIPTIONS

    def test_file_descriptions_values_are_strings(self):
        for key, desc in _FILE_DESCRIPTIONS.items():
            assert isinstance(desc, str)
            assert len(desc) > 10

    def test_negligible_amplitude_ratio_value(self):
        assert _NEGLIGIBLE_AMPLITUDE_RATIO == 0.05


# ---------------------------------------------------------------------------
# _is_phase_at_negligible_amplitude
# ---------------------------------------------------------------------------


class TestIsPhaseAtNegligibleAmplitude:
    """Tests for _is_phase_at_negligible_amplitude."""

    def test_zero_peak_returns_true(self):
        assert _is_phase_at_negligible_amplitude(0.0, 0.0) is True

    def test_negative_peak_returns_true(self):
        assert _is_phase_at_negligible_amplitude(0.01, -1.0) is True

    def test_below_threshold_returns_true(self):
        # 0.04 / 1.0 = 0.04 < 0.05
        assert _is_phase_at_negligible_amplitude(0.04, 1.0) is True

    def test_at_threshold_returns_false(self):
        # 0.05 / 1.0 = 0.05, NOT < 0.05
        assert _is_phase_at_negligible_amplitude(0.05, 1.0) is False

    def test_above_threshold_returns_false(self):
        assert _is_phase_at_negligible_amplitude(0.5, 1.0) is False

    def test_equal_magnitudes_returns_false(self):
        assert _is_phase_at_negligible_amplitude(1.0, 1.0) is False

    def test_large_peak_small_mag_returns_true(self):
        # 0.001 / 100 = 0.00001 < 0.05
        assert _is_phase_at_negligible_amplitude(0.001, 100.0) is True

    def test_barely_above_threshold(self):
        # 0.051 / 1.0 = 0.051 >= 0.05
        assert _is_phase_at_negligible_amplitude(0.051, 1.0) is False


# ---------------------------------------------------------------------------
# _parse_fdf_panels
# ---------------------------------------------------------------------------


class TestParseFdfPanels:
    """Tests for _parse_fdf_panels with synthetic FDF files."""

    def _write_fdf(self, tmp_path: Path, data_lines: list[str]) -> Path:
        """Write a fake FDF file with 4 header lines then data_lines."""
        lines = [
            "Title line\n",
            "RINNER 10.0\n",
            "NPF 100 NTCL 200\n",
            "NAL 300\n",
        ]
        lines.extend(dl + "\n" for dl in data_lines)
        fdf_path = tmp_path / "test.fdf"
        fdf_path.write_text("".join(lines))
        return fdf_path

    def test_single_panel(self, tmp_path):
        row = "1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0"
        path = self._write_fdf(tmp_path, [row])
        panels = _parse_fdf_panels(path)
        assert len(panels) == 1
        assert panels[0] == [
            [1.0, 5.0, 0.0],
            [2.0, 6.0, 0.0],
            [3.0, 7.0, 0.0],
            [4.0, 8.0, 0.0],
        ]

    def test_multiple_panels(self, tmp_path):
        rows = [
            "1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0",
            "10.0 20.0 30.0 40.0 50.0 60.0 70.0 80.0",
        ]
        path = self._write_fdf(tmp_path, rows)
        panels = _parse_fdf_panels(path)
        assert len(panels) == 2

    def test_empty_data_section(self, tmp_path):
        path = self._write_fdf(tmp_path, [])
        panels = _parse_fdf_panels(path)
        assert panels == []

    def test_blank_lines_skipped(self, tmp_path):
        rows = [
            "1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0",
            "",
            "   ",
            "10.0 20.0 30.0 40.0 50.0 60.0 70.0 80.0",
        ]
        path = self._write_fdf(tmp_path, rows)
        panels = _parse_fdf_panels(path)
        assert len(panels) == 2

    def test_wrong_column_count_skipped(self, tmp_path):
        rows = [
            "1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0",
            "1.0 2.0 3.0",  # only 3 values
        ]
        path = self._write_fdf(tmp_path, rows)
        panels = _parse_fdf_panels(path)
        assert len(panels) == 1

    def test_non_numeric_row_skipped(self, tmp_path):
        rows = [
            "1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0",
            "not a number line at all nope",
        ]
        path = self._write_fdf(tmp_path, rows)
        panels = _parse_fdf_panels(path)
        assert len(panels) == 1

    def test_nonexistent_file_returns_empty(self, tmp_path):
        panels = _parse_fdf_panels(tmp_path / "does_not_exist.fdf")
        assert panels == []

    def test_panel_vertex_structure(self, tmp_path):
        row = "0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8"
        path = self._write_fdf(tmp_path, [row])
        panels = _parse_fdf_panels(path)
        panel = panels[0]
        assert len(panel) == 4
        for vertex in panel:
            assert len(vertex) == 3
            assert vertex[2] == 0.0  # z is always 0


# ---------------------------------------------------------------------------
# generate_dof_observations
# ---------------------------------------------------------------------------


class TestGenerateDofObservations:
    """Tests for generate_dof_observations HTML generation."""

    def test_full_consensus(self):
        html = generate_dof_observations(
            dof_name="Heave",
            consensus="FULL",
            mag_corr=0.9999,
            phase_corr=0.999,
            max_mag_diff=0.001,
            max_phase_diff=1.0,
            unit="m/m",
        )
        assert "full agreement" in html
        assert "Heave" in html

    def test_majority_consensus(self):
        html = generate_dof_observations(
            dof_name="Surge",
            consensus="MAJORITY",
            mag_corr=0.998,
            phase_corr=0.95,
            max_mag_diff=0.05,
            max_phase_diff=10.0,
            unit="m/m",
        )
        assert "majority agreement" in html
        assert "Surge" in html

    def test_no_consensus(self):
        html = generate_dof_observations(
            dof_name="Roll",
            consensus="NONE",
            mag_corr=0.8,
            phase_corr=0.5,
            max_mag_diff=2.0,
            max_phase_diff=100.0,
            unit="deg/m",
            magnitude_at_max_phase_diff=3.0,
            peak_magnitude=5.0,
        )
        assert "no consensus" in html
        assert "review recommended" in html

    def test_virtually_identical_amplitude(self):
        html = generate_dof_observations(
            dof_name="Heave",
            consensus="FULL",
            mag_corr=0.9999,
            phase_corr=0.99,
            max_mag_diff=0.001,
            max_phase_diff=5.0,
            unit="m/m",
        )
        assert "virtually identical" in html

    def test_excellent_amplitude(self):
        html = generate_dof_observations(
            dof_name="Heave",
            consensus="FULL",
            mag_corr=0.995,
            phase_corr=0.99,
            max_mag_diff=0.05,
            max_phase_diff=5.0,
            unit="m/m",
        )
        assert "excellent" in html

    def test_good_amplitude(self):
        html = generate_dof_observations(
            dof_name="Heave",
            consensus="FULL",
            mag_corr=0.96,
            phase_corr=0.90,
            max_mag_diff=0.3,
            max_phase_diff=15.0,
            unit="m/m",
        )
        assert "good" in html.lower()

    def test_moderate_amplitude(self):
        html = generate_dof_observations(
            dof_name="Heave",
            consensus="NONE",
            mag_corr=0.80,
            phase_corr=0.50,
            max_mag_diff=1.0,
            max_phase_diff=10.0,
            unit="m/m",
        )
        assert "moderate" in html

    def test_phase_diff_small(self):
        html = generate_dof_observations(
            dof_name="Surge",
            consensus="FULL",
            mag_corr=0.999,
            phase_corr=0.999,
            max_mag_diff=0.001,
            max_phase_diff=5.0,
            unit="m/m",
        )
        assert "5.0" in html
        assert "agreement within" in html

    def test_phase_diff_moderate(self):
        html = generate_dof_observations(
            dof_name="Pitch",
            consensus="FULL",
            mag_corr=0.999,
            phase_corr=0.95,
            max_mag_diff=0.01,
            max_phase_diff=45.0,
            unit="deg/m",
        )
        assert "resonance" in html

    def test_phase_diff_large_at_negligible_amplitude(self):
        html = generate_dof_observations(
            dof_name="Yaw",
            consensus="MAJORITY",
            mag_corr=0.99,
            phase_corr=0.80,
            max_mag_diff=0.1,
            max_phase_diff=150.0,
            unit="deg/m",
            magnitude_at_max_phase_diff=0.001,
            peak_magnitude=5.0,
        )
        assert "insignificant" in html or "can be ignored" in html

    def test_phase_diff_large_at_significant_amplitude(self):
        html = generate_dof_observations(
            dof_name="Roll",
            consensus="NONE",
            mag_corr=0.90,
            phase_corr=0.70,
            max_mag_diff=1.0,
            max_phase_diff=120.0,
            unit="deg/m",
            magnitude_at_max_phase_diff=3.0,
            peak_magnitude=5.0,
        )
        assert "significant amplitude" in html
        assert "convention" in html or "resonance" in html

    def test_hidden_heading_phase(self):
        html = generate_dof_observations(
            dof_name="Sway",
            consensus="FULL",
            mag_corr=0.999,
            phase_corr=0.95,
            max_mag_diff=0.01,
            max_phase_diff=45.0,
            unit="m/m",
            phase_diff_at_visible_heading=False,
        )
        assert "omitted" in html
        assert "negligible" in html.lower() or "good phase" in html

    def test_returns_string(self):
        result = generate_dof_observations(
            dof_name="Heave",
            consensus="FULL",
            mag_corr=1.0,
            phase_corr=1.0,
            max_mag_diff=0.0,
            max_phase_diff=0.0,
            unit="m/m",
        )
        assert isinstance(result, str)

    def test_contains_html_tags(self):
        result = generate_dof_observations(
            dof_name="Heave",
            consensus="FULL",
            mag_corr=0.999,
            phase_corr=0.999,
            max_mag_diff=0.0,
            max_phase_diff=0.0,
            unit="m/m",
        )
        assert "<p>" in result
        assert "</p>" in result
