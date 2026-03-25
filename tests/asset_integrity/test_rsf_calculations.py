"""Unit tests for FFS standalone calculation helpers (rsf_calculations.py).

Covers calculate_rsf, check_ffs_level1, remaining_life, and process_ut_grid.
All tests use pure arithmetic verification with no mocks or external dependencies.
"""
import csv
import tempfile
from datetime import date
from pathlib import Path

import pytest

from digitalmodel.asset_integrity.rsf_calculations import (
    RSF_ALLOWABLE,
    calculate_rsf,
    check_ffs_level1,
    process_ut_grid,
    remaining_life,
)


class TestCalculateRSF:
    """Tests for calculate_rsf — API 579-1 Part 4 General Metal Loss."""

    def test_nominal_case_acceptable(self):
        """RSF for lightly corroded pipe should be above allowable."""
        result = calculate_rsf(t_measured=0.350, t_required=0.150, t_nominal=0.375)
        assert result["rsf"] == pytest.approx(0.350 / 0.375, rel=1e-6)
        assert result["ffs_acceptable"] is True

    def test_rsf_below_allowable(self):
        """RSF = 0.800 is below the 0.90 allowable — FFS not acceptable."""
        result = calculate_rsf(t_measured=0.300, t_required=0.150, t_nominal=0.375)
        # RSF = 0.300 / 0.375 = 0.800 < 0.90
        assert result["rsf"] == pytest.approx(0.800, rel=1e-6)
        assert result["ffs_acceptable"] is False

    def test_zero_metal_loss(self):
        """No metal loss: RSF = 1.0, metal_loss_fraction = 0."""
        result = calculate_rsf(t_measured=0.375, t_required=0.150, t_nominal=0.375)
        assert result["rsf"] == pytest.approx(1.0)
        assert result["metal_loss_fraction"] == pytest.approx(0.0)
        assert result["ffs_acceptable"] is True

    def test_heavy_metal_loss_rsf_below_allowable(self):
        """Severe corrosion: RSF well below 0.90, metal_loss_fraction correct."""
        result = calculate_rsf(t_measured=0.100, t_required=0.150, t_nominal=0.375)
        assert result["rsf"] < RSF_ALLOWABLE
        assert result["metal_loss_fraction"] == pytest.approx(
            (0.375 - 0.100) / 0.375, rel=1e-6
        )

    def test_mawp_ratio_correct(self):
        """MAWP ratio = t_measured / t_required."""
        result = calculate_rsf(t_measured=0.300, t_required=0.150, t_nominal=0.375)
        assert result["mawp_ratio"] == pytest.approx(0.300 / 0.150, rel=1e-6)

    def test_rsf_allowable_returned(self):
        """Result always includes rsf_allowable constant."""
        result = calculate_rsf(0.350, 0.150, 0.375)
        assert result["rsf_allowable"] == pytest.approx(RSF_ALLOWABLE)

    def test_invalid_t_nominal_zero(self):
        """t_nominal = 0 must raise ValueError."""
        with pytest.raises(ValueError, match="t_nominal"):
            calculate_rsf(0.3, 0.15, 0.0)

    def test_invalid_t_required_zero(self):
        """t_required = 0 must raise ValueError."""
        with pytest.raises(ValueError, match="t_required"):
            calculate_rsf(0.3, 0.0, 0.375)

    def test_invalid_t_measured_negative(self):
        """Negative t_measured must raise ValueError."""
        with pytest.raises(ValueError, match="t_measured"):
            calculate_rsf(-0.1, 0.15, 0.375)

    def test_rsf_exactly_at_allowable(self):
        """RSF exactly at 0.90 boundary: ffs_acceptable is True."""
        # t_measured = 0.90 * t_nominal
        t_nominal = 0.400
        t_measured = RSF_ALLOWABLE * t_nominal  # exactly 0.360
        result = calculate_rsf(t_measured=t_measured, t_required=0.150, t_nominal=t_nominal)
        assert result["rsf"] == pytest.approx(RSF_ALLOWABLE, rel=1e-6)
        assert result["ffs_acceptable"] is True


class TestCheckFFSLevel1:
    """Tests for check_ffs_level1 — API 579-1 Level 1 acceptability."""

    def test_acceptable_pipe(self):
        """Well-maintained pipe: acceptable=True, MAWP matches Barlow formula."""
        result = check_ffs_level1(
            t_measured=0.350,
            t_required=0.150,
            t_nominal=0.375,
            design_pressure_psi=1000.0,
            smys_psi=65000,
            outer_diameter_in=12.75,
        )
        assert result["acceptable"] is True
        expected_mawp = 2.0 * 65000 * 1.0 * 0.350 / 12.75
        assert result["mawp_psi"] == pytest.approx(expected_mawp, rel=1e-6)

    def test_reduced_mawp_case(self):
        """Wall below t_required: MAWP < design pressure, mawp_reduced=True."""
        result = check_ffs_level1(
            t_measured=0.120,
            t_required=0.150,
            t_nominal=0.375,
            design_pressure_psi=2000.0,
            smys_psi=65000,
            outer_diameter_in=12.75,
        )
        assert result["mawp_psi"] < 2000.0
        assert result["mawp_reduced"] is True
        assert result["acceptable"] is False

    def test_pressure_fraction_above_unity_for_thick_wall(self):
        """Full nominal wall: MAWP exceeds design pressure (fraction > 1.0)."""
        result = check_ffs_level1(
            t_measured=0.375,
            t_required=0.150,
            t_nominal=0.375,
            design_pressure_psi=1000.0,
            smys_psi=65000,
            outer_diameter_in=12.75,
        )
        assert result["remaining_pressure_fraction"] > 1.0

    def test_invalid_outer_diameter_zero(self):
        """outer_diameter_in = 0 must raise ValueError."""
        with pytest.raises(ValueError, match="outer_diameter_in"):
            check_ffs_level1(0.350, 0.150, 0.375, 1000.0, 65000, 0.0)

    def test_weld_efficiency_applied(self):
        """Weld efficiency factor scales MAWP linearly."""
        result_seamless = check_ffs_level1(
            t_measured=0.350, t_required=0.150, t_nominal=0.375,
            design_pressure_psi=1000.0, smys_psi=65000,
            outer_diameter_in=12.75, weld_efficiency=1.0,
        )
        result_welded = check_ffs_level1(
            t_measured=0.350, t_required=0.150, t_nominal=0.375,
            design_pressure_psi=1000.0, smys_psi=65000,
            outer_diameter_in=12.75, weld_efficiency=0.85,
        )
        assert result_welded["mawp_psi"] == pytest.approx(
            result_seamless["mawp_psi"] * 0.85, rel=1e-6
        )


class TestRemainingLife:
    """Tests for remaining_life — corrosion life projection."""

    def test_linear_model_years(self):
        """Linear model: remaining_years = margin / rate."""
        result = remaining_life(
            t_current=0.375,
            t_minimum=0.150,
            corrosion_rate_in_per_yr=0.010,
            model="linear",
        )
        assert result["remaining_years"] == pytest.approx(
            (0.375 - 0.150) / 0.010, rel=1e-6
        )
        assert result["already_below_minimum"] is False
        assert result["inspection_date"] is not None

    def test_linear_model_dates_set(self):
        """Inspection and half-life dates are ISO strings when life > 0."""
        ref = date(2026, 1, 1)
        result = remaining_life(
            t_current=0.375,
            t_minimum=0.150,
            corrosion_rate_in_per_yr=0.010,
            model="linear",
            reference_date=ref,
        )
        assert isinstance(result["inspection_date"], str)
        assert isinstance(result["half_life_date"], str)
        # half_life_date must be before inspection_date
        assert result["half_life_date"] < result["inspection_date"]

    def test_already_below_minimum(self):
        """When t_current < t_minimum, return zero life immediately."""
        result = remaining_life(
            t_current=0.100,
            t_minimum=0.150,
            corrosion_rate_in_per_yr=0.010,
        )
        assert result["remaining_years"] == 0.0
        assert result["already_below_minimum"] is True
        assert result["inspection_date"] is None

    def test_power_law_returns_positive_result(self):
        """Power-law model returns a positive remaining life."""
        result = remaining_life(
            t_current=0.375,
            t_minimum=0.150,
            corrosion_rate_in_per_yr=0.010,
            model="power_law",
            power_law_exponent=1.2,
        )
        assert result["remaining_years"] > 0
        assert result["already_below_minimum"] is False

    def test_power_law_exponent_1_matches_linear(self):
        """Power-law with exponent=1.0 must give same result as linear."""
        kwargs = dict(t_current=0.375, t_minimum=0.150, corrosion_rate_in_per_yr=0.010)
        linear = remaining_life(**kwargs, model="linear")
        power = remaining_life(**kwargs, model="power_law", power_law_exponent=1.0)
        assert power["remaining_years"] == pytest.approx(
            linear["remaining_years"], rel=1e-3
        )

    def test_invalid_corrosion_rate_zero(self):
        """corrosion_rate = 0 must raise ValueError."""
        with pytest.raises(ValueError, match="corrosion_rate"):
            remaining_life(0.375, 0.150, 0.0)

    def test_invalid_corrosion_rate_negative(self):
        """Negative corrosion rate must raise ValueError."""
        with pytest.raises(ValueError, match="corrosion_rate"):
            remaining_life(0.375, 0.150, -0.005)

    def test_invalid_model_name(self):
        """Unknown model string must raise ValueError."""
        with pytest.raises(ValueError, match="Unknown model"):
            remaining_life(0.375, 0.150, 0.010, model="exponential")


class TestProcessUTGrid:
    """Tests for process_ut_grid — UT measurement grid CSV ingestion."""

    def _write_temp_csv(self, data: list[list[float]]) -> Path:
        """Write measurement data to a temporary CSV file."""
        tmp = tempfile.NamedTemporaryFile(
            mode="w", suffix=".csv", delete=False, newline=""
        )
        writer = csv.writer(tmp)
        for row in data:
            writer.writerow(row)
        tmp.close()
        return Path(tmp.name)

    def test_uniform_grid_no_metal_loss(self):
        """All readings at nominal: zero metal loss, no critical points."""
        data = [[0.375, 0.375], [0.375, 0.375]]
        path = self._write_temp_csv(data)
        result = process_ut_grid(path, t_nominal=0.375, t_required=0.150)
        assert result["t_min"] == pytest.approx(0.375)
        assert result["t_avg"] == pytest.approx(0.375)
        assert result["metal_loss_pct"] == pytest.approx(0.0)
        assert result["critical_points"] == 0
        assert result["below_required"] is False
        assert result["grid_shape"] == (2, 2)

    def test_grid_with_one_critical_point(self):
        """Single point below t_required flagged as critical."""
        data = [[0.375, 0.100], [0.350, 0.360]]  # 0.100 < t_required=0.150
        path = self._write_temp_csv(data)
        result = process_ut_grid(path, t_nominal=0.375, t_required=0.150)
        assert result["t_min"] == pytest.approx(0.100)
        assert result["critical_points"] == 1
        assert result["below_required"] is True

    def test_total_points_count(self):
        """total_points equals rows * cols."""
        data = [[0.375, 0.360, 0.355], [0.350, 0.345, 0.340]]
        path = self._write_temp_csv(data)
        result = process_ut_grid(path, t_nominal=0.375, t_required=0.150)
        assert result["total_points"] == 6
        assert result["grid_shape"] == (2, 3)

    def test_metal_loss_pct_calculated_correctly(self):
        """metal_loss_pct = (t_nominal - t_avg) / t_nominal * 100."""
        data = [[0.300, 0.300], [0.300, 0.300]]
        path = self._write_temp_csv(data)
        result = process_ut_grid(path, t_nominal=0.375, t_required=0.150)
        expected_pct = (0.375 - 0.300) / 0.375 * 100
        assert result["metal_loss_pct"] == pytest.approx(expected_pct, rel=1e-6)

    def test_nonexistent_file_raises(self):
        """Missing CSV path must raise FileNotFoundError."""
        with pytest.raises(FileNotFoundError):
            process_ut_grid("/nonexistent/path/ut_grid.csv", 0.375, 0.150)
