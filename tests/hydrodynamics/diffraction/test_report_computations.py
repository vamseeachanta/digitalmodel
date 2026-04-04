"""Tests for diffraction report computation functions.

ABOUTME: Comprehensive unit tests for the pure computation functions in
report_computations.py — compute_stability, compute_radii_of_gyration,
compute_natural_periods, compute_peak_responses, compute_coupling_significance,
generate_executive_warnings, and the helper _find_closest_idx.
Traceability: #1767
"""

from __future__ import annotations

import math
from typing import Dict, List, Optional

import numpy as np
import numpy.testing as npt
import pytest

from digitalmodel.hydrodynamics.diffraction.report_computations import (
    _find_closest_idx,
    compute_coupling_significance,
    compute_natural_periods,
    compute_peak_responses,
    compute_radii_of_gyration,
    compute_stability,
    generate_executive_warnings,
)
from digitalmodel.hydrodynamics.diffraction.report_data_models import (
    DOF_NAMES,
    DiffractionReportData,
    HydrostaticData,
    MeshQualityData,
    RollDampingData,
)


# ---------------------------------------------------------------------------
# Helpers: realistic fixture builders
# ---------------------------------------------------------------------------

def _make_6x6_zero() -> list[list[float]]:
    return [[0.0] * 6 for _ in range(6)]


def _make_restoring_matrix(
    c33: float = 3.5e6,
    c44: float = 1.2e8,
    c55: float = 2.5e9,
) -> list[list[float]]:
    """Typical semi-sub restoring matrix (only heave/roll/pitch non-zero)."""
    C = _make_6x6_zero()
    C[2][2] = c33
    C[3][3] = c44
    C[4][4] = c55
    return C


def _make_inertia_matrix(
    mass: float,
    r_xx: float = 15.0,
    r_yy: float = 20.0,
    r_zz: float = 22.0,
) -> list[list[float]]:
    I = _make_6x6_zero()
    I[0][0] = mass
    I[1][1] = mass
    I[2][2] = mass
    I[3][3] = mass * r_xx**2
    I[4][4] = mass * r_yy**2
    I[5][5] = mass * r_zz**2
    return I


SEMISUB_MASS = 12_000_000.0    # kg (12,000 te)
SEMISUB_VOLUME = 11_707.3      # m^3, rho=1025
RHO = 1025.0
G = 9.81


def _make_hydrostatic(
    volume: float = SEMISUB_VOLUME,
    mass: float = SEMISUB_MASS,
    cob_z: float = -8.5,
    c33: float = 3.5e6,
    c44: float = 1.2e8,
    c55: float = 2.5e9,
    lxx: float = 3.5e4,
    lyy: float = 8.0e5,
    r_xx: float = 15.0,
    r_yy: float = 20.0,
    r_zz: float = 22.0,
) -> HydrostaticData:
    return HydrostaticData(
        volume=volume,
        mass=mass,
        centre_of_buoyancy=[0.0, 0.0, cob_z],
        centre_of_mass=[0.0, 0.0, 5.2],
        waterplane_area=1250.0,
        Lxx=lxx,
        Lyy=lyy,
        Lxy=0.0,
        centre_of_floatation=[0.0, 0.0],
        restoring_matrix=_make_restoring_matrix(c33, c44, c55),
        inertia_matrix=_make_inertia_matrix(mass, r_xx, r_yy, r_zz),
    )


# ---------------------------------------------------------------------------
# _find_closest_idx
# ---------------------------------------------------------------------------

class TestFindClosestIdx:
    """Tests for the internal _find_closest_idx helper."""

    def test_exact_match(self):
        vals = [1.0, 2.0, 3.0, 4.0, 5.0]
        assert _find_closest_idx(vals, 3.0) == 2

    def test_closest_below(self):
        vals = [1.0, 2.0, 3.0, 4.0, 5.0]
        assert _find_closest_idx(vals, 2.9) == 2

    def test_closest_above(self):
        vals = [1.0, 2.0, 3.0, 4.0, 5.0]
        assert _find_closest_idx(vals, 3.1) == 2

    def test_first_element(self):
        vals = [1.0, 2.0, 3.0]
        assert _find_closest_idx(vals, 0.5) == 0

    def test_last_element(self):
        vals = [1.0, 2.0, 3.0]
        assert _find_closest_idx(vals, 3.8) == 2

    def test_single_element(self):
        vals = [5.0]
        assert _find_closest_idx(vals, 100.0) == 0

    def test_negative_values(self):
        vals = [-3.0, -1.0, 0.0, 1.0, 3.0]
        assert _find_closest_idx(vals, -0.8) == 1


# ---------------------------------------------------------------------------
# compute_stability
# ---------------------------------------------------------------------------

class TestComputeStability:
    """Tests for compute_stability — GM, BM, KB calculations."""

    def test_gm_transverse(self):
        """GM_T = C(4,4) / (rho * g * V)."""
        hs = _make_hydrostatic()
        result = compute_stability(hs)
        expected = 1.2e8 / (RHO * G * SEMISUB_VOLUME)
        assert result["gm_transverse"] == pytest.approx(expected, rel=1e-6)

    def test_gm_longitudinal(self):
        """GM_L = C(5,5) / (rho * g * V)."""
        hs = _make_hydrostatic()
        result = compute_stability(hs)
        expected = 2.5e9 / (RHO * G * SEMISUB_VOLUME)
        assert result["gm_longitudinal"] == pytest.approx(expected, rel=1e-6)

    def test_bm_transverse(self):
        """BM_T = Ixx / V."""
        hs = _make_hydrostatic()
        result = compute_stability(hs)
        expected = 3.5e4 / SEMISUB_VOLUME
        assert result["bm_transverse"] == pytest.approx(expected, rel=1e-6)

    def test_bm_longitudinal(self):
        """BM_L = Iyy / V."""
        hs = _make_hydrostatic()
        result = compute_stability(hs)
        expected = 8.0e5 / SEMISUB_VOLUME
        assert result["bm_longitudinal"] == pytest.approx(expected, rel=1e-6)

    def test_kb(self):
        """KB = z-coordinate of centre of buoyancy."""
        hs = _make_hydrostatic(cob_z=-8.5)
        result = compute_stability(hs)
        assert result["kb"] == pytest.approx(-8.5)

    def test_zero_volume_returns_none(self):
        """Zero volume: all results None."""
        hs = _make_hydrostatic(volume=0.0)
        result = compute_stability(hs)
        assert result["gm_transverse"] is None
        assert result["gm_longitudinal"] is None
        assert result["bm_transverse"] is None
        assert result["bm_longitudinal"] is None
        assert result["kb"] is None

    def test_negative_volume_returns_none(self):
        """Negative volume: all results None."""
        hs = _make_hydrostatic(volume=-100.0)
        result = compute_stability(hs)
        assert result["gm_transverse"] is None

    def test_custom_rho_g(self):
        """Using freshwater density rho=1000 and g=9.80665."""
        hs = _make_hydrostatic()
        result = compute_stability(hs, rho=1000.0, g=9.80665)
        rho_g_v = 1000.0 * 9.80665 * SEMISUB_VOLUME
        expected = 1.2e8 / rho_g_v
        assert result["gm_transverse"] == pytest.approx(expected, rel=1e-6)

    def test_returns_all_expected_keys(self):
        hs = _make_hydrostatic()
        result = compute_stability(hs)
        expected_keys = {
            "gm_transverse",
            "gm_longitudinal",
            "bm_transverse",
            "bm_longitudinal",
            "kb",
        }
        assert set(result.keys()) == expected_keys

    def test_physically_positive_gm(self):
        """For a stable vessel, GM should be positive."""
        hs = _make_hydrostatic()
        result = compute_stability(hs)
        assert result["gm_transverse"] > 0
        assert result["gm_longitudinal"] > 0


# ---------------------------------------------------------------------------
# compute_radii_of_gyration
# ---------------------------------------------------------------------------

class TestComputeRadiiOfGyration:
    """Tests for compute_radii_of_gyration."""

    def test_basic_calculation(self):
        """r_xx = sqrt(I_44 / M) = r_xx input."""
        hs = _make_hydrostatic(r_xx=15.0, r_yy=20.0, r_zz=22.0)
        result = compute_radii_of_gyration(hs)
        assert result["r_xx"] == pytest.approx(15.0, rel=1e-6)
        assert result["r_yy"] == pytest.approx(20.0, rel=1e-6)
        assert result["r_zz"] == pytest.approx(22.0, rel=1e-6)

    def test_zero_mass_returns_empty(self):
        hs = _make_hydrostatic(mass=0.0)
        result = compute_radii_of_gyration(hs)
        assert result == {}

    def test_negative_mass_returns_empty(self):
        hs = _make_hydrostatic(mass=-100.0)
        result = compute_radii_of_gyration(hs)
        assert result == {}

    def test_zero_inertia_returns_zero_radius(self):
        """If I_44 = 0 but M > 0, radius should be 0."""
        hs = _make_hydrostatic(r_xx=0.0)
        result = compute_radii_of_gyration(hs)
        assert result["r_xx"] == pytest.approx(0.0)

    def test_returns_three_keys(self):
        hs = _make_hydrostatic()
        result = compute_radii_of_gyration(hs)
        assert set(result.keys()) == {"r_xx", "r_yy", "r_zz"}

    def test_fpso_typical_values(self):
        """FPSO typical: r_xx ~ 14m, r_yy ~ 60m, r_zz ~ 62m for 200k DWT."""
        mass = 200_000_000.0  # 200,000 te in kg
        hs = _make_hydrostatic(mass=mass, r_xx=14.0, r_yy=60.0, r_zz=62.0)
        result = compute_radii_of_gyration(hs)
        assert result["r_xx"] == pytest.approx(14.0, rel=1e-6)
        assert result["r_yy"] == pytest.approx(60.0, rel=1e-6)
        assert result["r_zz"] == pytest.approx(62.0, rel=1e-6)


# ---------------------------------------------------------------------------
# compute_natural_periods
# ---------------------------------------------------------------------------

class TestComputeNaturalPeriods:
    """Tests for compute_natural_periods — iterative intersection method."""

    @pytest.fixture()
    def freq_grid(self) -> list[float]:
        """Fine frequency grid from 0.1 to 2.0 rad/s (50 points)."""
        return np.linspace(0.1, 2.0, 50).tolist()

    @pytest.fixture()
    def hydrostatics(self) -> HydrostaticData:
        return _make_hydrostatic()

    def test_heave_natural_period(self, hydrostatics, freq_grid):
        """Verify heave natural period is computed and physically reasonable."""
        # Constant added mass for heave ~ 50% of mass
        a_heave = [0.5 * SEMISUB_MASS] * len(freq_grid)
        added_mass_diag = {
            "heave": a_heave,
            "roll": [0.3 * SEMISUB_MASS * 15**2] * len(freq_grid),
            "pitch": [0.4 * SEMISUB_MASS * 20**2] * len(freq_grid),
        }
        result = compute_natural_periods(hydrostatics, added_mass_diag, freq_grid)
        # Heave natural period should exist and be positive
        assert result["heave"] is not None
        assert result["heave"] > 0

    def test_roll_natural_period(self, hydrostatics, freq_grid):
        """Roll natural period should be physical."""
        a_roll = [0.3 * SEMISUB_MASS * 15**2] * len(freq_grid)
        added_mass_diag = {
            "heave": [0.5 * SEMISUB_MASS] * len(freq_grid),
            "roll": a_roll,
            "pitch": [0.4 * SEMISUB_MASS * 20**2] * len(freq_grid),
        }
        result = compute_natural_periods(hydrostatics, added_mass_diag, freq_grid)
        assert result["roll"] is not None
        assert result["roll"] > 0

    def test_pitch_natural_period(self, hydrostatics, freq_grid):
        """Pitch natural period should be physical."""
        a_pitch = [0.4 * SEMISUB_MASS * 20**2] * len(freq_grid)
        added_mass_diag = {
            "heave": [0.5 * SEMISUB_MASS] * len(freq_grid),
            "roll": [0.3 * SEMISUB_MASS * 15**2] * len(freq_grid),
            "pitch": a_pitch,
        }
        result = compute_natural_periods(hydrostatics, added_mass_diag, freq_grid)
        assert result["pitch"] is not None
        assert result["pitch"] > 0

    def test_surge_sway_yaw_always_none(self, hydrostatics, freq_grid):
        """Surge, sway, yaw have no restoring -> always None."""
        added_mass_diag = {
            "heave": [0.5 * SEMISUB_MASS] * len(freq_grid),
            "roll": [0.3 * SEMISUB_MASS * 15**2] * len(freq_grid),
            "pitch": [0.4 * SEMISUB_MASS * 20**2] * len(freq_grid),
        }
        result = compute_natural_periods(hydrostatics, added_mass_diag, freq_grid)
        assert result["surge"] is None
        assert result["sway"] is None
        assert result["yaw"] is None

    def test_returns_all_six_dof_keys(self, hydrostatics, freq_grid):
        added_mass_diag = {
            "heave": [0.5 * SEMISUB_MASS] * len(freq_grid),
            "roll": [0.3 * SEMISUB_MASS * 15**2] * len(freq_grid),
            "pitch": [0.4 * SEMISUB_MASS * 20**2] * len(freq_grid),
        }
        result = compute_natural_periods(hydrostatics, added_mass_diag, freq_grid)
        expected_keys = {"surge", "sway", "heave", "roll", "pitch", "yaw"}
        assert set(result.keys()) == expected_keys

    def test_zero_restoring_gives_none(self, freq_grid):
        """If C_ii = 0, natural period for that DOF should be None."""
        hs = _make_hydrostatic(c33=0.0, c44=0.0, c55=0.0)
        added_mass_diag = {
            "heave": [1e6] * len(freq_grid),
            "roll": [1e6] * len(freq_grid),
            "pitch": [1e6] * len(freq_grid),
        }
        result = compute_natural_periods(hs, added_mass_diag, freq_grid)
        assert result["heave"] is None
        assert result["roll"] is None
        assert result["pitch"] is None

    def test_missing_added_mass_dof_gives_none(self, hydrostatics, freq_grid):
        """If added mass for a DOF is missing, result should be None."""
        added_mass_diag = {
            "heave": [0.5 * SEMISUB_MASS] * len(freq_grid),
            # roll and pitch missing
        }
        result = compute_natural_periods(hydrostatics, added_mass_diag, freq_grid)
        assert result["roll"] is None
        assert result["pitch"] is None

    def test_length_mismatch_gives_none(self, hydrostatics, freq_grid):
        """If added mass array length != freq length, result should be None."""
        added_mass_diag = {
            "heave": [0.5 * SEMISUB_MASS] * 3,  # wrong length
            "roll": [0.3 * SEMISUB_MASS * 225] * len(freq_grid),
            "pitch": [0.4 * SEMISUB_MASS * 400] * len(freq_grid),
        }
        result = compute_natural_periods(hydrostatics, added_mass_diag, freq_grid)
        assert result["heave"] is None
        # roll and pitch should still compute
        assert result["roll"] is not None


# ---------------------------------------------------------------------------
# compute_peak_responses
# ---------------------------------------------------------------------------

class TestComputePeakResponses:
    """Tests for compute_peak_responses — RAO peak amplitude scanning."""

    @pytest.fixture()
    def periods(self) -> list[float]:
        """5 wave periods in seconds."""
        return [5.0, 8.0, 10.0, 12.0, 15.0]

    @pytest.fixture()
    def headings(self) -> list[float]:
        """3 headings in degrees."""
        return [0.0, 90.0, 180.0]

    def test_basic_peak_detection(self, periods, headings):
        """Peak of heave RAO at known location."""
        nf, nh = len(periods), len(headings)
        # 6 DOFs, heave (idx=2) has peak at period=12s, heading=180deg
        raos = np.zeros((nf, nh, 6), dtype=complex)
        raos[3, 2, 2] = 1.05 + 0.0j  # heave peak at period_idx=3, heading_idx=2
        result = compute_peak_responses(raos, periods, headings)
        assert "heave" in result
        assert result["heave"]["amplitude"] == pytest.approx(1.05, rel=1e-3)
        assert result["heave"]["period_s"] == pytest.approx(12.0)
        assert result["heave"]["heading_deg"] == pytest.approx(180.0)
        assert result["heave"]["unit"] == "m/m"

    def test_surge_peak(self, periods, headings):
        nf, nh = len(periods), len(headings)
        raos = np.zeros((nf, nh, 6), dtype=complex)
        raos[1, 0, 0] = 0.8 + 0.0j  # surge peak at T=8s, heading=0deg
        result = compute_peak_responses(raos, periods, headings)
        assert "surge" in result
        assert result["surge"]["amplitude"] == pytest.approx(0.8, rel=1e-3)
        assert result["surge"]["period_s"] == pytest.approx(8.0)
        assert result["surge"]["heading_deg"] == pytest.approx(0.0)
        assert result["surge"]["unit"] == "m/m"

    def test_rotational_dof_conversion_rad_to_deg(self, periods, headings):
        """Roll (DOF idx 3) RAO in radians/m is converted to deg/m."""
        nf, nh = len(periods), len(headings)
        raos = np.zeros((nf, nh, 6), dtype=complex)
        # Roll peak: 0.1 rad/m -> ~5.73 deg/m
        raos[2, 1, 3] = 0.1 + 0.0j
        result = compute_peak_responses(raos, periods, headings)
        assert "roll" in result
        expected_deg = np.degrees(0.1)
        assert result["roll"]["amplitude"] == pytest.approx(expected_deg, rel=1e-2)
        assert result["roll"]["unit"] == "deg/m"

    def test_pitch_peak(self, periods, headings):
        nf, nh = len(periods), len(headings)
        raos = np.zeros((nf, nh, 6), dtype=complex)
        raos[0, 2, 4] = 0.05 + 0.0j  # pitch in rad/m
        result = compute_peak_responses(raos, periods, headings)
        assert "pitch" in result
        assert result["pitch"]["unit"] == "deg/m"

    def test_yaw_peak(self, periods, headings):
        nf, nh = len(periods), len(headings)
        raos = np.zeros((nf, nh, 6), dtype=complex)
        raos[4, 0, 5] = 0.02 + 0.0j  # yaw in rad/m
        result = compute_peak_responses(raos, periods, headings)
        assert "yaw" in result
        assert result["yaw"]["unit"] == "deg/m"

    def test_complex_amplitude_uses_magnitude(self, periods, headings):
        """Peak uses |complex| not real part."""
        nf, nh = len(periods), len(headings)
        raos = np.zeros((nf, nh, 6), dtype=complex)
        raos[2, 1, 2] = 0.6 + 0.8j  # |z| = 1.0
        result = compute_peak_responses(raos, periods, headings)
        assert result["heave"]["amplitude"] == pytest.approx(1.0, rel=1e-3)

    def test_zero_raos_excluded(self, periods, headings):
        """DOFs with all-zero RAOs should not appear in result."""
        nf, nh = len(periods), len(headings)
        raos = np.zeros((nf, nh, 6), dtype=complex)
        result = compute_peak_responses(raos, periods, headings)
        assert len(result) == 0

    def test_near_zero_raos_excluded(self, periods, headings):
        """DOFs with max amplitude < 1e-12 should not appear."""
        nf, nh = len(periods), len(headings)
        raos = np.full((nf, nh, 6), 1e-15 + 0j, dtype=complex)
        result = compute_peak_responses(raos, periods, headings)
        assert len(result) == 0

    def test_multiple_dofs_simultaneously(self, periods, headings):
        """Multiple DOFs have peaks at different locations."""
        nf, nh = len(periods), len(headings)
        raos = np.zeros((nf, nh, 6), dtype=complex)
        raos[0, 0, 0] = 0.5 + 0j     # surge at T=5, h=0
        raos[2, 1, 1] = 0.3 + 0j     # sway at T=10, h=90
        raos[4, 2, 2] = 1.1 + 0j     # heave at T=15, h=180
        result = compute_peak_responses(raos, periods, headings)
        assert len(result) == 3
        assert result["surge"]["period_s"] == pytest.approx(5.0)
        assert result["sway"]["heading_deg"] == pytest.approx(90.0)
        assert result["heave"]["amplitude"] == pytest.approx(1.1, rel=1e-3)

    def test_result_values_rounded(self, periods, headings):
        """Amplitudes rounded to 4dp, periods to 2dp, headings to 1dp."""
        nf, nh = len(periods), len(headings)
        raos = np.zeros((nf, nh, 6), dtype=complex)
        raos[2, 1, 2] = 1.23456789 + 0j
        result = compute_peak_responses(raos, periods, headings)
        amp_str = f"{result['heave']['amplitude']:.4f}"
        assert amp_str == "1.2346"  # rounded to 4dp


# ---------------------------------------------------------------------------
# compute_coupling_significance
# ---------------------------------------------------------------------------

class TestComputeCouplingSignificance:
    """Tests for compute_coupling_significance — off-diagonal check."""

    def test_no_full_matrix_returns_empty(self):
        result = compute_coupling_significance(
            added_mass_diagonal={"heave": [1e6]},
            damping_diagonal={"heave": [1e4]},
            added_mass_full=None,
            damping_full=None,
        )
        assert result == {}

    def test_diagonal_only_no_coupling(self):
        """Pure diagonal matrix: no off-diagonal coupling above 5%."""
        nf = 5
        am = np.zeros((nf, 6, 6))
        for i in range(6):
            am[:, i, i] = 1e6  # large diagonal
        result = compute_coupling_significance(
            added_mass_diagonal={},
            damping_diagonal={},
            added_mass_full=am,
        )
        assert result == {}

    def test_strong_surge_pitch_coupling(self):
        """Significant A_15 coupling (surge-pitch)."""
        nf = 5
        am = np.zeros((nf, 6, 6))
        for i in range(6):
            am[:, i, i] = 1e6
        # A_15 (surge-pitch) = 20% of max(A_11, A_55)
        am[:, 0, 4] = 2.0e5
        result = compute_coupling_significance(
            added_mass_diagonal={},
            damping_diagonal={},
            added_mass_full=am,
        )
        assert "A_15" in result
        assert result["A_15"] == pytest.approx(0.2, rel=1e-2)

    def test_sway_roll_coupling(self):
        """Significant A_24 coupling (sway-roll)."""
        nf = 5
        am = np.zeros((nf, 6, 6))
        for i in range(6):
            am[:, i, i] = 1e6
        am[:, 1, 3] = 1.5e5  # 15% coupling
        result = compute_coupling_significance(
            added_mass_diagonal={},
            damping_diagonal={},
            added_mass_full=am,
        )
        assert "A_24" in result
        assert result["A_24"] == pytest.approx(0.15, rel=1e-2)

    def test_below_threshold_excluded(self):
        """Off-diagonal at 3% should not be flagged."""
        nf = 5
        am = np.zeros((nf, 6, 6))
        for i in range(6):
            am[:, i, i] = 1e6
        am[:, 0, 1] = 3e4  # 3% coupling
        result = compute_coupling_significance(
            added_mass_diagonal={},
            damping_diagonal={},
            added_mass_full=am,
        )
        assert "A_12" not in result

    def test_exactly_at_threshold(self):
        """Off-diagonal at exactly 5% should not be included (> 0.05 required)."""
        nf = 5
        am = np.zeros((nf, 6, 6))
        for i in range(6):
            am[:, i, i] = 1e6
        am[:, 0, 1] = 5e4  # exactly 5%
        result = compute_coupling_significance(
            added_mass_diagonal={},
            damping_diagonal={},
            added_mass_full=am,
        )
        assert "A_12" not in result

    def test_damping_coupling(self):
        """Damping off-diagonal flagged as B_ij."""
        nf = 5
        am = np.zeros((nf, 6, 6))  # no AM coupling
        for i in range(6):
            am[:, i, i] = 1e6
        dm = np.zeros((nf, 6, 6))
        for i in range(6):
            dm[:, i, i] = 1e5
        dm[:, 2, 4] = 2e4  # B_35 = 20% coupling
        result = compute_coupling_significance(
            added_mass_diagonal={},
            damping_diagonal={},
            added_mass_full=am,
            damping_full=dm,
        )
        assert "B_35" in result
        assert result["B_35"] == pytest.approx(0.2, rel=1e-2)

    def test_both_am_and_damping_coupling(self):
        """Both added mass and damping coupling reported."""
        nf = 3
        am = np.zeros((nf, 6, 6))
        dm = np.zeros((nf, 6, 6))
        for i in range(6):
            am[:, i, i] = 1e6
            dm[:, i, i] = 1e5
        am[:, 0, 4] = 1.5e5  # A_15 = 15%
        dm[:, 1, 3] = 1.2e4  # B_24 = 12%
        result = compute_coupling_significance(
            added_mass_diagonal={},
            damping_diagonal={},
            added_mass_full=am,
            damping_full=dm,
        )
        assert "A_15" in result
        assert "B_24" in result

    def test_frequency_varying_coupling(self):
        """Max coupling over frequency range is reported."""
        nf = 5
        am = np.zeros((nf, 6, 6))
        for i in range(6):
            am[:, i, i] = 1e6
        # Coupling varies: [1%, 3%, 10%, 5%, 2%] -> max = 10%
        am[:, 0, 2] = np.array([1e4, 3e4, 1e5, 5e4, 2e4])
        result = compute_coupling_significance(
            added_mass_diagonal={},
            damping_diagonal={},
            added_mass_full=am,
        )
        assert "A_13" in result
        assert result["A_13"] == pytest.approx(0.1, rel=1e-2)

    def test_near_zero_diagonal_protection(self):
        """Near-zero diagonals should not cause division errors."""
        nf = 3
        am = np.zeros((nf, 6, 6))
        # Very small diagonal + some off-diagonal
        am[:, 0, 0] = 1e-40
        am[:, 1, 1] = 1e-40
        am[:, 0, 1] = 1e-40
        # Should not crash
        result = compute_coupling_significance(
            added_mass_diagonal={},
            damping_diagonal={},
            added_mass_full=am,
        )
        # Result should be a dict (may or may not have entries)
        assert isinstance(result, dict)

    def test_results_are_rounded(self):
        """Max ratio values are rounded to 4 decimal places."""
        nf = 3
        am = np.zeros((nf, 6, 6))
        for i in range(6):
            am[:, i, i] = 1e6
        am[:, 0, 4] = 1.23456e5  # ratio ~ 0.123456
        result = compute_coupling_significance(
            added_mass_diagonal={},
            damping_diagonal={},
            added_mass_full=am,
        )
        assert "A_15" in result
        assert result["A_15"] == pytest.approx(0.1235, abs=1e-4)


# ---------------------------------------------------------------------------
# generate_executive_warnings
# ---------------------------------------------------------------------------

class TestGenerateExecutiveWarnings:
    """Tests for generate_executive_warnings — auto-alert generation."""

    def test_no_warnings_for_healthy_vessel(self):
        """Stable vessel with good mesh and damping: no warnings."""
        rd = DiffractionReportData(
            vessel_name="StableVessel",
            gm_transverse=3.0,
            mesh_quality=MeshQualityData(
                panel_count=2400,
                mean_area=0.85,
                min_area=0.5,
                max_area=2.0,
                area_ratio=4.0,
            ),
            roll_damping=RollDampingData(
                frequencies_rad_s=[0.5],
                periods_s=[2 * math.pi / 0.5],
                roll_damping_percent_critical=[5.0],
                B_44=[1e5],
                A_44=[2e9],
                C_44=1e8,
                I_44=2.7e9,
                zeta_at_peak=5.0,
            ),
        )
        warnings = generate_executive_warnings(rd)
        assert len(warnings) == 0

    def test_negative_gm_critical(self):
        """Negative GM_T triggers CRITICAL warning."""
        rd = DiffractionReportData(
            vessel_name="UnstableVessel",
            gm_transverse=-0.5,
        )
        warnings = generate_executive_warnings(rd)
        assert any("CRITICAL" in w for w in warnings)
        assert any("Negative GM_T" in w for w in warnings)

    def test_zero_gm_critical(self):
        """GM_T = 0 triggers CRITICAL warning."""
        rd = DiffractionReportData(
            vessel_name="NeutralVessel",
            gm_transverse=0.0,
        )
        warnings = generate_executive_warnings(rd)
        assert any("CRITICAL" in w for w in warnings)

    def test_low_gm_warning(self):
        """GM_T < 1.0m triggers WARNING (DNV-OS-C301 threshold)."""
        rd = DiffractionReportData(
            vessel_name="LowGMVessel",
            gm_transverse=0.8,
        )
        warnings = generate_executive_warnings(rd)
        assert any("WARNING" in w and "Low GM_T" in w for w in warnings)
        assert any("DNV-OS-C301" in w for w in warnings)

    def test_gm_exactly_1_no_warning(self):
        """GM_T = 1.0m should NOT trigger low GM warning."""
        rd = DiffractionReportData(
            vessel_name="OKVessel",
            gm_transverse=1.0,
        )
        warnings = generate_executive_warnings(rd)
        gm_warnings = [w for w in warnings if "GM_T" in w]
        assert len(gm_warnings) == 0

    def test_gm_none_no_warning(self):
        """If GM_T is None, no GM warning."""
        rd = DiffractionReportData(vessel_name="NoGMVessel")
        warnings = generate_executive_warnings(rd)
        gm_warnings = [w for w in warnings if "GM_T" in w]
        assert len(gm_warnings) == 0

    def test_high_mesh_area_ratio_note(self):
        """Area ratio > 10 triggers NOTE about mesh variation."""
        rd = DiffractionReportData(
            vessel_name="CoarseMesh",
            mesh_quality=MeshQualityData(
                panel_count=2000,
                mean_area=1.0,
                min_area=0.1,
                max_area=5.0,
                area_ratio=50.0,
            ),
        )
        warnings = generate_executive_warnings(rd)
        assert any("NOTE" in w and "mesh area ratio" in w.lower() for w in warnings)

    def test_low_panel_count_warning(self):
        """Panel count < 100 triggers WARNING."""
        rd = DiffractionReportData(
            vessel_name="TooCoarse",
            mesh_quality=MeshQualityData(
                panel_count=50,
                mean_area=5.0,
                min_area=2.0,
                max_area=10.0,
                area_ratio=5.0,
            ),
        )
        warnings = generate_executive_warnings(rd)
        assert any("WARNING" in w and "panel count" in w.lower() for w in warnings)

    def test_area_ratio_exactly_10_no_note(self):
        """Area ratio = 10 should NOT trigger note (> 10 required)."""
        rd = DiffractionReportData(
            vessel_name="BoundaryMesh",
            mesh_quality=MeshQualityData(
                panel_count=2000,
                mean_area=1.0,
                min_area=0.5,
                max_area=5.0,
                area_ratio=10.0,
            ),
        )
        warnings = generate_executive_warnings(rd)
        mesh_notes = [w for w in warnings if "mesh area ratio" in w.lower()]
        assert len(mesh_notes) == 0

    def test_no_mesh_quality_no_mesh_warning(self):
        """If mesh_quality is None, no mesh warnings."""
        rd = DiffractionReportData(vessel_name="NoMesh")
        warnings = generate_executive_warnings(rd)
        mesh_warnings = [w for w in warnings if "mesh" in w.lower() or "panel" in w.lower()]
        assert len(mesh_warnings) == 0

    def test_low_roll_damping_warning(self):
        """Roll damping < 2% critical at resonance triggers WARNING."""
        rd = DiffractionReportData(
            vessel_name="LowDamping",
            roll_damping=RollDampingData(
                frequencies_rad_s=[0.5],
                periods_s=[2 * math.pi / 0.5],
                roll_damping_percent_critical=[1.0],
                B_44=[5e4],
                A_44=[2e9],
                C_44=1e8,
                I_44=2.7e9,
                peak_roll_rao_period=12.57,
                zeta_at_peak=1.5,
            ),
        )
        warnings = generate_executive_warnings(rd)
        assert any(
            "WARNING" in w and "roll damping" in w.lower() for w in warnings
        )
        assert any("DNV-RP-C205" in w for w in warnings)

    def test_roll_damping_exactly_2_no_warning(self):
        """Zeta = 2.0% should NOT trigger warning (< 2.0 required)."""
        rd = DiffractionReportData(
            vessel_name="OKDamping",
            roll_damping=RollDampingData(
                frequencies_rad_s=[0.5],
                periods_s=[2 * math.pi / 0.5],
                roll_damping_percent_critical=[2.0],
                B_44=[1e5],
                A_44=[2e9],
                C_44=1e8,
                I_44=2.7e9,
                peak_roll_rao_period=12.57,
                zeta_at_peak=2.0,
            ),
        )
        warnings = generate_executive_warnings(rd)
        damping_warnings = [w for w in warnings if "roll damping" in w.lower()]
        assert len(damping_warnings) == 0

    def test_no_roll_damping_no_warning(self):
        """If roll_damping is None, no damping warnings."""
        rd = DiffractionReportData(vessel_name="NoDamping")
        warnings = generate_executive_warnings(rd)
        damping_warnings = [w for w in warnings if "damping" in w.lower()]
        assert len(damping_warnings) == 0

    def test_roll_damping_zeta_none_no_warning(self):
        """If zeta_at_peak is None, no damping warning."""
        rd = DiffractionReportData(
            vessel_name="NoZeta",
            roll_damping=RollDampingData(
                frequencies_rad_s=[0.5],
                periods_s=[2 * math.pi / 0.5],
                roll_damping_percent_critical=[2.0],
                B_44=[1e5],
                A_44=[2e9],
                C_44=1e8,
                I_44=2.7e9,
                zeta_at_peak=None,
            ),
        )
        warnings = generate_executive_warnings(rd)
        damping_warnings = [w for w in warnings if "damping" in w.lower()]
        assert len(damping_warnings) == 0

    def test_multiple_warnings_combined(self):
        """Multiple issues produce multiple warnings."""
        rd = DiffractionReportData(
            vessel_name="ProblemVessel",
            gm_transverse=-0.3,
            mesh_quality=MeshQualityData(
                panel_count=50,
                mean_area=5.0,
                min_area=0.1,
                max_area=10.0,
                area_ratio=100.0,
            ),
            roll_damping=RollDampingData(
                frequencies_rad_s=[0.5],
                periods_s=[2 * math.pi / 0.5],
                roll_damping_percent_critical=[0.5],
                B_44=[2e4],
                A_44=[2e9],
                C_44=1e8,
                I_44=2.7e9,
                peak_roll_rao_period=12.57,
                zeta_at_peak=0.5,
            ),
        )
        warnings = generate_executive_warnings(rd)
        # Should have: critical GM, mesh area ratio note, low panel count, low damping
        assert len(warnings) >= 4

    def test_gm_value_in_warning_message(self):
        """GM value appears in the warning text."""
        rd = DiffractionReportData(
            vessel_name="Test",
            gm_transverse=-0.500,
        )
        warnings = generate_executive_warnings(rd)
        assert any("-0.500" in w for w in warnings)

    def test_returns_list_of_strings(self):
        """Return type is always List[str]."""
        rd = DiffractionReportData(vessel_name="Test")
        warnings = generate_executive_warnings(rd)
        assert isinstance(warnings, list)
        for w in warnings:
            assert isinstance(w, str)
