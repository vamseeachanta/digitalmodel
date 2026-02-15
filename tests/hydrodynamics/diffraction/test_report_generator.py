#!/usr/bin/env python3
"""Unit tests for diffraction report generator computation functions.

Tests the new Phase 1 computation functions added as part of WRK-130
(Diffraction Report Content & Flow Redesign).
"""

import math
from typing import Dict, List

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.report_generator import (
    DOF_NAMES,
    DiffractionReportData,
    HydrostaticData,
    MeshQualityData,
    RollDampingData,
    compute_coupling_significance,
    compute_natural_periods,
    compute_peak_responses,
    compute_radii_of_gyration,
    compute_stability,
    generate_executive_warnings,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


def _make_hydrostatics(
    volume: float = 16000.0,
    mass: float = 16400000.0,
    c44: float = 1.608e9,
    c55: float = 2.5e10,
    lxx: float = 266666.667,
    lyy: float = 6666666.667,
    cob_z: float = -4.0,
    i44: float = 1.0e9,
    i55: float = 1.5e10,
    i66: float = 1.4e10,
) -> HydrostaticData:
    """Create a test HydrostaticData for a 100m x 20m x 8m barge."""
    C = [[0.0] * 6 for _ in range(6)]
    C[2][2] = 1025.0 * 9.81 * 2000.0  # Awp * rho * g (heave)
    C[3][3] = c44
    C[4][4] = c55

    I = [[0.0] * 6 for _ in range(6)]
    I[0][0] = mass
    I[1][1] = mass
    I[2][2] = mass
    I[3][3] = i44
    I[4][4] = i55
    I[5][5] = i66

    return HydrostaticData(
        volume=volume,
        mass=mass,
        centre_of_buoyancy=[0.0, 0.0, cob_z],
        centre_of_mass=[0.0, 0.0, -3.0],
        waterplane_area=2000.0,
        Lxx=lxx,
        Lyy=lyy,
        Lxy=0.0,
        centre_of_floatation=[0.0, 0.0],
        restoring_matrix=C,
        inertia_matrix=I,
    )


def _make_added_mass_diagonal(
    nfreq: int = 50,
    base_values: Dict[str, float] | None = None,
) -> Dict[str, List[float]]:
    """Create test added mass diagonal arrays."""
    if base_values is None:
        base_values = {
            "surge": 1e6, "sway": 1e6, "heave": 2e6,
            "roll": 5e8, "pitch": 8e9, "yaw": 7e9,
        }
    result: Dict[str, List[float]] = {}
    for dof in DOF_NAMES:
        base = base_values.get(dof, 1e6)
        # Slight frequency variation -- added mass decreases with frequency
        result[dof] = [base * (1.0 + 0.3 * (1 - i / nfreq)) for i in range(nfreq)]
    return result


# ---------------------------------------------------------------------------
# Tests: compute_stability
# ---------------------------------------------------------------------------


class TestComputeStability:
    def test_positive_gm(self):
        hs = _make_hydrostatics()
        result = compute_stability(hs)

        assert result["gm_transverse"] is not None
        assert result["gm_transverse"] > 0
        # GM_T = C44 / (rho * g * V) = 1.608e9 / (1025 * 9.81 * 16000)
        expected_gmt = 1.608e9 / (1025.0 * 9.81 * 16000.0)
        assert abs(result["gm_transverse"] - expected_gmt) < 0.01

    def test_gm_longitudinal(self):
        hs = _make_hydrostatics()
        result = compute_stability(hs)
        expected_gml = 2.5e10 / (1025.0 * 9.81 * 16000.0)
        assert result["gm_longitudinal"] is not None
        assert abs(result["gm_longitudinal"] - expected_gml) < 0.1

    def test_bm_transverse(self):
        hs = _make_hydrostatics()
        result = compute_stability(hs)
        expected_bmt = 266666.667 / 16000.0
        assert result["bm_transverse"] is not None
        assert abs(result["bm_transverse"] - expected_bmt) < 0.01

    def test_bm_longitudinal(self):
        hs = _make_hydrostatics()
        result = compute_stability(hs)
        expected_bml = 6666666.667 / 16000.0
        assert result["bm_longitudinal"] is not None
        assert abs(result["bm_longitudinal"] - expected_bml) < 0.1

    def test_kb(self):
        hs = _make_hydrostatics()
        result = compute_stability(hs)
        assert result["kb"] == -4.0

    def test_zero_volume_returns_none(self):
        hs = _make_hydrostatics(volume=0.0)
        result = compute_stability(hs)
        assert result["gm_transverse"] is None
        assert result["bm_transverse"] is None


# ---------------------------------------------------------------------------
# Tests: compute_radii_of_gyration
# ---------------------------------------------------------------------------


class TestComputeRadiiOfGyration:
    def test_basic(self):
        hs = _make_hydrostatics(mass=16400000.0, i44=1e9, i55=1.5e10, i66=1.4e10)
        result = compute_radii_of_gyration(hs)
        assert "r_xx" in result
        assert "r_yy" in result
        assert "r_zz" in result

        expected_rxx = math.sqrt(1e9 / 16400000.0)
        assert abs(result["r_xx"] - expected_rxx) < 0.01

    def test_zero_mass(self):
        hs = _make_hydrostatics(mass=0.0)
        result = compute_radii_of_gyration(hs)
        assert result == {}


# ---------------------------------------------------------------------------
# Tests: compute_natural_periods
# ---------------------------------------------------------------------------


class TestComputeNaturalPeriods:
    def test_heave_roll_pitch_computed(self):
        hs = _make_hydrostatics()
        nfreq = 50
        freq = np.linspace(0.1, 3.0, nfreq).tolist()
        am_diag = _make_added_mass_diagonal(nfreq)
        result = compute_natural_periods(hs, am_diag, freq)

        # Should have values for heave, roll, pitch
        assert result["heave"] is not None
        assert result["roll"] is not None
        assert result["pitch"] is not None

        # Natural periods should be positive and reasonable
        assert result["heave"] > 0
        assert result["roll"] > 0
        assert result["pitch"] > 0

    def test_surge_sway_yaw_are_none(self):
        hs = _make_hydrostatics()
        nfreq = 50
        freq = np.linspace(0.1, 3.0, nfreq).tolist()
        am_diag = _make_added_mass_diagonal(nfreq)
        result = compute_natural_periods(hs, am_diag, freq)

        assert result["surge"] is None
        assert result["sway"] is None
        assert result["yaw"] is None

    def test_zero_stiffness_returns_none(self):
        hs = _make_hydrostatics(c44=0.0, c55=0.0)
        nfreq = 50
        freq = np.linspace(0.1, 3.0, nfreq).tolist()
        am_diag = _make_added_mass_diagonal(nfreq)
        result = compute_natural_periods(hs, am_diag, freq)

        assert result["roll"] is None
        assert result["pitch"] is None


# ---------------------------------------------------------------------------
# Tests: compute_peak_responses
# ---------------------------------------------------------------------------


class TestComputePeakResponses:
    def test_finds_peak_per_dof(self):
        """Peak should be identified at correct period and heading."""
        nfreq, nhead = 10, 3
        raw = np.zeros((nfreq, nhead, 6), dtype=complex)
        periods = np.linspace(5.0, 20.0, nfreq).tolist()
        headings = [0.0, 90.0, 180.0]

        # Plant a peak in surge at freq_idx=4, heading_idx=2
        raw[4, 2, 0] = 2.5 + 0j
        # Plant a peak in roll at freq_idx=7, heading_idx=1 (in radians)
        raw[7, 1, 3] = 0.05 + 0j  # 0.05 rad/m ≈ 2.86 deg/m

        result = compute_peak_responses(raw, periods, headings)

        assert "surge" in result
        assert abs(result["surge"]["period_s"] - periods[4]) < 0.01
        assert result["surge"]["heading_deg"] == 180.0
        assert abs(result["surge"]["amplitude"] - 2.5) < 0.01
        assert result["surge"]["unit"] == "m/m"

        assert "roll" in result
        assert abs(result["roll"]["period_s"] - periods[7]) < 0.01
        assert result["roll"]["heading_deg"] == 90.0
        assert abs(result["roll"]["amplitude"] - np.degrees(0.05)) < 0.01
        assert result["roll"]["unit"] == "deg/m"

    def test_skips_zero_amplitude_dofs(self):
        """DOFs with near-zero RAOs should be omitted."""
        nfreq, nhead = 5, 2
        raw = np.zeros((nfreq, nhead, 6), dtype=complex)
        periods = np.linspace(5.0, 15.0, nfreq).tolist()
        headings = [0.0, 180.0]

        # Only plant a peak in heave
        raw[2, 0, 2] = 1.0 + 0j

        result = compute_peak_responses(raw, periods, headings)

        assert "heave" in result
        assert "surge" not in result
        assert "sway" not in result


# ---------------------------------------------------------------------------
# Tests: compute_coupling_significance
# ---------------------------------------------------------------------------


class TestComputeCouplingSignificance:
    def test_no_coupling_for_symmetric(self):
        """Symmetric body: off-diagonal should be zero."""
        nfreq = 10
        # Build diagonal-only added mass array
        am_full = np.zeros((nfreq, 6, 6))
        for i in range(6):
            am_full[:, i, i] = 1e6 * (i + 1)

        am_diag = {dof: am_full[:, i, i].tolist() for i, dof in enumerate(DOF_NAMES)}
        dm_diag = {dof: [0.0] * nfreq for dof in DOF_NAMES}

        result = compute_coupling_significance(
            am_diag, dm_diag, added_mass_full=am_full,
        )
        assert len(result) == 0

    def test_significant_coupling_detected(self):
        """Body with surge-pitch coupling above threshold."""
        nfreq = 10
        am_full = np.zeros((nfreq, 6, 6))
        for i in range(6):
            am_full[:, i, i] = 1e6

        # Add 10% surge-pitch coupling (above 5% threshold)
        am_full[:, 0, 4] = 1e5  # A_15
        am_full[:, 4, 0] = 1e5  # A_51

        am_diag = {dof: am_full[:, i, i].tolist() for i, dof in enumerate(DOF_NAMES)}
        dm_diag = {dof: [0.0] * nfreq for dof in DOF_NAMES}

        result = compute_coupling_significance(
            am_diag, dm_diag, added_mass_full=am_full,
        )
        assert "A_15" in result
        assert result["A_15"] >= 0.05


# ---------------------------------------------------------------------------
# Tests: generate_executive_warnings
# ---------------------------------------------------------------------------


class TestGenerateExecutiveWarnings:
    def test_no_warnings_for_healthy_vessel(self):
        data = DiffractionReportData(
            vessel_name="test",
            gm_transverse=5.0,
            mesh_quality=MeshQualityData(
                panel_count=500, mean_area=1.0,
                min_area=0.5, max_area=2.0, area_ratio=4.0,
            ),
            roll_damping=RollDampingData(
                frequencies_rad_s=[1.0], periods_s=[6.28],
                roll_damping_percent_critical=[5.0],
                B_44=[1e6], A_44=[1e7], C_44=1e9, I_44=1e9,
                zeta_at_peak=5.0,
            ),
        )
        warnings = generate_executive_warnings(data)
        assert len(warnings) == 0

    def test_negative_gm_warning(self):
        data = DiffractionReportData(
            vessel_name="test",
            gm_transverse=-0.5,
        )
        warnings = generate_executive_warnings(data)
        assert any("CRITICAL" in w and "Negative GM_T" in w for w in warnings)

    def test_low_gm_warning(self):
        data = DiffractionReportData(
            vessel_name="test",
            gm_transverse=0.5,
        )
        warnings = generate_executive_warnings(data)
        assert any("WARNING" in w and "Low GM_T" in w for w in warnings)

    def test_high_mesh_area_ratio_warning(self):
        data = DiffractionReportData(
            vessel_name="test",
            mesh_quality=MeshQualityData(
                panel_count=500, mean_area=1.0,
                min_area=0.1, max_area=5.0, area_ratio=50.0,
            ),
        )
        warnings = generate_executive_warnings(data)
        assert any("area ratio" in w for w in warnings)

    def test_low_panel_count_warning(self):
        data = DiffractionReportData(
            vessel_name="test",
            mesh_quality=MeshQualityData(
                panel_count=50, mean_area=1.0,
                min_area=0.5, max_area=2.0, area_ratio=4.0,
            ),
        )
        warnings = generate_executive_warnings(data)
        assert any("panel count" in w.lower() for w in warnings)

    def test_low_roll_damping_warning(self):
        data = DiffractionReportData(
            vessel_name="test",
            roll_damping=RollDampingData(
                frequencies_rad_s=[1.0], periods_s=[6.28],
                roll_damping_percent_critical=[0.5],
                B_44=[1e4], A_44=[1e7], C_44=1e9, I_44=1e9,
                zeta_at_peak=0.5,
            ),
        )
        warnings = generate_executive_warnings(data)
        assert any("roll damping" in w.lower() for w in warnings)
