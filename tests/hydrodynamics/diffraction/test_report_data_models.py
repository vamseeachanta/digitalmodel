"""Tests for diffraction report data models.

ABOUTME: Comprehensive unit tests for the Pydantic data models in
report_data_models.py — HydrostaticData, RollDampingData, LoadRAOData,
MeshQualityData, DiffractionReportData, and module-level constants.
Traceability: #1767
"""

from __future__ import annotations

import math
from datetime import datetime

import pytest

from digitalmodel.hydrodynamics.diffraction.report_data_models import (
    DOF_NAMES,
    DOF_UNITS,
    LOAD_RAO_UNITS,
    DiffractionReportData,
    HydrostaticData,
    LoadRAOData,
    MeshQualityData,
    RollDampingData,
)


# ---------------------------------------------------------------------------
# Realistic fixture data — loosely based on a semi-submersible
# ---------------------------------------------------------------------------

def _make_6x6_zero() -> list[list[float]]:
    """6x6 zero matrix."""
    return [[0.0] * 6 for _ in range(6)]


def _make_restoring_matrix() -> list[list[float]]:
    """Realistic 6x6 hydrostatic restoring matrix for a semi-sub.

    Only C(3,3) heave, C(4,4) roll, C(5,5) pitch are non-zero
    for a typical vessel.
    """
    C = _make_6x6_zero()
    C[2][2] = 3.5e6   # C_33 heave restoring (N/m)
    C[3][3] = 1.2e8   # C_44 roll restoring (N·m/rad)
    C[4][4] = 2.5e9   # C_55 pitch restoring (N·m/rad)
    return C


def _make_inertia_matrix(mass: float) -> list[list[float]]:
    """Realistic 6x6 mass/inertia matrix.

    Diagonal: [M, M, M, I_44, I_55, I_66].
    r_xx ~ 15m, r_yy ~ 20m, r_zz ~ 22m for a semi-sub of ~12000 te.
    """
    I = _make_6x6_zero()
    I[0][0] = mass
    I[1][1] = mass
    I[2][2] = mass
    I[3][3] = mass * 15.0**2   # I_44 = M * r_xx^2
    I[4][4] = mass * 20.0**2   # I_55 = M * r_yy^2
    I[5][5] = mass * 22.0**2   # I_66 = M * r_zz^2
    return I


SEMISUB_MASS = 12_000_000.0  # 12,000 tonnes in kg
SEMISUB_VOLUME = 11_707.3    # displaced volume m^3 (rho=1025)


def _make_hydrostatic() -> HydrostaticData:
    return HydrostaticData(
        volume=SEMISUB_VOLUME,
        mass=SEMISUB_MASS,
        centre_of_buoyancy=[0.0, 0.0, -8.5],
        centre_of_mass=[0.0, 0.0, 5.2],
        waterplane_area=1250.0,
        Lxx=3.5e4,
        Lyy=8.0e5,
        Lxy=0.0,
        centre_of_floatation=[0.0, 0.0],
        restoring_matrix=_make_restoring_matrix(),
        inertia_matrix=_make_inertia_matrix(SEMISUB_MASS),
    )


# ---------------------------------------------------------------------------
# Constants tests
# ---------------------------------------------------------------------------

class TestConstants:
    """Verify module-level DOF and unit constants."""

    def test_dof_names_length(self):
        assert len(DOF_NAMES) == 6

    def test_dof_names_content(self):
        assert DOF_NAMES == ["surge", "sway", "heave", "roll", "pitch", "yaw"]

    def test_dof_units_length(self):
        assert len(DOF_UNITS) == 6

    def test_dof_units_translational(self):
        assert DOF_UNITS[0] == "m/m"
        assert DOF_UNITS[1] == "m/m"
        assert DOF_UNITS[2] == "m/m"

    def test_dof_units_rotational(self):
        assert DOF_UNITS[3] == "deg/m"
        assert DOF_UNITS[4] == "deg/m"
        assert DOF_UNITS[5] == "deg/m"

    def test_load_rao_units_length(self):
        assert len(LOAD_RAO_UNITS) == 6

    def test_load_rao_units_translational(self):
        assert LOAD_RAO_UNITS[0] == "N/m"
        assert LOAD_RAO_UNITS[1] == "N/m"
        assert LOAD_RAO_UNITS[2] == "N/m"

    def test_load_rao_units_rotational(self):
        assert LOAD_RAO_UNITS[3] == "N.m/m"
        assert LOAD_RAO_UNITS[4] == "N.m/m"
        assert LOAD_RAO_UNITS[5] == "N.m/m"


# ---------------------------------------------------------------------------
# HydrostaticData tests
# ---------------------------------------------------------------------------

class TestHydrostaticData:
    """HydrostaticData Pydantic model validation and field access."""

    def test_creation_with_valid_data(self):
        hs = _make_hydrostatic()
        assert hs.volume == pytest.approx(SEMISUB_VOLUME)
        assert hs.mass == pytest.approx(SEMISUB_MASS)

    def test_centre_of_buoyancy(self):
        hs = _make_hydrostatic()
        assert hs.centre_of_buoyancy == [0.0, 0.0, -8.5]

    def test_centre_of_mass(self):
        hs = _make_hydrostatic()
        assert hs.centre_of_mass == [0.0, 0.0, 5.2]

    def test_waterplane_area(self):
        hs = _make_hydrostatic()
        assert hs.waterplane_area == pytest.approx(1250.0)

    def test_waterplane_inertia(self):
        hs = _make_hydrostatic()
        assert hs.Lxx == pytest.approx(3.5e4)
        assert hs.Lyy == pytest.approx(8.0e5)
        assert hs.Lxy == pytest.approx(0.0)

    def test_centre_of_floatation(self):
        hs = _make_hydrostatic()
        assert hs.centre_of_floatation == [0.0, 0.0]

    def test_restoring_matrix_shape(self):
        hs = _make_hydrostatic()
        assert len(hs.restoring_matrix) == 6
        assert all(len(row) == 6 for row in hs.restoring_matrix)

    def test_restoring_matrix_values(self):
        hs = _make_hydrostatic()
        assert hs.restoring_matrix[2][2] == pytest.approx(3.5e6)
        assert hs.restoring_matrix[3][3] == pytest.approx(1.2e8)
        assert hs.restoring_matrix[4][4] == pytest.approx(2.5e9)
        assert hs.restoring_matrix[0][0] == pytest.approx(0.0)

    def test_inertia_matrix_shape(self):
        hs = _make_hydrostatic()
        assert len(hs.inertia_matrix) == 6
        assert all(len(row) == 6 for row in hs.inertia_matrix)

    def test_inertia_matrix_diagonal(self):
        hs = _make_hydrostatic()
        I = hs.inertia_matrix
        assert I[0][0] == pytest.approx(SEMISUB_MASS)
        assert I[3][3] == pytest.approx(SEMISUB_MASS * 15.0**2)
        assert I[4][4] == pytest.approx(SEMISUB_MASS * 20.0**2)

    def test_serialization_roundtrip(self):
        hs = _make_hydrostatic()
        data = hs.model_dump()
        hs2 = HydrostaticData(**data)
        assert hs2.volume == pytest.approx(hs.volume)
        assert hs2.mass == pytest.approx(hs.mass)
        assert hs2.centre_of_buoyancy == hs.centre_of_buoyancy

    def test_json_roundtrip(self):
        hs = _make_hydrostatic()
        json_str = hs.model_dump_json()
        hs2 = HydrostaticData.model_validate_json(json_str)
        assert hs2.volume == pytest.approx(hs.volume)


# ---------------------------------------------------------------------------
# RollDampingData tests
# ---------------------------------------------------------------------------

class TestRollDampingData:
    """RollDampingData Pydantic model validation."""

    @pytest.fixture()
    def roll_damping(self) -> RollDampingData:
        """Realistic roll damping data for a barge-like vessel."""
        freqs = [0.2, 0.4, 0.6, 0.8, 1.0]
        periods = [2 * math.pi / f for f in freqs]
        return RollDampingData(
            frequencies_rad_s=freqs,
            periods_s=periods,
            roll_damping_percent_critical=[1.5, 3.2, 5.1, 2.8, 1.1],
            B_44=[1.0e5, 3.5e5, 6.0e5, 4.0e5, 1.5e5],
            A_44=[2.7e9, 2.5e9, 2.3e9, 2.2e9, 2.1e9],
            C_44=1.2e8,
            I_44=12_000_000.0 * 15.0**2,
            peak_roll_rao_period=10.47,
            zeta_at_peak=1.8,
        )

    def test_creation(self, roll_damping: RollDampingData):
        assert len(roll_damping.frequencies_rad_s) == 5
        assert len(roll_damping.periods_s) == 5

    def test_damping_percent_critical(self, roll_damping: RollDampingData):
        assert roll_damping.roll_damping_percent_critical[2] == pytest.approx(5.1)

    def test_radiation_damping(self, roll_damping: RollDampingData):
        assert roll_damping.B_44[0] == pytest.approx(1.0e5)

    def test_added_mass(self, roll_damping: RollDampingData):
        assert roll_damping.A_44[0] == pytest.approx(2.7e9)

    def test_stiffness(self, roll_damping: RollDampingData):
        assert roll_damping.C_44 == pytest.approx(1.2e8)

    def test_inertia(self, roll_damping: RollDampingData):
        assert roll_damping.I_44 == pytest.approx(12_000_000.0 * 225.0)

    def test_peak_roll_period(self, roll_damping: RollDampingData):
        assert roll_damping.peak_roll_rao_period == pytest.approx(10.47)

    def test_zeta_at_peak(self, roll_damping: RollDampingData):
        assert roll_damping.zeta_at_peak == pytest.approx(1.8)

    def test_optional_fields_default_none(self):
        rd = RollDampingData(
            frequencies_rad_s=[0.5],
            periods_s=[2 * math.pi / 0.5],
            roll_damping_percent_critical=[2.0],
            B_44=[1e5],
            A_44=[2e9],
            C_44=1e8,
            I_44=2.7e9,
        )
        assert rd.peak_roll_rao_period is None
        assert rd.zeta_at_peak is None

    def test_serialization_roundtrip(self, roll_damping: RollDampingData):
        data = roll_damping.model_dump()
        rd2 = RollDampingData(**data)
        assert rd2.C_44 == pytest.approx(roll_damping.C_44)
        assert rd2.zeta_at_peak == pytest.approx(roll_damping.zeta_at_peak)


# ---------------------------------------------------------------------------
# LoadRAOData tests
# ---------------------------------------------------------------------------

class TestLoadRAOData:
    """LoadRAOData Pydantic model validation."""

    @pytest.fixture()
    def load_rao(self) -> LoadRAOData:
        """Realistic load RAO with 3 frequencies x 2 headings for surge and heave."""
        freqs = [0.3, 0.6, 0.9]
        periods = [2 * math.pi / f for f in freqs]
        headings = [0.0, 180.0]
        return LoadRAOData(
            method="diffraction",
            frequencies_rad_s=freqs,
            periods_s=periods,
            headings_deg=headings,
            amplitude={
                "surge": [[1.2e6, 1.5e6], [0.8e6, 1.0e6], [0.3e6, 0.5e6]],
                "heave": [[2.0e6, 2.5e6], [1.5e6, 1.8e6], [0.5e6, 0.7e6]],
            },
            phase_deg={
                "surge": [[0.0, 10.0], [15.0, 25.0], [45.0, 60.0]],
                "heave": [[0.0, 5.0], [10.0, 20.0], [30.0, 40.0]],
            },
        )

    def test_creation(self, load_rao: LoadRAOData):
        assert load_rao.method == "diffraction"

    def test_method_haskind(self):
        lr = LoadRAOData(
            method="haskind",
            frequencies_rad_s=[0.5],
            periods_s=[2 * math.pi / 0.5],
            headings_deg=[0.0],
            amplitude={"surge": [[1e6]]},
            phase_deg={"surge": [[0.0]]},
        )
        assert lr.method == "haskind"

    def test_frequencies(self, load_rao: LoadRAOData):
        assert len(load_rao.frequencies_rad_s) == 3
        assert load_rao.frequencies_rad_s[0] == pytest.approx(0.3)

    def test_periods(self, load_rao: LoadRAOData):
        assert len(load_rao.periods_s) == 3

    def test_headings(self, load_rao: LoadRAOData):
        assert load_rao.headings_deg == [0.0, 180.0]

    def test_amplitude_shape(self, load_rao: LoadRAOData):
        surge_amp = load_rao.amplitude["surge"]
        assert len(surge_amp) == 3  # nfreq
        assert len(surge_amp[0]) == 2  # nheading

    def test_amplitude_values(self, load_rao: LoadRAOData):
        assert load_rao.amplitude["surge"][0][0] == pytest.approx(1.2e6)
        assert load_rao.amplitude["heave"][0][1] == pytest.approx(2.5e6)

    def test_phase_shape(self, load_rao: LoadRAOData):
        surge_ph = load_rao.phase_deg["surge"]
        assert len(surge_ph) == 3
        assert len(surge_ph[0]) == 2

    def test_phase_values(self, load_rao: LoadRAOData):
        assert load_rao.phase_deg["surge"][2][1] == pytest.approx(60.0)

    def test_serialization_roundtrip(self, load_rao: LoadRAOData):
        data = load_rao.model_dump()
        lr2 = LoadRAOData(**data)
        assert lr2.method == load_rao.method
        assert lr2.amplitude["surge"][0][0] == pytest.approx(
            load_rao.amplitude["surge"][0][0]
        )


# ---------------------------------------------------------------------------
# MeshQualityData tests
# ---------------------------------------------------------------------------

class TestMeshQualityData:
    """MeshQualityData Pydantic model validation."""

    def test_basic_creation(self):
        mq = MeshQualityData(
            panel_count=2400,
            mean_area=0.85,
            min_area=0.12,
            max_area=3.50,
            area_ratio=29.2,
        )
        assert mq.panel_count == 2400
        assert mq.mean_area == pytest.approx(0.85)

    def test_area_ratio(self):
        mq = MeshQualityData(
            panel_count=1000,
            mean_area=1.0,
            min_area=0.5,
            max_area=5.0,
            area_ratio=10.0,
        )
        assert mq.area_ratio == pytest.approx(10.0)

    def test_vertex_count_optional(self):
        mq = MeshQualityData(
            panel_count=500,
            mean_area=2.0,
            min_area=0.5,
            max_area=4.0,
            area_ratio=8.0,
        )
        assert mq.vertex_count is None

    def test_vertex_count_set(self):
        mq = MeshQualityData(
            panel_count=500,
            mean_area=2.0,
            min_area=0.5,
            max_area=4.0,
            area_ratio=8.0,
            vertex_count=520,
        )
        assert mq.vertex_count == 520

    def test_serialization_roundtrip(self):
        mq = MeshQualityData(
            panel_count=2400,
            mean_area=0.85,
            min_area=0.12,
            max_area=3.50,
            area_ratio=29.2,
            vertex_count=2450,
        )
        data = mq.model_dump()
        mq2 = MeshQualityData(**data)
        assert mq2.panel_count == mq.panel_count
        assert mq2.vertex_count == mq.vertex_count


# ---------------------------------------------------------------------------
# DiffractionReportData tests
# ---------------------------------------------------------------------------

class TestDiffractionReportData:
    """DiffractionReportData top-level model tests."""

    def test_minimal_creation(self):
        rd = DiffractionReportData(vessel_name="TestBarge")
        assert rd.vessel_name == "TestBarge"
        assert rd.schema_version == "1.0"
        assert rd.mode == "full"

    def test_report_date_auto_populated(self):
        rd = DiffractionReportData(vessel_name="TestBarge")
        # Should be a valid datetime string
        dt = datetime.strptime(rd.report_date, "%Y-%m-%d %H:%M:%S")
        assert dt.year >= 2024

    def test_report_date_explicit(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            report_date="2025-01-15 10:30:00",
        )
        assert rd.report_date == "2025-01-15 10:30:00"

    def test_optional_fields_default_none(self):
        rd = DiffractionReportData(vessel_name="TestBarge")
        assert rd.hydrostatics is None
        assert rd.roll_damping is None
        assert rd.load_raos is None
        assert rd.mesh_quality is None
        assert rd.gm_transverse is None
        assert rd.gm_longitudinal is None
        assert rd.bm_transverse is None
        assert rd.bm_longitudinal is None
        assert rd.kb is None
        assert rd.radii_of_gyration is None
        assert rd.natural_periods is None
        assert rd.peak_responses is None
        assert rd.coupling_significance is None
        assert rd.hull_type is None
        assert rd.infinite_freq_added_mass is None
        assert rd.added_mass_diagonal is None
        assert rd.damping_diagonal is None
        assert rd.benchmark_html_sections is None
        assert rd.source_file is None
        assert rd.report_title is None
        assert rd.report_subtitle is None

    def test_list_defaults_empty(self):
        rd = DiffractionReportData(vessel_name="TestBarge")
        assert rd.solver_names == []
        assert rd.frequencies_rad_s == []
        assert rd.periods_s == []
        assert rd.headings_deg == []
        assert rd.executive_warnings == []
        assert rd.notes == []

    def test_with_hydrostatics(self):
        hs = _make_hydrostatic()
        rd = DiffractionReportData(
            vessel_name="SemiSub",
            hydrostatics=hs,
        )
        assert rd.hydrostatics is not None
        assert rd.hydrostatics.volume == pytest.approx(SEMISUB_VOLUME)

    def test_with_mesh_quality(self):
        mq = MeshQualityData(
            panel_count=2400,
            mean_area=0.85,
            min_area=0.12,
            max_area=3.50,
            area_ratio=29.2,
        )
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            mesh_quality=mq,
        )
        assert rd.mesh_quality.panel_count == 2400

    def test_stability_fields(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            gm_transverse=2.5,
            gm_longitudinal=85.0,
            bm_transverse=3.0,
            bm_longitudinal=68.0,
            kb=8.5,
        )
        assert rd.gm_transverse == pytest.approx(2.5)
        assert rd.gm_longitudinal == pytest.approx(85.0)
        assert rd.bm_transverse == pytest.approx(3.0)
        assert rd.bm_longitudinal == pytest.approx(68.0)
        assert rd.kb == pytest.approx(8.5)

    def test_radii_of_gyration(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            radii_of_gyration={"r_xx": 15.0, "r_yy": 20.0, "r_zz": 22.0},
        )
        assert rd.radii_of_gyration["r_xx"] == pytest.approx(15.0)

    def test_natural_periods(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            natural_periods={
                "surge": None,
                "sway": None,
                "heave": 18.5,
                "roll": 25.0,
                "pitch": 12.0,
                "yaw": None,
            },
        )
        assert rd.natural_periods["heave"] == pytest.approx(18.5)
        assert rd.natural_periods["surge"] is None

    def test_peak_responses(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            peak_responses={
                "heave": {
                    "amplitude": 1.05,
                    "period_s": 12.0,
                    "heading_deg": 180.0,
                    "unit": "m/m",
                },
            },
        )
        assert rd.peak_responses["heave"]["amplitude"] == pytest.approx(1.05)

    def test_coupling_significance(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            coupling_significance={"A_15": 0.12, "A_24": 0.08},
        )
        assert rd.coupling_significance["A_15"] == pytest.approx(0.12)

    def test_executive_warnings(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            executive_warnings=["WARNING: Low GM_T"],
        )
        assert len(rd.executive_warnings) == 1

    def test_solver_names(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            solver_names=["OrcaWave", "AQWA"],
        )
        assert len(rd.solver_names) == 2

    def test_mode_compact(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            mode="compact",
        )
        assert rd.mode == "compact"

    def test_frequency_heading_grids(self):
        freqs = [0.2, 0.4, 0.6, 0.8, 1.0]
        periods = [2 * math.pi / f for f in freqs]
        headings = [0.0, 45.0, 90.0, 135.0, 180.0]
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            frequencies_rad_s=freqs,
            periods_s=periods,
            headings_deg=headings,
        )
        assert len(rd.frequencies_rad_s) == 5
        assert len(rd.periods_s) == 5
        assert len(rd.headings_deg) == 5

    def test_infinite_freq_added_mass(self):
        inf_am = _make_6x6_zero()
        inf_am[0][0] = 5.0e6
        inf_am[2][2] = 8.0e6
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            infinite_freq_added_mass=inf_am,
        )
        assert rd.infinite_freq_added_mass[0][0] == pytest.approx(5.0e6)

    def test_added_mass_diagonal(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            added_mass_diagonal={
                "surge": [1e6, 1.1e6, 1.2e6],
                "heave": [2e6, 2.1e6, 2.2e6],
            },
        )
        assert len(rd.added_mass_diagonal["surge"]) == 3

    def test_damping_diagonal(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            damping_diagonal={
                "surge": [5e4, 4e4, 3e4],
                "heave": [8e4, 7e4, 6e4],
            },
        )
        assert len(rd.damping_diagonal["heave"]) == 3

    def test_notes(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            notes=["Benchmark run completed", "No anomalies detected"],
        )
        assert len(rd.notes) == 2

    def test_benchmark_html_sections(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            benchmark_html_sections={
                "rao_comparison": "<div>plot</div>",
                "damping_comparison": "<div>damp</div>",
            },
        )
        assert "rao_comparison" in rd.benchmark_html_sections

    def test_hull_type(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            hull_type="semi-submersible",
        )
        assert rd.hull_type == "semi-submersible"

    def test_full_model_serialization_roundtrip(self):
        hs = _make_hydrostatic()
        mq = MeshQualityData(
            panel_count=2400,
            mean_area=0.85,
            min_area=0.12,
            max_area=3.50,
            area_ratio=29.2,
        )
        rd = DiffractionReportData(
            vessel_name="SemiSub",
            hydrostatics=hs,
            mesh_quality=mq,
            gm_transverse=2.5,
            gm_longitudinal=85.0,
            hull_type="semi-submersible",
            executive_warnings=["WARNING: test"],
            solver_names=["OrcaWave"],
        )
        data = rd.model_dump()
        rd2 = DiffractionReportData(**data)
        assert rd2.vessel_name == rd.vessel_name
        assert rd2.hydrostatics.volume == pytest.approx(rd.hydrostatics.volume)
        assert rd2.mesh_quality.panel_count == rd.mesh_quality.panel_count
        assert rd2.gm_transverse == pytest.approx(rd.gm_transverse)
        assert len(rd2.executive_warnings) == 1

    def test_json_roundtrip(self):
        rd = DiffractionReportData(
            vessel_name="TestBarge",
            gm_transverse=2.5,
            hull_type="barge",
        )
        json_str = rd.model_dump_json()
        rd2 = DiffractionReportData.model_validate_json(json_str)
        assert rd2.vessel_name == "TestBarge"
        assert rd2.gm_transverse == pytest.approx(2.5)
