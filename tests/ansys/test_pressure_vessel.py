# ABOUTME: Tests for PressureVesselGenerator and ASMEThicknessCalc
# ABOUTME: Verifies ASME UG-27/UG-32 calculations and APDL command generation

"""Tests for pressure_vessel — PressureVesselGenerator and ASMEThicknessCalc."""

import pytest

from digitalmodel.ansys.pressure_vessel import (
    ASMEThicknessCalc,
    DesignConditions,
    NozzleConfig,
    NozzleLoad,
    PressureVesselGenerator,
    VesselGeometry,
    WindSeismicLoad,
)


def _calc() -> ASMEThicknessCalc:
    return ASMEThicknessCalc()


def _gen() -> PressureVesselGenerator:
    return PressureVesselGenerator()


# ---------------------------------------------------------------------------
# ASME thickness calculations - UG-27
# ---------------------------------------------------------------------------

class TestUG27Cylindrical:
    def test_returns_positive_thickness(self):
        result = _calc().ug27_cylindrical_shell(
            pressure_mpa=10.0, inner_radius_mm=750.0,
            allowable_stress_mpa=138.0, joint_efficiency=1.0,
        )
        assert result["t_required_mm"] > 0

    def test_higher_pressure_needs_thicker_shell(self):
        r1 = _calc().ug27_cylindrical_shell(
            pressure_mpa=5.0, inner_radius_mm=750.0,
            allowable_stress_mpa=138.0,
        )
        r2 = _calc().ug27_cylindrical_shell(
            pressure_mpa=10.0, inner_radius_mm=750.0,
            allowable_stress_mpa=138.0,
        )
        assert r2["t_required_mm"] > r1["t_required_mm"]

    def test_corrosion_allowance_added(self):
        result = _calc().ug27_cylindrical_shell(
            pressure_mpa=10.0, inner_radius_mm=750.0,
            allowable_stress_mpa=138.0, corrosion_allowance_mm=3.0,
        )
        assert result["t_with_ca_mm"] == pytest.approx(
            result["t_required_mm"] + 3.0
        )

    def test_formula_string_present(self):
        result = _calc().ug27_cylindrical_shell(
            pressure_mpa=10.0, inner_radius_mm=750.0,
            allowable_stress_mpa=138.0,
        )
        assert "UG-27" in result["formula"]

    def test_known_value(self):
        """Verify against known hand calculation.
        P=10 MPa, R=750 mm, S=138 MPa, E=1.0
        t = (10 * 750) / (138 * 1.0 - 0.6 * 10) = 7500 / 132 = 56.82 mm
        """
        result = _calc().ug27_cylindrical_shell(
            pressure_mpa=10.0, inner_radius_mm=750.0,
            allowable_stress_mpa=138.0, joint_efficiency=1.0,
            corrosion_allowance_mm=0.0,
        )
        assert result["t_required_mm"] == pytest.approx(56.818, rel=0.01)


# ---------------------------------------------------------------------------
# ASME thickness calculations - UG-32
# ---------------------------------------------------------------------------

class TestUG32Heads:
    def test_ellipsoidal_returns_positive(self):
        result = _calc().ug32_ellipsoidal_head(
            pressure_mpa=10.0, inner_diameter_mm=1500.0,
            allowable_stress_mpa=138.0,
        )
        assert result["t_required_mm"] > 0

    def test_hemispherical_thinner_than_cylindrical(self):
        """Hemispherical heads are approximately half the thickness of shells."""
        t_shell = _calc().ug27_cylindrical_shell(
            pressure_mpa=10.0, inner_radius_mm=750.0,
            allowable_stress_mpa=138.0, corrosion_allowance_mm=0.0,
        )["t_required_mm"]
        t_hemi = _calc().ug32_hemispherical_head(
            pressure_mpa=10.0, inner_radius_mm=750.0,
            allowable_stress_mpa=138.0, corrosion_allowance_mm=0.0,
        )["t_required_mm"]
        assert t_hemi < t_shell

    def test_ellipsoidal_k_factor_is_one(self):
        """For 2:1 ellipsoidal, K = 1.0."""
        result = _calc().ug32_ellipsoidal_head(
            pressure_mpa=10.0, inner_diameter_mm=1500.0,
            allowable_stress_mpa=138.0, aspect_ratio=2.0,
        )
        assert result["K_factor"] == pytest.approx(1.0)


# ---------------------------------------------------------------------------
# Vessel geometry generation
# ---------------------------------------------------------------------------

class TestVesselGeometry:
    def test_generates_cyl4_command(self):
        geom = VesselGeometry()
        text = _gen().generate_vessel_geometry(geom)
        assert "CYL4" in text

    def test_includes_prep7(self):
        geom = VesselGeometry()
        text = _gen().generate_vessel_geometry(geom)
        assert "/PREP7" in text

    def test_nozzle_geometry_included(self):
        nzl = NozzleConfig(nozzle_id="N1", outer_diameter_mm=168.3)
        geom = VesselGeometry(nozzle_locations=[nzl])
        text = _gen().generate_vessel_geometry(geom)
        assert "Nozzle N1" in text


# ---------------------------------------------------------------------------
# Pressure loading
# ---------------------------------------------------------------------------

class TestInternalPressure:
    def test_generates_sfa_command(self):
        cond = DesignConditions(design_pressure_mpa=10.0)
        geom = VesselGeometry()
        text = _gen().generate_internal_pressure(cond, geom)
        assert "SFA,ALL,1,PRES,10.0" in text

    def test_static_analysis(self):
        cond = DesignConditions()
        geom = VesselGeometry()
        text = _gen().generate_internal_pressure(cond, geom)
        assert "ANTYPE,0" in text

    def test_nlgeom_enabled(self):
        cond = DesignConditions()
        geom = VesselGeometry()
        text = _gen().generate_internal_pressure(cond, geom)
        assert "NLGEOM,ON" in text


# ---------------------------------------------------------------------------
# Wind and seismic loads
# ---------------------------------------------------------------------------

class TestWindSeismicLoads:
    def test_wind_generates_sfa(self):
        load = WindSeismicLoad(wind_speed_ms=44.0)
        geom = VesselGeometry()
        text = _gen().generate_wind_load(load, geom)
        assert "SFA" in text

    def test_seismic_generates_acel(self):
        load = WindSeismicLoad(seismic_g_horizontal=0.15)
        geom = VesselGeometry()
        text = _gen().generate_seismic_load(load, geom)
        assert "ACEL" in text

    def test_hydrotest_pressure_is_1_3x_design(self):
        cond = DesignConditions(design_pressure_mpa=10.0)
        text = _gen().generate_hydrotest_load(cond)
        assert "13.00" in text  # 1.3 * 10.0


# ---------------------------------------------------------------------------
# Stress classification lines
# ---------------------------------------------------------------------------

class TestStressClassificationPaths:
    def test_generates_path_command(self):
        geom = VesselGeometry()
        text = _gen().generate_stress_classification_paths(geom)
        assert "PATH," in text

    def test_generates_prsect(self):
        geom = VesselGeometry()
        text = _gen().generate_stress_classification_paths(geom)
        assert "PRSECT" in text

    def test_nozzle_scls_included(self):
        nzl = NozzleConfig(nozzle_id="N1")
        geom = VesselGeometry(nozzle_locations=[nzl])
        text = _gen().generate_stress_classification_paths(geom)
        assert "SCL_N1" in text
