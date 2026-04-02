# ABOUTME: Tests for PressureVesselGenerator and ASMEThicknessCalc
# ABOUTME: Verifies ASME UG-27/UG-32 calculations, MAWP, nozzle reinforcement, and APDL generation

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
# MAWP calculation (NEW)
# ---------------------------------------------------------------------------

class TestMAWP:
    def test_mawp_returns_positive_pressure(self):
        """MAWP for a valid vessel must be positive."""
        mawp = _calc().calculate_mawp(
            wall_thickness_mm=60.0, inner_radius_mm=750.0,
            allowable_stress_mpa=138.0, joint_efficiency=1.0,
            corrosion_allowance_mm=3.0,
        )
        assert mawp > 0

    def test_mawp_known_value(self):
        """MAWP = S*E*(t-CA) / (R + 0.6*(t-CA))
        S=138, E=1.0, t=60, CA=3, R=750
        t_corr = 57
        MAWP = 138*1.0*57 / (750 + 0.6*57) = 7866 / 784.2 = 10.03 MPa
        """
        mawp = _calc().calculate_mawp(
            wall_thickness_mm=60.0, inner_radius_mm=750.0,
            allowable_stress_mpa=138.0, joint_efficiency=1.0,
            corrosion_allowance_mm=3.0,
        )
        assert mawp == pytest.approx(10.03, rel=0.01)

    def test_mawp_increases_with_thickness(self):
        mawp1 = _calc().calculate_mawp(
            wall_thickness_mm=30.0, inner_radius_mm=750.0,
            allowable_stress_mpa=138.0,
        )
        mawp2 = _calc().calculate_mawp(
            wall_thickness_mm=60.0, inner_radius_mm=750.0,
            allowable_stress_mpa=138.0,
        )
        assert mawp2 > mawp1

    def test_mawp_zero_thickness_after_ca_raises(self):
        """If wall_thickness <= corrosion_allowance, should raise ValueError."""
        with pytest.raises(ValueError, match="corroded thickness"):
            _calc().calculate_mawp(
                wall_thickness_mm=3.0, inner_radius_mm=750.0,
                allowable_stress_mpa=138.0, corrosion_allowance_mm=3.0,
            )


# ---------------------------------------------------------------------------
# Nozzle reinforcement check (NEW)
# ---------------------------------------------------------------------------

class TestNozzleReinforcement:
    def test_returns_dict_with_required_keys(self):
        result = _calc().check_nozzle_reinforcement(
            shell_inner_radius_mm=750.0,
            shell_thickness_mm=60.0,
            nozzle_outer_diameter_mm=168.3,
            nozzle_thickness_mm=10.97,
            design_pressure_mpa=10.0,
            allowable_stress_mpa=138.0,
        )
        assert "area_required_mm2" in result
        assert "area_available_mm2" in result
        assert "is_adequate" in result

    def test_thick_shell_passes_reinforcement(self):
        """A thick shell with a small nozzle at low pressure should pass."""
        result = _calc().check_nozzle_reinforcement(
            shell_inner_radius_mm=750.0,
            shell_thickness_mm=80.0,  # very generous thickness
            nozzle_outer_diameter_mm=168.3,
            nozzle_thickness_mm=18.0,  # thick nozzle too
            design_pressure_mpa=5.0,  # moderate pressure
            allowable_stress_mpa=138.0,
        )
        assert result["is_adequate"] is True

    def test_thin_shell_large_nozzle_may_fail(self):
        """Thin shell with large nozzle may need reinforcement."""
        result = _calc().check_nozzle_reinforcement(
            shell_inner_radius_mm=750.0,
            shell_thickness_mm=20.0,  # very thin
            nozzle_outer_diameter_mm=406.4,  # 16 inch nozzle
            nozzle_thickness_mm=9.53,
            design_pressure_mpa=10.0,
            allowable_stress_mpa=138.0,
        )
        # Just verify it returns a dict - pass/fail depends on geometry
        assert isinstance(result["area_required_mm2"], float)
        assert isinstance(result["area_available_mm2"], float)

    def test_reinforcement_pad_adds_area(self):
        """Adding a reinforcement pad should increase available area."""
        r1 = _calc().check_nozzle_reinforcement(
            shell_inner_radius_mm=750.0,
            shell_thickness_mm=30.0,
            nozzle_outer_diameter_mm=168.3,
            nozzle_thickness_mm=10.97,
            design_pressure_mpa=10.0,
            allowable_stress_mpa=138.0,
            reinforcement_pad_thickness_mm=0.0,
        )
        r2 = _calc().check_nozzle_reinforcement(
            shell_inner_radius_mm=750.0,
            shell_thickness_mm=30.0,
            nozzle_outer_diameter_mm=168.3,
            nozzle_thickness_mm=10.97,
            design_pressure_mpa=10.0,
            allowable_stress_mpa=138.0,
            reinforcement_pad_thickness_mm=15.0,
        )
        assert r2["area_available_mm2"] > r1["area_available_mm2"]


# ---------------------------------------------------------------------------
# Hydrostatic test pressure (NEW)
# ---------------------------------------------------------------------------

class TestHydrostaticTestPressure:
    def test_default_is_1_3_times_design(self):
        p = _calc().calculate_hydrotest_pressure(
            design_pressure_mpa=10.0,
        )
        assert p == pytest.approx(13.0)

    def test_custom_factor(self):
        p = _calc().calculate_hydrotest_pressure(
            design_pressure_mpa=10.0, test_factor=1.5,
        )
        assert p == pytest.approx(15.0)

    def test_stress_ratio_adjustment(self):
        """Per UG-99(b), Pt = 1.3 * P * (St/S) where St is allowable at test temp."""
        p = _calc().calculate_hydrotest_pressure(
            design_pressure_mpa=10.0,
            allowable_design_mpa=138.0,
            allowable_test_mpa=160.0,
        )
        expected = 1.3 * 10.0 * (160.0 / 138.0)
        assert p == pytest.approx(expected, rel=0.01)


# ---------------------------------------------------------------------------
# APDL generation for pressure vessel (NEW)
# ---------------------------------------------------------------------------

class TestGeneratePVApdl:
    def test_generates_complete_apdl(self):
        """generate_pv_apdl should produce a complete APDL macro string."""
        geom = VesselGeometry()
        cond = DesignConditions()
        text = _gen().generate_pv_apdl(geom, cond)
        assert isinstance(text, str)
        assert len(text) > 100

    def test_apdl_contains_geometry_section(self):
        geom = VesselGeometry()
        cond = DesignConditions()
        text = _gen().generate_pv_apdl(geom, cond)
        assert "/PREP7" in text

    def test_apdl_contains_pressure_loading(self):
        geom = VesselGeometry()
        cond = DesignConditions(design_pressure_mpa=15.0)
        text = _gen().generate_pv_apdl(geom, cond)
        assert "15.0" in text
        assert "SFA" in text

    def test_apdl_contains_solve(self):
        geom = VesselGeometry()
        cond = DesignConditions()
        text = _gen().generate_pv_apdl(geom, cond)
        assert "/SOLU" in text


# ---------------------------------------------------------------------------
# Vessel geometry generation (existing)
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
# Pressure loading (existing)
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
# Wind and seismic loads (existing)
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
# Stress classification lines (existing)
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
