# ABOUTME: Tests for WeldAssessmentGenerator — hotspot stress, linearization, ASME checks
# ABOUTME: Verifies stress extraction commands and numerical linearization

"""Tests for weld_assessment — WeldAssessmentGenerator methods."""

import pytest
import numpy as np

from digitalmodel.ansys.weld_assessment import (
    ASMEStressLimits,
    HotspotConfig,
    LinearizedStress,
    StressClassificationLine,
    WeldAssessmentGenerator,
    WeldGeometry,
)


def _gen() -> WeldAssessmentGenerator:
    return WeldAssessmentGenerator()


# ---------------------------------------------------------------------------
# Hotspot stress extraction
# ---------------------------------------------------------------------------

class TestHotspotStressExtraction:
    def test_generates_path_command(self):
        config = HotspotConfig(plate_thickness_mm=25.0)
        weld = WeldGeometry()
        text = _gen().generate_hotspot_stress_extraction(config, weld)
        assert "PATH,HS_PATH" in text

    def test_generates_pdef_for_seqv(self):
        config = HotspotConfig()
        weld = WeldGeometry()
        text = _gen().generate_hotspot_stress_extraction(config, weld)
        assert "PDEF,SEQV_HS,S,EQV" in text

    def test_04t_10t_extrapolation_coefficients(self):
        config = HotspotConfig(extrapolation_points="0.4t_1.0t")
        weld = WeldGeometry()
        text = _gen().generate_hotspot_stress_extraction(config, weld)
        assert "1.67" in text
        assert "-0.67" in text

    def test_05t_15t_extrapolation_coefficients(self):
        config = HotspotConfig(extrapolation_points="0.5t_1.5t")
        weld = WeldGeometry()
        text = _gen().generate_hotspot_stress_extraction(config, weld)
        assert "1.50" in text
        assert "-0.50" in text

    def test_sigma_hs_calculation_command(self):
        config = HotspotConfig()
        weld = WeldGeometry()
        text = _gen().generate_hotspot_stress_extraction(config, weld)
        assert "SIGMA_HS" in text


# ---------------------------------------------------------------------------
# Stress linearization
# ---------------------------------------------------------------------------

class TestStressLinearization:
    def test_generates_path_for_scl(self):
        scl = StressClassificationLine(scl_id="SCL_1")
        text = _gen().generate_stress_linearization(scl)
        assert "PATH,SCL_1" in text

    def test_generates_prsect(self):
        scl = StressClassificationLine(scl_id="SCL_1")
        text = _gen().generate_stress_linearization(scl)
        assert "PRSECT" in text

    def test_maps_all_stress_components(self):
        scl = StressClassificationLine(scl_id="SCL_1")
        text = _gen().generate_stress_linearization(scl)
        assert "PDEF,SX_SCL_1,S,X" in text
        assert "PDEF,SY_SCL_1,S,Y" in text
        assert "PDEF,SZ_SCL_1,S,Z" in text

    def test_gets_membrane_and_bending(self):
        scl = StressClassificationLine(scl_id="SCL_TEST")
        text = _gen().generate_stress_linearization(scl)
        assert "MEMBRANE" in text
        assert "BENDING" in text


# ---------------------------------------------------------------------------
# Numerical linearization
# ---------------------------------------------------------------------------

class TestCalculateLinearizedStresses:
    def test_uniform_stress_gives_pure_membrane(self):
        """Uniform 100 MPa through thickness = 100 MPa membrane, 0 bending."""
        stress = [100.0] * 20
        result = _gen().calculate_linearized_stresses(stress, 25.0)
        assert result.membrane_stress_mpa == pytest.approx(100.0, rel=0.01)
        assert result.bending_stress_mpa == pytest.approx(0.0, abs=0.5)

    def test_linear_gradient_gives_bending(self):
        """Linear stress from 0 to 200 MPa: membrane=100, bending=100."""
        stress = list(np.linspace(0, 200, 50))
        result = _gen().calculate_linearized_stresses(stress, 25.0)
        assert result.membrane_stress_mpa == pytest.approx(100.0, rel=0.01)
        assert result.bending_stress_mpa == pytest.approx(100.0, rel=0.05)

    def test_empty_stress_returns_zeros(self):
        result = _gen().calculate_linearized_stresses([], 25.0)
        assert result.membrane_stress_mpa == 0.0
        assert result.bending_stress_mpa == 0.0

    def test_returns_linearized_stress_dataclass(self):
        stress = [100.0] * 10
        result = _gen().calculate_linearized_stresses(stress, 25.0)
        assert isinstance(result, LinearizedStress)


# ---------------------------------------------------------------------------
# Weld toe SCF
# ---------------------------------------------------------------------------

class TestWeldToeSCF:
    def test_generates_scf_parameter(self):
        weld = WeldGeometry(plate_thickness_mm=25.0, weld_toe_angle_deg=45.0)
        text = _gen().generate_weld_toe_scf(weld)
        assert "SCF_K" in text

    def test_includes_monahan_equation(self):
        weld = WeldGeometry()
        text = _gen().generate_weld_toe_scf(weld)
        assert "Monahan" in text

    def test_reports_scf_value(self):
        weld = WeldGeometry()
        text = _gen().generate_weld_toe_scf(weld)
        assert "*MSG,INFO,SCF_K" in text


# ---------------------------------------------------------------------------
# ASME stress check
# ---------------------------------------------------------------------------

class TestASMEStressCheck:
    def test_generates_pm_check(self):
        scl = StressClassificationLine(scl_id="SCL_1")
        limits = ASMEStressLimits(Sm=138.0)
        text = _gen().generate_asme_stress_check(scl, limits)
        assert "PM_RATIO" in text
        assert "138.0" in text

    def test_generates_plpb_check(self):
        scl = StressClassificationLine(scl_id="SCL_1")
        limits = ASMEStressLimits()
        text = _gen().generate_asme_stress_check(scl, limits)
        assert "PLPb_RATIO" in text

    def test_generates_conditional_warning(self):
        scl = StressClassificationLine(scl_id="SCL_1")
        limits = ASMEStressLimits()
        text = _gen().generate_asme_stress_check(scl, limits)
        assert "*IF," in text
        assert "WARN" in text
