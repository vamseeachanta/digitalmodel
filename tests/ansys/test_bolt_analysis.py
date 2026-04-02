# ABOUTME: Tests for BoltAnalysisGenerator — bolt pretension, flange, gasket APDL
# ABOUTME: Verifies bolt circle geometry, pretension commands, gasket elements

"""Tests for bolt_analysis — BoltAnalysisGenerator command generation."""

import math

import pytest

from digitalmodel.ansys.bolt_analysis import (
    BoltAnalysisGenerator,
    BoltCircleConfig,
    BoltConfig,
    FlangeConfig,
    GasketConfig,
)


def _gen() -> BoltAnalysisGenerator:
    return BoltAnalysisGenerator()


# ---------------------------------------------------------------------------
# Bolt circle geometry
# ---------------------------------------------------------------------------

class TestBoltCircleGeometry:
    def test_generates_keypoints_for_all_bolts(self):
        circle = BoltCircleConfig(num_bolts=8)
        text = _gen().generate_bolt_circle_geometry(circle)
        # Each bolt gets a K command for base and top
        assert text.count("\nK,") >= 16  # 8 bolts * 2 keypoints

    def test_generates_line_commands(self):
        circle = BoltCircleConfig(num_bolts=4)
        text = _gen().generate_bolt_circle_geometry(circle)
        assert "L," in text

    def test_bolt_angles_correct_for_4_bolts(self):
        circle = BoltCircleConfig(num_bolts=4, start_angle_deg=0.0)
        text = _gen().generate_bolt_circle_geometry(circle)
        assert "0.0 degrees" in text
        assert "90.0 degrees" in text
        assert "180.0 degrees" in text
        assert "270.0 degrees" in text

    def test_bolt_positions_on_pcd(self):
        """Verify bolt keypoints lie on the bolt circle diameter."""
        circle = BoltCircleConfig(
            num_bolts=1,
            bolt_circle_diameter_mm=500.0,
            start_angle_deg=0.0,
        )
        text = _gen().generate_bolt_circle_geometry(circle)
        # First bolt at 0 degrees: x=250, y=0
        assert "250.0000" in text


# ---------------------------------------------------------------------------
# Bolt pretension
# ---------------------------------------------------------------------------

class TestBoltPretension:
    def test_generates_prets179_element(self):
        circle = BoltCircleConfig(num_bolts=4)
        text = _gen().generate_bolt_pretension(circle)
        assert "ET,10,PRETS179" in text

    def test_generates_sload_commands(self):
        circle = BoltCircleConfig(
            num_bolts=4,
            bolt=BoltConfig(pretension_force_n=100000.0),
        )
        text = _gen().generate_bolt_pretension(circle)
        assert "SLOAD," in text
        assert "100000.0" in text

    def test_total_bolt_load_comment(self):
        circle = BoltCircleConfig(
            num_bolts=8,
            bolt=BoltConfig(pretension_force_n=120000.0),
        )
        text = _gen().generate_bolt_pretension(circle)
        # Total = 8 * 120000 = 960000
        assert "960000" in text

    def test_psmesh_for_each_bolt(self):
        circle = BoltCircleConfig(num_bolts=4)
        text = _gen().generate_bolt_pretension(circle)
        assert text.count("PSMESH,") == 4


# ---------------------------------------------------------------------------
# Bolt material
# ---------------------------------------------------------------------------

class TestBoltMaterial:
    def test_generates_mp_ex(self):
        bolt = BoltConfig(material_id=2)
        text = _gen().generate_bolt_material(bolt)
        assert "MP,EX,2,205000" in text

    def test_generates_mp_nuxy(self):
        bolt = BoltConfig(material_id=2)
        text = _gen().generate_bolt_material(bolt)
        assert "MP,NUXY,2,0.3" in text

    def test_sa193_b7_properties(self):
        bolt = BoltConfig(material_id=2)
        text = _gen().generate_bolt_material(bolt)
        assert "SA-193 B7" in text


# ---------------------------------------------------------------------------
# Gasket elements
# ---------------------------------------------------------------------------

class TestGasketElements:
    def test_generates_inter195(self):
        gasket = GasketConfig()
        text = _gen().generate_gasket_elements(gasket)
        assert "ET,20,INTER195" in text

    def test_generates_tb_gasket(self):
        gasket = GasketConfig()
        text = _gen().generate_gasket_elements(gasket)
        assert "TB,GASKET," in text

    def test_generates_compression_curve_points(self):
        gasket = GasketConfig()
        text = _gen().generate_gasket_elements(gasket)
        assert "TBPT," in text

    def test_includes_seating_stress(self):
        gasket = GasketConfig(seating_stress_mpa=68.9)
        text = _gen().generate_gasket_elements(gasket)
        assert "68.9" in text


# ---------------------------------------------------------------------------
# Flange model
# ---------------------------------------------------------------------------

class TestFlangeModel:
    def test_generates_cyl4(self):
        flange = FlangeConfig()
        text = _gen().generate_flange_model(flange)
        assert "CYL4" in text

    def test_includes_raised_face(self):
        flange = FlangeConfig(raised_face_height_mm=1.6)
        text = _gen().generate_flange_model(flange)
        assert "Raised face" in text

    def test_weld_neck_has_hub(self):
        flange = FlangeConfig(flange_type="weld_neck", hub_length_mm=100.0)
        text = _gen().generate_flange_model(flange)
        assert "Hub" in text


# ---------------------------------------------------------------------------
# Multi-step solve
# ---------------------------------------------------------------------------

class TestMultistepSolve:
    def test_generates_three_load_steps(self):
        circle = BoltCircleConfig(num_bolts=4)
        text = _gen().generate_multistep_solve(circle, design_pressure_mpa=10.0)
        assert "LSWRITE,1" in text
        assert "LSWRITE,2" in text
        assert "LSWRITE,3" in text

    def test_lssolve_command(self):
        circle = BoltCircleConfig(num_bolts=4)
        text = _gen().generate_multistep_solve(circle)
        assert "LSSOLVE,1,3" in text

    def test_bolt_lock_in_step2(self):
        circle = BoltCircleConfig(num_bolts=4)
        text = _gen().generate_multistep_solve(circle)
        assert "LOCK" in text
