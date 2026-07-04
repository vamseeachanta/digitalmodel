#!/usr/bin/env python3
"""
ABOUTME: Tests for the prescribed single-DOF forced-motion engine (#658) —
kinematics (omega/frequency/amplitude vector), dynamicMeshDict rendering for
rotational and translational DOFs, the standalone file writer, and integration
with the case builder / SloshingSetup.
"""

import math

import pytest

from digitalmodel.solvers.openfoam.motion import (
    MotionType,
    PrescribedMotion,
    render_dynamic_mesh_dict,
    render_dynamic_mesh_dict_body,
    write_dynamic_mesh_dict,
)
from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
from digitalmodel.solvers.openfoam.marine_solvers import SloshingSetup


# ============================================================================
# Kinematics
# ============================================================================


class TestKinematics:
    def test_omega_from_period(self):
        m = PrescribedMotion(MotionType.ROLL, amplitude=10.0, period=18.0)
        assert m.omega == pytest.approx(2.0 * math.pi / 18.0)

    def test_frequency_hz(self):
        m = PrescribedMotion(MotionType.ROLL, amplitude=10.0, period=18.0)
        assert m.frequency_hz == pytest.approx(1.0 / 18.0)

    def test_from_frequency_hz_roundtrip(self):
        m = PrescribedMotion.from_frequency_hz(MotionType.ROLL, 5.0, 0.05)
        assert m.period == pytest.approx(20.0)
        assert m.frequency_hz == pytest.approx(0.05)

    def test_from_omega_roundtrip(self):
        omega = 0.35
        m = PrescribedMotion.from_omega(MotionType.SWAY, 0.2, omega)
        assert m.omega == pytest.approx(omega)

    @pytest.mark.parametrize(
        "dof,idx",
        [
            (MotionType.ROLL, 0),
            (MotionType.PITCH, 1),
            (MotionType.YAW, 2),
            (MotionType.SURGE, 0),
            (MotionType.SWAY, 1),
            (MotionType.HEAVE, 2),
        ],
    )
    def test_amplitude_vector_axis(self, dof, idx):
        m = PrescribedMotion(dof, amplitude=7.5, period=10.0)
        vec = m.amplitude_vector
        assert vec[idx] == pytest.approx(7.5)
        assert sum(1 for v in vec if v != 0.0) == 1

    @pytest.mark.parametrize(
        "dof,rot",
        [
            (MotionType.ROLL, True),
            (MotionType.PITCH, True),
            (MotionType.YAW, True),
            (MotionType.SURGE, False),
            (MotionType.SWAY, False),
            (MotionType.HEAVE, False),
        ],
    )
    def test_is_rotational(self, dof, rot):
        assert dof.is_rotational is rot


class TestValidation:
    def test_zero_period_rejected(self):
        with pytest.raises(ValueError):
            PrescribedMotion(MotionType.ROLL, amplitude=10.0, period=0.0)

    def test_negative_amplitude_rejected(self):
        with pytest.raises(ValueError):
            PrescribedMotion(MotionType.ROLL, amplitude=-1.0, period=10.0)

    def test_bad_origin_rejected(self):
        with pytest.raises(ValueError):
            PrescribedMotion(
                MotionType.ROLL, amplitude=10.0, period=10.0, origin=(0.0, 0.0)
            )

    def test_from_frequency_hz_zero_rejected(self):
        with pytest.raises(ValueError):
            PrescribedMotion.from_frequency_hz(MotionType.ROLL, 1.0, 0.0)


# ============================================================================
# Dict rendering
# ============================================================================


class TestRenderRotational:
    def test_uses_esi_solid_body_motion_solver(self):
        # ESI v2312: dynamicMotionSolverFvMesh + motionSolver solidBody
        # (Foundation-branch solidBodyMotionFvMesh does NOT exist in v2312).
        m = PrescribedMotion(MotionType.ROLL, amplitude=10.0, period=18.0)
        body = render_dynamic_mesh_dict_body(m)
        assert "dynamicFvMesh    dynamicMotionSolverFvMesh;" in body
        assert "motionSolver     solidBody;" in body
        assert "solidBodyMotionFvMesh" not in body  # Foundation-only type

    def test_uses_oscillating_rotating_motion(self):
        m = PrescribedMotion(MotionType.ROLL, amplitude=10.0, period=18.0)
        body = render_dynamic_mesh_dict_body(m)
        assert "oscillatingRotatingMotion" in body
        assert "oscillatingLinearMotion" not in body

    def test_encodes_origin_omega_amplitude(self):
        m = PrescribedMotion(
            MotionType.ROLL, amplitude=10.0, period=18.0, origin=(17.5, 0.0, 2.0)
        )
        body = render_dynamic_mesh_dict_body(m)
        assert "origin      (17.5 0 2);" in body
        # amplitude on the x (roll) axis, in degrees
        assert "amplitude   (10 0 0);" in body
        # omega = 2*pi/18 rad/s
        assert f"{2.0 * math.pi / 18.0:.10g}" in body

    def test_whole_mesh_no_cellzone(self):
        # No cellZone => the whole rigid mesh moves (the forced-tank rig).
        m = PrescribedMotion(MotionType.ROLL, amplitude=10.0, period=18.0)
        assert "cellZone" not in render_dynamic_mesh_dict_body(m)

    def test_pitch_amplitude_on_y_axis(self):
        m = PrescribedMotion(MotionType.PITCH, amplitude=4.0, period=12.0)
        body = render_dynamic_mesh_dict_body(m)
        assert "amplitude   (0 4 0);" in body


class TestRenderTranslational:
    def test_uses_oscillating_linear_motion(self):
        m = PrescribedMotion(MotionType.SWAY, amplitude=0.3, period=8.0)
        body = render_dynamic_mesh_dict_body(m)
        assert "oscillatingLinearMotion" in body
        assert "oscillatingRotatingMotion" not in body

    def test_amplitude_metres_on_axis(self):
        m = PrescribedMotion(MotionType.SWAY, amplitude=0.3, period=8.0)
        body = render_dynamic_mesh_dict_body(m)
        assert "amplitude   (0 0.3 0);" in body


class TestFullFileRender:
    def test_has_foam_header_and_object(self):
        m = PrescribedMotion(MotionType.ROLL, amplitude=10.0, period=18.0)
        text = render_dynamic_mesh_dict(m)
        assert "FoamFile" in text
        assert "object      dynamicMeshDict;" in text
        assert "dynamicMotionSolverFvMesh" in text

    def test_write_dynamic_mesh_dict(self, tmp_path):
        m = PrescribedMotion(MotionType.ROLL, amplitude=10.0, period=18.0)
        constant_dir = tmp_path / "constant"
        target = write_dynamic_mesh_dict(m, constant_dir)
        assert target.exists()
        assert target.name == "dynamicMeshDict"
        content = target.read_text()
        assert "FoamFile" in content
        assert "oscillatingRotatingMotion" in content


# ============================================================================
# Integration with the case builder / SloshingSetup
# ============================================================================


class TestCaseBuilderIntegration:
    def test_motion_emits_dynamic_mesh_dict(self, tmp_path):
        motion = PrescribedMotion(
            MotionType.ROLL, amplitude=8.0, period=18.0, origin=(17.5, 0.0, 1.5)
        )
        setup = SloshingSetup(fill_level=0.5, name="slosh_forced", motion=motion)
        case_dir = OpenFOAMCaseBuilder(setup.case).build(tmp_path)
        dmd = case_dir / "constant" / "dynamicMeshDict"
        assert dmd.exists()
        content = dmd.read_text()
        assert "dynamicMotionSolverFvMesh" in content
        assert "motionSolver     solidBody;" in content
        assert "oscillatingRotatingMotion" in content
        assert "amplitude   (8 0 0);" in content

    def test_no_motion_no_dynamic_mesh_dict(self, tmp_path):
        setup = SloshingSetup(fill_level=0.5, name="slosh_static")
        case_dir = OpenFOAMCaseBuilder(setup.case).build(tmp_path)
        assert not (case_dir / "constant" / "dynamicMeshDict").exists()

    def test_sloshing_setup_forwards_motion_to_case(self):
        motion = PrescribedMotion(MotionType.ROLL, amplitude=8.0, period=18.0)
        setup = SloshingSetup(motion=motion)
        assert setup.case.motion is motion
