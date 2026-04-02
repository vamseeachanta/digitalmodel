"""Tests for scf_library — Stress Concentration Factor calculations."""

import math
import pytest

from digitalmodel.fatigue.scf_library import (
    TubularJointGeometry,
    efthymiou_ty_axial,
    efthymiou_ty_ipb,
    efthymiou_ty_opb,
    efthymiou_k_axial,
    scf_butt_weld_misalignment,
    scf_cruciform_joint,
    scf_fillet_weld_toe,
    scf_shoulder_fillet,
    scf_circumferential_groove,
)


class TestTubularJointSCF:
    """Test Efthymiou SCF equations for tubular joints."""

    def _make_typical_ty(self) -> TubularJointGeometry:
        """Typical T/Y joint: 600mm chord, 25mm thick, 300mm brace, 12mm thick."""
        return TubularJointGeometry(
            D=600.0, T=25.0, d=300.0, t=12.0, theta=90.0
        )

    def test_ty_axial_scf_realistic(self):
        """T/Y axial SCF should be in range 1–20 for typical geometry."""
        geom = self._make_typical_ty()
        result = efthymiou_ty_axial(geom)
        assert result.scf_chord >= 1.0
        assert result.scf_brace >= 1.0
        assert result.governing <= 25.0
        assert "Efthymiou" in result.method

    def test_ty_ipb_returns_scf(self):
        """In-plane bending SCF is finite and ≥ 1."""
        geom = self._make_typical_ty()
        result = efthymiou_ty_ipb(geom)
        assert result.scf_chord >= 1.0
        assert result.scf_brace >= 1.0

    def test_ty_opb_returns_scf(self):
        """Out-of-plane bending SCF is finite and ≥ 1."""
        geom = self._make_typical_ty()
        result = efthymiou_ty_opb(geom)
        assert result.scf_chord >= 1.0

    def test_k_joint_with_gap(self):
        """K-joint SCF with positive gap gives physical result."""
        geom = TubularJointGeometry(
            D=800.0, T=30.0, d=400.0, t=15.0, theta=45.0, g=100.0
        )
        result = efthymiou_k_axial(geom)
        assert result.scf_chord >= 1.0
        assert result.governing >= 1.0


class TestPlateSCF:
    """Test plate/weld SCF functions."""

    def test_butt_weld_no_misalignment(self):
        """No misalignment and equal thickness → SCF = 1.0."""
        scf = scf_butt_weld_misalignment(t1=20.0, t2=20.0, e=0.0)
        assert scf == 1.0

    def test_butt_weld_with_misalignment(self):
        """1mm misalignment on 20mm plate → SCF > 1."""
        scf = scf_butt_weld_misalignment(t1=20.0, t2=20.0, e=1.0)
        assert scf > 1.0
        assert scf < 2.0  # 3*1/20 = 0.15, so SCF ≈ 1.15

    def test_cruciform_joint_scf(self):
        """Cruciform joint with 5mm throat, 12mm plate, 1mm misalignment."""
        scf = scf_cruciform_joint(t=12.0, a_weld=5.0, e=1.0)
        assert scf > 1.0
        # Should be moderate
        assert scf < 3.0

    def test_fillet_weld_toe_scf(self):
        """Fillet weld toe on 16mm plate at 45° → reasonable SCF."""
        scf = scf_fillet_weld_toe(t=16.0, theta=45.0, r=1.0)
        assert 1.0 < scf < 5.0


class TestParametricSCF:
    """Test general parametric SCF formulae."""

    def test_shoulder_fillet_scf(self):
        """Stepped shaft: D=50mm, d=40mm, r=5mm → SCF around 1.5-3."""
        scf = scf_shoulder_fillet(D=50.0, d=40.0, r=5.0)
        assert 1.0 < scf < 4.0

    def test_circumferential_groove(self):
        """U-groove: D=50mm, d=40mm, r=3mm → SCF ≈ 1 + 2√(5/3) ≈ 3.58."""
        scf = scf_circumferential_groove(D=50.0, d=40.0, r=3.0)
        t = (50.0 - 40.0) / 2.0
        expected = 1.0 + 2.0 * math.sqrt(t / 3.0)
        assert abs(scf - expected) < 0.01

    def test_invalid_geometry_raises(self):
        """D ≤ d should raise ValueError."""
        with pytest.raises(ValueError):
            scf_shoulder_fillet(D=30.0, d=40.0, r=5.0)
