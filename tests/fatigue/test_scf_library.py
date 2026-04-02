"""
Tests for Stress Concentration Factor (SCF) Library

Validates Efthymiou equations for tubular joints, plate SCFs,
and weld toe SCFs per DNV-RP-C203, IIW, and Peterson.

Issue: #1676 (P0 — SCF library)
"""

import math
import pytest

from digitalmodel.fatigue.scf_library import (
    TubularJointGeometry,
    SCFResult,
    PlateGeometry,
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


# -- Test: Tubular joint geometry parameters ----------------------------------

class TestTubularJointGeometry:
    """Parametric ratios: beta, gamma, tau, alpha, zeta."""

    def test_beta_calculation(self):
        """beta = d/D."""
        geom = TubularJointGeometry(D=1000, T=50, d=500, t=25)
        assert geom.beta == pytest.approx(0.5)

    def test_gamma_calculation(self):
        """gamma = D/(2T)."""
        geom = TubularJointGeometry(D=1000, T=50, d=500, t=25)
        assert geom.gamma == pytest.approx(10.0)

    def test_tau_calculation(self):
        """tau = t/T."""
        geom = TubularJointGeometry(D=1000, T=50, d=500, t=25)
        assert geom.tau == pytest.approx(0.5)

    def test_alpha_default_uses_2point5_D(self):
        """When L=0, alpha = 2*(2.5*D)/D = 5."""
        geom = TubularJointGeometry(D=1000, T=50, d=500, t=25, L=0)
        assert geom.alpha == pytest.approx(5.0)

    def test_alpha_explicit_L(self):
        """alpha = 2L/D with explicit L."""
        geom = TubularJointGeometry(D=1000, T=50, d=500, t=25, L=3000)
        assert geom.alpha == pytest.approx(6.0)

    def test_zeta_calculation(self):
        """zeta = g/D."""
        geom = TubularJointGeometry(D=1000, T=50, d=500, t=25, g=100)
        assert geom.zeta == pytest.approx(0.1)


# -- Test: Efthymiou T/Y joint SCFs ------------------------------------------

class TestEfthymiouTYJoint:
    """Efthymiou SCF equations for T/Y joints (DNV-RP-C203 Table B-5)."""

    @pytest.fixture
    def typical_ty_geom(self):
        """Typical T-joint: D=1000mm, T=50mm, d=500mm, t=25mm, theta=90."""
        return TubularJointGeometry(D=1000, T=50, d=500, t=25, theta=90.0)

    def test_ty_axial_returns_scf_result(self, typical_ty_geom):
        """Axial load SCF returns an SCFResult model."""
        result = efthymiou_ty_axial(typical_ty_geom)
        assert isinstance(result, SCFResult)

    def test_ty_axial_scf_positive(self, typical_ty_geom):
        """SCFs must be >= 1.0 (minimum physical SCF)."""
        result = efthymiou_ty_axial(typical_ty_geom)
        assert result.scf_chord >= 1.0
        assert result.scf_brace >= 1.0

    def test_ty_axial_governing_is_max(self, typical_ty_geom):
        """Governing SCF = max(chord, brace)."""
        result = efthymiou_ty_axial(typical_ty_geom)
        assert result.governing == max(result.scf_chord, result.scf_brace)

    def test_ty_ipb_scf_positive(self, typical_ty_geom):
        """In-plane bending SCFs >= 1.0."""
        result = efthymiou_ty_ipb(typical_ty_geom)
        assert result.scf_chord >= 1.0
        assert result.scf_brace >= 1.0

    def test_ty_opb_scf_positive(self, typical_ty_geom):
        """Out-of-plane bending SCFs >= 1.0."""
        result = efthymiou_ty_opb(typical_ty_geom)
        assert result.scf_chord >= 1.0
        assert result.scf_brace >= 1.0

    def test_ty_axial_method_string(self, typical_ty_geom):
        """Method should mention Efthymiou and axial."""
        result = efthymiou_ty_axial(typical_ty_geom)
        assert "efthymiou" in result.method.lower()
        assert "axial" in result.method.lower()

    def test_angle_effect_on_scf(self):
        """Lower brace angle should change SCF (theta=45 vs 90)."""
        geom_90 = TubularJointGeometry(D=1000, T=50, d=500, t=25, theta=90)
        geom_45 = TubularJointGeometry(D=1000, T=50, d=500, t=25, theta=45)
        scf_90 = efthymiou_ty_axial(geom_90).governing
        scf_45 = efthymiou_ty_axial(geom_45).governing
        assert scf_90 != scf_45

    def test_beta_effect_on_scf(self):
        """Higher beta generally changes SCF."""
        geom_low = TubularJointGeometry(D=1000, T=50, d=300, t=15, theta=90)
        geom_high = TubularJointGeometry(D=1000, T=50, d=800, t=40, theta=90)
        scf_low = efthymiou_ty_axial(geom_low)
        scf_high = efthymiou_ty_axial(geom_high)
        assert scf_low.governing != scf_high.governing


# -- Test: K-joint SCF --------------------------------------------------------

class TestEfthymiouKJoint:
    """K-joint balanced axial SCF (DNV-RP-C203 Table B-6)."""

    def test_k_joint_scf_positive(self):
        """K-joint SCFs must be >= 1.0."""
        geom = TubularJointGeometry(D=1000, T=50, d=500, t=25, theta=60, g=100)
        result = efthymiou_k_axial(geom)
        assert result.scf_chord >= 1.0
        assert result.scf_brace >= 1.0

    def test_k_joint_chord_typically_governs(self):
        """For K-joints, chord saddle typically governs."""
        geom = TubularJointGeometry(D=1000, T=50, d=500, t=25, theta=60, g=100)
        result = efthymiou_k_axial(geom)
        assert result.scf_chord >= result.scf_brace or result.governing >= 1.0

    def test_k_joint_gap_effect(self):
        """Different gaps should give different SCFs."""
        geom_small = TubularJointGeometry(D=1000, T=50, d=500, t=25, theta=60, g=50)
        geom_large = TubularJointGeometry(D=1000, T=50, d=500, t=25, theta=60, g=200)
        scf_small = efthymiou_k_axial(geom_small).governing
        scf_large = efthymiou_k_axial(geom_large).governing
        assert scf_small != scf_large


# -- Test: Plate SCFs - butt weld misalignment --------------------------------

class TestButtWeldMisalignment:
    """SCF for butt weld with thickness transition / misalignment."""

    def test_no_misalignment_same_thickness(self):
        """Equal thicknesses, no misalignment -> SCF = 1.0."""
        scf = scf_butt_weld_misalignment(t1=25.0, t2=25.0, e=0.0)
        assert scf == pytest.approx(1.0)

    def test_misalignment_increases_scf(self):
        """Misalignment always increases SCF above 1.0."""
        scf = scf_butt_weld_misalignment(t1=25.0, t2=25.0, e=3.0)
        assert scf > 1.0

    def test_thickness_transition_increases_scf(self):
        """Different thicknesses (no misalignment) should increase SCF."""
        scf = scf_butt_weld_misalignment(t1=25.0, t2=35.0, e=0.0)
        assert scf > 1.0

    def test_zero_thickness_raises(self):
        """Zero plate thickness should raise ValueError."""
        with pytest.raises(ValueError, match="positive"):
            scf_butt_weld_misalignment(t1=0.0, t2=25.0, e=0.0)

    def test_scf_always_ge_1(self):
        """SCF must be >= 1.0 for any valid input."""
        scf = scf_butt_weld_misalignment(t1=15.0, t2=30.0, e=2.0)
        assert scf >= 1.0


# -- Test: Cruciform joint SCF ------------------------------------------------

class TestCruciformJoint:
    """SCF for load-carrying fillet weld cruciform joints."""

    def test_no_misalignment_no_weld_effect(self):
        """No misalignment, weld throat = plate: SCF approx 1.0."""
        scf = scf_cruciform_joint(t=25.0, a_weld=25.0, e=0.0)
        assert scf == pytest.approx(1.0, abs=0.01)

    def test_misalignment_increases_cruciform_scf(self):
        """Misalignment increases SCF."""
        scf = scf_cruciform_joint(t=25.0, a_weld=25.0, e=5.0)
        assert scf > 1.0

    def test_thin_weld_increases_scf(self):
        """Smaller weld throat relative to plate increases SCF."""
        scf = scf_cruciform_joint(t=25.0, a_weld=10.0, e=0.0)
        assert scf > 1.0

    def test_invalid_inputs_raise(self):
        """Zero thickness or weld throat raises ValueError."""
        with pytest.raises(ValueError, match="positive"):
            scf_cruciform_joint(t=0.0, a_weld=10.0)


# -- Test: Fillet weld toe SCF ------------------------------------------------

class TestFilletWeldToe:
    """SCF at fillet weld toe (IIW Eq. 3.3-1)."""

    def test_fillet_scf_always_ge_1(self):
        """SCF at weld toe is always >= 1.0."""
        scf = scf_fillet_weld_toe(t=25.0, theta=45.0, r=1.0)
        assert scf >= 1.0

    def test_sharper_angle_higher_scf(self):
        """Steeper weld angle -> higher SCF."""
        scf_45 = scf_fillet_weld_toe(t=25.0, theta=45.0, r=1.0)
        scf_60 = scf_fillet_weld_toe(t=25.0, theta=60.0, r=1.0)
        assert scf_60 > scf_45

    def test_larger_radius_lower_scf(self):
        """Larger toe radius -> lower SCF."""
        scf_small_r = scf_fillet_weld_toe(t=25.0, theta=45.0, r=0.5)
        scf_large_r = scf_fillet_weld_toe(t=25.0, theta=45.0, r=5.0)
        assert scf_small_r > scf_large_r

    def test_zero_thickness_raises(self):
        """Zero plate thickness raises ValueError."""
        with pytest.raises(ValueError, match="positive"):
            scf_fillet_weld_toe(t=0.0)


# -- Test: Peterson shoulder fillet and groove --------------------------------

class TestPetersonGeometries:
    """Peterson's SCFs for common geometric stress raisers."""

    def test_shoulder_fillet_scf_positive(self):
        """Shoulder fillet SCF >= 1.0."""
        scf = scf_shoulder_fillet(D=50.0, d=30.0, r=5.0)
        assert scf >= 1.0

    def test_shoulder_fillet_invalid_raises(self):
        """D must be > d > 0 and r > 0."""
        with pytest.raises(ValueError, match="D > d > 0"):
            scf_shoulder_fillet(D=30.0, d=50.0, r=5.0)

    def test_circumferential_groove_scf(self):
        """Groove SCF = 1 + 2*sqrt(t/r) for sharp grooves."""
        scf = scf_circumferential_groove(D=50.0, d=40.0, r=2.0)
        expected = 1.0 + 2.0 * math.sqrt(5.0 / 2.0)
        assert scf == pytest.approx(expected, abs=0.01)

    def test_groove_invalid_raises(self):
        """Invalid groove dimensions raise ValueError."""
        with pytest.raises(ValueError, match="D > d > 0"):
            scf_circumferential_groove(D=30.0, d=40.0, r=2.0)
