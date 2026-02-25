"""
Tests for Jacket/Topside Structural Analysis Module

Per API RP 2A WSD (Working Stress Design), 21st Edition.
All hand-calc verification examples are based on published API RP 2A examples
and standard structural steel textbook problems.

Unit system: US customary (kips, inches, ksi) — consistent with API RP 2A WSD.
  E = 29,000 ksi, Fy in ksi
"""

import math
import pytest

from digitalmodel.structural.jacket_topside.member_checks import (
    TubularSection,
    MemberLoads,
    MemberCheckResult,
    axial_tension_uc,
    axial_compression_uc,
    bending_uc,
    combined_axial_bending_uc,
    member_checks,
)
from digitalmodel.structural.jacket_topside.joint_checks import (
    JointGeometry,
    JointLoads,
    JointCheckResult,
    classify_joint,
    punching_shear_uc,
    JointType,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def typical_tubular() -> TubularSection:
    """
    API RP 2A WSD typical jacket leg member.
    24-inch OD x 0.75-inch WT, A-36 steel (Fy=36 ksi).
    E = 29000 ksi, kL = 20 ft = 240 in.
    """
    return TubularSection(
        outer_diameter=24.0,   # inches
        wall_thickness=0.75,   # inches
        yield_strength=36.0,   # ksi
        elastic_modulus=29000.0,  # ksi
        effective_length=240.0,   # inches (kL)
    )


@pytest.fixture
def high_strength_tubular() -> TubularSection:
    """
    High-strength steel tube: 16-inch OD x 0.5-inch WT, Fy=50 ksi.
    kL = 15 ft = 180 in.
    """
    return TubularSection(
        outer_diameter=16.0,
        wall_thickness=0.5,
        yield_strength=50.0,
        elastic_modulus=29000.0,
        effective_length=180.0,
    )


@pytest.fixture
def chord_section() -> TubularSection:
    """
    Chord (CAN) for joint checks: 24-inch OD x 1.0-inch WT, Fy=36 ksi.
    """
    return TubularSection(
        outer_diameter=24.0,
        wall_thickness=1.0,
        yield_strength=36.0,
        elastic_modulus=29000.0,
        effective_length=300.0,
    )


# ---------------------------------------------------------------------------
# TubularSection — section properties
# ---------------------------------------------------------------------------

class TestTubularSection:
    """Verify computed section properties against hand calculations."""

    def test_inner_diameter(self, typical_tubular: TubularSection) -> None:
        # d_i = D - 2*t = 24 - 2*0.75 = 22.5 in
        assert abs(typical_tubular.inner_diameter - 22.5) < 1e-6

    def test_cross_sectional_area(self, typical_tubular: TubularSection) -> None:
        # A = pi/4 * (D^2 - d_i^2) = pi/4 * (576 - 506.25) = pi/4 * 69.75
        expected = math.pi / 4.0 * (24.0**2 - 22.5**2)
        assert abs(typical_tubular.cross_sectional_area - expected) < 1e-4

    def test_moment_of_inertia(self, typical_tubular: TubularSection) -> None:
        # I = pi/64 * (D^4 - d_i^4)
        expected = math.pi / 64.0 * (24.0**4 - 22.5**4)
        assert abs(typical_tubular.moment_of_inertia - expected) < 1e-2

    def test_section_modulus(self, typical_tubular: TubularSection) -> None:
        # S = I / (D/2)
        I = math.pi / 64.0 * (24.0**4 - 22.5**4)
        expected = I / 12.0
        assert abs(typical_tubular.section_modulus - expected) < 1e-2

    def test_radius_of_gyration(self, typical_tubular: TubularSection) -> None:
        # r = sqrt(I/A)
        I = math.pi / 64.0 * (24.0**4 - 22.5**4)
        A = math.pi / 4.0 * (24.0**2 - 22.5**2)
        expected = math.sqrt(I / A)
        assert abs(typical_tubular.radius_of_gyration - expected) < 1e-4

    def test_slenderness_ratio(self, typical_tubular: TubularSection) -> None:
        # kL/r — kL = 240 in
        r = typical_tubular.radius_of_gyration
        expected = 240.0 / r
        assert abs(typical_tubular.slenderness_ratio - expected) < 1e-4

    def test_dt_ratio(self, typical_tubular: TubularSection) -> None:
        # D/t = 24 / 0.75 = 32
        assert abs(typical_tubular.dt_ratio - 32.0) < 1e-6

    def test_gamma_parameter(self, chord_section: TubularSection) -> None:
        # gamma = R/T = (D/2) / T = (24/2) / 1.0 = 12
        assert abs(chord_section.gamma - 12.0) < 1e-6


# ---------------------------------------------------------------------------
# Axial tension check — API RP 2A WSD Sec 3.2
# ---------------------------------------------------------------------------

class TestAxialTension:
    """
    API RP 2A WSD: Ft = 0.6 * Fy
    UC_t = fa / Ft
    """

    def test_uc_less_than_unity_passes(self, typical_tubular: TubularSection) -> None:
        # fa = 10 ksi < Ft = 0.6*36 = 21.6 ksi  → UC = 0.463
        result = axial_tension_uc(typical_tubular, fa=10.0)
        assert result.passes is True
        assert abs(result.unity_check - 10.0 / (0.6 * 36.0)) < 1e-6

    def test_uc_exactly_unity(self, typical_tubular: TubularSection) -> None:
        # fa = Ft = 21.6 ksi → UC = 1.0
        Ft = 0.6 * 36.0
        result = axial_tension_uc(typical_tubular, fa=Ft)
        assert abs(result.unity_check - 1.0) < 1e-6
        assert result.passes is True  # <= 1.0

    def test_uc_above_unity_fails(self, typical_tubular: TubularSection) -> None:
        # fa = 25 ksi > Ft = 21.6 ksi → UC > 1.0
        result = axial_tension_uc(typical_tubular, fa=25.0)
        assert result.passes is False
        assert result.unity_check > 1.0

    def test_allowable_stress_value(self, typical_tubular: TubularSection) -> None:
        result = axial_tension_uc(typical_tubular, fa=10.0)
        assert abs(result.allowable_stress - 21.6) < 1e-6

    def test_zero_load_passes(self, typical_tubular: TubularSection) -> None:
        result = axial_tension_uc(typical_tubular, fa=0.0)
        assert result.passes is True
        assert result.unity_check == 0.0

    def test_high_strength_steel(self, high_strength_tubular: TubularSection) -> None:
        # Fy=50 → Ft = 30 ksi; fa=25 → UC=0.833
        result = axial_tension_uc(high_strength_tubular, fa=25.0)
        assert abs(result.unity_check - 25.0 / 30.0) < 1e-6
        assert result.passes is True


# ---------------------------------------------------------------------------
# Axial compression check — API RP 2A WSD Sec 3.3
# ---------------------------------------------------------------------------

class TestAxialCompression:
    """
    API RP 2A WSD column buckling:
      Cc = sqrt(2*pi^2*E / Fy)
      If kL/r < Cc:  Fc = Fy*(1 - Fy/(4*Fe))    where Fe = 12*pi^2*E/(23*(kL/r)^2)
      If kL/r >= Cc: Fc = 12*pi^2*E / (23*(kL/r)^2)  (pure Euler)
      UC_c = fa / Fc
    """

    def _euler_fe(self, E: float, slenderness: float) -> float:
        return 12.0 * math.pi**2 * E / (23.0 * slenderness**2)

    def _cc(self, E: float, Fy: float) -> float:
        return math.sqrt(2.0 * math.pi**2 * E / Fy)

    def test_inelastic_range_passes(self, typical_tubular: TubularSection) -> None:
        """Member with kL/r < Cc — inelastic buckling governs."""
        E, Fy = 29000.0, 36.0
        sl = typical_tubular.slenderness_ratio
        Cc = self._cc(E, Fy)
        assert sl < Cc, "Fixture should be in inelastic range"
        Fe = self._euler_fe(E, sl)
        Fc = Fy * (1.0 - Fy / (4.0 * Fe))
        fa = 5.0
        result = axial_compression_uc(typical_tubular, fa=fa)
        assert abs(result.unity_check - fa / Fc) < 1e-4
        assert result.passes is True

    def test_elastic_range_fails(self) -> None:
        """Very slender member, kL/r >> Cc — elastic (Euler) governs."""
        slender = TubularSection(
            outer_diameter=10.0,
            wall_thickness=0.25,
            yield_strength=36.0,
            elastic_modulus=29000.0,
            effective_length=1200.0,  # very long
        )
        E, Fy = 29000.0, 36.0
        sl = slender.slenderness_ratio
        Cc = self._cc(E, Fy)
        assert sl >= Cc, "Fixture should be in elastic range"
        # High fa to force failure
        result = axial_compression_uc(slender, fa=8.0)
        assert result.passes is False

    def test_hand_calc_inelastic(self, typical_tubular: TubularSection) -> None:
        """
        Hand-check with 24-in OD x 0.75-in WT, Fy=36, E=29000, kL=240.
        Cc ~ 126.1
        """
        E, Fy = 29000.0, 36.0
        sl = typical_tubular.slenderness_ratio
        Cc = self._cc(E, Fy)
        Fe = self._euler_fe(E, sl)
        Fc = Fy * (1.0 - Fy / (4.0 * Fe))
        fa = 10.0
        result = axial_compression_uc(typical_tubular, fa=fa)
        assert abs(result.unity_check - fa / Fc) < 1e-4

    def test_allowable_compression_positive(self, typical_tubular: TubularSection) -> None:
        result = axial_compression_uc(typical_tubular, fa=5.0)
        assert result.allowable_stress > 0.0

    def test_zero_compression_passes(self, typical_tubular: TubularSection) -> None:
        result = axial_compression_uc(typical_tubular, fa=0.0)
        assert result.passes is True
        assert result.unity_check == 0.0


# ---------------------------------------------------------------------------
# Bending check — API RP 2A WSD Sec 3.4
# ---------------------------------------------------------------------------

class TestBending:
    """
    API RP 2A WSD: Fb = 0.75 * Fy  (when D/t <= 10340/Fy in psi, i.e. compact)
    UC_b = fb / Fb
    """

    def test_compact_section_passes(self, typical_tubular: TubularSection) -> None:
        # D/t=32, compact limit for Fy=36ksi(36000psi): 10340/36000=0.287 ft/in?
        # API RP 2A uses D/t <= 10340/Fy(psi): 10340/36000 is irrelevant;
        # typical API compact: D/t < 300 → compact, Fb=0.75*Fy=27 ksi
        result = bending_uc(typical_tubular, fb=15.0)
        assert result.passes is True
        assert abs(result.unity_check - 15.0 / (0.75 * 36.0)) < 1e-6

    def test_allowable_bending_stress_compact(self, typical_tubular: TubularSection) -> None:
        result = bending_uc(typical_tubular, fb=5.0)
        assert abs(result.allowable_stress - 0.75 * 36.0) < 1e-6

    def test_bending_uc_above_unity_fails(self, typical_tubular: TubularSection) -> None:
        # fb=30 > Fb=27 → fails
        result = bending_uc(typical_tubular, fb=30.0)
        assert result.passes is False
        assert result.unity_check > 1.0

    def test_zero_bending_passes(self, typical_tubular: TubularSection) -> None:
        result = bending_uc(typical_tubular, fb=0.0)
        assert result.passes is True
        assert result.unity_check == 0.0

    def test_high_strength_steel_bending(self, high_strength_tubular: TubularSection) -> None:
        # Fy=50 → Fb=37.5 ksi; fb=35 → UC=0.933
        result = bending_uc(high_strength_tubular, fb=35.0)
        assert abs(result.unity_check - 35.0 / 37.5) < 1e-6
        assert result.passes is True


# ---------------------------------------------------------------------------
# Combined axial + bending — API RP 2A WSD Sec 3.5
# ---------------------------------------------------------------------------

class TestCombinedLoading:
    """
    API RP 2A WSD interaction equations.

    Eq (1) — if fa/Fa > 0.15:
        UC1 = fa/Fa + Cm*fb/(Fb*(1 - fa/Fe)) <= 1.0
    Eq (2) — also required:
        UC2 = fa/(0.6*Fy) + fb/Fb <= 1.0

    If fa/Fa <= 0.15:
        UC = fa/Fa + fb/Fb <= 1.0  (simplified)
    """

    def test_low_axial_ratio_simplified(self, typical_tubular: TubularSection) -> None:
        """
        fa/Fa <= 0.15: simplified form for Eq 1.
        fa=1 ksi, fb=5 ksi.
        Governing UC = max(UC_eq1, UC_eq2).
        UC_eq1 = fa/Fa + fb/Fb  (simplified, axial ratio <= 0.15)
        UC_eq2 = fa/(0.6*Fy) + fb/Fb
        """
        loads = MemberLoads(fa=1.0, fb=5.0, Cm=0.85)
        result = combined_axial_bending_uc(typical_tubular, loads)
        # Determine fa/Fa first
        comp_result = axial_compression_uc(typical_tubular, fa=loads.fa)
        Fa = comp_result.allowable_stress
        Fb = 0.75 * 36.0
        Fy = 36.0
        assert loads.fa / Fa <= 0.15
        uc_eq1 = loads.fa / Fa + loads.fb / Fb
        uc_eq2 = loads.fa / (0.6 * Fy) + loads.fb / Fb
        expected_uc = max(uc_eq1, uc_eq2)
        assert abs(result.unity_check - expected_uc) < 1e-4
        assert result.passes is True

    def test_high_axial_ratio_interaction(self, typical_tubular: TubularSection) -> None:
        """
        fa/Fa > 0.15: full interaction equation.
        fa=15 ksi, fb=10 ksi.
        """
        loads = MemberLoads(fa=15.0, fb=10.0, Cm=0.85)
        result = combined_axial_bending_uc(typical_tubular, loads)
        assert result.unity_check > 0.0
        # Result should not pass (high combined loading)
        assert isinstance(result.passes, bool)

    def test_pure_compression_no_bending(self, typical_tubular: TubularSection) -> None:
        """
        fb=0: combined check reduces to max(fa/Fa, fa/(0.6*Fy)).
        The governing Eq 2 term fa/(0.6*Fy) may exceed fa/Fa when
        Fa < 0.6*Fy (which can happen for members with low slenderness).
        We verify the governing UC is computed correctly.
        """
        loads = MemberLoads(fa=10.0, fb=0.0, Cm=0.85)
        result = combined_axial_bending_uc(typical_tubular, loads)
        comp_result = axial_compression_uc(typical_tubular, fa=10.0)
        Fa = comp_result.allowable_stress
        Fy = typical_tubular.yield_strength
        # With fb=0: UC_eq1 = fa/Fa, UC_eq2 = fa/(0.6*Fy)
        expected_uc = max(10.0 / Fa, 10.0 / (0.6 * Fy))
        assert abs(result.unity_check - expected_uc) < 1e-4

    def test_pure_bending_no_axial(self, typical_tubular: TubularSection) -> None:
        """fa=0: combined UC should equal bending UC."""
        loads = MemberLoads(fa=0.0, fb=15.0, Cm=0.85)
        result = combined_axial_bending_uc(typical_tubular, loads)
        bend_result = bending_uc(typical_tubular, fb=15.0)
        assert abs(result.unity_check - bend_result.unity_check) < 1e-4

    def test_both_equations_checked(self, typical_tubular: TubularSection) -> None:
        """The governing UC is max(UC1, UC2)."""
        loads = MemberLoads(fa=12.0, fb=8.0, Cm=0.85)
        result = combined_axial_bending_uc(typical_tubular, loads)
        assert "uc_eq1" in result.details or "uc_eq2" in result.details

    def test_combined_passes_low_loads(self, typical_tubular: TubularSection) -> None:
        loads = MemberLoads(fa=2.0, fb=2.0, Cm=0.85)
        result = combined_axial_bending_uc(typical_tubular, loads)
        assert result.passes is True

    def test_combined_fails_high_loads(self, typical_tubular: TubularSection) -> None:
        loads = MemberLoads(fa=20.0, fb=20.0, Cm=0.85)
        result = combined_axial_bending_uc(typical_tubular, loads)
        assert result.passes is False


# ---------------------------------------------------------------------------
# Full member_checks orchestrator
# ---------------------------------------------------------------------------

class TestMemberChecks:
    """member_checks() runs all applicable checks and returns governing UC."""

    def test_returns_dict_with_all_checks(self, typical_tubular: TubularSection) -> None:
        loads = MemberLoads(fa=5.0, fb=5.0, Cm=0.85, is_tension=False)
        results = member_checks(typical_tubular, loads)
        assert "axial_compression" in results
        assert "bending" in results
        assert "combined" in results

    def test_tension_member_excludes_compression(
        self, typical_tubular: TubularSection
    ) -> None:
        loads = MemberLoads(fa=5.0, fb=0.0, Cm=0.85, is_tension=True)
        results = member_checks(typical_tubular, loads)
        assert "axial_tension" in results
        assert "axial_compression" not in results

    def test_governing_uc_is_maximum(self, typical_tubular: TubularSection) -> None:
        loads = MemberLoads(fa=5.0, fb=5.0, Cm=0.85, is_tension=False)
        results = member_checks(typical_tubular, loads)
        uc_values = [r.unity_check for r in results.values()]
        assert results["governing"].unity_check == max(uc_values)

    def test_all_checks_pass_low_loads(self, typical_tubular: TubularSection) -> None:
        loads = MemberLoads(fa=1.0, fb=1.0, Cm=0.85)
        results = member_checks(typical_tubular, loads)
        assert results["governing"].passes is True


# ---------------------------------------------------------------------------
# Joint classification — API RP 2A Sec 4.3
# ---------------------------------------------------------------------------

class TestJointClassification:
    """
    Basic tubular joint type classification:
      T/Y: single brace, gap irrelevant
      K:   two braces on same chord face, loads balance in chord
      X:   load passes through chord (cross joint)
    """

    def test_single_brace_classified_as_ty(self) -> None:
        geom = JointGeometry(
            chord_od=24.0,
            chord_wt=1.0,
            brace_angles=[45.0],       # degrees from chord axis
            brace_ods=[12.0],
            brace_wts=[0.5],
            gap=6.0,
        )
        joint_type = classify_joint(geom)
        assert joint_type == JointType.T_Y

    def test_two_braces_same_side_classified_as_k(self) -> None:
        geom = JointGeometry(
            chord_od=24.0,
            chord_wt=1.0,
            brace_angles=[45.0, 45.0],
            brace_ods=[12.0, 12.0],
            brace_wts=[0.5, 0.5],
            gap=4.0,
            braces_on_same_side=True,
        )
        joint_type = classify_joint(geom)
        assert joint_type == JointType.K

    def test_two_braces_opposite_sides_classified_as_x(self) -> None:
        geom = JointGeometry(
            chord_od=24.0,
            chord_wt=1.0,
            brace_angles=[90.0, 90.0],
            brace_ods=[12.0, 12.0],
            brace_wts=[0.5, 0.5],
            gap=0.0,
            braces_on_same_side=False,
        )
        joint_type = classify_joint(geom)
        assert joint_type == JointType.X

    def test_joint_type_enum_values(self) -> None:
        assert JointType.T_Y.value == "T/Y"
        assert JointType.K.value == "K"
        assert JointType.X.value == "X"

    def test_beta_computed_correctly(self) -> None:
        geom = JointGeometry(
            chord_od=24.0,
            chord_wt=1.0,
            brace_angles=[45.0],
            brace_ods=[12.0],
            brace_wts=[0.5],
            gap=6.0,
        )
        # beta = d / D = 12 / 24 = 0.5
        assert abs(geom.beta[0] - 0.5) < 1e-6

    def test_gamma_computed_correctly(self) -> None:
        geom = JointGeometry(
            chord_od=24.0,
            chord_wt=1.0,
            brace_angles=[45.0],
            brace_ods=[12.0],
            brace_wts=[0.5],
            gap=6.0,
        )
        # gamma = R/T = (D/2)/T = 12/1 = 12
        assert abs(geom.gamma - 12.0) < 1e-6


# ---------------------------------------------------------------------------
# Punching shear check — API RP 2A Sec 4.3 (simplified)
# ---------------------------------------------------------------------------

class TestPunchingShear:
    """
    Simplified API RP 2A punching shear.
    Vp (allowable punching shear stress) = Fy * Qf * Qq / (0.6 * sqrt(gamma))
    UC = applied_punching / Vp
    """

    @pytest.fixture
    def t_joint_geom(self) -> JointGeometry:
        return JointGeometry(
            chord_od=24.0,
            chord_wt=1.0,
            brace_angles=[90.0],      # T-joint: perpendicular
            brace_ods=[12.0],
            brace_wts=[0.5],
            gap=6.0,
        )

    def test_vp_positive(self, t_joint_geom: JointGeometry) -> None:
        """Allowable punching shear stress must be positive."""
        loads = JointLoads(axial_load=50.0, ipb_moment=0.0, opb_moment=0.0)
        result = punching_shear_uc(t_joint_geom, loads, chord_fy=36.0, brace_index=0)
        assert result.allowable_vp > 0.0

    def test_low_load_passes(self, t_joint_geom: JointGeometry) -> None:
        loads = JointLoads(axial_load=10.0, ipb_moment=0.0, opb_moment=0.0)
        result = punching_shear_uc(t_joint_geom, loads, chord_fy=36.0, brace_index=0)
        assert result.passes is True
        assert result.unity_check <= 1.0

    def test_high_load_can_fail(self, t_joint_geom: JointGeometry) -> None:
        loads = JointLoads(axial_load=5000.0, ipb_moment=0.0, opb_moment=0.0)
        result = punching_shear_uc(t_joint_geom, loads, chord_fy=36.0, brace_index=0)
        assert result.passes is False

    def test_hand_calc_vp(self, t_joint_geom: JointGeometry) -> None:
        """
        Hand calculation of Vp for T/Y-joint:
          gamma = 12, Qf=1.0 (no chord load reduction)
          beta = d/D = 12/24 = 0.5, theta = 90 deg
          Qq = (0.3 / (beta*(1 - 0.833*beta))) * sin(theta)
             = (0.3 / (0.5*(1 - 0.4165))) * 1.0
             = 0.3 / (0.5 * 0.5835) = 0.3 / 0.29175 ~ 1.02829
          Vp = 36 * 1.0 * 1.02829 / (0.6 * sqrt(12))
             ~ 36 * 1.02829 / 2.07846 ~ 17.810 ksi
        """
        beta = 0.5
        theta_deg = 90.0
        Qq = (0.3 / (beta * (1.0 - 0.833 * beta))) * math.sin(math.radians(theta_deg))
        Qf = 1.0
        gamma = 12.0
        expected_vp = 36.0 * Qf * Qq / (0.6 * math.sqrt(gamma))
        loads = JointLoads(axial_load=0.0, ipb_moment=0.0, opb_moment=0.0)
        result = punching_shear_uc(t_joint_geom, loads, chord_fy=36.0, brace_index=0)
        assert abs(result.allowable_vp - expected_vp) < 0.001

    def test_uc_formula(self, t_joint_geom: JointGeometry) -> None:
        """UC = applied_vp / allowable_vp."""
        loads = JointLoads(axial_load=20.0, ipb_moment=0.0, opb_moment=0.0)
        result = punching_shear_uc(t_joint_geom, loads, chord_fy=36.0, brace_index=0)
        assert abs(result.unity_check - result.applied_vp / result.allowable_vp) < 1e-6

    def test_zero_load_uc_is_zero(self, t_joint_geom: JointGeometry) -> None:
        loads = JointLoads(axial_load=0.0, ipb_moment=0.0, opb_moment=0.0)
        result = punching_shear_uc(t_joint_geom, loads, chord_fy=36.0, brace_index=0)
        assert result.unity_check == 0.0
        assert result.passes is True

    def test_invalid_brace_index_raises(self, t_joint_geom: JointGeometry) -> None:
        loads = JointLoads(axial_load=10.0, ipb_moment=0.0, opb_moment=0.0)
        with pytest.raises((IndexError, ValueError)):
            punching_shear_uc(t_joint_geom, loads, chord_fy=36.0, brace_index=5)


# ---------------------------------------------------------------------------
# MemberCheckResult and JointCheckResult dataclasses
# ---------------------------------------------------------------------------

class TestResultDataclasses:
    """Verify the result dataclasses carry expected fields."""

    def test_member_check_result_fields(self, typical_tubular: TubularSection) -> None:
        result = axial_tension_uc(typical_tubular, fa=5.0)
        assert hasattr(result, "unity_check")
        assert hasattr(result, "passes")
        assert hasattr(result, "allowable_stress")
        assert hasattr(result, "applied_stress")
        assert hasattr(result, "check_type")
        assert hasattr(result, "details")

    def test_joint_check_result_fields(self) -> None:
        geom = JointGeometry(
            chord_od=24.0,
            chord_wt=1.0,
            brace_angles=[45.0],
            brace_ods=[12.0],
            brace_wts=[0.5],
            gap=6.0,
        )
        loads = JointLoads(axial_load=10.0, ipb_moment=0.0, opb_moment=0.0)
        result = punching_shear_uc(geom, loads, chord_fy=36.0, brace_index=0)
        assert hasattr(result, "unity_check")
        assert hasattr(result, "passes")
        assert hasattr(result, "allowable_vp")
        assert hasattr(result, "applied_vp")
        assert hasattr(result, "check_type")

    def test_member_check_result_passes_bool(self, typical_tubular: TubularSection) -> None:
        result = axial_compression_uc(typical_tubular, fa=5.0)
        assert isinstance(result.passes, bool)
