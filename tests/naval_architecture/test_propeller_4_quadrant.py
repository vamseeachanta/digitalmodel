"""TDD tests for 4-quadrant propeller performance.

Test data sources:
- Oosterveld & van Oossanen (1975): 1st-quadrant B-series polynomials
  (39 KT + 47 KQ coefficients from Bernitsas et al. 1981 / nickholt15/wageningen)
- Viviani, Roddy, Hess & Faller (2007): 4-quadrant formulation, Eqs. 1-6
  PDF: /mnt/ace/digitalmodel/docs/domains/hydrodynamics/literature/
       viviani-2007-four-quadrant-wageningen-b-series.pdf
- McTaggart (2005): Esso Osaka KT polynomial, DRDC Atlantic TM 2005-071
  PDF: digitalmodel/docs/domains/ship-design/maneuvering_ship.pdf
"""

import math
import pytest

from digitalmodel.naval_architecture.propeller import (
    advance_angle,
    ct_star_from_kt,
    cq_star_from_kq,
    kt_from_ct_star,
    kq_from_cq_star,
    reference_velocity,
    wageningen_kt,
    wageningen_kq,
    four_quadrant_thrust,
    four_quadrant_torque,
    FourQuadrantPropeller,
)


# ---------------------------------------------------------------------------
# 1. β angle computation — Viviani Eq. 4
#    β = atan2(V_A, 0.7π·n·D)
# ---------------------------------------------------------------------------
class TestAdvanceAngle:
    """Verify β-angle parameterisation at known operating points."""

    def test_bollard_pull_ahead(self):
        """β = 0° when V_A = 0, n > 0 (bollard pull, ahead rotation)."""
        beta = advance_angle(V_A=0.0, n=2.0, D=5.0)
        assert beta == pytest.approx(0.0, abs=1e-10)

    def test_free_wheeling_forward(self):
        """β = 90° when n = 0, V_A > 0 (propeller stopped, ship moving fwd)."""
        beta = advance_angle(V_A=5.0, n=0.0, D=5.0)
        assert beta == pytest.approx(90.0, abs=1e-10)

    def test_bollard_pull_astern(self):
        """β = 180° when V_A = 0, n < 0 (bollard pull, astern rotation)."""
        beta = advance_angle(V_A=0.0, n=-2.0, D=5.0)
        assert beta == pytest.approx(180.0, abs=1e-10)

    def test_free_wheeling_astern(self):
        """β = 270° when n = 0, V_A < 0 (propeller stopped, ship moving astern)."""
        beta = advance_angle(V_A=-5.0, n=0.0, D=5.0)
        assert beta == pytest.approx(270.0, abs=1e-10)

    def test_first_quadrant_typical(self):
        """1st quadrant: V_A > 0, n > 0 → 0° < β < 90°."""
        # J = V_A / (n*D) = 5 / (2*5) = 0.5
        # β = atan(J / (0.7π)) = atan(0.5 / 2.19911) = atan(0.22736) ≈ 12.8°
        beta = advance_angle(V_A=5.0, n=2.0, D=5.0)
        expected = math.degrees(math.atan2(5.0, 0.7 * math.pi * 2.0 * 5.0))
        assert beta == pytest.approx(expected, abs=1e-6)
        assert 0.0 < beta < 90.0

    def test_second_quadrant_crash_stop(self):
        """2nd quadrant: V_A > 0, n < 0 → 90° < β < 180°."""
        beta = advance_angle(V_A=5.0, n=-2.0, D=5.0)
        assert 90.0 < beta < 180.0

    def test_third_quadrant_backing(self):
        """3rd quadrant: V_A < 0, n < 0 → 180° < β < 270°."""
        beta = advance_angle(V_A=-5.0, n=-2.0, D=5.0)
        assert 180.0 < beta < 270.0

    def test_fourth_quadrant(self):
        """4th quadrant: V_A < 0, n > 0 → 270° < β < 360°."""
        beta = advance_angle(V_A=-5.0, n=2.0, D=5.0)
        assert 270.0 < beta < 360.0

    def test_relation_to_advance_ratio(self):
        """In 1st quadrant: tan(β) = J / (0.7π) — Viviani Eq. 1 rearranged."""
        n, D = 2.0, 5.0
        V_A = 5.0
        J = V_A / (n * D)
        beta_rad = math.radians(advance_angle(V_A=V_A, n=n, D=D))
        assert math.tan(beta_rad) == pytest.approx(J / (0.7 * math.pi), rel=1e-6)


# ---------------------------------------------------------------------------
# 2. Reference velocity V_R
# ---------------------------------------------------------------------------
class TestReferenceVelocity:
    """V_R = sqrt(V_A² + (0.7π·n·D)²)."""

    def test_bollard(self):
        """At bollard (V_A=0), V_R = 0.7π·n·D."""
        n, D = 2.0, 5.0
        V_R = reference_velocity(V_A=0.0, n=n, D=D)
        assert V_R == pytest.approx(0.7 * math.pi * n * D, rel=1e-10)

    def test_free_wheeling(self):
        """At free-wheeling (n=0), V_R = |V_A|."""
        V_R = reference_velocity(V_A=8.0, n=0.0, D=5.0)
        assert V_R == pytest.approx(8.0, rel=1e-10)

    def test_general(self):
        V_A, n, D = 5.0, 2.0, 5.0
        V_R = reference_velocity(V_A=V_A, n=n, D=D)
        expected = math.sqrt(V_A**2 + (0.7 * math.pi * n * D) ** 2)
        assert V_R == pytest.approx(expected, rel=1e-10)


# ---------------------------------------------------------------------------
# 3. Conversion between (J, KT, KQ) and (β, CT*, CQ*) — Viviani Eqs. 2-6
# ---------------------------------------------------------------------------
class TestCoefficientConversion:
    """Round-trip: KT → CT* → KT must be identity. Source: Viviani Eqs. 5, 2."""

    @pytest.mark.parametrize("J, KT", [
        (0.1, 0.45),   # high loading
        (0.5, 0.20),   # moderate loading
        (0.8, 0.08),   # light loading
        (1.0, 0.02),   # near zero-thrust
    ])
    def test_kt_roundtrip(self, J, KT):
        """KT → CT* → KT must recover original value."""
        ct_star = ct_star_from_kt(KT, J)
        kt_back = kt_from_ct_star(ct_star, J)
        assert kt_back == pytest.approx(KT, rel=1e-10)

    @pytest.mark.parametrize("J, KQ", [
        (0.1, 0.060),
        (0.5, 0.030),
        (0.8, 0.012),
    ])
    def test_kq_roundtrip(self, J, KQ):
        """KQ → CQ* → KQ must recover original value."""
        cq_star = cq_star_from_kq(KQ, J)
        kq_back = kq_from_cq_star(cq_star, J)
        assert kq_back == pytest.approx(KQ, rel=1e-10)

    def test_ct_star_formula(self):
        """CT* = 8·KT / (π·(J² + (0.7π)²)) — Viviani Eq. 5."""
        J, KT = 0.5, 0.20
        ct_star = ct_star_from_kt(KT, J)
        expected = 8.0 * KT / (math.pi * (J**2 + (0.7 * math.pi) ** 2))
        assert ct_star == pytest.approx(expected, rel=1e-10)

    def test_cq_star_formula(self):
        """CQ* = 8·KQ / (π·(J² + (0.7π)²)) — Viviani Eq. 6."""
        J, KQ = 0.5, 0.030
        cq_star = cq_star_from_kq(KQ, J)
        expected = 8.0 * KQ / (math.pi * (J**2 + (0.7 * math.pi) ** 2))
        assert cq_star == pytest.approx(expected, rel=1e-10)


# ---------------------------------------------------------------------------
# 4. Wageningen B-series 1st-quadrant polynomials
#    Source: Oosterveld & van Oossanen (1975) via Bernitsas et al. (1981)
#    Code ref: github.com/nickholt15/wageningen/bseries.py
# ---------------------------------------------------------------------------
class TestWageningenFirstQuadrant:
    """Validate KT/KQ polynomials against published values."""

    # B4-70 (Z=4, AE/A0=0.70) at P/D=1.0
    # Values from Mehdipour (2013) Table for STREAMLINE propeller (similar geometry)
    # and cross-checked with nickholt15 code output
    def test_b4_70_kt_at_design_j(self):
        """B4-70 P/D=1.0 at J=0.6: KT ≈ 0.17–0.23 (typical design point)."""
        KT = wageningen_kt(J=0.6, PD=1.0, AE_A0=0.70, Z=4)
        assert 0.15 < KT < 0.24

    def test_b4_70_kq_at_design_j(self):
        """B4-70 P/D=1.0 at J=0.6: 10KQ ≈ 0.30–0.38."""
        KQ = wageningen_kq(J=0.6, PD=1.0, AE_A0=0.70, Z=4)
        assert 0.030 < KQ < 0.040

    def test_b4_70_kt_at_bollard(self):
        """B4-70 P/D=1.0 at J=0: KT ≈ 0.42–0.48 (bollard pull)."""
        KT = wageningen_kt(J=0.0, PD=1.0, AE_A0=0.70, Z=4)
        assert 0.40 < KT < 0.50

    def test_kt_decreases_with_j(self):
        """KT must decrease monotonically with increasing J."""
        kt_values = [
            wageningen_kt(J=j, PD=1.0, AE_A0=0.70, Z=4)
            for j in [0.0, 0.2, 0.4, 0.6, 0.8]
        ]
        for i in range(len(kt_values) - 1):
            assert kt_values[i] > kt_values[i + 1]

    def test_efficiency_at_design(self):
        """Open-water efficiency η₀ = J·KT / (2π·KQ) ≈ 0.55–0.70 at design J."""
        J = 0.6
        KT = wageningen_kt(J=J, PD=1.0, AE_A0=0.70, Z=4)
        KQ = wageningen_kq(J=J, PD=1.0, AE_A0=0.70, Z=4)
        eta0 = J * KT / (2 * math.pi * KQ)
        assert 0.50 < eta0 < 0.75

    # McTaggart Esso Osaka polynomial: KT = 0.394 - 0.197*J - 0.148*J²
    # Source: McTaggart (2005) Eq. 79
    @pytest.mark.parametrize("J, kt_expected", [
        (0.0, 0.394),
        (0.2, 0.349),   # 0.394 - 0.197*0.2 - 0.148*0.04
        (0.5, 0.258),   # 0.394 - 0.197*0.5 - 0.148*0.25
        (0.8, 0.141),   # 0.394 - 0.197*0.8 - 0.148*0.64
    ])
    def test_esso_osaka_kt_polynomial(self, J, kt_expected):
        """McTaggart Esso Osaka KT polynomial — not B-series but validates range."""
        kt_calc = 0.394 - 0.197 * J - 0.148 * J**2
        assert kt_calc == pytest.approx(kt_expected, abs=0.002)


# ---------------------------------------------------------------------------
# 5. 1st-quadrant to 4-quadrant consistency
#    In quadrant 1, CT*(β) from Fourier = CT* computed from KT(J) polynomial
# ---------------------------------------------------------------------------
class TestFirstQuadrantConsistency:
    """CT* from polynomial KT must match CT* from 4-quadrant model in Q1."""

    @pytest.mark.parametrize("J", [0.2, 0.4, 0.6, 0.8])
    def test_ct_star_consistency(self, J):
        """CT*(β) in Q1 should match CT* derived from KT(J) polynomial."""
        KT = wageningen_kt(J=J, PD=1.0, AE_A0=0.70, Z=4)
        ct_star_from_poly = ct_star_from_kt(KT, J)

        # β for this J in 1st quadrant
        beta = math.degrees(math.atan(J / (0.7 * math.pi)))

        # From the 4-quadrant model (if Fourier coefficients loaded)
        prop = FourQuadrantPropeller(Z=4, AE_A0=0.70, PD=1.0)
        ct_star_from_4q = prop.ct_star(beta)

        # In Q1 the model uses the polynomial directly — should be exact
        assert ct_star_from_4q == pytest.approx(ct_star_from_poly, rel=1e-6)


# ---------------------------------------------------------------------------
# 6. Physical constraints — must hold in ALL quadrants
# ---------------------------------------------------------------------------
class TestPhysicalConstraints:
    """Validate physics-based invariants across all four quadrants."""

    @pytest.fixture
    def prop(self):
        return FourQuadrantPropeller(Z=4, AE_A0=0.70, PD=1.0)

    def test_ct_star_is_periodic(self, prop):
        """CT*(β) must be periodic: CT*(0°) == CT*(360°)."""
        assert prop.ct_star(0.0) == pytest.approx(prop.ct_star(360.0), rel=1e-6)

    def test_cq_star_is_periodic(self, prop):
        """CQ*(β) must be periodic: CQ*(0°) == CQ*(360°)."""
        assert prop.cq_star(0.0) == pytest.approx(prop.cq_star(360.0), rel=1e-6)

    def test_ct_star_positive_at_bollard(self, prop):
        """At β = 0° (bollard ahead), CT* > 0 (forward thrust)."""
        assert prop.ct_star(0.0) > 0

    def test_ct_star_negative_in_crash_stop(self, prop):
        """In Q2 (crash-stop), CT* < 0 near β ≈ 120°–150° (braking force).

        Source: Carlton (2007) Ch. 4 — thrust reverses in 2nd quadrant.
        """
        assert prop.ct_star(135.0) < 0

    def test_cq_star_positive_at_bollard(self, prop):
        """At β = 0° (bollard ahead), CQ* > 0 (resisting torque).

        Source: At bollard, propeller produces maximum torque resisting rotation.
        """
        assert prop.cq_star(0.0) > 0

    def test_cq_star_positive_in_q3(self, prop):
        """In Q3 (backing), CQ* > 0 (torque resists astern rotation)."""
        assert prop.cq_star(200.0) > 0


# ---------------------------------------------------------------------------
# 7. Thrust/torque dimensional output
# ---------------------------------------------------------------------------
class TestDimensionalOutput:
    """Verify thrust and torque computation from CT*, CQ* via V_R."""

    def test_thrust_at_bollard(self):
        """T = CT* · ½ρ · V_R² · π/4 · D² at bollard."""
        rho = 1025.0  # seawater
        n = 2.0       # rev/s
        D = 5.0       # m
        V_A = 0.0     # bollard

        T = four_quadrant_thrust(
            V_A=V_A, n=n, D=D, rho=rho, Z=4, AE_A0=0.70, PD=1.0
        )
        # At bollard, KT ≈ 0.45, T = KT·ρ·n²·D⁴ ≈ 0.45*1025*4*625 ≈ 1.15 MN
        assert T > 0
        assert 500_000 < T < 2_000_000  # 0.5–2.0 MN reasonable for 5m prop

    def test_torque_at_bollard(self):
        """Q = CQ* · ½ρ · V_R² · π/4 · D³ at bollard."""
        rho = 1025.0
        n = 2.0
        D = 5.0
        V_A = 0.0

        Q = four_quadrant_torque(
            V_A=V_A, n=n, D=D, rho=rho, Z=4, AE_A0=0.70, PD=1.0
        )
        assert Q > 0

    def test_thrust_reverses_in_crash_stop(self):
        """During crash-stop (V_A > 0, n < 0), thrust should be negative."""
        T = four_quadrant_thrust(
            V_A=5.0, n=-2.0, D=5.0, rho=1025.0, Z=4, AE_A0=0.70, PD=1.0
        )
        assert T < 0


# ---------------------------------------------------------------------------
# 8. Viviani Table III — propellers with 4-quadrant data
# ---------------------------------------------------------------------------
class TestVivianiBSeriesGeometries:
    """Verify model instantiation for all B-series geometries in Table III."""

    @pytest.mark.parametrize("Z, AE_A0, PD", [
        (3, 0.65, 1.0),
        (4, 0.40, 1.0),
        (4, 0.55, 1.0),
        (4, 0.70, 0.5),
        (4, 0.70, 0.6),
        (4, 0.70, 0.8),
        (4, 0.70, 1.0),
        (4, 0.70, 1.2),
        (4, 0.70, 1.4),
        (4, 0.85, 1.0),
        (4, 1.00, 1.0),
        (5, 0.75, 1.0),
        (6, 0.80, 1.0),
        (7, 0.85, 1.0),
    ])
    def test_propeller_instantiation(self, Z, AE_A0, PD):
        """All Table III propellers must instantiate without error."""
        prop = FourQuadrantPropeller(Z=Z, AE_A0=AE_A0, PD=PD)
        # Must produce finite CT* at β = 0° (bollard)
        ct = prop.ct_star(0.0)
        assert math.isfinite(ct)


# ---------------------------------------------------------------------------
# 9. Mehdipour (2013) STREAMLINE validation data
#    Source: propeller-rudder-literature.md §6, Table 4.3
# ---------------------------------------------------------------------------
class TestMehdipourValidation:
    """Cross-validate against Mehdipour STREAMLINE 7000 DWT tanker data."""

    # STREAMLINE propeller: Z=4, P/D=1.0, EAR=0.58, D=3.85m
    # Experimental open-water values at J=0.629:
    #   KT = 0.246, 10KQ = 0.420, η₀ = 0.600
    def test_streamline_kt_range(self):
        """B4-58 at J=0.629, P/D=1.0: KT should be near 0.246 (±15%)."""
        KT = wageningen_kt(J=0.629, PD=1.0, AE_A0=0.58, Z=4)
        assert KT == pytest.approx(0.246, rel=0.15)

    def test_streamline_efficiency(self):
        """η₀ at design J should be near 0.60 (±20%)."""
        J = 0.629
        KT = wageningen_kt(J=J, PD=1.0, AE_A0=0.58, Z=4)
        KQ = wageningen_kq(J=J, PD=1.0, AE_A0=0.58, Z=4)
        eta0 = J * KT / (2 * math.pi * KQ)
        assert eta0 == pytest.approx(0.600, rel=0.20)
