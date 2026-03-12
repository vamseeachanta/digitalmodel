"""Doc-verified tests for drilling riser structural damping calculations.

References:
- 50043-PRS-0001-1 — DeepStar Structural Damping Test
- 2H-TNE-0050-03 §4.9 — Damping
- Rayleigh damping theory for structural dynamics
"""

import pytest

from digitalmodel.drilling_riser.damping import (
    modal_damping_equivalent,
    rayleigh_damping_coefficients,
    structural_damping_ratio,
)


class TestStructuralDampingRatio:
    """Tabular damping ratio lookup per DeepStar test data."""

    def test_bare_steel_pipe(self):
        """Plain bare steel pipe = 0.003 (0.3%) per DeepStar §3."""
        result = structural_damping_ratio(material="steel", joint_type="bare")
        assert result == pytest.approx(0.003, rel=0.01)

    def test_buoyant_joint(self):
        """Buoyant joint has higher damping than bare pipe."""
        bare = structural_damping_ratio("steel", "bare")
        buoyant = structural_damping_ratio("steel", "buoyant")
        assert buoyant > bare

    def test_bolted_connector(self):
        """Bolted connectors add damping through friction."""
        bare = structural_damping_ratio("steel", "bare")
        bolted = structural_damping_ratio("steel", "bolted_connector")
        assert bolted > bare

    def test_damping_always_positive(self):
        """Damping ratio must be positive for all types."""
        for jt in ["bare", "buoyant", "bolted_connector"]:
            result = structural_damping_ratio("steel", jt)
            assert result > 0.0


class TestRayleighDampingCoefficients:
    """Rayleigh damping: C = alpha*M + beta*K."""

    def test_symmetric_frequencies(self):
        """alpha and beta from two target frequencies and damping ratio."""
        alpha, beta = rayleigh_damping_coefficients(
            omega_1=0.5, omega_2=2.0, zeta=0.03
        )
        # alpha = 2*zeta*w1*w2/(w1+w2) = 2*0.03*0.5*2.0/2.5 = 0.024
        # beta = 2*zeta/(w1+w2) = 2*0.03/2.5 = 0.024
        assert alpha == pytest.approx(0.024, rel=0.02)
        assert beta == pytest.approx(0.024, rel=0.02)

    def test_alpha_positive(self):
        """Mass-proportional coefficient must be positive."""
        alpha, _ = rayleigh_damping_coefficients(0.3, 1.5, 0.02)
        assert alpha > 0.0

    def test_beta_positive(self):
        """Stiffness-proportional coefficient must be positive."""
        _, beta = rayleigh_damping_coefficients(0.3, 1.5, 0.02)
        assert beta > 0.0

    def test_higher_damping_gives_larger_coefficients(self):
        """More damping -> larger alpha and beta."""
        a1, b1 = rayleigh_damping_coefficients(0.5, 2.0, 0.01)
        a2, b2 = rayleigh_damping_coefficients(0.5, 2.0, 0.05)
        assert a2 > a1
        assert b2 > b1


class TestModalDampingEquivalent:
    """Compute equivalent modal damping from Rayleigh coefficients."""

    def test_modal_damping_at_target_frequency(self):
        """At mid-frequency, damping should approximate target zeta."""
        # For omega_1=0.5, omega_2=2.0, zeta=0.03:
        alpha, beta = rayleigh_damping_coefficients(0.5, 2.0, 0.03)
        freq_hz = 1.0 / (2 * 3.14159)  # omega=1.0 rad/s
        result = modal_damping_equivalent(alpha, beta, freq_hz)
        # At omega=1.0: zeta = alpha/(2*omega) + beta*omega/2
        # = 0.024/2 + 0.024*1/2 = 0.012 + 0.012 = 0.024
        # (slightly less than 0.03 because omega=1.0 is not exactly mid)
        assert 0.01 < result < 0.06

    def test_modal_damping_increases_with_alpha(self):
        """Higher mass-proportional damping increases modal damping."""
        d1 = modal_damping_equivalent(0.01, 0.02, 0.5)
        d2 = modal_damping_equivalent(0.05, 0.02, 0.5)
        assert d2 > d1

    def test_modal_damping_increases_with_beta(self):
        """Higher stiffness-proportional damping increases modal damping."""
        d1 = modal_damping_equivalent(0.02, 0.01, 0.5)
        d2 = modal_damping_equivalent(0.02, 0.05, 0.5)
        assert d2 > d1
