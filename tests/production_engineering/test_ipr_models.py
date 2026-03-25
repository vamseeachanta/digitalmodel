# ABOUTME: TDD tests for Inflow Performance Relationship (IPR) models
# ABOUTME: Covers Vogel (1968), Fetkovich (1973), Composite PI, and PI linear IPR

"""Tests for production_engineering.ipr_models module."""

import math

import pytest

from digitalmodel.production_engineering.ipr_models import (
    CompositeIpr,
    FetkovichIpr,
    LinearIpr,
    ReservoirConditions,
    VogelIpr,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

def make_reservoir(**overrides) -> ReservoirConditions:
    defaults = dict(
        reservoir_pressure_psi=3000.0,
        bubble_point_psi=2500.0,
        productivity_index_bopd_psi=2.0,
    )
    defaults.update(overrides)
    return ReservoirConditions(**defaults)


# ---------------------------------------------------------------------------
# Vogel IPR
# ---------------------------------------------------------------------------

class TestVogelIpr:
    """Vogel (1968): q/qmax = 1 - 0.2*(Pwf/Pr) - 0.8*(Pwf/Pr)^2"""

    def setup_method(self):
        res = make_reservoir(reservoir_pressure_psi=3000, bubble_point_psi=3000)
        self.model = VogelIpr(reservoir=res, qmax_bopd=1000.0)

    def test_zero_pwf_gives_aof(self):
        # At Pwf=0, q should equal qmax (AOF)
        q = self.model.flow_rate(pwf_psi=0.0)
        assert q == pytest.approx(1000.0, rel=0.01)

    def test_pwf_equal_pr_gives_zero_rate(self):
        q = self.model.flow_rate(pwf_psi=3000.0)
        assert q == pytest.approx(0.0, abs=1.0)

    def test_intermediate_pwf_follows_vogel_equation(self):
        # q/qmax = 1 - 0.2*(Pwf/Pr) - 0.8*(Pwf/Pr)^2
        Pwf, Pr, qmax = 1500.0, 3000.0, 1000.0
        ratio = Pwf / Pr
        expected = qmax * (1 - 0.2 * ratio - 0.8 * ratio**2)
        q = self.model.flow_rate(pwf_psi=Pwf)
        assert q == pytest.approx(expected, rel=0.01)

    def test_flowing_pressure_from_rate(self):
        # Inverse: given q, find Pwf
        q_target = 500.0
        pwf = self.model.flowing_pressure(q_bopd=q_target)
        # Verify by forward calculation
        q_back = self.model.flow_rate(pwf_psi=pwf)
        assert q_back == pytest.approx(q_target, rel=0.01)

    def test_rate_increases_as_pwf_decreases(self):
        pressures = [2500.0, 2000.0, 1500.0, 1000.0, 500.0]
        rates = [self.model.flow_rate(p) for p in pressures]
        assert all(rates[i] < rates[i + 1] for i in range(len(rates) - 1))

    def test_aof_from_pi_and_vogel(self):
        # qmax = PI * Pr / 1.8 (standard Vogel derivation)
        res = make_reservoir(reservoir_pressure_psi=3000, bubble_point_psi=3000)
        model = VogelIpr.from_productivity_index(
            reservoir=res, pi_bopd_psi=2.0
        )
        expected_qmax = 2.0 * 3000.0 / 1.8
        assert model.qmax_bopd == pytest.approx(expected_qmax, rel=0.01)


# ---------------------------------------------------------------------------
# Fetkovich IPR
# ---------------------------------------------------------------------------

class TestFetkovichIpr:
    """Fetkovich (1973): q = C*(Pr^2 - Pwf^2)^n"""

    def setup_method(self):
        res = make_reservoir(reservoir_pressure_psi=3000)
        self.model = FetkovichIpr(reservoir=res, c_coeff=0.0001, n_exponent=1.0)

    def test_zero_pwf_gives_maximum_rate(self):
        q_max = self.model.flow_rate(pwf_psi=0.0)
        expected = 0.0001 * (3000.0**2) ** 1.0
        assert q_max == pytest.approx(expected, rel=0.01)

    def test_pwf_equal_pr_gives_zero(self):
        q = self.model.flow_rate(pwf_psi=3000.0)
        assert q == pytest.approx(0.0, abs=0.1)

    def test_n_exponent_effect(self):
        res = make_reservoir(reservoir_pressure_psi=3000)
        model_n1 = FetkovichIpr(reservoir=res, c_coeff=1e-6, n_exponent=1.0)
        model_n05 = FetkovichIpr(reservoir=res, c_coeff=1e-3, n_exponent=0.5)
        # Both should give positive rates at half reservoir pressure
        assert model_n1.flow_rate(pwf_psi=1500) > 0
        assert model_n05.flow_rate(pwf_psi=1500) > 0

    def test_flowing_pressure_inverse(self):
        q_target = 200.0
        pwf = self.model.flowing_pressure(q_bopd=q_target)
        q_back = self.model.flow_rate(pwf_psi=pwf)
        assert q_back == pytest.approx(q_target, rel=0.01)


# ---------------------------------------------------------------------------
# Linear (PI) IPR — above bubble point
# ---------------------------------------------------------------------------

class TestLinearIpr:
    """Linear IPR: q = PI * (Pr - Pwf), valid above bubble point."""

    def setup_method(self):
        res = make_reservoir(reservoir_pressure_psi=3000)
        self.model = LinearIpr(reservoir=res, pi_bopd_psi=2.0)

    def test_rate_at_half_pressure(self):
        # q = 2.0 * (3000 - 1500) = 3000 bopd
        q = self.model.flow_rate(pwf_psi=1500.0)
        assert q == pytest.approx(3000.0, rel=0.01)

    def test_aof(self):
        q = self.model.flow_rate(pwf_psi=0.0)
        assert q == pytest.approx(6000.0, rel=0.01)

    def test_zero_rate_at_reservoir_pressure(self):
        q = self.model.flow_rate(pwf_psi=3000.0)
        assert q == pytest.approx(0.0, abs=0.1)

    def test_flowing_pressure_inverse(self):
        q_target = 1500.0
        pwf = self.model.flowing_pressure(q_bopd=q_target)
        assert pwf == pytest.approx(2250.0, rel=0.01)  # 3000 - 1500/2


# ---------------------------------------------------------------------------
# Composite IPR — Klins-Clark: linear above Pb, Vogel below Pb
# ---------------------------------------------------------------------------

class TestCompositeIpr:
    """Composite IPR: linear above bubble point, Vogel shape below."""

    def setup_method(self):
        res = make_reservoir(
            reservoir_pressure_psi=3000,
            bubble_point_psi=2000,
            productivity_index_bopd_psi=1.0,
        )
        self.model = CompositeIpr(reservoir=res)

    def test_rate_above_bubble_point_is_linear(self):
        # At Pwf > Pb, q = PI * (Pr - Pwf)
        q = self.model.flow_rate(pwf_psi=2500.0)
        # q = 1.0 * (3000 - 2500) = 500 bopd
        assert q == pytest.approx(500.0, rel=0.02)

    def test_rate_at_bubble_point(self):
        # q_b = PI * (Pr - Pb) = 1.0 * (3000 - 2000) = 1000 bopd
        q = self.model.flow_rate(pwf_psi=2000.0)
        assert q == pytest.approx(1000.0, rel=0.02)

    def test_rate_below_bubble_point_exceeds_bubble_point_rate(self):
        q_at_pb = self.model.flow_rate(pwf_psi=2000.0)
        q_below_pb = self.model.flow_rate(pwf_psi=1000.0)
        assert q_below_pb > q_at_pb

    def test_rate_at_zero_pwf_is_aof(self):
        q_aof = self.model.flow_rate(pwf_psi=0.0)
        # AOF > qb = 1000
        assert q_aof > 1000.0

    def test_flowing_pressure_above_bubble_point(self):
        q_target = 300.0
        pwf = self.model.flowing_pressure(q_bopd=q_target)
        assert pwf > 2000.0  # above bubble point
        q_back = self.model.flow_rate(pwf_psi=pwf)
        assert q_back == pytest.approx(q_target, rel=0.02)

    def test_flowing_pressure_below_bubble_point(self):
        q_target = 1500.0  # above q_b, so Pwf < Pb
        pwf = self.model.flowing_pressure(q_bopd=q_target)
        assert pwf < 2000.0  # below bubble point
        q_back = self.model.flow_rate(pwf_psi=pwf)
        assert q_back == pytest.approx(q_target, rel=0.02)

    def test_curve_is_monotonically_decreasing(self):
        pressures = [2800, 2400, 2000, 1600, 1200, 800, 400, 0]
        rates = [self.model.flow_rate(p) for p in pressures]
        assert all(rates[i] < rates[i + 1] for i in range(len(rates) - 1))
