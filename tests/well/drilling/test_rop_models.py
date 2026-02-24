# ABOUTME: Unit tests for Bourgoyne-Young and Warren ROP prediction models
# ABOUTME: Covers physics behaviour, edge cases, fitting, and false-positive safety

"""Tests for rop_models — BourgoineYoungROP and WarrenROP."""

import math

import pytest

from digitalmodel.well.drilling.rop_models import (
    BourgoineYoungROP,
    RopPrediction,
    WarrenROP,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _by_default() -> BourgoineYoungROP:
    """Return a BourgoineYoungROP with SPE textbook baseline parameters."""
    return BourgoineYoungROP(
        a1=0.5,   # formation strength (lower = harder)
        a2=0.0,   # compaction (normally pressured)
        a3=0.0,   # compaction (transition zone)
        a4=0.3,   # overbalance penalty
        a5=0.5,   # WOB exponent
        a6=0.6,   # RPM exponent
        a7=0.5,   # tooth wear
        a8=0.3,   # jet impact
    )


def _warren_default() -> WarrenROP:
    """Return a WarrenROP with K=10, a=0.6, b=0.5."""
    return WarrenROP(K=10.0, a=0.6, b=0.5)


# ---------------------------------------------------------------------------
# RopPrediction dataclass
# ---------------------------------------------------------------------------

class TestRopPrediction:
    def test_fields_accessible(self):
        pred = RopPrediction(rop_ft_hr=45.0, model="bourgoyne_young")
        assert pred.rop_ft_hr == 45.0
        assert pred.model == "bourgoyne_young"

    def test_rop_non_negative(self):
        pred = RopPrediction(rop_ft_hr=0.0, model="warren")
        assert pred.rop_ft_hr >= 0.0


# ---------------------------------------------------------------------------
# BourgoineYoungROP — construction
# ---------------------------------------------------------------------------

class TestBourgoineYoungROPConstruction:
    def test_default_params_stored(self):
        model = _by_default()
        assert model.a1 == 0.5
        assert model.a5 == 0.5
        assert model.a6 == 0.6

    def test_invalid_wob_raises(self):
        model = _by_default()
        with pytest.raises(ValueError, match="wob_klb"):
            model.predict(
                depth_ft=5000.0, wob_klb=-1.0, rpm=100.0,
                bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.0,
            )

    def test_invalid_rpm_raises(self):
        model = _by_default()
        with pytest.raises(ValueError, match="rpm"):
            model.predict(
                depth_ft=5000.0, wob_klb=20.0, rpm=-10.0,
                bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.0,
            )

    def test_invalid_bit_dia_raises(self):
        model = _by_default()
        with pytest.raises(ValueError, match="bit_dia_in"):
            model.predict(
                depth_ft=5000.0, wob_klb=20.0, rpm=100.0,
                bit_dia_in=0.0, overbalance_kpsi=0.5, bit_wear=0.0,
            )

    def test_bit_wear_out_of_range_raises(self):
        model = _by_default()
        with pytest.raises(ValueError, match="bit_wear"):
            model.predict(
                depth_ft=5000.0, wob_klb=20.0, rpm=100.0,
                bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=1.5,
            )


# ---------------------------------------------------------------------------
# BourgoineYoungROP — physics behaviour
# ---------------------------------------------------------------------------

class TestBourgoineYoungROPPhysics:
    def test_predict_returns_rop_prediction(self):
        model = _by_default()
        result = model.predict(
            depth_ft=5000.0, wob_klb=20.0, rpm=100.0,
            bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.0,
        )
        assert isinstance(result, RopPrediction)
        assert result.model == "bourgoyne_young"

    def test_rop_positive_for_normal_conditions(self):
        model = _by_default()
        result = model.predict(
            depth_ft=5000.0, wob_klb=20.0, rpm=100.0,
            bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.0,
        )
        assert result.rop_ft_hr > 0.0

    def test_higher_wob_increases_rop(self):
        """More WOB on bit → faster penetration (all else equal)."""
        model = _by_default()
        low = model.predict(
            depth_ft=5000.0, wob_klb=10.0, rpm=100.0,
            bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.0,
        )
        high = model.predict(
            depth_ft=5000.0, wob_klb=30.0, rpm=100.0,
            bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.0,
        )
        assert high.rop_ft_hr > low.rop_ft_hr

    def test_higher_rpm_increases_rop(self):
        """More RPM → faster penetration (all else equal)."""
        model = _by_default()
        low = model.predict(
            depth_ft=5000.0, wob_klb=20.0, rpm=60.0,
            bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.0,
        )
        high = model.predict(
            depth_ft=5000.0, wob_klb=20.0, rpm=160.0,
            bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.0,
        )
        assert high.rop_ft_hr > low.rop_ft_hr

    def test_higher_overbalance_reduces_rop(self):
        """Higher mud weight overbalance → chip hold-down → slower drilling."""
        model = _by_default()
        low_ob = model.predict(
            depth_ft=5000.0, wob_klb=20.0, rpm=100.0,
            bit_dia_in=8.5, overbalance_kpsi=0.2, bit_wear=0.0,
        )
        high_ob = model.predict(
            depth_ft=5000.0, wob_klb=20.0, rpm=100.0,
            bit_dia_in=8.5, overbalance_kpsi=2.0, bit_wear=0.0,
        )
        assert high_ob.rop_ft_hr < low_ob.rop_ft_hr

    def test_higher_bit_wear_reduces_rop(self):
        """Worn bit → less cutting ability → slower ROP."""
        model = _by_default()
        fresh = model.predict(
            depth_ft=5000.0, wob_klb=20.0, rpm=100.0,
            bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.0,
        )
        worn = model.predict(
            depth_ft=5000.0, wob_klb=20.0, rpm=100.0,
            bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.8,
        )
        assert worn.rop_ft_hr < fresh.rop_ft_hr

    def test_zero_wob_gives_zero_rop(self):
        """No weight on bit → no penetration."""
        model = _by_default()
        result = model.predict(
            depth_ft=5000.0, wob_klb=0.0, rpm=100.0,
            bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.0,
        )
        assert result.rop_ft_hr == pytest.approx(0.0)

    def test_zero_rpm_gives_zero_rop(self):
        """Static bit → no rotation → no penetration."""
        model = _by_default()
        result = model.predict(
            depth_ft=5000.0, wob_klb=20.0, rpm=0.0,
            bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.0,
        )
        assert result.rop_ft_hr == pytest.approx(0.0)


# ---------------------------------------------------------------------------
# BourgoineYoungROP — sensitivity
# ---------------------------------------------------------------------------

class TestBourgoineYoungROPSensitivity:
    def test_wob_sensitivity(self):
        """sensitivity_wob returns finite positive value."""
        model = _by_default()
        s = model.sensitivity_wob(
            depth_ft=5000.0, wob_klb=20.0, rpm=100.0,
            bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.0,
        )
        assert s > 0.0

    def test_rpm_sensitivity(self):
        """sensitivity_rpm returns finite positive value."""
        model = _by_default()
        s = model.sensitivity_rpm(
            depth_ft=5000.0, wob_klb=20.0, rpm=100.0,
            bit_dia_in=8.5, overbalance_kpsi=0.5, bit_wear=0.0,
        )
        assert s > 0.0


# ---------------------------------------------------------------------------
# WarrenROP — construction and physics
# ---------------------------------------------------------------------------

class TestWarrenROPPhysics:
    def test_predict_returns_rop_prediction(self):
        model = _warren_default()
        result = model.predict(wob_klb=20.0, rpm=100.0, bit_dia_in=8.5)
        assert isinstance(result, RopPrediction)
        assert result.model == "warren"

    def test_rop_positive_for_normal_conditions(self):
        model = _warren_default()
        result = model.predict(wob_klb=20.0, rpm=100.0, bit_dia_in=8.5)
        assert result.rop_ft_hr > 0.0

    def test_higher_wob_increases_rop(self):
        model = _warren_default()
        low = model.predict(wob_klb=10.0, rpm=100.0, bit_dia_in=8.5)
        high = model.predict(wob_klb=30.0, rpm=100.0, bit_dia_in=8.5)
        assert high.rop_ft_hr > low.rop_ft_hr

    def test_higher_rpm_increases_rop(self):
        model = _warren_default()
        low = model.predict(wob_klb=20.0, rpm=60.0, bit_dia_in=8.5)
        high = model.predict(wob_klb=20.0, rpm=160.0, bit_dia_in=8.5)
        assert high.rop_ft_hr > low.rop_ft_hr

    def test_zero_wob_gives_zero_rop(self):
        model = _warren_default()
        result = model.predict(wob_klb=0.0, rpm=100.0, bit_dia_in=8.5)
        assert result.rop_ft_hr == pytest.approx(0.0)

    def test_zero_rpm_gives_zero_rop(self):
        model = _warren_default()
        result = model.predict(wob_klb=20.0, rpm=0.0, bit_dia_in=8.5)
        assert result.rop_ft_hr == pytest.approx(0.0)

    def test_invalid_wob_raises(self):
        model = _warren_default()
        with pytest.raises(ValueError, match="wob_klb"):
            model.predict(wob_klb=-5.0, rpm=100.0, bit_dia_in=8.5)

    def test_invalid_rpm_raises(self):
        model = _warren_default()
        with pytest.raises(ValueError, match="rpm"):
            model.predict(wob_klb=20.0, rpm=-10.0, bit_dia_in=8.5)

    def test_invalid_bit_dia_raises(self):
        model = _warren_default()
        with pytest.raises(ValueError, match="bit_dia_in"):
            model.predict(wob_klb=20.0, rpm=100.0, bit_dia_in=0.0)

    def test_power_law_formula(self):
        """Verify K*(WOB/d_b)^a * RPM^b matches manual calculation."""
        K, a, b = 10.0, 0.6, 0.5
        wob, rpm, d = 20.0, 100.0, 8.5
        expected = K * (wob / d) ** a * rpm ** b
        model = WarrenROP(K=K, a=a, b=b)
        result = model.predict(wob_klb=wob, rpm=rpm, bit_dia_in=d)
        assert result.rop_ft_hr == pytest.approx(expected, rel=1e-6)


# ---------------------------------------------------------------------------
# WarrenROP.fit_from_data — log-linear regression
# ---------------------------------------------------------------------------

class TestWarrenROPFit:
    def _generate_synthetic_data(
        self, K: float, a: float, b: float, n: int = 20
    ) -> tuple[list[float], list[float], list[float], list[float]]:
        """Generate synthetic ROP data from known K, a, b for regression test."""
        import random
        rng = random.Random(42)
        wobs, rpms, bits, rops = [], [], [], []
        for _ in range(n):
            wob = rng.uniform(10.0, 40.0)
            rpm = rng.uniform(60.0, 160.0)
            dia = 8.5
            rop = K * (wob / dia) ** a * rpm ** b
            wobs.append(wob)
            rpms.append(rpm)
            bits.append(dia)
            rops.append(rop)
        return wobs, rpms, bits, rops

    def test_fit_recovers_parameters(self):
        """fit_from_data should recover K, a, b from noiseless synthetic data."""
        K_true, a_true, b_true = 8.5, 0.55, 0.45
        wobs, rpms, bits, rops = self._generate_synthetic_data(K_true, a_true, b_true)
        model = WarrenROP.fit_from_data(
            wob_klb=wobs, rpm=rpms, bit_dia_in=bits, rop_ft_hr=rops
        )
        assert model.K == pytest.approx(K_true, rel=0.01)
        assert model.a == pytest.approx(a_true, rel=0.01)
        assert model.b == pytest.approx(b_true, rel=0.01)

    def test_fit_requires_minimum_points(self):
        """At least 3 data points required (3 unknowns)."""
        with pytest.raises(ValueError, match="at least"):
            WarrenROP.fit_from_data(
                wob_klb=[20.0, 25.0],
                rpm=[100.0, 110.0],
                bit_dia_in=[8.5, 8.5],
                rop_ft_hr=[40.0, 45.0],
            )

    def test_fit_mismatched_lengths_raises(self):
        with pytest.raises(ValueError, match="same length"):
            WarrenROP.fit_from_data(
                wob_klb=[20.0, 25.0, 30.0],
                rpm=[100.0, 110.0],
                bit_dia_in=[8.5, 8.5, 8.5],
                rop_ft_hr=[40.0, 45.0, 50.0],
            )

    def test_fit_predicts_within_5pct_on_training_data(self):
        """Fitted model should predict noiseless training data within 5%."""
        K_true, a_true, b_true = 12.0, 0.7, 0.4
        wobs, rpms, bits, rops = self._generate_synthetic_data(K_true, a_true, b_true)
        model = WarrenROP.fit_from_data(
            wob_klb=wobs, rpm=rpms, bit_dia_in=bits, rop_ft_hr=rops
        )
        for wob, rpm, dia, rop_actual in zip(wobs, rpms, bits, rops):
            pred = model.predict(wob_klb=wob, rpm=rpm, bit_dia_in=dia)
            assert pred.rop_ft_hr == pytest.approx(rop_actual, rel=0.05)
