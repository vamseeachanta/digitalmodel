"""
Tests for sea-state scatter diagram fatigue analysis.

Validates scatter_fatigue_damage against known single- and multi-sea-state
scenarios using simple synthetic inputs (flat transfer function, power-law SN).
"""

import numpy as np
import pytest

from digitalmodel.structural.fatigue.scatter_fatigue import (
    SeaStateEntry,
    ScatterFatigueResult,
    scatter_fatigue_damage,
)
from digitalmodel.structural.fatigue.sn_curves import PowerLawSNCurve


# ---------------------------------------------------------------------------
# Fixtures (local, self-contained)
# ---------------------------------------------------------------------------


@pytest.fixture
def simple_sn():
    """N = 1e12 * S^(-3), no fatigue limit."""
    return PowerLawSNCurve(name="TestSN", A=1e12, m=3.0, fatigue_limit=0.0)


@pytest.fixture
def freqs():
    """Frequency array 0.01-0.5 Hz, 200 points."""
    return np.linspace(0.01, 0.5, 200)


@pytest.fixture
def flat_tf():
    """Constant transfer function |H(f)| = 1.0."""
    return lambda f: np.ones_like(f, dtype=float)


def _jonswap_hz(freqs_hz, hs, tp, gamma=3.3):
    """Minimal JONSWAP in Hz for test use (no external dependency)."""
    g = 9.81
    fp = 1.0 / tp
    alpha = 5.0 / 16.0 * hs**2 * (2 * np.pi * fp) ** 4 / g**2
    omega = 2 * np.pi * freqs_hz
    omega_p = 2 * np.pi * fp
    sigma = np.where(omega <= omega_p, 0.07, 0.09)
    S_pm = alpha * g**2 / omega**5 * np.exp(-1.25 * (omega_p / omega) ** 4)
    A_peak = np.exp(-(omega - omega_p) ** 2 / (2 * sigma**2 * omega_p**2))
    S_jonswap = S_pm * gamma**A_peak
    # Convert from rad/s density to Hz density: S(f) = 2*pi * S(omega)
    return S_jonswap * 2 * np.pi


# ---------------------------------------------------------------------------
# Test: single sea state matches direct frequency domain call
# ---------------------------------------------------------------------------


class TestSingleSeaState:
    """Single-sea-state scatter must match direct frequency-domain damage."""

    def test_single_state_damage_matches_direct(self, simple_sn, freqs, flat_tf):
        """Damage from a 1-entry scatter table must equal direct spectral damage."""
        from digitalmodel.structural.fatigue.frequency_domain import DirlikMethod

        hs, tp = 2.0, 8.0
        design_life_years = 25.0
        design_life_seconds = design_life_years * 365.25 * 24 * 3600

        # Build wave PSD (JONSWAP)
        wave_psd = _jonswap_hz(freqs, hs, tp)

        # Response PSD = |H(f)|^2 * S_wave(f) -- flat TF so response == wave
        response_psd = wave_psd

        # Direct frequency-domain damage
        dirlik = DirlikMethod()
        fd_result = dirlik.calculate_damage_rate(freqs, response_psd, simple_sn)
        direct_damage = fd_result.damage_rate * design_life_seconds

        # Scatter table with single entry (probability=1.0)
        table = [SeaStateEntry(hs_m=hs, tp_s=tp, probability=1.0)]

        def wave_spec_func(f, hs_val, tp_val):
            return _jonswap_hz(f, hs_val, tp_val)

        result = scatter_fatigue_damage(
            scatter_table=table,
            stress_transfer_function=flat_tf,
            sn_curve=simple_sn,
            frequencies=freqs,
            method="dirlik",
            design_life_years=design_life_years,
            wave_spectrum_func=wave_spec_func,
        )

        assert isinstance(result, ScatterFatigueResult)
        assert result.n_sea_states == 1
        np.testing.assert_allclose(result.total_damage, direct_damage, rtol=1e-6)


# ---------------------------------------------------------------------------
# Test: two equal-probability sea states
# ---------------------------------------------------------------------------


class TestTwoSeaStates:
    """Two sea states with equal probability -- damage is average of individual."""

    def test_two_states_weighted_sum(self, simple_sn, freqs, flat_tf):
        hs1, tp1 = 1.5, 7.0
        hs2, tp2 = 3.0, 10.0

        table = [
            SeaStateEntry(hs_m=hs1, tp_s=tp1, probability=0.5),
            SeaStateEntry(hs_m=hs2, tp_s=tp2, probability=0.5),
        ]

        def wave_spec_func(f, hs_val, tp_val):
            return _jonswap_hz(f, hs_val, tp_val)

        result = scatter_fatigue_damage(
            scatter_table=table,
            stress_transfer_function=flat_tf,
            sn_curve=simple_sn,
            frequencies=freqs,
            method="dirlik",
            design_life_years=25.0,
            wave_spectrum_func=wave_spec_func,
        )

        assert result.n_sea_states == 2
        assert result.total_damage > 0
        # Verify per-sea-state fractions sum to 1.0
        fractions = [s["damage_fraction"] for s in result.per_sea_state]
        np.testing.assert_allclose(sum(fractions), 1.0, atol=1e-6)


# ---------------------------------------------------------------------------
# Test: probability validation
# ---------------------------------------------------------------------------


class TestProbabilityValidation:
    """Scatter table probabilities must sum to ~1.0."""

    def test_probabilities_not_summing_to_one_raises(self, simple_sn, freqs, flat_tf):
        table = [
            SeaStateEntry(hs_m=2.0, tp_s=8.0, probability=0.3),
            SeaStateEntry(hs_m=3.0, tp_s=10.0, probability=0.3),
        ]

        def wave_spec_func(f, hs_val, tp_val):
            return _jonswap_hz(f, hs_val, tp_val)

        with pytest.raises(ValueError, match="[Pp]robabilit"):
            scatter_fatigue_damage(
                scatter_table=table,
                stress_transfer_function=flat_tf,
                sn_curve=simple_sn,
                frequencies=freqs,
                wave_spectrum_func=wave_spec_func,
            )


# ---------------------------------------------------------------------------
# Test: zero Hs contributes zero damage
# ---------------------------------------------------------------------------


class TestZeroHs:
    """Sea state with Hs=0 should contribute zero damage (calm sea)."""

    def test_zero_hs_zero_damage(self, simple_sn, freqs, flat_tf):
        table = [
            SeaStateEntry(hs_m=0.0, tp_s=8.0, probability=0.5),
            SeaStateEntry(hs_m=2.0, tp_s=8.0, probability=0.5),
        ]

        def wave_spec_func(f, hs_val, tp_val):
            return _jonswap_hz(f, hs_val, tp_val)

        result = scatter_fatigue_damage(
            scatter_table=table,
            stress_transfer_function=flat_tf,
            sn_curve=simple_sn,
            frequencies=freqs,
            wave_spectrum_func=wave_spec_func,
        )

        # First sea state (Hs=0) must have zero damage
        assert result.per_sea_state[0]["damage"] == pytest.approx(0.0, abs=1e-15)


# ---------------------------------------------------------------------------
# Test: all-zero transfer function -> zero total damage
# ---------------------------------------------------------------------------


class TestZeroTransferFunction:
    """All-zero transfer function means no stress response -> zero damage."""

    def test_zero_tf_zero_damage(self, simple_sn, freqs):
        zero_tf = lambda f: np.zeros_like(f, dtype=float)

        table = [SeaStateEntry(hs_m=3.0, tp_s=10.0, probability=1.0)]

        def wave_spec_func(f, hs_val, tp_val):
            return _jonswap_hz(f, hs_val, tp_val)

        result = scatter_fatigue_damage(
            scatter_table=table,
            stress_transfer_function=zero_tf,
            sn_curve=simple_sn,
            frequencies=freqs,
            wave_spectrum_func=wave_spec_func,
        )

        assert result.total_damage == pytest.approx(0.0, abs=1e-15)


# ---------------------------------------------------------------------------
# Test: fatigue life calculation
# ---------------------------------------------------------------------------


class TestFatigueLife:
    """fatigue_life_years = design_life / total_damage."""

    def test_fatigue_life_inverse_of_damage(self, simple_sn, freqs, flat_tf):
        design_life = 25.0
        table = [SeaStateEntry(hs_m=2.5, tp_s=9.0, probability=1.0)]

        def wave_spec_func(f, hs_val, tp_val):
            return _jonswap_hz(f, hs_val, tp_val)

        result = scatter_fatigue_damage(
            scatter_table=table,
            stress_transfer_function=flat_tf,
            sn_curve=simple_sn,
            frequencies=freqs,
            design_life_years=design_life,
            wave_spectrum_func=wave_spec_func,
        )

        if result.total_damage > 0:
            expected_life = design_life / result.total_damage
            assert result.fatigue_life_years == pytest.approx(expected_life, rel=1e-6)


# ---------------------------------------------------------------------------
# Test: ScatterFatigueResult fields
# ---------------------------------------------------------------------------


class TestResultStructure:
    """Verify result dataclass has all required fields."""

    def test_result_fields(self, simple_sn, freqs, flat_tf):
        table = [SeaStateEntry(hs_m=2.0, tp_s=8.0, probability=1.0)]

        def wave_spec_func(f, hs_val, tp_val):
            return _jonswap_hz(f, hs_val, tp_val)

        result = scatter_fatigue_damage(
            scatter_table=table,
            stress_transfer_function=flat_tf,
            sn_curve=simple_sn,
            frequencies=freqs,
            wave_spectrum_func=wave_spec_func,
        )

        assert hasattr(result, "total_damage")
        assert hasattr(result, "fatigue_life_years")
        assert hasattr(result, "per_sea_state")
        assert hasattr(result, "method")
        assert hasattr(result, "n_sea_states")
        assert hasattr(result, "design_life_years")
        assert result.method == "dirlik"
        assert result.design_life_years == 25.0


# ---------------------------------------------------------------------------
# Test: unknown method raises ValueError
# ---------------------------------------------------------------------------


class TestUnknownMethod:
    """Unknown spectral method must raise ValueError."""

    def test_unknown_method_raises(self, simple_sn, freqs, flat_tf):
        table = [SeaStateEntry(hs_m=2.0, tp_s=8.0, probability=1.0)]

        def wave_spec_func(f, hs_val, tp_val):
            return _jonswap_hz(f, hs_val, tp_val)

        with pytest.raises(ValueError, match="Unknown method"):
            scatter_fatigue_damage(
                scatter_table=table,
                stress_transfer_function=flat_tf,
                sn_curve=simple_sn,
                frequencies=freqs,
                method="bogus_method",
                wave_spectrum_func=wave_spec_func,
            )


# ---------------------------------------------------------------------------
# Test: array transfer function (not callable)
# ---------------------------------------------------------------------------


class TestArrayTransferFunction:
    """Pass ndarray instead of callable as transfer function."""

    def test_array_tf_matches_callable(self, simple_sn, freqs, flat_tf):
        """Array TF with all ones should match callable flat TF."""
        table = [SeaStateEntry(hs_m=2.0, tp_s=8.0, probability=1.0)]

        def wave_spec_func(f, hs_val, tp_val):
            return _jonswap_hz(f, hs_val, tp_val)

        # Use array instead of callable
        tf_array = np.ones_like(freqs)

        result_arr = scatter_fatigue_damage(
            scatter_table=table,
            stress_transfer_function=tf_array,
            sn_curve=simple_sn,
            frequencies=freqs,
            wave_spectrum_func=wave_spec_func,
        )

        result_fn = scatter_fatigue_damage(
            scatter_table=table,
            stress_transfer_function=flat_tf,
            sn_curve=simple_sn,
            frequencies=freqs,
            wave_spectrum_func=wave_spec_func,
        )

        np.testing.assert_allclose(
            result_arr.total_damage, result_fn.total_damage, rtol=1e-10
        )

    def test_array_tf_wrong_shape_raises(self, simple_sn, freqs):
        """Array TF with wrong shape must raise ValueError."""
        table = [SeaStateEntry(hs_m=2.0, tp_s=8.0, probability=1.0)]

        def wave_spec_func(f, hs_val, tp_val):
            return _jonswap_hz(f, hs_val, tp_val)

        wrong_shape = np.ones(10)  # freqs has 200 points

        with pytest.raises(ValueError, match="shape"):
            scatter_fatigue_damage(
                scatter_table=table,
                stress_transfer_function=wrong_shape,
                sn_curve=simple_sn,
                frequencies=freqs,
                wave_spectrum_func=wave_spec_func,
            )


# ---------------------------------------------------------------------------
# Test: duration_hours override
# ---------------------------------------------------------------------------


class TestDurationHours:
    """SeaStateEntry with explicit duration_hours overrides probability scaling."""

    def test_duration_hours_overrides_probability(self, simple_sn, freqs, flat_tf):
        """When duration_hours > 0, damage uses explicit duration not probability."""
        # 1 hour duration
        table = [SeaStateEntry(hs_m=2.0, tp_s=8.0, probability=1.0, duration_hours=1.0)]

        def wave_spec_func(f, hs_val, tp_val):
            return _jonswap_hz(f, hs_val, tp_val)

        result_hours = scatter_fatigue_damage(
            scatter_table=table,
            stress_transfer_function=flat_tf,
            sn_curve=simple_sn,
            frequencies=freqs,
            design_life_years=25.0,
            wave_spectrum_func=wave_spec_func,
        )

        # With probability scaling, duration would be 25 * 365.25 * 24 * 3600 seconds
        # With explicit 1 hour, it's 3600 seconds -- much less damage
        result_prob = scatter_fatigue_damage(
            scatter_table=[SeaStateEntry(hs_m=2.0, tp_s=8.0, probability=1.0)],
            stress_transfer_function=flat_tf,
            sn_curve=simple_sn,
            frequencies=freqs,
            design_life_years=25.0,
            wave_spectrum_func=wave_spec_func,
        )

        # Duration_hours=1 should give much less damage than 25-year probability=1
        assert result_hours.total_damage < result_prob.total_damage
        assert result_hours.total_damage > 0
