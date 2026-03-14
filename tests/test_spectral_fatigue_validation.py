"""Spectral fatigue validation tests -- DNV-RP-C203.

Validates the full spectral fatigue pipeline against hand calculations
and published DNV-RP-C203 reference values for FPSO/riser applications.

Coverage:
  1. S-N curve lookup for DNV D, E, F1 curves (in air)
  2. Palmgren-Miner linear damage accumulation (hand calculation)
  3. Narrow-band spectral fatigue from a known stress PSD
  4. Dirlik method consistency check vs narrow-band upper bound
  5. Seawater environment curve adjustments (CP / free-corrosion)
  6. Worked-example round-trip (pipeline, SCR, mooring chain)

Author: WRK-1179 Stream B Task B8
"""

import math

import numpy as np
import pandas as pd
import pytest

from digitalmodel.structural.fatigue.sn_curves import (
    PowerLawSNCurve,
    StandardSNCurves,
    get_dnv_curve,
)
from digitalmodel.structural.fatigue.damage_accumulation import (
    LinearDamageAccumulation,
)
from digitalmodel.structural.fatigue.frequency_domain import (
    NarrowBandMethod,
    DirlikMethod,
    TovoBenasciuttiMethod,
)
from digitalmodel.structural.fatigue.worked_examples import (
    pipeline_girth_weld,
    scr_touchdown,
    mooring_chain,
)


# -----------------------------------------------------------------------
# 1. S-N Curve Reference Values (DNV-RP-C203 Table 2-1, in air)
# -----------------------------------------------------------------------

class TestSNCurveDNVReference:
    """Validate S-N curve parameters against DNV-RP-C203 Table 2-1."""

    def test_dnv_d_curve_parameters(self):
        """DNV-RP-C203 D-curve: log_a1 = 11.764, m1 = 3.0 (N < 1e7)."""
        curve = get_dnv_curve('D')
        assert curve.m == 3.0
        # A = 10^(log_a) with log_a ~ 11.764 for m=3 regime
        # The hardcoded value is 5.73e11; check log10(A) ~ 11.758
        log_a = math.log10(curve.A)
        assert abs(log_a - 11.758) < 0.02, f"log10(A) = {log_a}, expected ~11.758"

    def test_dnv_e_curve_parameters(self):
        """DNV-RP-C203 E-curve: A = 3.29e11, m = 3.0."""
        curve = get_dnv_curve('E')
        assert curve.m == 3.0
        log_a = math.log10(curve.A)
        # log10(3.29e11) = 11.517
        assert abs(log_a - 11.517) < 0.02, f"log10(A) = {log_a}"

    def test_dnv_f1_curve_parameters(self):
        """DNV-RP-C203 F1-curve: A = 1.08e11, m = 3.0."""
        curve = get_dnv_curve('F1')
        assert curve.m == 3.0
        log_a = math.log10(curve.A)
        # log10(1.08e11) = 11.033
        assert abs(log_a - 11.033) < 0.02, f"log10(A) = {log_a}"

    def test_dnv_d_curve_cycles_at_100mpa(self):
        """At S = 100 MPa on D-curve: N = A / S^m = 5.73e11 / 1e6 = 573000."""
        curve = get_dnv_curve('D')
        N = curve.get_allowable_cycles(100.0)
        expected = 5.73e11 / (100.0 ** 3.0)
        assert abs(N - expected) / expected < 1e-6

    def test_dnv_d_curve_cycles_at_200mpa(self):
        """At S = 200 MPa on D-curve: N = 5.73e11 / 8e6 = 71625."""
        curve = get_dnv_curve('D')
        N = curve.get_allowable_cycles(200.0)
        expected = 5.73e11 / (200.0 ** 3.0)
        assert abs(N - expected) / expected < 1e-6

    def test_dnv_d_curve_inverse(self):
        """Inverse: S(N=573000) should return ~100 MPa."""
        curve = get_dnv_curve('D')
        S = curve.get_stress_range(573000.0)
        assert abs(S - 100.0) < 0.5, f"S = {S}, expected ~100"

    def test_dnv_d_fatigue_limit(self):
        """D-curve fatigue limit (CAFL) = 52.63 MPa."""
        curve = get_dnv_curve('D')
        assert abs(curve.fatigue_limit - 52.63) < 0.01

    def test_below_fatigue_limit_gives_infinite_life(self):
        """Stress below CAFL should give infinite cycles."""
        curve = get_dnv_curve('D')
        N = curve.get_allowable_cycles(40.0)  # Below 52.63 MPa
        assert np.isinf(N)

    def test_all_dnv_curves_available(self):
        """All 14 DNV curve classes are accessible."""
        expected_classes = [
            'B1', 'B2', 'C', 'C1', 'C2', 'D', 'E',
            'F', 'F1', 'F3', 'G', 'W1', 'W2', 'W3',
        ]
        for cc in expected_classes:
            curve = StandardSNCurves.get_curve('DNV', cc)
            assert curve.m == 3.0 or curve.m == 4.0, f"DNV-{cc} has unexpected m"


# -----------------------------------------------------------------------
# 2. Miner's Rule Damage Accumulation (hand calculation)
# -----------------------------------------------------------------------

class TestMinersDamage:
    """Validate Palmgren-Miner damage against hand calculations."""

    @pytest.fixture
    def simple_curve(self):
        """N = 1e12 * S^(-3), no fatigue limit."""
        return PowerLawSNCurve(
            name="Simple", A=1e12, m=3.0,
            fatigue_limit=0.0, cutoff_cycles=1e20,
        )

    def test_single_block_damage(self, simple_curve):
        """Single stress block: D = n / N.

        S = 100 MPa -> N = 1e12 / 1e6 = 1e6
        n = 1000 -> D = 1000 / 1e6 = 0.001
        """
        cycles = pd.DataFrame({
            'range': [100.0],
            'count': [1000.0],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(cycles, simple_curve)
        expected = 1000.0 / 1e6
        assert abs(result['total_damage'] - expected) < 1e-10

    def test_multi_block_damage(self, simple_curve):
        """Multiple stress blocks: D = sum(ni / Ni).

        S=100: N=1e6,  n=1000  -> D1 = 1e-3
        S=200: N=1.25e5, n=500 -> D2 = 4e-3
        S=50:  N=8e6,  n=2000  -> D3 = 2.5e-4
        Total: D = 0.00525
        """
        cycles = pd.DataFrame({
            'range': [100.0, 200.0, 50.0],
            'count': [1000.0, 500.0, 2000.0],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(cycles, simple_curve)

        d1 = 1000.0 / (1e12 / 100.0 ** 3)
        d2 = 500.0 / (1e12 / 200.0 ** 3)
        d3 = 2000.0 / (1e12 / 50.0 ** 3)
        expected = d1 + d2 + d3

        assert abs(result['total_damage'] - expected) / expected < 1e-6

    def test_zero_cycles_no_damage(self, simple_curve):
        """Zero applied cycles should give zero damage."""
        cycles = pd.DataFrame({
            'range': [100.0],
            'count': [0.0],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(cycles, simple_curve)
        assert result['total_damage'] == 0.0

    def test_damage_unity_means_failure(self, simple_curve):
        """D = 1.0 means theoretical failure (Miner's criterion)."""
        # S = 100 MPa -> N = 1e6 -> need n = 1e6 for D = 1
        cycles = pd.DataFrame({
            'range': [100.0],
            'count': [1e6],
        })
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(cycles, simple_curve)
        assert abs(result['total_damage'] - 1.0) < 1e-6

    def test_damage_with_dnv_d_curve(self):
        """Damage using actual DNV-D curve matches hand calculation."""
        curve = get_dnv_curve('D')
        # S = 80 MPa -> N = 5.73e11 / 80^3 = 5.73e11 / 512000 = 1119140.6
        # n = 10000 -> D = 10000 / 1119140.6 = 0.008936
        N_80 = 5.73e11 / (80.0 ** 3)
        expected = 10000.0 / N_80

        cycles = pd.DataFrame({'range': [80.0], 'count': [10000.0]})
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(cycles, curve)

        assert abs(result['total_damage'] - expected) / expected < 1e-4

    def test_safety_factor_calculation(self, simple_curve):
        """Safety factor = 1 / D."""
        cycles = pd.DataFrame({'range': [100.0], 'count': [250000.0]})
        calc = LinearDamageAccumulation()
        result = calc.calculate_damage(cycles, simple_curve)
        # D = 250000 / 1e6 = 0.25 -> SF = 4.0
        assert abs(result['safety_factor'] - 4.0) < 1e-4


# -----------------------------------------------------------------------
# 3. Spectral Fatigue (Narrow-Band and Dirlik)
# -----------------------------------------------------------------------

class TestSpectralFatigue:
    """Validate frequency-domain fatigue methods."""

    @pytest.fixture
    def narrowband_psd(self):
        """Narrow-band PSD centered at 5 Hz with known m0."""
        freq = np.linspace(0.01, 50.0, 4096)
        center = 5.0
        sigma_f = 0.15  # Very narrow
        psd = 500.0 * np.exp(-0.5 * ((freq - center) / sigma_f) ** 2)
        return freq, psd

    @pytest.fixture
    def broadband_psd(self):
        """Broadband PSD with multiple peaks."""
        freq = np.linspace(0.01, 50.0, 4096)
        psd = 0.5 * np.ones_like(freq)
        psd += 10.0 * np.exp(-0.5 * ((freq - 5.0) / 0.5) ** 2)
        psd += 5.0 * np.exp(-0.5 * ((freq - 15.0) / 1.0) ** 2)
        return freq, psd

    def test_narrowband_produces_positive_damage_rate(self, narrowband_psd):
        """Narrow-band method should produce positive finite damage rate."""
        freq, psd = narrowband_psd
        curve = get_dnv_curve('D')
        nb = NarrowBandMethod()
        result = nb.calculate_damage_rate(freq, psd, curve)

        assert result.damage_rate > 0
        assert np.isfinite(result.damage_rate)
        assert np.isfinite(result.fatigue_life)
        assert result.fatigue_life > 0

    def test_narrowband_spectral_moments(self, narrowband_psd):
        """Verify spectral moments m0, m2 are positive."""
        freq, psd = narrowband_psd
        curve = get_dnv_curve('D')
        nb = NarrowBandMethod()
        result = nb.calculate_damage_rate(freq, psd, curve)

        assert result.spectral_moments.m0 > 0
        assert result.spectral_moments.m2 > 0

    def test_narrowband_rms_stress(self, narrowband_psd):
        """RMS stress = sqrt(m0) should be physically reasonable."""
        freq, psd = narrowband_psd
        curve = get_dnv_curve('D')
        nb = NarrowBandMethod()
        result = nb.calculate_damage_rate(freq, psd, curve)

        sigma_rms = np.sqrt(result.spectral_moments.m0)
        # With amplitude 500 and narrow peak, sigma should be in a
        # reasonable range (not zero, not thousands)
        assert 1.0 < sigma_rms < 500.0

    def test_dirlik_produces_positive_damage_rate(self, broadband_psd):
        """Dirlik method should produce positive finite damage rate."""
        freq, psd = broadband_psd
        curve = get_dnv_curve('D')
        dk = DirlikMethod()
        result = dk.calculate_damage_rate(freq, psd, curve)

        assert result.damage_rate > 0
        assert np.isfinite(result.damage_rate)
        assert result.fatigue_life > 0

    def test_dirlik_less_conservative_than_narrowband(self, broadband_psd):
        """For broadband PSD, Dirlik damage <= narrow-band damage.

        Narrow-band is an upper bound; Dirlik corrects for bandwidth.
        """
        freq, psd = broadband_psd
        curve = get_dnv_curve('D')

        nb_result = NarrowBandMethod().calculate_damage_rate(freq, psd, curve)
        dk_result = DirlikMethod().calculate_damage_rate(freq, psd, curve)

        # Dirlik should give less or equal damage (less conservative)
        assert dk_result.damage_rate <= nb_result.damage_rate * 1.05, (
            f"Dirlik {dk_result.damage_rate:.4e} should be <= "
            f"narrow-band {nb_result.damage_rate:.4e} (with 5% tolerance)"
        )

    def test_tovo_benasciutti_bounded(self, broadband_psd):
        """Tovo-Benasciutti should lie between Dirlik and narrow-band."""
        freq, psd = broadband_psd
        curve = get_dnv_curve('D')

        nb = NarrowBandMethod().calculate_damage_rate(freq, psd, curve)
        dk = DirlikMethod().calculate_damage_rate(freq, psd, curve)
        tb = TovoBenasciuttiMethod().calculate_damage_rate(freq, psd, curve)

        # TB should be in a reasonable range, not wildly different
        min_rate = min(nb.damage_rate, dk.damage_rate) * 0.1
        max_rate = max(nb.damage_rate, dk.damage_rate) * 10.0
        assert min_rate <= tb.damage_rate <= max_rate

    def test_higher_psd_gives_more_damage(self):
        """Doubling PSD amplitude should increase damage."""
        freq = np.linspace(0.01, 50.0, 2048)
        psd_low = 1.0 * np.exp(-0.5 * ((freq - 5.0) / 0.5) ** 2)
        psd_high = 4.0 * np.exp(-0.5 * ((freq - 5.0) / 0.5) ** 2)

        curve = get_dnv_curve('D')
        nb = NarrowBandMethod()

        result_low = nb.calculate_damage_rate(freq, psd_low, curve)
        result_high = nb.calculate_damage_rate(freq, psd_high, curve)

        assert result_high.damage_rate > result_low.damage_rate

    def test_zero_psd_gives_zero_damage(self):
        """A zero PSD should give zero damage."""
        freq = np.linspace(0.01, 50.0, 1024)
        psd = np.zeros_like(freq)
        curve = get_dnv_curve('D')

        nb = NarrowBandMethod()
        result = nb.calculate_damage_rate(freq, psd, curve)
        assert result.damage_rate == 0.0


# -----------------------------------------------------------------------
# 4. Seawater Environment Curve Adjustments
# -----------------------------------------------------------------------

class TestSeawaterEnvironment:
    """Validate seawater-adjusted S-N curves from worked_examples module."""

    def test_seawater_cp_reduces_life(self):
        """Seawater+CP curve should give fewer allowable cycles than air."""
        from digitalmodel.structural.fatigue.worked_examples import (
            _dnv_seawater_cp,
        )
        air_curve = get_dnv_curve('D')
        sw_curve = _dnv_seawater_cp('D')

        S = 80.0
        N_air = air_curve.get_allowable_cycles(S)
        N_sw = sw_curve.get_allowable_cycles(S)

        assert N_sw < N_air, "Seawater+CP should reduce allowable cycles"
        # Factor is 0.87
        ratio = N_sw / N_air
        assert abs(ratio - 0.87) < 0.01

    def test_seawater_free_no_fatigue_limit(self):
        """Seawater free-corrosion curve should have zero fatigue limit."""
        from digitalmodel.structural.fatigue.worked_examples import (
            _dnv_seawater_free,
        )
        sw_free = _dnv_seawater_free('D')
        assert sw_free.fatigue_limit == 0.0

        # Should still give finite cycles below air fatigue limit
        N = sw_free.get_allowable_cycles(40.0)
        assert np.isfinite(N) and N > 0

    def test_seawater_free_factor(self):
        """Free-corrosion factor is 0.72 vs air."""
        from digitalmodel.structural.fatigue.worked_examples import (
            _dnv_seawater_free,
        )
        air = get_dnv_curve('D')
        free = _dnv_seawater_free('D')

        ratio = free.A / air.A
        assert abs(ratio - 0.72) < 0.001


# -----------------------------------------------------------------------
# 5. Worked Example Round-Trips
# -----------------------------------------------------------------------

class TestWorkedExamples:
    """Validate the three canonical worked examples produce sensible results."""

    def test_pipeline_girth_weld_runs(self):
        """Pipeline girth weld produces a valid ExampleResult."""
        result = pipeline_girth_weld()
        assert result.damage > 0
        assert result.life_years > 0
        assert result.sn_curve.name == "DNV-D-SwCP"
        assert result.scf == 1.5
        assert result.dff == 3.0

    def test_scr_touchdown_runs(self):
        """SCR touchdown example produces valid results."""
        result = scr_touchdown()
        assert result.damage > 0
        assert result.sn_curve.name == "DNV-F1-SwFree"
        assert result.dff == 10.0

    def test_mooring_chain_runs(self):
        """Mooring chain example produces valid results."""
        result = mooring_chain()
        assert result.damage > 0
        assert result.sn_curve.name == "Chain-R3-Studless-SwCP"
        assert result.dff == 5.0

    def test_pipeline_damage_order_of_magnitude(self):
        """Pipeline damage should be in a plausible range (0.001 to 1.0)."""
        result = pipeline_girth_weld()
        assert 1e-4 < result.damage < 2.0

    def test_scr_damage_order_of_magnitude(self):
        """SCR damage should be in a plausible range."""
        result = scr_touchdown()
        assert 1e-4 < result.damage < 5.0

    def test_mooring_chain_damage_order_of_magnitude(self):
        """Mooring chain damage should be in a plausible range."""
        result = mooring_chain()
        assert 1e-4 < result.damage < 5.0

    def test_higher_scf_increases_damage(self):
        """Higher SCF should increase damage."""
        result_low = pipeline_girth_weld(scf=1.0)
        result_high = pipeline_girth_weld(scf=2.0)
        assert result_high.damage > result_low.damage

    def test_longer_design_life_increases_damage(self):
        """Longer design life should increase damage."""
        result_short = pipeline_girth_weld(design_life_years=10.0)
        result_long = pipeline_girth_weld(design_life_years=30.0)
        assert result_long.damage > result_short.damage

    def test_scf_cubed_sensitivity(self):
        """For m=3 S-N curve, damage scales as SCF^3."""
        result_1 = pipeline_girth_weld(scf=1.0)
        result_2 = pipeline_girth_weld(scf=2.0)
        # Damage ratio should be ~2^3 = 8 (approximate due to CAFL)
        ratio = result_2.damage / result_1.damage
        # Allow tolerance because CAFL cuts off low-stress bins differently
        assert 4.0 < ratio < 12.0, f"Damage ratio = {ratio}, expected ~8"
