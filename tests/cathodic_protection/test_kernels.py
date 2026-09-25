"""Tests for the shared CP formula kernel (issue #2211).

Every expected value is hand-derived from DNV-RP-B401 Sec. 7 / Table 10-7.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.cathodic_protection import _kernels as k


class TestDemandMassBreakdown:
    def test_current_demand_eq_1(self):
        """I_c = 1021 * 0.220 * 0.11 = 24.71 A."""
        assert k.current_demand(1021.0, 0.220, 0.11) == pytest.approx(24.7082, abs=1e-4)

    def test_current_demand_zero_breakdown_is_zero(self):
        assert k.current_demand(1000.0, 0.2, 0.0) == 0.0

    @pytest.mark.parametrize(
        "kwargs, name",
        [
            ({"area_m2": -1.0, "current_density_A_m2": 0.1, "breakdown_factor": 0.5}, "area_m2"),
            ({"area_m2": 1.0, "current_density_A_m2": -0.1, "breakdown_factor": 0.5}, "current_density_A_m2"),
            ({"area_m2": 1.0, "current_density_A_m2": 0.1, "breakdown_factor": 1.5}, "breakdown_factor"),
            ({"area_m2": 1.0, "current_density_A_m2": float("nan"), "breakdown_factor": 0.5}, "current_density_A_m2"),
        ],
    )
    def test_current_demand_rejects_bad_inputs_naming_parameter(self, kwargs, name):
        with pytest.raises(ValueError, match=name):
            k.current_demand(**kwargs)

    def test_anode_mass_eq_2(self):
        """M = 800 * 25 * 8760 / (2000 * 0.90) = 97 333.3 kg (review A2)."""
        assert k.anode_mass(800.0, 25.0, 2000.0, 0.90) == pytest.approx(97333.333, abs=0.01)

    @pytest.mark.parametrize(
        "args, name",
        [
            ((10.0, 0.0, 2000.0, 0.9), "life_years"),
            ((10.0, 25.0, 0.0, 0.9), "capacity_Ah_kg"),
            ((10.0, 25.0, 2000.0, 0.0), "utilisation"),
            ((10.0, 25.0, 2000.0, 1.2), "utilisation"),
            ((-1.0, 25.0, 2000.0, 0.9), "I_mean_A"),
        ],
    )
    def test_anode_mass_rejects_bad_inputs(self, args, name):
        with pytest.raises(ValueError, match=name):
            k.anode_mass(*args)

    def test_mass_consumed_is_faraday_without_utilisation(self):
        """1 A for 1 yr at 2000 Ah/kg dissolves 4.38 kg."""
        assert k.mass_consumed(1.0, 1.0, 2000.0) == pytest.approx(4.38)

    def test_coating_breakdown_mean_and_final(self):
        """FBE Table A.1 (a = 0.010, b = 0.0003), 25 yr: f_cm 0.01375, f_cf 0.0175."""
        assert k.coating_breakdown_linear(0.010, 0.0003, 12.5) == pytest.approx(0.01375)
        assert k.coating_breakdown_mean(0.010, 0.0003, 25.0) == pytest.approx(0.01375)
        assert k.coating_breakdown_final(0.010, 0.0003, 25.0) == pytest.approx(0.0175)

    def test_coating_breakdown_clamped_at_one(self):
        assert k.coating_breakdown_final(0.30, 0.03, 40.0) == 1.0
        assert k.coating_breakdown_mean(0.30, 0.03, 60.0) == 1.0

    def test_coating_breakdown_rejects_negative(self):
        with pytest.raises(ValueError, match="t_years"):
            k.coating_breakdown_linear(0.01, 0.003, -1.0)


class TestOutputAndCounts:
    def test_anode_current_output(self):
        assert k.anode_current_output(0.25, 0.0790) == pytest.approx(3.1646, abs=1e-4)

    def test_anode_current_output_rejects_zero_resistance(self):
        with pytest.raises(ValueError, match="R_a"):
            k.anode_current_output(0.25, 0.0)

    def test_anode_count_ceiling_and_even(self):
        assert k.anode_count(1030.0, 119.0) == 9
        assert k.anode_count(1030.0, 119.0, round_to_even=True) == 10
        assert k.anode_count(0.0, 119.0) == 0

    def test_anode_count_zero_unit_mass_raises_value_error(self):
        with pytest.raises(ValueError, match="anode_net_mass_kg"):
            k.anode_count(100.0, 0.0)

    def test_anodes_for_current(self):
        assert k.anodes_for_current(1600.0, 3.1648) == 506
        with pytest.raises(ValueError, match="anode_output_A"):
            k.anodes_for_current(1.0, 0.0)


class TestTable107Resistance:
    def test_long_slender_standoff_review_a2(self):
        """rho 0.30, L 2.0, r 0.1076: 0.30/(4 pi) (ln(74.35) - 1) = 0.0790 ohm."""
        r = k.equivalent_radius_from_mass(200.0, 2.0, 2750.0)
        assert r == pytest.approx(0.10759, abs=1e-5)
        assert k.long_slender_standoff(0.30, 2.0, r) == pytest.approx(0.0790, abs=1e-4)

    def test_long_slender_requires_l_ge_4r(self):
        with pytest.raises(ValueError, match="L >= 4 r"):
            k.long_slender_standoff(0.30, 0.30, 0.10)

    def test_short_slender_standoff_formula(self):
        """L = 0.30, r = 0.10 (L < 4r), rho = 0.30.

        r/(2L) = 1/6; sqrt(1 + 1/36) = 1.013794;
        ln((6)(2.013794)) = ln(12.08276) = 2.491754;
        bracket = 2.491754 + 0.166667 - 1.013794 = 1.644627;
        R = 0.30 / (2 pi 0.30) * 1.644627 = 0.159155 * 1.644627 = 0.26175 ohm.
        """
        assert k.short_slender_standoff(0.30, 0.30, 0.10) == pytest.approx(0.26175, abs=1e-5)

    def test_short_slender_requires_l_lt_4r(self):
        with pytest.raises(ValueError, match="L < 4 r"):
            k.short_slender_standoff(0.30, 2.0, 0.10)

    def test_slender_dispatch_matches_forms(self):
        assert k.slender_standoff(0.30, 2.0, 0.10) == k.long_slender_standoff(0.30, 2.0, 0.10)
        assert k.slender_standoff(0.30, 0.30, 0.10) == k.short_slender_standoff(0.30, 0.30, 0.10)
        # Exactly L = 4r belongs to the long form.
        assert k.slender_standoff(0.30, 0.40, 0.10) == k.long_slender_standoff(0.30, 0.40, 0.10)

    def test_short_and_long_forms_are_close_at_the_boundary(self):
        """At L = 4r the two Table 10-7 forms differ by about 7 % (0.2116 vs 0.2260 ohm)."""
        r = 0.10
        long_form = k.long_slender_standoff(0.30, 4.0 * r, r)
        short_form = k.short_slender_standoff(0.30, 4.0 * r * 0.999999, r)
        assert long_form == pytest.approx(0.21159, abs=1e-4)
        assert short_form == pytest.approx(0.22604, abs=1e-4)
        assert short_form == pytest.approx(long_form, rel=0.10)

    def test_long_flush(self):
        """rho/(2S): L 1.0, W 0.16, t 0.05 -> S 0.58, R = 0.30/1.16 = 0.25862."""
        assert k.long_flush(0.30, 1.0, 0.16, 0.05) == pytest.approx(0.258621, abs=1e-6)

    def test_long_flush_validity(self):
        with pytest.raises(ValueError, match="length >= 4 width"):
            k.long_flush(0.30, 0.5, 0.16, 0.05)
        with pytest.raises(ValueError, match="thickness"):
            k.long_flush(0.30, 1.0, 0.16, 0.30)

    def test_short_flush_or_bracelet(self):
        """0.315 * 0.30 / sqrt(0.38067) = 0.15316 ohm (review A3 bracelet)."""
        assert k.short_flush_or_bracelet(0.30, 0.38067) == pytest.approx(0.15316, abs=1e-5)
        with pytest.raises(ValueError, match="exposed_area_m2"):
            k.short_flush_or_bracelet(0.30, 0.0)

    def test_resistance_scales_with_resistivity(self):
        for fn, args in (
            (k.long_slender_standoff, (2.0, 0.1)),
            (k.short_slender_standoff, (0.3, 0.1)),
            (k.long_flush, (1.0, 0.16, 0.05)),
            (k.short_flush_or_bracelet, (0.4,)),
        ):
            assert fn(0.60, *args) == pytest.approx(2.0 * fn(0.30, *args), rel=1e-12)

    def test_equivalent_radius_from_periphery(self):
        """Table 10-7 note 2: r = c / (2 pi); a 0.5 m periphery gives 0.07958 m."""
        assert k.equivalent_radius_from_periphery(0.5) == pytest.approx(0.5 / (2 * math.pi))
        with pytest.raises(ValueError, match="periphery_m"):
            k.equivalent_radius_from_periphery(0.0)

    def test_equivalent_radius_from_mass_rejects_zero_length(self):
        with pytest.raises(ValueError, match="L"):
            k.equivalent_radius_from_mass(200.0, 0.0)


class TestProximityFactor:
    @pytest.mark.parametrize("distance, factor", [(0.30, 1.0), (1.0, 1.0), (0.15, 1.3), (0.25, 1.3)])
    def test_table_10_7_note_1(self, distance, factor):
        assert k.resistance_proximity_factor(distance) == factor

    def test_below_150_mm_raises(self):
        with pytest.raises(ValueError, match="0.15 m"):
            k.resistance_proximity_factor(0.10)
        with pytest.raises(ValueError, match="distance_m"):
            k.resistance_proximity_factor(-0.1)
