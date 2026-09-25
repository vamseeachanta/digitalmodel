"""Verification tests for DNV-RP-B401 module against document worked examples.

Each test references the source document section and expected values.
Documents use generic engineering names (no client identifiers).
"""

from __future__ import annotations


import pytest

from digitalmodel.cathodic_protection.dnv_rp_b401 import (
    ANODE_CAPACITY_ALZNI,
    ANODE_DENSITY_ALZNI,
    DESIGN_DRIVING_VOLTAGE,
    STEEL_RESISTIVITY,
    UTILIZATION_FACTOR_STANDOFF,
    anode_current_output,
    anode_mass_requirement,
    anode_resistance_slender_standoff,
    coating_breakdown_factor,
    current_demand,
    equivalent_radius_from_mass,
    flush_anode_resistance,
    number_of_anodes,
    protected_length,
)


class TestCurrentDemand:
    """Verify current demand I_c = A_c * i_c * f_c (DNV-RP-B401 §7.4.1)."""

    def test_buoyancy_tank_initial_current_demand(self):
        """Verify against hybrid riser CP design report §5.1.1: buoyancy tank initial.

        Buoyancy tank: area=1021 m2, i_c=0.440 A/m2, f_c=0.02 -> I_c=8.99 A
        """
        result = current_demand(
            surface_area_m2=1021.0,
            current_density_A_m2=0.440,
            breakdown_factor=0.02,
        )
        assert result == pytest.approx(8.98, abs=0.05)

    def test_buoyancy_tank_mean_current_demand(self):
        """Verify against hybrid riser CP design report §5.1.1: buoyancy tank mean.

        Buoyancy tank: area=1021 m2, i_c=0.220 A/m2, f_c=0.11 -> I_c=24.70 A
        """
        result = current_demand(
            surface_area_m2=1021.0,
            current_density_A_m2=0.220,
            breakdown_factor=0.11,
        )
        assert result == pytest.approx(24.71, abs=0.15)

    def test_buoyancy_tank_final_current_demand(self):
        """Verify against hybrid riser CP design report §5.1.1: buoyancy tank final.

        Buoyancy tank: area=1021 m2, i_c=0.220 A/m2, f_c=0.20 -> I_c=44.92 A
        """
        result = current_demand(
            surface_area_m2=1021.0,
            current_density_A_m2=0.220,
            breakdown_factor=0.20,
        )
        assert result == pytest.approx(44.92, abs=0.1)

    def test_foundation_buried_current_demand(self):
        """Verify foundation buried: area=152.7 m2, i_c=0.020 A/m2, f_c=1.0.

        Bare metal below mudline: 152.7 * 0.020 * 1.0 = 3.054 A
        """
        result = current_demand(
            surface_area_m2=152.7,
            current_density_A_m2=0.020,
            breakdown_factor=1.0,
        )
        assert result == pytest.approx(3.054, abs=0.01)


class TestAnodeMassRequirement:
    """Verify M_a = (I_cm * t * 8760) / (u * epsilon) (DNV-RP-B401 §7.7.1)."""

    def test_buoyancy_tank_mass_12in_production(self):
        """Verify hybrid riser CP design report §5.1.1: buoyancy tank anode mass = 2645 kg.

        I_mean=24.70 A, T=22 yr, u=0.90, epsilon=2000 A-h/kg
        M = 24.70 * 22 * 8760 / (0.90 * 2000) = 2642.7 kg
        """
        result = anode_mass_requirement(
            I_mean_A=24.70,
            T_design_years=22.0,
            E_capacity=ANODE_CAPACITY_ALZNI,
            u_f=UTILIZATION_FACTOR_STANDOFF,
        )
        assert result == pytest.approx(2645, rel=0.01)

    def test_ura_mass_12in_production(self):
        """Verify hybrid riser CP design report §5.1.1: URA anode mass = 1030 kg.

        I_mean=9.62 A, T=22 yr, u=0.90, epsilon=2000 A-h/kg
        """
        result = anode_mass_requirement(
            I_mean_A=9.62,
            T_design_years=22.0,
            E_capacity=ANODE_CAPACITY_ALZNI,
            u_f=UTILIZATION_FACTOR_STANDOFF,
        )
        assert result == pytest.approx(1030, rel=0.02)

    def test_line_pipe_mass_12in_production(self):
        """Verify hybrid riser CP design report §5.1.1: line pipe anode mass = 159 kg.

        I_mean=1.48 A, T=22 yr, u=0.90, epsilon=2000 A-h/kg
        """
        result = anode_mass_requirement(
            I_mean_A=1.48,
            T_design_years=22.0,
            E_capacity=ANODE_CAPACITY_ALZNI,
            u_f=UTILIZATION_FACTOR_STANDOFF,
        )
        assert result == pytest.approx(159, rel=0.02)

    def test_ballast_box_mass(self):
        """Verify hybrid riser CP design report §5.1.1: ballast box anode mass = 1860 kg.

        I_mean=17.38 A, T=22 yr, u=0.90, epsilon=2000 A-h/kg
        """
        result = anode_mass_requirement(
            I_mean_A=17.38,
            T_design_years=22.0,
            E_capacity=ANODE_CAPACITY_ALZNI,
            u_f=UTILIZATION_FACTOR_STANDOFF,
        )
        assert result == pytest.approx(1860, rel=0.02)


class TestCoatingBreakdownFactor:
    """Verify f_c = a + b*t (DNV-RP-B401 Table 10-4)."""

    def test_cat_iii_initial(self):
        """Category III coating: a=0.02, f_c(0) = 0.02."""
        result = coating_breakdown_factor(a=0.02, b=0.00818, t_years=0.0)
        assert result == pytest.approx(0.02, abs=0.001)

    def test_cat_iii_mean(self):
        """Category III coating: f_c(11) = 0.02 + 0.00818*11 = 0.11.

        Mean is at t = T/2 = 22/2 = 11 years.
        """
        result = coating_breakdown_factor(a=0.02, b=0.00818, t_years=11.0)
        assert result == pytest.approx(0.11, abs=0.005)

    def test_cat_iii_final(self):
        """Category III coating: f_c(22) = 0.02 + 0.00818*22 = 0.20."""
        result = coating_breakdown_factor(a=0.02, b=0.00818, t_years=22.0)
        assert result == pytest.approx(0.20, abs=0.005)

    def test_3lpp_initial(self):
        """3LPP coating: f_c(0) = 0.005."""
        result = coating_breakdown_factor(a=0.005, b=0.0002, t_years=0.0)
        assert result == pytest.approx(0.005, abs=0.001)

    def test_3lpp_final(self):
        """3LPP coating: f_c(22) = 0.005 + 0.0002*22 = 0.0094."""
        result = coating_breakdown_factor(a=0.005, b=0.0002, t_years=22.0)
        assert result == pytest.approx(0.0094, abs=0.001)


class TestAnodeResistanceSlenderStandoff:
    """Verify R_a for long slender stand-off anodes (DNV-RP-B401 Table 10-7)."""

    def test_afa1190_initial_resistance(self):
        """AFA1190 anode: L=1.530 m, net_mass=119 kg.

        r_eq = sqrt(119 / (pi * 1.530 * 2750)) = 0.1058 m
        R_a = (0.22 / (2*pi*1.530)) * (ln(4*1.530/0.1058) - 1)
        Then multiply by proximity factor 1.3.
        """
        r_eq = equivalent_radius_from_mass(119.0, 1.530)
        R_a = anode_resistance_slender_standoff(
            rho=0.22, L_a=1.530, r_a=r_eq, proximity_factor=1.3
        )
        # Reasonable resistance range for offshore anode
        assert 0.01 < R_a < 0.15

    def test_afa1190_buoyancy_tank(self):
        """AFA1190 on buoyancy tank: rho=0.22 ohm-m."""
        r_eq = equivalent_radius_from_mass(119.0, 1.530)
        R_a = anode_resistance_slender_standoff(
            rho=0.22, L_a=1.530, r_a=r_eq, proximity_factor=1.3
        )
        # Current output should be ~ 0.25 / R_a
        I_a = DESIGN_DRIVING_VOLTAGE / R_a
        # Doc shows 24 anodes on buoyancy tank giving 63.7 A initial
        # So ~2.65 A per anode
        assert 1.5 < I_a < 5.0


class TestAnodeCurrentOutput:
    """Verify anode current output I_a = delta_E / R_a."""

    def test_standoff_anode_default_voltage(self):
        """Standoff anode: rho=0.30 ohm-m, L=1.530m, r=0.085m.

        Uses anode_current_output() directly with default delta_E=0.25V.
        """
        I_a = anode_current_output(rho=0.30, L_a=1.530, r_a=0.085)
        assert I_a > 0.0
        assert I_a < 10.0  # physically reasonable for a single anode

    def test_standoff_anode_custom_voltage(self):
        """Higher driving voltage gives more current (physical law)."""
        I_low = anode_current_output(rho=0.30, L_a=1.530, r_a=0.085, delta_E=0.20)
        I_high = anode_current_output(rho=0.30, L_a=1.530, r_a=0.085, delta_E=0.30)
        assert I_high > I_low

    def test_proximity_factor_reduces_current(self):
        """Proximity factor > 1 increases effective resistance → less current."""
        I_open = anode_current_output(rho=0.30, L_a=1.530, r_a=0.085, proximity_factor=1.0)
        I_shielded = anode_current_output(
            rho=0.30, L_a=1.530, r_a=0.085, proximity_factor=2.0
        )
        assert I_shielded < I_open

    def test_flush_mount_suction_pile_initial(self):
        """Flush-mount 29lb anode on suction pile, B401 Table 10-7 short flush.

        Re-baselined for #2211: the deprecated wrapper now evaluates
        R_a = 0.315 rho / sqrt(L W) in SI. rho = 0.31 ohm-m,
        A = 24 in x 5 in = 0.07742 m2, R_a = 0.3510 ohm,
        I_a = 0.25 / 0.3510 = 0.712 A. (The riser modification CP calc's
        0.59 A came from a half-space slender-body spreadsheet formula
        that is not in B401.)
        """
        with pytest.warns(DeprecationWarning):
            R_a = flush_anode_resistance(
                rho_ohm_cm=31.0, L_a_in=24.0, W_in=5.0, H_in=2.5, r_eq_in=2.39
            )
        I_a = 0.25 / R_a
        assert I_a == pytest.approx(0.712, abs=0.005)

    def test_flush_mount_buoyancy_can_initial(self):
        """Flush-mount 29lb anode on buoyancy can: rho = 24 ohm-cm.

        Re-baselined for #2211 (Table 10-7 short flush, A = L W):
        R_a = 0.315 * 0.24 / sqrt(0.07742) = 0.2717 ohm,
        I_a = 0.25 / 0.2717 = 0.920 A.
        """
        with pytest.warns(DeprecationWarning):
            R_a = flush_anode_resistance(
                rho_ohm_cm=24.0, L_a_in=24.0, W_in=5.0, H_in=2.5, r_eq_in=2.39
            )
        I_a = 0.25 / R_a
        assert I_a == pytest.approx(0.920, abs=0.005)


class TestFlushAnodeResistance:
    """Deprecated ``flush_anode_resistance`` = Table 10-7 short flush in SI.

    Re-baselined for #2211. The earlier expected values (0.4209 / 0.3258 /
    0.5114 ohm) were a half-space slender-body spreadsheet formula that
    ignored the width and height; B401 Table 10-7 gives
    R_a = 0.315 rho / sqrt(A) with A the exposed area, here L x W.
    """

    def test_suction_pile_initial(self):
        """Suction pile: rho = 31 ohm-cm = 0.31 ohm-m, L = 24 in, W = 5 in.

        A = 24 * 5 * 0.0254^2 = 0.077419 m2; sqrt(A) = 0.27824 m;
        R_a = 0.315 * 0.31 / 0.27824 = 0.35095 ohm.
        """
        with pytest.warns(DeprecationWarning):
            R_a = flush_anode_resistance(
                rho_ohm_cm=31.0, L_a_in=24.0, W_in=5.0, H_in=2.5, r_eq_in=2.39
            )
        assert R_a == pytest.approx(0.35095, rel=1e-4)

    def test_buoyancy_can_initial(self):
        """Buoyancy can: rho = 0.24 ohm-m, same 24 in x 5 in face.

        R_a = 0.315 * 0.24 / 0.27824 = 0.27170 ohm.
        """
        with pytest.warns(DeprecationWarning):
            R_a = flush_anode_resistance(
                rho_ohm_cm=24.0, L_a_in=24.0, W_in=5.0, H_in=2.5, r_eq_in=2.39
            )
        assert R_a == pytest.approx(0.27170, rel=1e-4)

    def test_suction_pile_final_ignores_equivalent_radius(self):
        """The short flush form depends on the exposed area only.

        ``r_eq_in`` (and ``H_in``) are accepted for signature compatibility
        and do not change the result: the depleted 0.90 in radius gives the
        same 0.35095 ohm as the fresh 2.39 in. A depleted flush anode is
        modelled by passing its reduced exposed face.
        """
        with pytest.warns(DeprecationWarning):
            R_a = flush_anode_resistance(
                rho_ohm_cm=31.0, L_a_in=24.0, W_in=5.0, H_in=2.5, r_eq_in=0.90
            )
        assert R_a == pytest.approx(0.35095, rel=1e-4)

    def test_matches_kernel_short_flush(self):
        """The wrapper is the kernel formula after the unit conversion."""
        from digitalmodel.cathodic_protection._kernels import short_flush_or_bracelet

        with pytest.warns(DeprecationWarning):
            R_a = flush_anode_resistance(31.0, 24.0, 5.0, 2.5, 2.39)
        assert R_a == pytest.approx(
            short_flush_or_bracelet(0.31, 24.0 * 0.0254 * 5.0 * 0.0254), rel=1e-12
        )


class TestEquivalentRadius:
    """Verify equivalent radius calculation from mass."""

    def test_afa1190_radius(self):
        """AFA1190: 119 kg net mass, 1.530 m length, 2750 kg/m3."""
        r = equivalent_radius_from_mass(119.0, 1.530, ANODE_DENSITY_ALZNI)
        assert 0.05 < r < 0.20  # reasonable range

    def test_radius_increases_with_mass(self):
        """Physical law: more mass -> larger radius at same length."""
        r1 = equivalent_radius_from_mass(100.0, 1.0)
        r2 = equivalent_radius_from_mass(200.0, 1.0)
        assert r2 > r1


class TestNumberOfAnodes:
    """Verify anode count calculation."""

    def test_buoyancy_tank_anode_count(self):
        """Buoyancy tank: 2645 kg total / 119 kg per anode = ceil(22.23) = 23.

        Doc shows 24 anodes (rounded to nearest larger even integer for symmetric placement).
        """
        assert number_of_anodes(2645.0, 119.0) == 23
        assert number_of_anodes(2645.0, 119.0, round_to_even=True) == 24

    def test_ura_anode_count(self):
        """URA: 1030 kg total / 119 kg per anode = ceil(8.66) = 9.

        Doc shows 10 (nearest larger even integer for symmetric placement).
        """
        assert number_of_anodes(1030.0, 119.0) == 9
        assert number_of_anodes(1030.0, 119.0, round_to_even=True) == 10

    def test_zero_mass_raises_value_error(self):
        """A zero anode mass raises ValueError naming the parameter (#2211)."""
        with pytest.raises(ValueError, match="anode_net_mass_kg"):
            number_of_anodes(100.0, 0.0)

    def test_short_standoff_uses_table_10_7_short_form(self):
        """L < 4r selects the Table 10-7 short slender stand-off formula."""
        from digitalmodel.cathodic_protection._kernels import short_slender_standoff

        R = anode_resistance_slender_standoff(0.30, 0.30, 0.10)
        assert R == pytest.approx(short_slender_standoff(0.30, 0.30, 0.10), rel=1e-12)
        assert R > 0.0

    def test_negative_resistance_impossible(self):
        """r_a > 4 L / e used to give a negative resistance silently."""
        with pytest.raises(ValueError):
            anode_resistance_slender_standoff(0.30, 1.0, -0.5)
        with pytest.raises(ValueError):
            anode_current_output(0.30, 0.0, 0.05)


class TestProtectedLength:
    """Verify protected length per DNV-RP-F103 §5.6.7."""

    def test_12in_production_upper_pipe(self):
        """12-inch production riser upper line pipe.

        D=0.329 m, WT=0.022 m, f_cf=0.0042 (insulating coating),
        i_cm=0.250 A/m2, delta_E_me=0.15 V, rho_me=2.0e-7 ohm-m
        PL = sqrt(0.15 * 0.022 * 0.307 / (2e-7 * 0.329 * 0.0042 * 0.250))
        Doc: PL = 3829 m
        """
        PL = protected_length(
            delta_E_me=0.15,
            WT=0.022,
            D=0.329,
            rho_me=STEEL_RESISTIVITY,
            f_cf=0.0042,
            i_cm=0.250,
        )
        assert PL == pytest.approx(3829, rel=0.02)

    def test_8in_gas_lift_upper_pipe(self):
        """8-inch gas lift riser upper line pipe.

        D=0.219 m, WT=0.0239 m, f_cf=0.0042 (insulating coating),
        i_cm=0.235 A/m2
        Doc: PL = 4022 m
        """
        PL = protected_length(
            delta_E_me=0.15,
            WT=0.0239,
            D=0.219,
            rho_me=STEEL_RESISTIVITY,
            f_cf=0.0042,
            i_cm=0.235,
        )
        assert PL == pytest.approx(4022, rel=0.05)


class TestParametricSweeps:
    """Physical monotonicity and trend checks."""

    def test_current_demand_increases_with_area(self):
        """Larger area -> more current needed."""
        areas = [100, 500, 1000, 2000]
        demands = [
            current_demand(a, 0.220, 0.11) for a in areas
        ]
        for i in range(len(demands) - 1):
            assert demands[i + 1] > demands[i]

    def test_current_demand_increases_with_breakdown(self):
        """Higher breakdown -> more current needed."""
        bfs = [0.02, 0.05, 0.10, 0.20, 0.50, 1.0]
        demands = [
            current_demand(1000.0, 0.220, bf) for bf in bfs
        ]
        for i in range(len(demands) - 1):
            assert demands[i + 1] > demands[i]

    def test_anode_mass_increases_with_design_life(self):
        """Longer design life -> more anode mass needed."""
        lives = [5, 10, 15, 22, 30]
        masses = [
            anode_mass_requirement(10.0, t) for t in lives
        ]
        for i in range(len(masses) - 1):
            assert masses[i + 1] > masses[i]

    def test_resistance_decreases_with_seawater_resistivity(self):
        """Lower resistivity -> lower resistance.

        This is physically correct: more conductive seawater
        provides a lower-resistance path.
        """
        rhos = [0.15, 0.22, 0.32, 0.50]
        r_eq = equivalent_radius_from_mass(119.0, 1.530)
        resistances = [
            anode_resistance_slender_standoff(rho, 1.530, r_eq)
            for rho in rhos
        ]
        for i in range(len(resistances) - 1):
            assert resistances[i + 1] > resistances[i]

    def test_protected_length_decreases_with_higher_current_density(self):
        """Higher current density -> shorter protected length."""
        densities = [0.100, 0.200, 0.250, 0.400]
        lengths = [
            protected_length(0.15, 0.022, 0.329, STEEL_RESISTIVITY, 0.0094, d)
            for d in densities
        ]
        for i in range(len(lengths) - 1):
            assert lengths[i + 1] < lengths[i]

    def test_coating_breakdown_monotonically_increases(self):
        """Breakdown factor always increases with time."""
        times = [0, 5, 11, 15, 22]
        factors = [
            coating_breakdown_factor(0.02, 0.00818, t) for t in times
        ]
        for i in range(len(factors) - 1):
            assert factors[i + 1] > factors[i]
