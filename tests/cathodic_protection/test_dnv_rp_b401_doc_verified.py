"""Verification of the DNV-RP-B401 module against abstracted CP calculations.

The reference values are NOT DNV worked examples (DNV-RP-B401 publishes
none). They come from two abstracted client calculations kept in the repo:

- ``docs/domains/cathodic_protection/examples/calc-008-dnv-b401-2005-slhr-deepwater.md``:
  single-leg hybrid riser (SLHR) design under DNV-RP-B401:2005 with
  DNV-RP-F103:2003 for the line-pipe attenuation check (Tables 4.9, 4.10,
  4.11, 5.1, 5.9 and the "Anode Resistance and Current Output" table).
- ``docs/domains/cathodic_protection/examples/calc-009-dnv-b401-2005-fsr-temporary.md``:
  free-standing riser, temporary service, flush-mount 29 lb hull anodes
  (24 in x 5 in x 2.5 in) in 24 / 31 ohm-cm seawater.

calc-009's own resistance figures live in scanned sheets (its "Appendix 2")
that are not in the repo, so every flush-mount expected value below is
hand-derived from the DNV-RP-B401 Table 10-7 short flush-mounted formula
``R_a = 0.315 rho / sqrt(A)`` with the calc-009 inputs; the derivation is
stated in each docstring. Everything else is checked against the number
printed in calc-008.

The DNV-RP-F103 Eq. 14 protected-length cases from calc-008 are in
``test_dnv_rp_f103.py``.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.cathodic_protection._kernels import (
    short_flush_or_bracelet,
    short_slender_standoff,
)
from digitalmodel.cathodic_protection.dnv_rp_b401 import (
    ANODE_CAPACITY_ALZNI,
    ANODE_DENSITY_ALZNI,
    DESIGN_DRIVING_VOLTAGE,
    UTILIZATION_FACTOR_STANDOFF,
    anode_current_output,
    anode_mass_requirement,
    anode_resistance_slender_standoff,
    coating_breakdown_factor,
    current_demand,
    equivalent_radius_from_mass,
    flush_anode_resistance,
    number_of_anodes,
)

# calc-008 AFA1190 anode (Table 4.11): 1.530 m long, 119 kg net, 2750 kg/m3.
AFA1190_LENGTH_M = 1.530
AFA1190_NET_MASS_KG = 119.0

# calc-009 flush-mount 29 lb hull anode: 24 in x 5 in face.
FLUSH_FACE_AREA_M2 = (24.0 * 0.0254) * (5.0 * 0.0254)  # 0.077419 m2


class TestCurrentDemand:
    """I_c = A_c * i_c * f_c (DNV-RP-B401 Sec. 7.4.1) against calc-008 Table 5.1."""

    def test_buoyancy_tank_initial_current_demand(self):
        """calc-008 Table 5.1, buoyancy tank initial: 1021 * 0.440 * 0.02 = 8.98 A (doc 8.99 A)."""
        result = current_demand(
            surface_area_m2=1021.0,
            current_density_A_m2=0.440,
            breakdown_factor=0.02,
        )
        assert result == pytest.approx(8.98, abs=0.05)

    def test_buoyancy_tank_mean_current_demand(self):
        """calc-008 Table 5.1, buoyancy tank mean: 1021 * 0.220 * 0.11 = 24.71 A (doc 24.70 A)."""
        result = current_demand(
            surface_area_m2=1021.0,
            current_density_A_m2=0.220,
            breakdown_factor=0.11,
        )
        assert result == pytest.approx(24.71, abs=0.15)

    def test_buoyancy_tank_final_current_demand(self):
        """calc-008 Table 5.1, buoyancy tank final: 1021 * 0.220 * 0.20 = 44.92 A."""
        result = current_demand(
            surface_area_m2=1021.0,
            current_density_A_m2=0.220,
            breakdown_factor=0.20,
        )
        assert result == pytest.approx(44.92, abs=0.1)

    def test_foundation_buried_current_demand(self):
        """calc-008 buried exposed zone: 152.7 m2 (15.3 pile + 137.4 ballast box) bare.

        152.7 * 0.020 * 1.0 = 3.054 A (doc 3.05 A).
        """
        result = current_demand(
            surface_area_m2=152.7,
            current_density_A_m2=0.020,
            breakdown_factor=1.0,
        )
        assert result == pytest.approx(3.054, abs=0.01)


class TestAnodeMassRequirement:
    """M_a = I_cm * t * 8760 / (u * epsilon) (Sec. 7.7.1) against calc-008 Table 5.1."""

    def test_buoyancy_tank_mass_12in_production(self):
        """calc-008 Table 5.1, buoyancy tank: 24.70 * 22 * 8760 / (0.90 * 2000) = 2644.5 kg (doc 2645)."""
        result = anode_mass_requirement(
            I_mean_A=24.70,
            T_design_years=22.0,
            E_capacity=ANODE_CAPACITY_ALZNI,
            u_f=UTILIZATION_FACTOR_STANDOFF,
        )
        assert result == pytest.approx(2645, rel=0.01)

    def test_ura_mass_12in_production(self):
        """calc-008 Table 5.1, URA: 9.62 * 22 * 8760 / 1800 = 1030.0 kg."""
        result = anode_mass_requirement(
            I_mean_A=9.62,
            T_design_years=22.0,
            E_capacity=ANODE_CAPACITY_ALZNI,
            u_f=UTILIZATION_FACTOR_STANDOFF,
        )
        assert result == pytest.approx(1030, rel=0.02)

    def test_line_pipe_mass_12in_production(self):
        """calc-008 Table 5.1, line pipe: 1.48 * 22 * 8760 / 1800 = 158.5 kg (doc 159)."""
        result = anode_mass_requirement(
            I_mean_A=1.48,
            T_design_years=22.0,
            E_capacity=ANODE_CAPACITY_ALZNI,
            u_f=UTILIZATION_FACTOR_STANDOFF,
        )
        assert result == pytest.approx(159, rel=0.02)

    def test_ballast_box_mass(self):
        """calc-008 Table 5.1, ballast box: 17.38 * 22 * 8760 / 1800 = 1860.8 kg (doc 1860)."""
        result = anode_mass_requirement(
            I_mean_A=17.38,
            T_design_years=22.0,
            E_capacity=ANODE_CAPACITY_ALZNI,
            u_f=UTILIZATION_FACTOR_STANDOFF,
        )
        assert result == pytest.approx(1860, rel=0.02)


class TestCoatingBreakdownFactor:
    """f_c = a + b * t against calc-008 Table 4.10 (22-year design life).

    calc-008 states the initial / mean / final factors, not a and b. The
    constants used here are back-calculated from those factors
    (b = (final - initial) / 22) and are project values, not DNV-RP-B401
    Table 10-4 (Category III there is a = 0.02, b = 0.012 or 0.008).
    """

    def test_cat_iii_initial(self):
        """calc-008 Table 4.10 'DNV Category III (min.)' initial 0.02."""
        result = coating_breakdown_factor(a=0.02, b=0.00818, t_years=0.0)
        assert result == pytest.approx(0.02, abs=0.001)

    def test_cat_iii_mean(self):
        """calc-008 Table 4.10 mean 0.11 at t = T/2 = 11 yr: 0.02 + 0.00818 * 11 = 0.110."""
        result = coating_breakdown_factor(a=0.02, b=0.00818, t_years=11.0)
        assert result == pytest.approx(0.11, abs=0.005)

    def test_cat_iii_final(self):
        """calc-008 Table 4.10 final 0.20 at 22 yr: 0.02 + 0.00818 * 22 = 0.200."""
        result = coating_breakdown_factor(a=0.02, b=0.00818, t_years=22.0)
        assert result == pytest.approx(0.20, abs=0.005)

    def test_3lpp_initial(self):
        """calc-008 Table 4.10, line pipe 3LPP (incl. field joints) initial 0.0050."""
        result = coating_breakdown_factor(a=0.005, b=0.0002, t_years=0.0)
        assert result == pytest.approx(0.005, abs=0.001)

    def test_3lpp_final(self):
        """calc-008 Table 4.10, line pipe 3LPP final 0.0094 = 0.005 + 0.0002 * 22."""
        result = coating_breakdown_factor(a=0.005, b=0.0002, t_years=22.0)
        assert result == pytest.approx(0.0094, abs=0.001)


class TestAnodeResistanceSlenderStandoff:
    """Table 10-7 long slender stand-off against calc-008 'Anode Resistance and Current Output'.

    calc-008 tabulates r = 95 mm, R_a = 0.072 ohm, I_a = 3.45 A at
    rho = 0.22 ohm-m and R_a = 0.105 ohm, I_a = 2.37 A at 0.32 ohm-m for the
    fresh AFA1190; those figures are without the 1.3 proximity factor that
    the same document says applies (its buoyancy-tank total of 63.7 A from
    24 anodes, 2.65 A each, is the value with the factor).
    """

    def test_afa1190_initial_resistance(self):
        """rho = 0.22 ohm-m, L = 1.530 m, r = sqrt(119 / (pi 1.530 2750)) = 0.09488 m.

        R_a = 0.22 / (2 pi 1.530) * (ln(4 * 1.530 / 0.09488) - 1)
            = 0.022885 * (4.1666 - 1) = 0.07247 ohm (calc-008: 0.072).
        With the 1.3 proximity factor: 0.09421 ohm.
        """
        r_eq = equivalent_radius_from_mass(AFA1190_NET_MASS_KG, AFA1190_LENGTH_M)
        R_open = anode_resistance_slender_standoff(rho=0.22, L_a=AFA1190_LENGTH_M, r_a=r_eq)
        R_close = anode_resistance_slender_standoff(
            rho=0.22, L_a=AFA1190_LENGTH_M, r_a=r_eq, proximity_factor=1.3
        )
        assert R_open == pytest.approx(0.07247, abs=5e-5)
        assert R_open == pytest.approx(0.072, abs=5e-4)
        assert R_close == pytest.approx(0.07247 * 1.3, rel=1e-3)

    def test_afa1190_buoyancy_tank_current_output(self):
        """I_a = 0.25 / R_a: 3.45 A open (calc-008: 3.45) and 2.65 A with factor 1.3.

        calc-008 reports 63.7 A from 24 buoyancy-tank anodes = 2.65 A each,
        which is 0.25 / (1.3 * 0.07247) = 2.654 A.
        """
        r_eq = equivalent_radius_from_mass(AFA1190_NET_MASS_KG, AFA1190_LENGTH_M)
        R_open = anode_resistance_slender_standoff(rho=0.22, L_a=AFA1190_LENGTH_M, r_a=r_eq)
        R_close = anode_resistance_slender_standoff(
            rho=0.22, L_a=AFA1190_LENGTH_M, r_a=r_eq, proximity_factor=1.3
        )
        assert DESIGN_DRIVING_VOLTAGE / R_open == pytest.approx(3.45, abs=0.01)
        assert DESIGN_DRIVING_VOLTAGE / R_close == pytest.approx(2.65, abs=0.01)
        assert 24 * DESIGN_DRIVING_VOLTAGE / R_close == pytest.approx(63.7, abs=0.2)

    def test_afa1190_foundation_resistivity(self):
        """rho = 0.32 ohm-m scales the same geometry: 0.07247 * 0.32 / 0.22 = 0.10541 ohm.

        calc-008: R_a = 0.105 ohm, I_a = 0.25 / 0.10541 = 2.372 A (doc 2.37).
        """
        r_eq = equivalent_radius_from_mass(AFA1190_NET_MASS_KG, AFA1190_LENGTH_M)
        R = anode_resistance_slender_standoff(rho=0.32, L_a=AFA1190_LENGTH_M, r_a=r_eq)
        assert R == pytest.approx(0.10541, abs=5e-5)
        assert DESIGN_DRIVING_VOLTAGE / R == pytest.approx(2.37, abs=0.01)

    def test_short_standoff_uses_table_10_7_short_form(self):
        """L < 4r selects the Table 10-7 short slender stand-off formula."""
        R = anode_resistance_slender_standoff(0.30, 0.30, 0.10)
        assert R == pytest.approx(short_slender_standoff(0.30, 0.30, 0.10), rel=1e-12)
        assert R > 0.0

    def test_negative_resistance_impossible(self):
        """r_a > 4 L / e used to give a negative resistance silently."""
        with pytest.raises(ValueError):
            anode_resistance_slender_standoff(0.30, 1.0, -0.5)
        with pytest.raises(ValueError):
            anode_current_output(0.30, 0.0, 0.05)


class TestAnodeCurrentOutput:
    """I_a = delta_E / R_a (Sec. 7.8)."""

    def test_standoff_anode_default_voltage(self):
        """rho = 0.30 ohm-m, L = 1.530 m, r = 0.085 m, default delta_E = 0.25 V.

        R_a = 0.30 / (2 pi 1.530) * (ln(4 * 1.530 / 0.085) - 1)
            = 0.031207 * (4.2767 - 1) = 0.10226 ohm; I_a = 0.25 / 0.10226 = 2.4449 A.
        """
        I_a = anode_current_output(rho=0.30, L_a=1.530, r_a=0.085)
        assert I_a == pytest.approx(2.4449, abs=5e-4)

    def test_standoff_anode_custom_voltage(self):
        """Current is proportional to the driving voltage: 0.30 / 0.20 = 1.5."""
        I_low = anode_current_output(rho=0.30, L_a=1.530, r_a=0.085, delta_E=0.20)
        I_high = anode_current_output(rho=0.30, L_a=1.530, r_a=0.085, delta_E=0.30)
        assert I_high / I_low == pytest.approx(1.5, rel=1e-12)

    def test_proximity_factor_reduces_current(self):
        """Proximity factor k multiplies R_a, so the current divides by k exactly."""
        I_open = anode_current_output(rho=0.30, L_a=1.530, r_a=0.085, proximity_factor=1.0)
        I_shielded = anode_current_output(
            rho=0.30, L_a=1.530, r_a=0.085, proximity_factor=2.0
        )
        assert I_shielded == pytest.approx(I_open / 2.0, rel=1e-12)

    def test_flush_mount_suction_pile_initial(self):
        """calc-009 suction pile: 29 lb flush anode, 24 in x 5 in face, 31 ohm-cm.

        Table 10-7 short flush: A = 24 * 5 * 0.0254^2 = 0.077419 m2,
        R_a = 0.315 * 0.31 / sqrt(0.077419) = 0.35095 ohm,
        I_a = 0.25 / 0.35095 = 0.7123 A.
        """
        with pytest.warns(DeprecationWarning):
            R_a = flush_anode_resistance(
                rho_ohm_cm=31.0, L_a_in=24.0, W_in=5.0, H_in=2.5, r_eq_in=2.39
            )
        I_a = 0.25 / R_a
        assert I_a == pytest.approx(0.7123, abs=5e-4)

    def test_flush_mount_buoyancy_can_initial(self):
        """calc-009 buoyancy can: same anode face in 24 ohm-cm seawater.

        R_a = 0.315 * 0.24 / sqrt(0.077419) = 0.27170 ohm,
        I_a = 0.25 / 0.27170 = 0.9201 A.
        """
        with pytest.warns(DeprecationWarning):
            R_a = flush_anode_resistance(
                rho_ohm_cm=24.0, L_a_in=24.0, W_in=5.0, H_in=2.5, r_eq_in=2.39
            )
        I_a = 0.25 / R_a
        assert I_a == pytest.approx(0.9201, abs=5e-4)


class TestFlushAnodeResistance:
    """Deprecated ``flush_anode_resistance`` = Table 10-7 short flush in SI.

    Inputs are the calc-009 flush-mount hull anode (24 in x 5 in x 2.5 in,
    29 lb net) in the calc-009 resistivities (31 ohm-cm suction pile,
    24 ohm-cm buoyancy can). calc-009 does not print resistance values
    (its calculation sheets are scanned and not in the repo), so the
    expected values are hand-derived from ``R_a = 0.315 rho / sqrt(A)``.
    """

    def test_suction_pile_initial(self):
        """rho = 0.31 ohm-m; A = 0.077419 m2; sqrt(A) = 0.27824 m.

        R_a = 0.315 * 0.31 / 0.27824 = 0.35095 ohm.
        """
        with pytest.warns(DeprecationWarning):
            R_a = flush_anode_resistance(
                rho_ohm_cm=31.0, L_a_in=24.0, W_in=5.0, H_in=2.5, r_eq_in=2.39
            )
        assert R_a == pytest.approx(0.35095, rel=1e-4)

    def test_buoyancy_can_initial(self):
        """rho = 0.24 ohm-m; R_a = 0.315 * 0.24 / 0.27824 = 0.27170 ohm."""
        with pytest.warns(DeprecationWarning):
            R_a = flush_anode_resistance(
                rho_ohm_cm=24.0, L_a_in=24.0, W_in=5.0, H_in=2.5, r_eq_in=2.39
            )
        assert R_a == pytest.approx(0.27170, rel=1e-4)

    def test_suction_pile_final_ignores_equivalent_radius(self):
        """The short flush form depends on the exposed area only.

        ``r_eq_in`` (and ``H_in``) are accepted for signature compatibility
        and do not change the result: a 0.90 in radius gives the same
        0.35095 ohm as 2.39 in. A depleted flush anode is modelled by
        passing its reduced exposed face.
        """
        with pytest.warns(DeprecationWarning):
            R_a = flush_anode_resistance(
                rho_ohm_cm=31.0, L_a_in=24.0, W_in=5.0, H_in=2.5, r_eq_in=0.90
            )
        assert R_a == pytest.approx(0.35095, rel=1e-4)

    def test_matches_kernel_short_flush(self):
        """The wrapper is the kernel formula after the unit conversion."""
        with pytest.warns(DeprecationWarning):
            R_a = flush_anode_resistance(31.0, 24.0, 5.0, 2.5, 2.39)
        assert R_a == pytest.approx(short_flush_or_bracelet(0.31, FLUSH_FACE_AREA_M2), rel=1e-12)


class TestEquivalentRadius:
    """Equivalent cylinder radius from net mass and length."""

    def test_afa1190_radius(self):
        """calc-008 r = 95 mm: sqrt(119 / (pi * 1.530 * 2750)) = 0.09488 m."""
        r = equivalent_radius_from_mass(AFA1190_NET_MASS_KG, AFA1190_LENGTH_M, ANODE_DENSITY_ALZNI)
        assert r == pytest.approx(0.09488, abs=5e-5)
        assert r == pytest.approx(0.095, abs=5e-4)

    def test_radius_scales_with_root_mass(self):
        """r ~ sqrt(m): doubling the mass multiplies r by sqrt(2)."""
        r1 = equivalent_radius_from_mass(100.0, 1.0)
        r2 = equivalent_radius_from_mass(200.0, 1.0)
        assert r2 / r1 == pytest.approx(math.sqrt(2.0), rel=1e-12)


class TestNumberOfAnodes:
    """Anode count against calc-008 Appendix A1.0 quantities."""

    def test_buoyancy_tank_anode_count(self):
        """2645 kg / 119 kg = ceil(22.23) = 23; calc-008 installs 24 (even, symmetric)."""
        assert number_of_anodes(2645.0, 119.0) == 23
        assert number_of_anodes(2645.0, 119.0, round_to_even=True) == 24

    def test_ura_anode_count(self):
        """1030 kg / 119 kg = ceil(8.66) = 9; calc-008 installs 10 (even, symmetric)."""
        assert number_of_anodes(1030.0, 119.0) == 9
        assert number_of_anodes(1030.0, 119.0, round_to_even=True) == 10

    def test_zero_mass_raises_value_error(self):
        """A zero anode mass raises ValueError naming the parameter (#2211)."""
        with pytest.raises(ValueError, match="anode_net_mass_kg"):
            number_of_anodes(100.0, 0.0)


class TestParametricSweeps:
    """Trend checks on the calc-008 inputs (the hypothesis versions are in test_properties.py)."""

    def test_current_demand_increases_with_area(self):
        areas = [100, 500, 1000, 2000]
        demands = [current_demand(a, 0.220, 0.11) for a in areas]
        for i in range(len(demands) - 1):
            assert demands[i + 1] > demands[i]

    def test_current_demand_increases_with_breakdown(self):
        bfs = [0.02, 0.05, 0.10, 0.20, 0.50, 1.0]
        demands = [current_demand(1000.0, 0.220, bf) for bf in bfs]
        for i in range(len(demands) - 1):
            assert demands[i + 1] > demands[i]

    def test_anode_mass_increases_with_design_life(self):
        lives = [5, 10, 15, 22, 30]
        masses = [anode_mass_requirement(10.0, t) for t in lives]
        for i in range(len(masses) - 1):
            assert masses[i + 1] > masses[i]

    def test_resistance_increases_with_seawater_resistivity(self):
        """R_a is proportional to rho: each step scales by the resistivity ratio."""
        rhos = [0.15, 0.22, 0.32, 0.50]
        r_eq = equivalent_radius_from_mass(AFA1190_NET_MASS_KG, AFA1190_LENGTH_M)
        resistances = [
            anode_resistance_slender_standoff(rho, AFA1190_LENGTH_M, r_eq) for rho in rhos
        ]
        for i in range(len(resistances) - 1):
            assert resistances[i + 1] / resistances[i] == pytest.approx(
                rhos[i + 1] / rhos[i], rel=1e-12
            )

    def test_coating_breakdown_monotonically_increases(self):
        times = [0, 5, 11, 15, 22]
        factors = [coating_breakdown_factor(0.02, 0.00818, t) for t in times]
        for i in range(len(factors) - 1):
            assert factors[i + 1] > factors[i]
