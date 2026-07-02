# ABOUTME: Tests for the production casing design-check layer (casing_design).
# ABOUTME: Golden numbers from Hansen (Devon) EPA HF workshop 2011 + NACE MR0175.
"""Tests for :mod:`digitalmodel.well.tubulars.casing_design`.

Golden values come from the source presentation (Hansen, Devon Energy, EPA
Hydraulic Fracturing Technical Workshop Session 2, 2011): the 5-1/2" 23# P110
Barlow worked example (14,520 psi after API rounding), the operator minimum
design factors, and the NACE MR0175 sour-service thresholds and grade
temperature windows.
"""
from __future__ import annotations

import math

import pytest

from digitalmodel.well.tubulars.casing import Casing as _Casing, casing as build_casing
from digitalmodel.well.tubulars.casing_design import (
    CONNECTION_CLASSES,
    DesignFactors,
    PPG_TO_PSI_PER_FT,
    PressureProfile,
    ProductionCasingWell,
    api_round_force_lbf,
    api_round_pressure_psi,
    axial_load_profile,
    ballooning_tension_delta_lbf,
    buoyancy_factor,
    burst_external_profile,
    check_burst,
    check_collapse,
    check_production_casing,
    check_tension,
    check_triaxial,
    connection_class,
    cooling_tension_delta_lbf,
    fluid_column_profile,
    full_evacuation_internal_profile,
    grade_sour_ok,
    h2s_partial_pressure_psia,
    injection_internal_profile,
    max_frac_surface_pressure,
    shut_in_tubing_pressure,
    sour_service_screen,
    tubing_leak_internal_profile,
)


@pytest.fixture
def p110_55_23() -> _Casing:
    """The presentation's worked-example product: 5-1/2\" 23# P110."""
    return build_casing("P110", od_in=5.5, weight_ppf=23.0)


# ---------------------------------------------------------------------------
# Golden worked example + API rounding
# ---------------------------------------------------------------------------
class TestGoldenBarlow:
    def test_worked_example_wall_and_id(self, p110_55_23):
        # ID 4.67" -> wall (5.500 - 4.670)/2 = 0.415"
        assert p110_55_23.wt_in == pytest.approx(0.415)
        assert p110_55_23.id_in == pytest.approx(4.67)

    def test_barlow_burst_rounds_to_14520_psi(self, p110_55_23):
        # P = 0.875 * (2 * 110,000 * 0.415) / 5.5 = 14,525 -> 14,520 psi
        assert p110_55_23.burst_psi == pytest.approx(14525.0, abs=1.0)
        assert api_round_pressure_psi(p110_55_23.burst_psi) == 14520.0

    def test_api_rounding_conventions(self):
        assert api_round_pressure_psi(14525.4) == 14530.0
        assert api_round_pressure_psi(14524.9) == 14520.0
        assert api_round_force_lbf(830_499.0) == 830_000.0
        assert api_round_force_lbf(830_501.0) == 831_000.0


# ---------------------------------------------------------------------------
# Pressure profiles
# ---------------------------------------------------------------------------
class TestProfiles:
    def test_fluid_column_gradient(self):
        p = fluid_column_profile(10.0, 10_000.0)
        assert float(p.at(0.0)) == 0.0
        assert float(p.at(10_000.0)) == pytest.approx(
            PPG_TO_PSI_PER_FT * 10.0 * 10_000.0)

    def test_sitp_is_reservoir_minus_gas_column(self):
        sitp = shut_in_tubing_pressure(9_000.0, 12_000.0,
                                       gas_gradient_psi_ft=0.1)
        assert sitp == pytest.approx(9_000.0 - 1_200.0)

    def test_tubing_leak_puts_sitp_on_packer_fluid(self):
        prof = tubing_leak_internal_profile(
            reservoir_pressure_psi=9_000.0, reservoir_depth_ft=12_000.0,
            packer_fluid_ppg=9.0, td_ft=12_000.0, gas_gradient_psi_ft=0.1)
        sitp = 9_000.0 - 1_200.0
        assert float(prof.at(0.0)) == pytest.approx(sitp)
        assert float(prof.at(12_000.0)) == pytest.approx(
            sitp + 0.052 * 9.0 * 12_000.0)

    def test_injection_profile_is_surface_plus_gradient(self):
        prof = injection_internal_profile(8_000.0, 8.6, 10_000.0)
        assert float(prof.at(0.0)) == 8_000.0
        assert float(prof.at(10_000.0)) == pytest.approx(
            8_000.0 + 0.052 * 8.6 * 10_000.0)

    def test_burst_external_three_segments(self):
        prof = burst_external_profile(mud_ppg=10.0, toc_ft=4_000.0,
                                      outer_shoe_ft=8_000.0, td_ft=12_000.0,
                                      mix_water_ppg=8.4, pore_ppg=9.0)
        p_toc = 0.052 * 10.0 * 4_000.0
        p_shoe = p_toc + 0.052 * 8.4 * 4_000.0
        p_td = p_shoe + 0.052 * 9.0 * 4_000.0
        assert float(prof.at(4_000.0)) == pytest.approx(p_toc)
        assert float(prof.at(8_000.0)) == pytest.approx(p_shoe)
        assert float(prof.at(12_000.0)) == pytest.approx(p_td)

    def test_burst_external_rejects_bad_ordering(self):
        with pytest.raises(ValueError):
            burst_external_profile(10.0, toc_ft=9_000.0, outer_shoe_ft=8_000.0,
                                   td_ft=12_000.0)

    def test_profile_validation(self):
        with pytest.raises(ValueError):
            PressureProfile((0.0,), (0.0,))
        with pytest.raises(ValueError):
            PressureProfile((0.0, 100.0), (0.0,))
        with pytest.raises(ValueError):
            PressureProfile((100.0, 0.0), (0.0, 0.0))


# ---------------------------------------------------------------------------
# Design factors
# ---------------------------------------------------------------------------
class TestDesignFactors:
    def test_published_operator_minimums(self):
        df = DesignFactors()
        assert df.burst == 1.25
        assert df.collapse == 1.1
        assert df.tension == 1.4
        assert df.compression == 1.2
        assert df.triaxial == 1.25

    def test_burst_relaxes_below_sicp_threshold(self):
        df = DesignFactors()
        assert df.burst_for_sicp(4_999.0) == 1.1
        assert df.burst_for_sicp(5_000.0) == 1.25


# ---------------------------------------------------------------------------
# Mode checks
# ---------------------------------------------------------------------------
class TestBurstCheck:
    def test_burst_check_governing_depth_surface_for_gas(self, p110_55_23):
        # Tubing leak with light packer fluid vs heavier backup: the net
        # differential is greatest at surface.
        td = 12_000.0
        internal = tubing_leak_internal_profile(9_000.0, td, 8.6, td)
        external = burst_external_profile(10.0, 4_000.0, 8_000.0, td)
        res = check_burst(p110_55_23, internal, external, td)
        assert res.mode == "burst"
        assert res.rating == 14_520.0
        assert res.governing_depth_ft == 0.0
        assert res.max_load == pytest.approx(float(internal.at(0.0)))
        assert res.min_design_factor == pytest.approx(
            14_520.0 / float(internal.at(0.0)))
        assert res.passes

    def test_burst_check_uses_relaxed_df_for_low_sicp(self, p110_55_23):
        td = 10_000.0
        internal = fluid_column_profile(9.0, td, surface_pressure_psi=4_000.0)
        external = fluid_column_profile(9.0, td)
        res = check_burst(p110_55_23, internal, external, td, sicp_psi=4_000.0)
        assert res.required_design_factor == 1.1

    def test_burst_check_no_load_is_infinite_df(self, p110_55_23):
        td = 10_000.0
        internal = fluid_column_profile(8.6, td)
        external = fluid_column_profile(10.0, td)
        res = check_burst(p110_55_23, internal, external, td)
        assert math.isinf(res.min_design_factor)
        assert res.passes


class TestCollapseCheck:
    def test_full_evacuation_governs_at_shoe(self, p110_55_23):
        td = 10_000.0
        external = fluid_column_profile(10.0, td)
        res = check_collapse(p110_55_23, external, td)
        assert res.mode == "collapse"
        assert res.governing_depth_ft == td
        assert res.max_load == pytest.approx(0.052 * 10.0 * td)
        expected_df = res.rating / (0.052 * 10.0 * td)
        assert res.min_design_factor == pytest.approx(expected_df)

    def test_collapse_rating_is_api_rounded(self, p110_55_23):
        td = 1_000.0
        external = fluid_column_profile(9.0, td)
        res = check_collapse(p110_55_23, external, td)
        assert res.rating == api_round_pressure_psi(p110_55_23.collapse_psi)
        assert res.rating % 10.0 == 0.0


class TestTensionCheck:
    def test_buoyancy_factor(self):
        assert buoyancy_factor(0.0) == 1.0
        assert buoyancy_factor(10.0) == pytest.approx(1.0 - 10.0 / 65.4)

    def test_axial_profile_max_at_surface(self):
        z, f = axial_load_profile(23.0, 12_000.0, 10.0)
        assert f[0] == pytest.approx(
            23.0 * 12_000.0 * buoyancy_factor(10.0))
        assert f[-1] == pytest.approx(0.0)

    def test_tension_check(self, p110_55_23):
        res = check_tension(p110_55_23, 23.0, 12_000.0, 10.0)
        assert res.mode == "tension"
        assert res.governing_depth_ft == 0.0
        assert res.rating == api_round_force_lbf(p110_55_23.body_yield_lbf)
        assert res.required_design_factor == 1.4
        assert res.passes  # 23# P110 body yield >> buoyed string weight

    def test_frac_adders_reduce_margin(self, p110_55_23):
        base = check_tension(p110_55_23, 23.0, 12_000.0, 10.0)
        loaded = check_tension(p110_55_23, 23.0, 12_000.0, 10.0,
                               extra_tension_lbf=200_000.0)
        assert loaded.min_design_factor < base.min_design_factor

    def test_ballooning_adder_positive_and_scales(self, p110_55_23):
        d1 = ballooning_tension_delta_lbf(1_000.0, p110_55_23)
        d2 = ballooning_tension_delta_lbf(2_000.0, p110_55_23)
        a_i = math.pi / 4.0 * 4.67 ** 2
        assert d1 == pytest.approx(2.0 * 0.30 * 1_000.0 * a_i)
        assert d2 == pytest.approx(2.0 * d1)

    def test_cooling_adder_matches_thermal_formula(self, p110_55_23):
        dt = 50.0
        expected = 30.0e6 * 6.9e-6 * dt * p110_55_23.metal_area_in2
        assert cooling_tension_delta_lbf(dt, p110_55_23) == pytest.approx(
            expected)


class TestTriaxialCheck:
    def test_triaxial_df_positive_and_mode_labelled(self, p110_55_23):
        td = 12_000.0
        internal = tubing_leak_internal_profile(9_000.0, td, 8.6, td)
        external = burst_external_profile(10.0, 4_000.0, 8_000.0, td)
        res = check_triaxial(p110_55_23, internal, external, 23.0, td, 10.0)
        assert res.mode == "triaxial"
        assert res.rating == 110_000.0
        assert 0.0 < res.max_load < 110_000.0
        assert res.min_design_factor > 1.0
        assert res.required_design_factor == 1.25


# ---------------------------------------------------------------------------
# Frac surface pressure
# ---------------------------------------------------------------------------
class TestMaxFracSurfacePressure:
    def test_round_trip_at_limit_df(self, p110_55_23):
        td = 10_000.0
        external = burst_external_profile(10.0, 3_000.0, 7_000.0, td)
        p_max = max_frac_surface_pressure(p110_55_23, 8.6, external, td)
        # Applying exactly p_max must give a burst DF equal to the minimum.
        internal = injection_internal_profile(p_max, 8.6, td)
        res = check_burst(p110_55_23, internal, external, td)
        assert res.min_design_factor == pytest.approx(1.25, rel=1e-6)

    def test_higher_df_means_lower_allowable(self, p110_55_23):
        td = 10_000.0
        external = burst_external_profile(10.0, 3_000.0, 7_000.0, td)
        p_125 = max_frac_surface_pressure(p110_55_23, 8.6, external, td,
                                          design_factor=1.25)
        p_110 = max_frac_surface_pressure(p110_55_23, 8.6, external, td,
                                          design_factor=1.10)
        assert p_110 > p_125


# ---------------------------------------------------------------------------
# Sour service (NACE MR0175 / ISO 15156)
# ---------------------------------------------------------------------------
class TestSourService:
    def test_partial_pressure_formula(self):
        # 100 ppm at 10,000 psia -> 1.0 psia
        assert h2s_partial_pressure_psia(100.0, 10_000.0) == pytest.approx(1.0)

    def test_sour_when_both_thresholds_exceeded(self):
        res = sour_service_screen(100.0, 10_000.0, "gas")
        assert res.is_sour

    def test_not_sour_below_partial_pressure_threshold(self):
        # 1 ppm at 1,000 psia -> 0.001 psia < 0.05 psia
        res = sour_service_screen(1.0, 1_000.0, "gas")
        assert not res.is_sour

    def test_not_sour_below_total_pressure_gas(self):
        # Very high ppm but only 60 psia total (< 65 psia gas threshold).
        res = sour_service_screen(10_000.0, 60.0, "gas")
        assert not res.is_sour

    def test_oil_total_pressure_threshold_is_265(self):
        res_oil = sour_service_screen(10_000.0, 200.0, "oil")
        res_gas = sour_service_screen(10_000.0, 200.0, "gas")
        assert not res_oil.is_sour
        assert res_gas.is_sour

    def test_grade_temperature_windows(self):
        # All-temperature grades.
        for g in ("H40", "J55", "K55", "M65", "L80", "C90", "T95"):
            assert grade_sour_ok(g, 40.0)
        # 150F window.
        assert not grade_sour_ok("C95", 149.0)
        assert grade_sour_ok("C95", 150.0)
        assert grade_sour_ok("N80Q", 150.0)
        # 175F window.
        assert not grade_sour_ok("P110", 174.0)
        assert grade_sour_ok("P110", 175.0)
        assert not grade_sour_ok("N80", 150.0)
        # 225F window.
        assert not grade_sour_ok("Q125", 200.0)
        assert grade_sour_ok("Q125", 225.0)

    def test_sour_screen_filters_grades_by_temperature(self):
        res = sour_service_screen(100.0, 10_000.0, "gas", service_temp_f=80.0)
        assert res.is_sour
        assert "P110" not in res.acceptable_grades
        assert "L80" in res.acceptable_grades
        hot = sour_service_screen(100.0, 10_000.0, "gas", service_temp_f=250.0)
        assert "Q125" in hot.acceptable_grades

    def test_unknown_grade_raises(self):
        with pytest.raises(KeyError):
            grade_sour_ok("X999", 100.0)


# ---------------------------------------------------------------------------
# Connections
# ---------------------------------------------------------------------------
class TestConnections:
    def test_classes_present(self):
        assert set(CONNECTION_CLASSES) == {"STC", "LTC", "BTC", "MTC", "IJ",
                                           "FJ"}

    def test_api_connections_ultimate_basis(self):
        for name in ("STC", "LTC", "BTC"):
            assert connection_class(name).strength_basis == "ultimate"
        for name in ("MTC", "IJ", "FJ"):
            assert connection_class(name).strength_basis == "yield"

    def test_efficiency_ordering(self, p110_55_23):
        # MTC = pipe body; IJ 70-80%; FJ 45-60% (source presentation).
        mtc = connection_class("MTC").joint_strength_lbf(p110_55_23)
        ij = connection_class("IJ").joint_strength_lbf(p110_55_23)
        fj = connection_class("FJ").joint_strength_lbf(p110_55_23)
        assert mtc == pytest.approx(p110_55_23.body_yield_lbf)
        assert ij == pytest.approx(0.70 * p110_55_23.body_yield_lbf)
        assert fj == pytest.approx(0.45 * p110_55_23.body_yield_lbf)
        assert mtc > ij > fj

    def test_thread_counts(self):
        assert connection_class("STC").threads_per_inch == 8
        assert connection_class("LTC").threads_per_inch == 8
        assert connection_class("BTC").threads_per_inch == 5

    def test_unknown_connection_raises(self):
        with pytest.raises(KeyError):
            connection_class("XYZ")


# ---------------------------------------------------------------------------
# Orchestrated check
# ---------------------------------------------------------------------------
class TestCheckProductionCasing:
    def test_full_check_modes_and_pass(self, p110_55_23):
        well = ProductionCasingWell(
            td_ft=12_000.0, mud_ppg=10.0, toc_ft=4_000.0,
            outer_shoe_ft=8_000.0, reservoir_pressure_psi=8_500.0,
            packer_fluid_ppg=9.0, frac_surface_pressure_psi=9_000.0,
            frac_fluid_ppg=8.6)
        results = check_production_casing(p110_55_23, 23.0, well)
        assert set(results) == {"burst", "collapse", "tension", "triaxial"}
        for mode, res in results.items():
            assert res.min_design_factor > 0.0, mode
        assert results["collapse"].governing_depth_ft == well.td_ft
        assert results["tension"].governing_depth_ft == 0.0

    def test_frac_case_can_govern_burst(self, p110_55_23):
        base = ProductionCasingWell(
            td_ft=12_000.0, mud_ppg=10.0, toc_ft=4_000.0,
            outer_shoe_ft=8_000.0, reservoir_pressure_psi=6_000.0,
            packer_fluid_ppg=9.0, frac_surface_pressure_psi=0.0)
        frac = ProductionCasingWell(
            td_ft=12_000.0, mud_ppg=10.0, toc_ft=4_000.0,
            outer_shoe_ft=8_000.0, reservoir_pressure_psi=6_000.0,
            packer_fluid_ppg=9.0, frac_surface_pressure_psi=11_000.0)
        df_base = check_production_casing(p110_55_23, 23.0, base)
        df_frac = check_production_casing(p110_55_23, 23.0, frac)
        assert (df_frac["burst"].min_design_factor
                < df_base["burst"].min_design_factor)

    def test_undersized_string_fails(self):
        weak = build_casing("J55", od_in=5.5, weight_ppf=14.0)
        well = ProductionCasingWell(
            td_ft=12_000.0, mud_ppg=10.0, toc_ft=4_000.0,
            outer_shoe_ft=8_000.0, reservoir_pressure_psi=9_500.0,
            packer_fluid_ppg=9.0, frac_surface_pressure_psi=9_000.0)
        results = check_production_casing(weak, 14.0, well)
        assert not results["burst"].passes
