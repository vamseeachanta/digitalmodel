# ABOUTME: Tests for the production tubing design-check layer (tubing_design)
# ABOUTME: and the API 5CT tubing catalog. Golden: 2-7/8" 6.5# N80 ratings.
"""Tests for :mod:`digitalmodel.well.tubulars.tubing_design` + tubing catalog.

Golden values are the published API ratings for 2-7/8" 6.5# N80 tubing
(API 5C3 / standard vendor tubing tables): internal yield (burst) 10,570 psi,
pipe-body yield 145,000 lbf, collapse ~11,160 psi (see the collapse test for
the 10-psi API-rounding delta).  Tubing dimensions per API 5CT.
"""
from __future__ import annotations

import math

import pytest

from digitalmodel.well.tubulars.casing import (
    TUBING_SIZES,
    Casing as _Casing,
    list_tubing_sizes,
    tubing as build_tubing,
    tubing_wall_for_size,
)
from digitalmodel.well.tubulars.casing_design import (
    DesignFactors,
    PPG_TO_PSI_PER_FT,
    api_round_force_lbf,
    api_round_pressure_psi,
    buoyancy_factor,
    shut_in_tubing_pressure,
)
from digitalmodel.well.tubulars.tubing_design import (
    TubingWell,
    check_production_tubing,
    packer_fluid_annulus_profile,
    shut_in_piston_force_lbf,
    shut_in_tubing_profile,
)


@pytest.fixture
def n80_278_65() -> _Casing:
    """The golden product: 2-7/8\" 6.5# N80 tubing."""
    return build_tubing("N80", od_in=2.875, weight_ppf=6.5)


# ---------------------------------------------------------------------------
# Tubing catalog (API 5CT)
# ---------------------------------------------------------------------------
class TestTubingCatalog:
    def test_published_api_5ct_walls(self):
        # API 5CT tubing dimension tables (wall in inches).
        assert TUBING_SIZES[(2.375, 4.70)] == 0.190   # ID 1.995"
        assert TUBING_SIZES[(2.375, 5.95)] == 0.254   # ID 1.867"
        assert TUBING_SIZES[(2.875, 6.50)] == 0.217   # ID 2.441"
        assert TUBING_SIZES[(2.875, 8.70)] == 0.308   # ID 2.259"
        assert TUBING_SIZES[(3.5, 9.30)] == 0.254     # ID 2.992"
        assert TUBING_SIZES[(3.5, 12.95)] == 0.375    # ID 2.750"
        assert TUBING_SIZES[(4.0, 11.00)] == 0.262    # ID 3.476"
        assert TUBING_SIZES[(4.5, 12.75)] == 0.271    # ID 3.958"

    def test_list_tubing_sizes_sorted_and_complete(self):
        sizes = list_tubing_sizes()
        assert sizes == sorted(TUBING_SIZES)
        assert len(sizes) == 8
        assert {od for od, _ in sizes} == {2.375, 2.875, 3.5, 4.0, 4.5}

    def test_wall_lookup_and_unknown_size_raises(self):
        assert tubing_wall_for_size(2.875, 6.5) == 0.217
        with pytest.raises(KeyError):
            tubing_wall_for_size(2.875, 99.0)

    def test_builder_returns_casing_product(self, n80_278_65):
        assert isinstance(n80_278_65, _Casing)
        assert n80_278_65.grade.name == "N80"
        assert n80_278_65.od_in == 2.875
        assert n80_278_65.weight_ppf == 6.5

    def test_builder_explicit_wall(self):
        t = build_tubing("L80", od_in=2.875, wt_in=0.217)
        assert t.wt_in == 0.217

    def test_builder_error_paths(self):
        with pytest.raises(ValueError):
            build_tubing("N80", od_in=2.875, weight_ppf=6.5, wt_in=0.217)
        with pytest.raises(ValueError):
            build_tubing("N80", od_in=2.875)
        with pytest.raises(KeyError):
            build_tubing("X999", od_in=2.875, weight_ppf=6.5)


# ---------------------------------------------------------------------------
# Golden published ratings: 2-7/8" 6.5# N80
# ---------------------------------------------------------------------------
class TestGoldenN80Tubing:
    def test_wall_and_id(self, n80_278_65):
        assert n80_278_65.wt_in == pytest.approx(0.217)
        assert n80_278_65.id_in == pytest.approx(2.441)

    def test_burst_rounds_to_published_10570_psi(self, n80_278_65):
        # Barlow: 0.875 * 2 * 80,000 * 0.217 / 2.875 = 10,567 psi
        assert n80_278_65.burst_psi == pytest.approx(10_566.96, abs=0.1)
        assert api_round_pressure_psi(n80_278_65.burst_psi) == 10_570.0

    def test_body_yield_rounds_to_published_145000_lbf(self, n80_278_65):
        # A = pi/4 * (2.875^2 - 2.441^2) = 1.812 in^2; * 80,000 = 144,962 lbf
        assert n80_278_65.body_yield_lbf == pytest.approx(144_962.1, abs=1.0)
        assert api_round_force_lbf(n80_278_65.body_yield_lbf) == 145_000.0

    def test_collapse_near_published_11160_psi(self, n80_278_65):
        # Published API rating is 11,160 psi.  The code's 4-regime API 5C3
        # collapse gives 11,165.0 psi (D/t = 13.25 falls in the YIELD regime,
        # boundary at 13.38 for 80 ksi), which API-rounds to 11,170 psi —
        # a 10 psi (0.09%) delta from the published table value, attributable
        # to rounding conventions in the legacy tables.  Assert the code's
        # own value and its closeness to the published number.
        assert n80_278_65.collapse_regime == "yield"
        assert n80_278_65.collapse_psi == pytest.approx(11_165.0, abs=0.1)
        assert api_round_pressure_psi(n80_278_65.collapse_psi) == 11_170.0
        assert n80_278_65.collapse_psi == pytest.approx(11_160.0, rel=0.01)


# ---------------------------------------------------------------------------
# Load-case profile builders
# ---------------------------------------------------------------------------
class TestProfileBuilders:
    def test_shut_in_tubing_profile_is_sitp_plus_gas_gradient(self):
        prof = shut_in_tubing_profile(3_200.0, 8_000.0,
                                      gas_gradient_psi_ft=0.1)
        assert float(prof.at(0.0)) == pytest.approx(3_200.0)
        assert float(prof.at(8_000.0)) == pytest.approx(3_200.0 + 800.0)
        assert "shut-in" in prof.label

    def test_shut_in_profile_rejects_nonpositive_td(self):
        with pytest.raises(ValueError):
            shut_in_tubing_profile(3_200.0, 0.0)

    def test_packer_fluid_annulus_gradient_and_surface_pressure(self):
        prof = packer_fluid_annulus_profile(9.0, 8_000.0,
                                            surface_pressure_psi=500.0)
        assert float(prof.at(0.0)) == pytest.approx(500.0)
        assert float(prof.at(8_000.0)) == pytest.approx(
            500.0 + PPG_TO_PSI_PER_FT * 9.0 * 8_000.0)
        assert prof.label == "packer-fluid annulus"


# ---------------------------------------------------------------------------
# Packer piston force
# ---------------------------------------------------------------------------
class TestPistonForce:
    def test_lubinski_piston_formula(self, n80_278_65):
        # F = (Ap - Ai) * P_tbg - (Ap - Ao) * P_ann
        bore = 3.25
        ap = math.pi / 4.0 * bore ** 2
        ai = math.pi / 4.0 * 2.441 ** 2
        ao = math.pi / 4.0 * 2.875 ** 2
        expected = (ap - ai) * 5_000.0 - (ap - ao) * 1_000.0
        got = shut_in_piston_force_lbf(bore, n80_278_65, 5_000.0, 1_000.0)
        assert got == pytest.approx(expected)

    def test_bore_equal_to_od_reduces_to_dp_on_wall_area(self, n80_278_65):
        # Ap = Ao -> F = (Ao - Ai) * P_tbg (the metal-area dP term).
        got = shut_in_piston_force_lbf(2.875, n80_278_65, 4_000.0, 3_744.0)
        assert got == pytest.approx(n80_278_65.metal_area_in2 * 4_000.0)

    def test_zero_pressures_zero_force(self, n80_278_65):
        assert shut_in_piston_force_lbf(3.25, n80_278_65, 0.0, 0.0) == 0.0

    def test_nonpositive_bore_raises(self, n80_278_65):
        with pytest.raises(ValueError):
            shut_in_piston_force_lbf(0.0, n80_278_65, 1_000.0)


# ---------------------------------------------------------------------------
# Orchestrated check
# ---------------------------------------------------------------------------
class TestCheckProductionTubing:
    @pytest.fixture
    def moderate_well(self) -> TubingWell:
        return TubingWell(td_ft=8_000.0, packer_fluid_ppg=9.0,
                          reservoir_pressure_psi=4_000.0)

    def test_modes_present_and_n80_passes(self, n80_278_65, moderate_well):
        results = check_production_tubing(n80_278_65, 6.5, moderate_well)
        assert set(results) == {"burst", "collapse", "tension"}
        for mode, res in results.items():
            assert res.passes, mode
            assert res.min_design_factor > 0.0, mode

    def test_burst_governs_at_surface_with_sitp(self, n80_278_65,
                                                moderate_well):
        results = check_production_tubing(n80_278_65, 6.5, moderate_well)
        res = results["burst"]
        sitp = shut_in_tubing_pressure(4_000.0, 8_000.0)
        assert res.rating == 10_570.0
        # Gas inside vs packer fluid outside: net differential is greatest
        # at surface, where the backup is zero.
        assert res.governing_depth_ft == 0.0
        assert res.max_load == pytest.approx(sitp)
        # SITP < 5,000 psi -> relaxed burst DF minimum (operator practice).
        assert res.required_design_factor == 1.1

    def test_collapse_evacuated_governs_at_packer(self, n80_278_65,
                                                  moderate_well):
        results = check_production_tubing(n80_278_65, 6.5, moderate_well)
        res = results["collapse"]
        assert res.governing_depth_ft == moderate_well.td_ft
        assert res.max_load == pytest.approx(
            PPG_TO_PSI_PER_FT * 9.0 * 8_000.0)
        assert res.rating == 11_170.0

    def test_surface_casing_pressure_reduces_collapse_margin(
            self, n80_278_65, moderate_well):
        pressurized = TubingWell(td_ft=8_000.0, packer_fluid_ppg=9.0,
                                 reservoir_pressure_psi=4_000.0,
                                 surface_casing_pressure_psi=1_500.0)
        base = check_production_tubing(n80_278_65, 6.5, moderate_well)
        loaded = check_production_tubing(n80_278_65, 6.5, pressurized)
        assert (loaded["collapse"].min_design_factor
                < base["collapse"].min_design_factor)
        assert loaded["collapse"].max_load == pytest.approx(
            1_500.0 + PPG_TO_PSI_PER_FT * 9.0 * 8_000.0)

    def test_tension_includes_buoyed_weight_plus_piston(self, n80_278_65,
                                                        moderate_well):
        results = check_production_tubing(n80_278_65, 6.5, moderate_well)
        res = results["tension"]
        assert res.rating == 145_000.0
        assert res.governing_depth_ft == 0.0
        buoyed = 6.5 * 8_000.0 * buoyancy_factor(9.0)
        # Default bore = tubing OD -> piston = (Ao - Ai) * P_tbg at packer;
        # shut-in tubing pressure at the packer equals reservoir pressure.
        piston = n80_278_65.metal_area_in2 * 4_000.0
        assert res.max_load == pytest.approx(buoyed + piston)
        assert res.passes

    def test_explicit_packer_bore_changes_tension_load(self, n80_278_65,
                                                       moderate_well):
        big_bore = TubingWell(td_ft=8_000.0, packer_fluid_ppg=9.0,
                              reservoir_pressure_psi=4_000.0,
                              packer_bore_in=3.25)
        base = check_production_tubing(n80_278_65, 6.5, moderate_well)
        wide = check_production_tubing(n80_278_65, 6.5, big_bore)
        assert (wide["tension"].max_load
                != pytest.approx(base["tension"].max_load))

    def test_negative_piston_force_not_credited(self, n80_278_65):
        # Depleted reservoir + heavy annulus + big bore -> negative piston
        # force; the tension check must not take credit for the relief.
        well = TubingWell(td_ft=8_000.0, packer_fluid_ppg=15.0,
                          reservoir_pressure_psi=1_500.0,
                          surface_casing_pressure_psi=2_000.0,
                          packer_bore_in=4.0)
        sitp = shut_in_tubing_pressure(1_500.0, 8_000.0)
        p_tbg = sitp + 0.1 * 8_000.0
        p_ann = 2_000.0 + PPG_TO_PSI_PER_FT * 15.0 * 8_000.0
        assert shut_in_piston_force_lbf(4.0, n80_278_65, p_tbg, p_ann) < 0.0
        res = check_production_tubing(n80_278_65, 6.5, well)["tension"]
        buoyed = 6.5 * 8_000.0 * buoyancy_factor(15.0)
        assert res.max_load == pytest.approx(buoyed)

    def test_undersized_string_fails_burst(self):
        weak = build_tubing("J55", od_in=2.375, weight_ppf=4.7)
        hot = TubingWell(td_ft=12_000.0, packer_fluid_ppg=9.0,
                         reservoir_pressure_psi=11_000.0)
        results = check_production_tubing(weak, 4.7, hot)
        # J55 burst = 0.875 * 2 * 55,000 * 0.190 / 2.375 = 7,700 psi vs
        # SITP = 11,000 - 1,200 = 9,800 psi.
        assert results["burst"].rating == 7_700.0
        assert not results["burst"].passes

    def test_custom_design_factors_flow_through(self, n80_278_65,
                                                moderate_well):
        strict = DesignFactors(collapse=5.0)
        results = check_production_tubing(n80_278_65, 6.5, moderate_well,
                                          factors=strict)
        assert results["collapse"].required_design_factor == 5.0
        assert not results["collapse"].passes
