"""Tests for the DNV-RP-F103 bracelet anode design module (issue #2211).

The headline case is review Appendix A3, written from the Table A.1 row
for FBE (a = 0.010, b = 0.0003) rather than the a = 0.030 the review's
reviewer recalled.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.cathodic_protection.dnv_rp_f103 import (
    STEEL_RESISTIVITY,
    BraceletDesignInput,
    BraceletDesignResult,
    design_bracelet_cp,
    protected_length,
)
from digitalmodel.cathodic_protection.f103_tables import (
    Exposure,
    FieldJointCoating,
    LinepipeCoating,
)

EDITION = "2010"


def _a3_input(**overrides) -> BraceletDesignInput:
    base = dict(
        outer_diameter_m=0.3239,
        wall_thickness_m=0.0127,
        length_m=10000.0,
        linepipe_coating=LinepipeCoating.FBE,
        exposure=Exposure.NON_BURIED,
        fluid_temperature_c=20.0,
        design_life_years=25.0,
        seawater_resistivity_ohm_m=0.30,
        bracelet_net_mass_kg=40.0,
        bracelet_length_m=0.30,
        bracelet_thickness_m=0.04,
    )
    base.update(overrides)
    return BraceletDesignInput(**base)


class TestReviewAppendixA3:
    """10 km x 12.75 in FBE flowline, non-buried, fluid <= 50 °C, 25 yr.

    A = pi * 0.3239 * 10 000 = 10 175.6 m2 (review: 10 176 m2).
    i_cm = 0.050 A/m2 (Table 5-1, non-buried, <= 50 °C).
    Table A.1 FBE: a = 0.010, b = 0.0003 -> f_cm = 0.010 + 0.0003 * 12.5
    = 0.01375; f_cf = 0.010 + 0.0003 * 25 = 0.0175.
    I_cm = 10 175.6 * 0.050 * 0.01375 = 6.9957 A (review: 6.996 A).
    I_cf = 10 175.6 * 0.050 * 0.0175 = 8.9037 A.
    M = 6.9957 * 25 * 8760 / (2000 * 0.80) = 957.54 kg (review: 957.7 kg
    from the rounded 6.996 A); 40 kg bracelets -> ceil(23.94) = 24 anodes.
    Bracelet 0.30 m long, 0.04 m thick: A_a = pi * (0.3239 + 0.08) * 0.30
    = 0.38067 m2; R_a = 0.315 * 0.30 / sqrt(0.38067) = 0.15316 ohm;
    I_a = 0.25 / 0.15316 = 1.6322 A; N_final = ceil(8.9037 / 1.6322) = 6.
    N = max(24, 6) = 24, governing 'mass'; spacing 10 000 / 24 = 416.7 m.
    PL (Eq. 14, f_cf = 0.0175, rho_me = 2.0e-7, delta_E_me = 0.15 V):
    sqrt(0.15 * 0.0127 * 0.3112 / (2e-7 * 0.3239 * 0.0175 * 0.050))
    = sqrt(5.9283e-4 / 5.6683e-11) = sqrt(1.04588e7) = 3234.0 m;
    spacing 416.7 m <= 2 * 3234 m.
    """

    def test_appendix_a3_numbers(self):
        result = design_bracelet_cp(_a3_input(), edition=EDITION)
        assert isinstance(result, BraceletDesignResult)
        assert result.surface_area_m2 == pytest.approx(10175.6, abs=0.1)
        assert result.field_joint_area_m2 == 0.0
        assert result.mean_current_density_A_m2 == pytest.approx(0.050)
        assert result.f_cm_linepipe == pytest.approx(0.01375)
        assert result.f_cf_linepipe == pytest.approx(0.0175)
        assert result.mean_current_demand_A == pytest.approx(6.9957, abs=1e-3)
        assert result.final_current_demand_A == pytest.approx(8.9037, abs=1e-3)
        assert result.utilisation_factor == pytest.approx(0.80)
        assert result.anode_capacity_Ah_kg == pytest.approx(2000.0)
        assert result.driving_voltage_V == pytest.approx(0.25)
        assert result.total_net_mass_kg == pytest.approx(957.54, abs=0.05)
        assert result.number_of_anodes_mass == 24
        assert result.bracelet_exposed_area_m2 == pytest.approx(0.38067, abs=1e-4)
        assert result.anode_resistance_ohm == pytest.approx(0.15316, abs=1e-4)
        assert result.anode_current_output_A == pytest.approx(1.6322, abs=1e-3)
        assert result.number_of_anodes_final == 6
        assert result.number_of_anodes == 24
        assert result.governing_case == "mass"
        assert result.anode_spacing_m == pytest.approx(416.67, abs=0.01)
        assert result.protected_length_m == pytest.approx(3234.0, abs=0.5)
        assert result.spacing_ok
        assert result.current_output_ok

    def test_appendix_a3_hand_formulas(self):
        """The result equals the formulas evaluated explicitly in the test."""
        result = design_bracelet_cp(_a3_input(), edition=EDITION)
        area = math.pi * 0.3239 * 10000.0
        I_cm = area * 0.050 * (0.010 + 0.0003 * 12.5)
        M = I_cm * 25.0 * 8760.0 / (2000.0 * 0.80)
        assert result.mean_current_demand_A == pytest.approx(I_cm, rel=1e-12)
        assert result.total_net_mass_kg == pytest.approx(M, rel=1e-12)
        assert result.number_of_anodes_mass == math.ceil(M / 40.0)
        PL = math.sqrt(
            0.15 * 0.0127 * (0.3239 - 0.0127) / (2.0e-7 * 0.3239 * 0.0175 * 0.050)
        )
        assert result.protected_length_m == pytest.approx(PL, rel=1e-12)

    def test_citations_and_provenance(self):
        result = design_bracelet_cp(_a3_input(), edition=EDITION)
        assert result.citations == [
            "dnv-rp-f103 2010 Table 5-1",
            "dnv-rp-f103 2010 Table A.1",
            "dnv-rp-b401 2011 Table 10-8",
            "dnv-rp-b401 2011 Table 10-6",
            "dnv-rp-b401 2011 Table 10-6 with Sec. 5 (structure-to-electrolyte potential criteria)",
        ]
        assert result.edition_used == "2010"
        assert result.standard == "DNV-RP-F103 (October 2010)"
        assert result.provenance == "verified-2010-tables"

    def test_2016_edition_inherits_tables(self):
        r10 = design_bracelet_cp(_a3_input(), edition="2010")
        r16 = design_bracelet_cp(_a3_input(), edition="2016")
        assert r16.number_of_anodes == r10.number_of_anodes
        assert r16.total_net_mass_kg == pytest.approx(r10.total_net_mass_kg)
        assert r16.standard == "DNVGL-RP-F103 (2016)"
        assert r16.provenance == "inherited-2010-unverified"

    def test_missing_edition_warns_and_defaults_to_2010(self):
        with pytest.warns(UserWarning, match="DNV-RP-F103"):
            result = design_bracelet_cp(_a3_input())
        assert result.edition_used == "2010"


class TestFieldJoints:
    def test_bare_field_joints_raise_demand_and_shorten_protected_length(self):
        """2 % bare field joints (Table A.2 'None': a 0.30, b 0.03).

        f_cm,fjc = 0.30 + 0.03 * 12.5 = 0.675; f_cf,fjc = 1.0 (capped).
        I_cm = 0.98 A_lp i f_cm,lp + 0.02 A i 0.675 grows from 6.996 to
        10 175.6 * 0.05 * (0.98 * 0.01375 + 0.02 * 0.675) = 13.72 A.
        """
        result = design_bracelet_cp(
            _a3_input(field_joint_area_fraction=0.02), edition=EDITION
        )
        plain = design_bracelet_cp(_a3_input(), edition=EDITION)
        assert result.field_joint_area_m2 == pytest.approx(0.02 * plain.surface_area_m2)
        assert result.f_cm_field_joint == pytest.approx(0.675)
        assert result.f_cf_field_joint == 1.0
        expected = 10175.6186 * 0.05 * (0.98 * 0.01375 + 0.02 * 0.675)
        assert result.mean_current_demand_A == pytest.approx(expected, rel=1e-6)
        assert result.protected_length_m < plain.protected_length_m
        assert "dnv-rp-f103 2010 Table A.2" in result.citations
        assert "dnv-rp-f103 2010 Table A.2" not in plain.citations

    def test_joint_count_equals_area_fraction(self):
        """820 joints x 0.244 m on 10 km is the same 2 % as the fraction."""
        by_count = design_bracelet_cp(
            _a3_input(field_joint_count=820, field_joint_length_m=0.24390243902439),
            edition=EDITION,
        )
        by_fraction = design_bracelet_cp(
            _a3_input(field_joint_area_fraction=0.02), edition=EDITION
        )
        assert by_count.field_joint_area_m2 == pytest.approx(by_fraction.field_joint_area_m2, rel=1e-9)
        assert by_count.mean_current_demand_A == pytest.approx(by_fraction.mean_current_demand_A, rel=1e-9)

    def test_joint_count_requires_length(self):
        with pytest.raises(ValueError, match="field_joint_length_m"):
            _a3_input(field_joint_count=100)

    def test_fraction_and_count_exclusive(self):
        with pytest.raises(ValueError, match="not both"):
            _a3_input(field_joint_area_fraction=0.02, field_joint_count=10, field_joint_length_m=0.3)

    def test_coated_field_joints(self):
        result = design_bracelet_cp(
            _a3_input(
                field_joint_coating=FieldJointCoating.FJC_3A_FBE,
                field_joint_area_fraction=0.02,
            ),
            edition=EDITION,
        )
        # 3A FBE: a 0.03, b 0.003 -> f_cm 0.0675, f_cf 0.105
        assert result.f_cm_field_joint == pytest.approx(0.0675)
        assert result.f_cf_field_joint == pytest.approx(0.105)


class TestBuriedAndGeometry:
    def test_buried_uses_sediment_capacity_and_lower_density(self):
        """Buried <= 50 °C: i_cm 0.020 A/m2; Al in sediments 1500 Ah/kg."""
        result = design_bracelet_cp(_a3_input(exposure=Exposure.BURIED), edition=EDITION)
        assert result.mean_current_density_A_m2 == pytest.approx(0.020)
        assert result.anode_capacity_Ah_kg == pytest.approx(1500.0)
        # Al in sediments: E_a = -0.95 V -> driving voltage -0.80 - (-0.95) = 0.15 V
        assert result.driving_voltage_V == pytest.approx(0.15)

    def test_explicit_exposed_area_overrides_thickness(self):
        result = design_bracelet_cp(
            _a3_input(bracelet_exposed_area_m2=0.5, bracelet_thickness_m=None),
            edition=EDITION,
        )
        assert result.bracelet_exposed_area_m2 == 0.5
        assert result.anode_resistance_ohm == pytest.approx(0.315 * 0.30 / math.sqrt(0.5))

    def test_bracelet_area_required(self):
        with pytest.raises(ValueError, match="bracelet"):
            _a3_input(bracelet_thickness_m=None)

    def test_wall_thickness_must_fit(self):
        with pytest.raises(ValueError, match="wall_thickness_m"):
            _a3_input(wall_thickness_m=0.2)

    def test_final_case_can_govern(self):
        """400 kg bracelets with a 0.005 m2 exposed face: N_mass = ceil(957.5 / 400)
        = 3; R = 0.315 * 0.30 / sqrt(0.005) = 1.336 ohm, I_a = 0.25 / 1.336 =
        0.187 A, N_final = ceil(8.904 / 0.187) = 48 > 3, so 'final' governs."""
        result = design_bracelet_cp(
            _a3_input(bracelet_net_mass_kg=400.0, bracelet_exposed_area_m2=0.005, bracelet_thickness_m=None),
            edition=EDITION,
        )
        assert result.number_of_anodes_mass == 3
        assert result.number_of_anodes_final == 48
        assert result.number_of_anodes == 48
        assert result.governing_case == "final"
        assert result.current_output_ok


class TestProtectedLength:
    def test_doc_verified_12in_riser(self):
        """D 0.329, WT 0.022, f_cf 0.0042, i_cm 0.250: PL = 3829 m."""
        PL = protected_length(0.15, 0.022, 0.329, STEEL_RESISTIVITY, 0.0042, 0.250, edition=EDITION)
        assert PL == pytest.approx(3829, rel=0.02)

    def test_b401_wrapper_is_the_same_function(self):
        from digitalmodel.cathodic_protection.dnv_rp_b401 import (
            protected_length as b401_protected_length,
        )

        direct = protected_length(0.15, 0.022, 0.329, 2.0e-7, 0.0042, 0.250, edition="2010")
        via_b401 = b401_protected_length(0.15, 0.022, 0.329, 2.0e-7, 0.0042, 0.250, edition="2010")
        assert via_b401 == pytest.approx(direct, rel=1e-12)
        assert b401_protected_length(
            0.15, 0.022, 0.329, 2.0e-7, 0.0042, 0.250, edition="2021"
        ) == pytest.approx(direct, rel=1e-12)

    def test_validation(self):
        with pytest.raises(ValueError, match="WT must be less than D"):
            protected_length(0.15, 0.4, 0.329, 2.0e-7, 0.0042, 0.250, edition=EDITION)
        with pytest.raises(ValueError, match="f_cf"):
            protected_length(0.15, 0.022, 0.329, 2.0e-7, 0.0, 0.250, edition=EDITION)

    def test_package_exports(self):
        import digitalmodel.cathodic_protection as cp

        assert cp.f103_protected_length is protected_length
        assert cp.f103_design_bracelet_cp is design_bracelet_cp
        assert cp.F103BraceletDesignInput is BraceletDesignInput
