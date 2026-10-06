"""Published-value checks for potential criteria and corrosion-rate models (#2213).

One point per model, each against a value derived here from the published
model form (derivation in the docstring), or marked ``xfail`` when the
published value could not be derived confidently from a source in the repo.

The corrosion-rate tests live in this file rather than ``test_corrosion_rate.py``
because that file is owned by PR #2215.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.cathodic_protection.corrosion_rate import (
    CO2CorrosionInput,
    de_waard_milliams_co2,
    norsok_m506_co2,
)
from digitalmodel.cathodic_protection.cp_reporting import (
    ComplianceStatus,
    compliance_check_potential,
)
from digitalmodel.cathodic_protection.iso_15589_2 import check_protection_potential


class TestIso15589_1Table1Potentials:
    """ISO 15589-1:2015 Table 1 protection potentials vs Cu/CuSO4 (CSE).

    The package has no ISO 15589-1 module; the criteria are exercised through
    ``cp_reporting.compliance_check_potential`` (criterion + 20 mV marginal
    band, NACE SP0169 wording) with the Table 1 values as the criterion:

    - aerobic soil / water, normal conditions: -0.85 V
    - anaerobic (sulphate-reducing bacteria) conditions: -0.95 V
    - aerobic, soil resistivity 100-1000 ohm-m: -0.75 V
    - aerobic, soil resistivity > 1000 ohm-m: -0.65 V
    """

    TABLE_1 = [
        pytest.param(-0.85, id="aerobic_normal"),
        pytest.param(-0.95, id="anaerobic_srb"),
        pytest.param(-0.75, id="resistivity_100_to_1000_ohm_m"),
        pytest.param(-0.65, id="resistivity_above_1000_ohm_m"),
    ]

    @pytest.mark.parametrize("criterion", TABLE_1)
    def test_potential_50_mv_more_negative_is_compliant(self, criterion):
        check = compliance_check_potential(criterion - 0.05, criterion_V=criterion)
        assert check.status is ComplianceStatus.COMPLIANT
        assert check.criterion_value == pytest.approx(criterion)
        assert check.unit == "V vs CSE"

    @pytest.mark.parametrize("criterion", TABLE_1)
    def test_potential_at_criterion_is_compliant(self, criterion):
        assert compliance_check_potential(criterion, criterion_V=criterion).status is ComplianceStatus.COMPLIANT

    @pytest.mark.parametrize("criterion", TABLE_1)
    def test_potential_10_mv_short_is_marginal(self, criterion):
        assert (
            compliance_check_potential(criterion + 0.01, criterion_V=criterion).status
            is ComplianceStatus.MARGINAL
        )

    @pytest.mark.parametrize("criterion", TABLE_1)
    def test_potential_50_mv_short_is_non_compliant(self, criterion):
        assert (
            compliance_check_potential(criterion + 0.05, criterion_V=criterion).status
            is ComplianceStatus.NON_COMPLIANT
        )

    def test_default_criterion_is_the_aerobic_value(self):
        """The -0.85 V CSE default equals ISO 15589-1 Table 1, aerobic normal."""
        assert compliance_check_potential(-0.90).criterion_value == pytest.approx(-0.85)

    def test_iso_15589_2_offshore_window(self):
        """ISO 15589-2 (offshore) window -0.80 to -1.10 V vs Ag/AgCl/seawater.

        -0.80 V vs Ag/AgCl/seawater is the -0.85 V vs CSE aerobic criterion
        of ISO 15589-1 Table 1 expressed against the seawater electrode
        (NACE SP0169 lists the pair -0.850 CSE / -0.800 Ag/AgCl seawater).
        """
        assert check_protection_potential(-0.80)["pass"]
        assert check_protection_potential(-1.10)["pass"]
        assert not check_protection_potential(-0.79)["pass"]
        assert not check_protection_potential(-1.11)["pass"]
        window = check_protection_potential(-0.90)
        assert window["criterion_min"] == pytest.approx(-0.80)
        assert window["criterion_max"] == pytest.approx(-1.10)


class TestDeWaardMilliams1975:
    def test_60c_1bar_co2_from_the_published_equation(self):
        """de Waard & Milliams (1975): log10 V = 5.8 - 1710 / T[K] + 0.67 log10 pCO2.

        At 60 degC (333.15 K), pCO2 = 1 bar (log10 = 0) and pH 4.0 (below the
        module's pH-correction threshold, and no fugacity correction below
        10 bar):
            log10 V = 5.8 - 1710 / 333.15 = 5.8 - 5.13282 = 0.66718
            V = 10^0.66718 = 4.647 mm/yr.
        The published nomogram reads ~4.6-4.7 mm/yr at this point (the paper
        uses T = t + 273, which gives 4.62 mm/yr; the module uses 273.15).
        """
        result = de_waard_milliams_co2(
            CO2CorrosionInput(temperature_c=60.0, co2_partial_pressure_bar=1.0, ph=4.0)
        )
        expected = 10.0 ** (5.8 - 1710.0 / 333.15)
        assert expected == pytest.approx(4.647, abs=0.001)
        assert result.corrosion_rate_mm_yr == pytest.approx(expected, abs=5e-4)
        assert result.model_used == "de_Waard_Milliams_1975"
        assert result.fugacity_correction == 1.0

    def test_pco2_exponent_0_67(self):
        """Tenfold pCO2 multiplies the rate by 10^0.67 = 4.677 (pH 4, < 10 bar)."""
        r1 = de_waard_milliams_co2(CO2CorrosionInput(temperature_c=40.0, co2_partial_pressure_bar=0.5, ph=4.0))
        r10 = de_waard_milliams_co2(CO2CorrosionInput(temperature_c=40.0, co2_partial_pressure_bar=5.0, ph=4.0))
        assert r10.corrosion_rate_mm_yr / r1.corrosion_rate_mm_yr == pytest.approx(10.0**0.67, rel=2e-3)


class TestNorsokM506:
    @pytest.mark.xfail(strict=False, reason="published value pending source PDF")
    def test_20c_1bar_ph4_6_19pa_published_point(self):
        """NORSOK M-506 rev. 2 (2005): CR = K_t f_CO2^0.62 (S/19)^(0.146 + 0.0324 log f_CO2) f(pH)_t.

        Candidate published point (unverified, from memory of the M-506
        K_t table, K_t(20 degC) = 4.762): f_CO2 = 1 bar, S = 19 Pa, pH 4.6
        (where the tabulated f(pH) is ~1.0) gives CR ~ 4.8 mm/yr. The module's
        ``norsok_m506_co2`` is a documented simplification (linear K_t ramp
        0.42 + 0.066 (T - 15), shear normalised at 10 Pa, its own pH ramp)
        and returns ~0.8 mm/yr here; the comparison stays xfail until the
        M-506 tables are read from the source PDF and either the model or
        this expected value is corrected.
        """
        result = norsok_m506_co2(
            temperature_c=20.0, co2_partial_pressure_bar=1.0, ph=4.6, wall_shear_stress_Pa=19.0
        )
        assert result.model_used == "NORSOK_M506_simplified"
        assert result.corrosion_rate_mm_yr == pytest.approx(4.762, rel=0.05)

    def test_pco2_exponent_0_62_is_the_published_one(self):
        """The one published M-506 coefficient the module reproduces: f_CO2^0.62.

        Tenfold pCO2 at fixed T, pH and shear multiplies the rate by 10^0.62
        = 4.169 (the module has no fugacity correction).
        """
        r1 = norsok_m506_co2(40.0, 0.5, ph=4.5, wall_shear_stress_Pa=10.0)
        r10 = norsok_m506_co2(40.0, 5.0, ph=4.5, wall_shear_stress_Pa=10.0)
        assert r10.corrosion_rate_mm_yr / r1.corrosion_rate_mm_yr == pytest.approx(
            math.pow(10.0, 0.62), rel=2e-3
        )
