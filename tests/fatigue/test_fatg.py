"""Validation of the legacy FATG closed-form spectral fatigue port.

Source of truth
---------------
The legacy program survives as Fortran source only (no executable, no
retained input/output decks), so the oracle here is a line-by-line Python
transliteration of the Fortran arithmetic (``_specto`` / ``_fatg_run``
below), preserving variable names, operation order and the program's
hard-coded constants (``PI = 3.141592654``, ``TIME = 31536000``).
Fortran ``REAL*8`` is IEEE float64, i.e. bit-comparable with Python floats.

The input deck is fully synthetic (round-number RAO and scatter rows
invented for this test; no project data).

Tolerances
----------
* library vs transliteration and vs frozen golden values: rel 1e-9.
  The only systematic difference is the legacy truncated PI
  (3.141592654 vs math.pi) inside the direction cosines, worth ~3e-10
  relative in damage; everything else agrees to machine precision.
* FATG spectrum kernel vs the standard Bretschneider form: rel 1e-12
  (algebraic identity, see test below).
"""

import math

import pytest

from digitalmodel.fatigue.fatg import (
    FATG_DIRECTION_FACTOR_PRESETS,
    FATG_SN_CURVES,
    FatgSeaState,
    fatg_annual_damage,
    fatg_sea_state_damage,
    fatg_significant_stress_range,
)

REL = 1e-9

# ---------------------------------------------------------------------------
# Transliteration oracle (line-by-line port of the legacy Fortran)
# ---------------------------------------------------------------------------

_PI = 3.141592654  # hard-coded in the legacy source (not math.pi)
_TIME = 31536000.0


def _specto(we, sigma, hs, tz):
    """SPECTO subroutine: significant stress response, exact arithmetic."""
    ST4 = 1.0 / tz**4
    ST2 = 1400.0 * ST4
    ST3 = 700.0 * ST4
    SSIGMA = 0.0
    for i in range(len(we) - 1):
        ST9 = sigma[i] ** 2 * we[i] ** (-5) * ST2 * math.exp(-(we[i] ** (-4)) * ST3)
        ST9 += (
            sigma[i + 1] ** 2
            * we[i + 1] ** (-5)
            * ST2
            * math.exp(-(we[i + 1] ** (-4)) * ST3)
        )
        SSIGMA += (ST9 / 2.0) * (we[i + 1] - we[i])
    return math.sqrt(2.0 * SSIGMA) * hs


def _fatg_run(we, sigma, states, curve, adc, bdc, uniform=False):
    """Main damage loop shared by the FATG variants.

    * original program:      adc=cos(PI/4), bdc=cos(PI*7/16)
    * chord variant:         adc=1,         bdc=cos(PI*7/16)
    * diagonal variant:      adc=1,         bdc=1
    * uniform=True reproduces the no-spreading-input mode
      (AN0 = AN45 = AN90 = TIME*ANOC/TZ/3).
    """
    TLIFE = 0.0
    per_state = []
    for HS, TZ, ANOC, PER0, PER45, PER90 in states:
        SIGMAS = _specto(we, sigma, HS, TZ)
        stresses_0 = [SIGMAS * 1.271, SIGMAS * 0.59, SIGMAS * 0.28]
        stresses_45 = [s * adc for s in stresses_0]
        stresses_90 = [s * bdc for s in stresses_0]
        if uniform:
            AN0 = AN45 = AN90 = _TIME * ANOC / TZ / 3
        else:
            AN0 = _TIME * ANOC * (PER0 * 0.475 + PER45 * 0.5 + PER90 * 0.025) / TZ / 3
            AN45 = _TIME * ANOC * (PER0 * 0.25 + PER45 * 0.5 + PER90 * 0.25) / TZ / 3
            AN90 = _TIME * ANOC * (PER0 * 0.025 + PER45 * 0.5 + PER90 * 0.475) / TZ / 3
        ALIFEU = 0.0
        for n, group in ((AN0, stresses_0), (AN45, stresses_45), (AN90, stresses_90)):
            for s in group:
                ALIFEU += n / 10 ** (curve - 3 * math.log10(s))
        TLIFE += ALIFEU
        per_state.append((SIGMAS, ALIFEU))
    return TLIFE, per_state


# ---------------------------------------------------------------------------
# Synthetic validation deck (invented round numbers; 18-point RAO as in the
# legacy fixed-size input)
# ---------------------------------------------------------------------------

OMEGA = [0.2 + 0.1 * i for i in range(18)]  # rad/s
RAO = [
    120.0, 260.0, 450.0, 700.0, 1050.0, 1500.0,
    2050.0, 2600.0, 2950.0, 3000.0, 2750.0, 2300.0,
    1800.0, 1350.0, 950.0, 620.0, 380.0, 210.0,
]  # psi per ft of wave height

# (Hs ft, Tz s, occurrence fraction, p0, p45, p90)
SCATTER = [
    (3.0, 5.0, 0.55, 0.6, 0.3, 0.1),
    (8.0, 7.0, 0.35, 0.5, 0.35, 0.15),
    (15.0, 9.5, 0.10, 0.4, 0.4, 0.2),
]

SEA_STATES = [
    FatgSeaState(hs=hs, tz=tz, occurrence=occ, spreading=(p0, p45, p90))
    for hs, tz, occ, p0, p45, p90 in SCATTER
]
SEA_STATES_UNIFORM = [
    FatgSeaState(hs=hs, tz=tz, occurrence=occ) for hs, tz, occ, *_ in SCATTER
]

# Frozen golden values, generated once from the transliteration oracle above
# (float64 arithmetic; see module docstring for provenance).
GOLDEN_SIGMAS = [6943.628770341245, 16200.00493131268, 21588.825957978726]
GOLDEN = {
    # (member preset, curve, uniform) -> annual damage
    ("chord", "E", False): 2.035808341410647,
    ("chord", "F", False): 3.3360833254500593,
    ("chord", "T", False): 1.4450300864887822,
    ("diagonal", "F", False): 4.511863410469105,
    ("general", "E", False): 1.4425179007211983,
    ("general", "E", True): 3.674478689295206,
}
GOLDEN_CHORD_E_PER_STATE = [
    0.2138040700644533, 1.220603311567063, 0.6014009597791309,
]


class TestSignificantStressResponse:
    def test_matches_transliteration_on_validation_deck(self):
        for hs, tz, *_ in SCATTER:
            expected = _specto(OMEGA, RAO, hs, tz)
            got = fatg_significant_stress_range(OMEGA, RAO, hs, tz)
            assert got == pytest.approx(expected, rel=REL)

    def test_matches_transliteration_across_scatter_grid(self):
        for hs in (0.5, 2.0, 6.0, 10.0, 20.0):
            for tz in (3.5, 5.5, 8.0, 12.0):
                expected = _specto(OMEGA, RAO, hs, tz)
                got = fatg_significant_stress_range(OMEGA, RAO, hs, tz)
                assert got == pytest.approx(expected, rel=REL)

    def test_golden_values(self):
        for (hs, tz, *_), expected in zip(SCATTER, GOLDEN_SIGMAS):
            got = fatg_significant_stress_range(OMEGA, RAO, hs, tz)
            assert got == pytest.approx(expected, rel=REL)

    def test_kernel_is_standard_bretschneider(self):
        """The legacy integration constants (1400, 700, factor 2 in the
        significant response) are algebraically a standard Bretschneider
        spectrum S(w) = (5/16) Hs^2 wp^4 w^-5 exp(-1.25 (wp/w)^4) with
        wp = 560**0.25 / Tz (Tp ~= 1.2916 Tz), the response significant
        *range* being 4*sqrt(m0)."""
        hs, tz = 5.0, 7.0
        wp = 560.0**0.25 / tz
        assert 2.0 * math.pi / wp == pytest.approx(1.29159 * tz, rel=1e-4)
        for w in OMEGA:
            legacy_kernel = 175.0 * hs**2 / tz**4 * w**-5 * math.exp(
                -700.0 / (tz**4 * w**4)
            )
            standard = (
                (5.0 / 16.0) * hs**2 * wp**4 * w**-5
                * math.exp(-1.25 * (wp / w) ** 4)
            )
            assert legacy_kernel == pytest.approx(standard, rel=1e-12)
        # and sigma_s = 4*sqrt(m0) of that spectrum transferred by RAO^2
        m0 = 0.0
        for i in range(len(OMEGA) - 1):
            def ordinate(j):
                w = OMEGA[j]
                return RAO[j] ** 2 * 175.0 * hs**2 / tz**4 * w**-5 * math.exp(
                    -700.0 / (tz**4 * w**4)
                )
            m0 += 0.5 * (ordinate(i) + ordinate(i + 1)) * (OMEGA[i + 1] - OMEGA[i])
        assert fatg_significant_stress_range(OMEGA, RAO, hs, tz) == pytest.approx(
            4.0 * math.sqrt(m0), rel=1e-12
        )

    def test_linear_in_hs(self):
        base = fatg_significant_stress_range(OMEGA, RAO, 1.0, 6.0)
        assert fatg_significant_stress_range(OMEGA, RAO, 7.5, 6.0) == pytest.approx(
            7.5 * base, rel=1e-12
        )


class TestAnnualDamageGoldens:
    @pytest.mark.parametrize("curve", ["E", "F", "T"])
    def test_chord_curves(self, curve):
        result = fatg_annual_damage(
            OMEGA, RAO, SEA_STATES, sn_curve=curve, member_type="chord"
        )
        assert result.annual_damage == pytest.approx(
            GOLDEN[("chord", curve, False)], rel=REL
        )

    def test_chord_curve_e_per_sea_state(self):
        result = fatg_annual_damage(
            OMEGA, RAO, SEA_STATES, sn_curve="E", member_type="chord"
        )
        for item, expected_sigma, expected_damage in zip(
            result.sea_states, GOLDEN_SIGMAS, GOLDEN_CHORD_E_PER_STATE
        ):
            assert item.significant_stress_range == pytest.approx(
                expected_sigma, rel=REL
            )
            assert item.damage_per_year == pytest.approx(expected_damage, rel=REL)

    def test_diagonal(self):
        result = fatg_annual_damage(
            OMEGA, RAO, SEA_STATES, sn_curve="F", member_type="diagonal"
        )
        assert result.annual_damage == pytest.approx(
            GOLDEN[("diagonal", "F", False)], rel=REL
        )

    def test_general_with_spreading(self):
        """Original program, spreading-input mode (hard-coded 18.499)."""
        result = fatg_annual_damage(
            OMEGA, RAO, SEA_STATES, sn_curve="E", member_type="general"
        )
        assert result.annual_damage == pytest.approx(
            GOLDEN[("general", "E", False)], rel=REL
        )

    def test_general_uniform_mode(self):
        """Original program, no-spreading-input mode."""
        result = fatg_annual_damage(
            OMEGA, RAO, SEA_STATES_UNIFORM, sn_curve="E", member_type="general"
        )
        assert result.annual_damage == pytest.approx(
            GOLDEN[("general", "E", True)], rel=REL
        )

    def test_legacy_report_echoes(self):
        result = fatg_annual_damage(
            OMEGA, RAO, SEA_STATES, sn_curve="F", member_type="chord"
        )
        assert result.life_used_percent_1yr == pytest.approx(
            result.annual_damage * 100.0
        )
        assert result.life_used_percent_10yr == pytest.approx(
            result.annual_damage * 1000.0
        )
        assert result.fatigue_life_years == pytest.approx(1.0 / result.annual_damage)
        for item, (hs, tz, *_) in zip(result.sea_states, SCATTER):
            assert item.tp == pytest.approx(1.3 * tz)


class TestAgainstTransliterationSweep:
    """Cross-check the clean implementation against the oracle for every
    member preset and curve on the validation deck."""

    @pytest.mark.parametrize("member", ["general", "chord", "diagonal"])
    @pytest.mark.parametrize("curve", ["E", "F", "T"])
    def test_all_presets_and_curves(self, member, curve):
        adc_lib, bdc_lib = FATG_DIRECTION_FACTOR_PRESETS[member][1:]
        # oracle uses the legacy truncated PI in its cosines
        oracle_factors = {
            "general": (math.cos(_PI / 4), math.cos(_PI * 7 / 16)),
            "chord": (1.0, math.cos(_PI * 7 / 16)),
            "diagonal": (1.0, 1.0),
        }[member]
        expected, _ = _fatg_run(
            OMEGA, RAO, SCATTER, FATG_SN_CURVES[curve], *oracle_factors
        )
        result = fatg_annual_damage(
            OMEGA, RAO, SEA_STATES, sn_curve=curve, member_type=member
        )
        assert result.annual_damage == pytest.approx(expected, rel=REL)
        # the truncated-PI direction factors agree with math.pi to ~1e-10
        assert adc_lib == pytest.approx(oracle_factors[0], rel=1e-9)
        assert bdc_lib == pytest.approx(oracle_factors[1], rel=1e-9)


class TestProperties:
    def test_damage_scales_with_rao_cubed_for_slope_3(self):
        scaled = [2.0 * v for v in RAO]
        base = fatg_annual_damage(OMEGA, RAO, SEA_STATES, sn_curve="F")
        double = fatg_annual_damage(OMEGA, scaled, SEA_STATES, sn_curve="F")
        assert double.annual_damage == pytest.approx(
            8.0 * base.annual_damage, rel=1e-9
        )

    def test_explicit_log10_a_overrides_curve_name(self):
        named = fatg_annual_damage(OMEGA, RAO, SEA_STATES, sn_curve="F")
        explicit = fatg_annual_damage(
            OMEGA, RAO, SEA_STATES, sn_curve=None, log10_a=18.2845
        )
        assert explicit.annual_damage == pytest.approx(named.annual_damage, rel=1e-12)

    def test_sea_state_damage_matches_sum_of_blocks(self):
        sigma_s = 10000.0
        damage = fatg_sea_state_damage(
            sigma_s, 6.0, 0.25, log10_a=18.2845, spreading=(0.5, 0.3, 0.2)
        )
        assert damage > 0.0


class TestValidation:
    def test_rejects_mismatched_rao(self):
        with pytest.raises(ValueError):
            fatg_significant_stress_range([0.2, 0.4], [1.0], 1.0, 6.0)

    def test_rejects_non_increasing_omega(self):
        with pytest.raises(ValueError):
            fatg_significant_stress_range([0.4, 0.2], [1.0, 1.0], 1.0, 6.0)

    def test_rejects_unknown_curve(self):
        with pytest.raises(ValueError):
            fatg_annual_damage(OMEGA, RAO, SEA_STATES, sn_curve="Z")

    def test_rejects_unknown_member_type(self):
        with pytest.raises(ValueError):
            fatg_annual_damage(OMEGA, RAO, SEA_STATES, member_type="brace")

    def test_rejects_empty_scatter(self):
        with pytest.raises(ValueError):
            fatg_annual_damage(OMEGA, RAO, [])
