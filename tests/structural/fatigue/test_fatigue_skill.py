"""
Tests for the fatigue assessment agent-callable skill.

Covers:
- FatigueAssessmentInput default values
- FatigueAssessmentResult field presence and types
- Correct Miner's rule damage computation
- Utilisation ratio == damage
- Life years relationship
- Endurance-only (sub-fatigue-limit) input → zero damage
- Various valid DNV S-N curves
- API and BS design codes
- Invalid sn_curve_name raises ValueError
- Invalid design_code raises ValueError
- Empty stress_ranges raises ValueError
- Mismatched list lengths raise ValueError
- Governing load case identification
- damage_per_block length matches input length
- Source field is always "skill:fatigue_assessment"
- SKILL_NAME constant
"""

import math
import pytest
import numpy as np

from digitalmodel.structural.fatigue.skill import (
    SKILL_NAME,
    FatigueAssessmentInput,
    FatigueAssessmentResult,
    fatigue_assessment,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _simple_input(**overrides) -> FatigueAssessmentInput:
    """Return a minimal valid FatigueAssessmentInput with sensible defaults."""
    defaults = dict(
        stress_ranges=[100.0, 60.0, 40.0],
        cycle_counts=[1000.0, 10000.0, 100000.0],
        sn_curve_name="D",
        design_code="DNV",
        output_formats=["summary"],
    )
    defaults.update(overrides)
    return FatigueAssessmentInput(**defaults)


# ---------------------------------------------------------------------------
# 1. Skill identity
# ---------------------------------------------------------------------------

class TestSkillIdentity:
    def test_skill_name_constant(self):
        assert SKILL_NAME == "fatigue_assessment"

    def test_skill_name_is_string(self):
        assert isinstance(SKILL_NAME, str)


# ---------------------------------------------------------------------------
# 2. FatigueAssessmentInput defaults
# ---------------------------------------------------------------------------

class TestFatigueAssessmentInputDefaults:
    def test_default_sn_curve_name(self):
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0], cycle_counts=[1000.0]
        )
        assert inp.sn_curve_name == "D"

    def test_default_design_code(self):
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0], cycle_counts=[1000.0]
        )
        assert inp.design_code == "DNV"

    def test_default_output_formats(self):
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0], cycle_counts=[1000.0]
        )
        assert inp.output_formats == ["summary"]

    def test_custom_sn_curve_name(self):
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0], cycle_counts=[1000.0], sn_curve_name="E"
        )
        assert inp.sn_curve_name == "E"

    def test_custom_design_code(self):
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0], cycle_counts=[1000.0], design_code="API"
        )
        assert inp.design_code == "API"


# ---------------------------------------------------------------------------
# 3. Basic result structure
# ---------------------------------------------------------------------------

class TestResultStructure:
    def test_returns_fatigue_assessment_result(self):
        result = fatigue_assessment(_simple_input())
        assert isinstance(result, FatigueAssessmentResult)

    def test_source_field(self):
        result = fatigue_assessment(_simple_input())
        assert result.source == "skill:fatigue_assessment"

    def test_utilisation_ratio_is_float(self):
        result = fatigue_assessment(_simple_input())
        assert isinstance(result.utilisation_ratio, float)

    def test_life_years_is_float(self):
        result = fatigue_assessment(_simple_input())
        assert isinstance(result.life_years, float)

    def test_governing_load_case_is_str(self):
        result = fatigue_assessment(_simple_input())
        assert isinstance(result.governing_load_case, str)

    def test_sn_curve_used_is_str(self):
        result = fatigue_assessment(_simple_input())
        assert isinstance(result.sn_curve_used, str)

    def test_damage_per_block_is_list(self):
        result = fatigue_assessment(_simple_input())
        assert isinstance(result.damage_per_block, list)

    def test_summary_is_dict(self):
        result = fatigue_assessment(_simple_input())
        assert isinstance(result.summary, dict)

    def test_summary_contains_total_damage(self):
        result = fatigue_assessment(_simple_input())
        assert "total_damage" in result.summary

    def test_summary_contains_life_years(self):
        result = fatigue_assessment(_simple_input())
        assert "life_years" in result.summary

    def test_summary_contains_sn_curve(self):
        result = fatigue_assessment(_simple_input())
        assert "sn_curve" in result.summary


# ---------------------------------------------------------------------------
# 4. Miner's rule correctness
# ---------------------------------------------------------------------------

class TestMinersRule:
    def test_utilisation_equals_damage(self):
        """utilisation_ratio must equal Miner's damage sum."""
        result = fatigue_assessment(_simple_input())
        assert result.utilisation_ratio == pytest.approx(
            result.summary["total_damage"], rel=1e-9
        )

    def test_life_years_inverse_of_damage(self):
        """life_years == 1 / total_damage when damage > 0."""
        result = fatigue_assessment(_simple_input())
        d = result.utilisation_ratio
        if d > 0:
            assert result.life_years == pytest.approx(1.0 / d, rel=1e-6)

    def test_life_years_positive(self):
        result = fatigue_assessment(_simple_input())
        assert result.life_years > 0

    def test_damage_positive_for_typical_offshore_input(self):
        """Stress ranges above fatigue limit produce non-zero damage."""
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0, 80.0, 60.0],
            cycle_counts=[5000.0, 20000.0, 80000.0],
        )
        result = fatigue_assessment(inp)
        assert result.utilisation_ratio > 0.0

    def test_single_constant_amplitude_damage(self):
        """
        For one stress range S with N cycles:
        N_allowable = A * S^(-m) (DNV-D: A=5.73e11, m=3)
        damage = N / N_allowable
        """
        S = 100.0
        n = 10000.0
        A = 5.73e11
        m = 3.0
        N_allow = A * (S ** (-m))
        expected_damage = n / N_allow

        inp = FatigueAssessmentInput(
            stress_ranges=[S],
            cycle_counts=[n],
        )
        result = fatigue_assessment(inp)
        assert result.utilisation_ratio == pytest.approx(expected_damage, rel=1e-6)

    def test_damage_per_block_length_matches_input(self):
        inp = _simple_input()
        result = fatigue_assessment(inp)
        assert len(result.damage_per_block) == len(inp.stress_ranges)

    def test_damage_per_block_sums_to_total_damage(self):
        result = fatigue_assessment(_simple_input())
        assert sum(result.damage_per_block) == pytest.approx(
            result.utilisation_ratio, rel=1e-6
        )

    def test_damage_per_block_all_non_negative(self):
        result = fatigue_assessment(_simple_input())
        for d in result.damage_per_block:
            assert d >= 0.0


# ---------------------------------------------------------------------------
# 5. Below endurance limit → zero damage
# ---------------------------------------------------------------------------

class TestEnduranceLimit:
    def test_stress_below_fatigue_limit_gives_zero_damage(self):
        """
        DNV-D fatigue limit ~ 52.63 MPa. Stress ranges below this give
        infinite N_allowable → zero damage contribution.
        """
        inp = FatigueAssessmentInput(
            stress_ranges=[10.0, 20.0, 30.0],
            cycle_counts=[1e6, 1e6, 1e6],
        )
        result = fatigue_assessment(inp)
        assert result.utilisation_ratio == pytest.approx(0.0, abs=1e-12)

    def test_life_years_infinite_when_damage_zero(self):
        inp = FatigueAssessmentInput(
            stress_ranges=[10.0],
            cycle_counts=[1e6],
        )
        result = fatigue_assessment(inp)
        assert math.isinf(result.life_years)

    def test_mixed_below_and_above_limit(self):
        """Only bins above the fatigue limit contribute to damage."""
        inp_above_only = FatigueAssessmentInput(
            stress_ranges=[100.0],
            cycle_counts=[1000.0],
        )
        inp_mixed = FatigueAssessmentInput(
            stress_ranges=[100.0, 20.0],
            cycle_counts=[1000.0, 1e6],
        )
        r1 = fatigue_assessment(inp_above_only)
        r2 = fatigue_assessment(inp_mixed)
        # The 20 MPa bin (below CAFL) should add zero damage
        assert r1.utilisation_ratio == pytest.approx(r2.utilisation_ratio, rel=1e-9)


# ---------------------------------------------------------------------------
# 6. Various DNV S-N curves
# ---------------------------------------------------------------------------

class TestDNVCurves:
    @pytest.mark.parametrize("curve_class", ["D", "E", "F", "F1", "F3", "G", "W1"])
    def test_dnv_curve_runs_without_error(self, curve_class):
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0, 60.0],
            cycle_counts=[5000.0, 50000.0],
            sn_curve_name=curve_class,
            design_code="DNV",
        )
        result = fatigue_assessment(inp)
        assert result.utilisation_ratio >= 0.0

    @pytest.mark.parametrize("curve_class", ["D", "E", "F"])
    def test_lower_class_gives_higher_damage(self, curve_class):
        """
        Weaker curves (G < F3 < F1 < F < E < D) should give higher damage
        for the same applied cycles.  Test that D < E < F in terms of damage.
        """
        # We just test relative ordering between neighbouring classes
        inp_d = FatigueAssessmentInput(
            stress_ranges=[100.0],
            cycle_counts=[1000.0],
            sn_curve_name="D",
        )
        inp_f = FatigueAssessmentInput(
            stress_ranges=[100.0],
            cycle_counts=[1000.0],
            sn_curve_name="F",
        )
        r_d = fatigue_assessment(inp_d)
        r_f = fatigue_assessment(inp_f)
        # DNV-F has lower A constant → fewer allowable cycles → higher damage
        assert r_f.utilisation_ratio > r_d.utilisation_ratio

    def test_sn_curve_used_label_contains_standard(self):
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0], cycle_counts=[1000.0], sn_curve_name="E"
        )
        result = fatigue_assessment(inp)
        assert "DNV" in result.sn_curve_used

    def test_sn_curve_used_label_contains_class(self):
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0], cycle_counts=[1000.0], sn_curve_name="E"
        )
        result = fatigue_assessment(inp)
        assert "E" in result.sn_curve_used


# ---------------------------------------------------------------------------
# 7. API and BS design codes
# ---------------------------------------------------------------------------

class TestDesignCodes:
    def test_api_design_code(self):
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0, 60.0],
            cycle_counts=[5000.0, 50000.0],
            design_code="API",
            sn_curve_name="X",
        )
        result = fatigue_assessment(inp)
        assert result.utilisation_ratio >= 0.0
        assert "API" in result.sn_curve_used

    def test_bs_design_code(self):
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0, 60.0],
            cycle_counts=[5000.0, 50000.0],
            design_code="BS",
            sn_curve_name="D",
        )
        result = fatigue_assessment(inp)
        assert result.utilisation_ratio >= 0.0
        assert "BS" in result.sn_curve_used

    def test_api_case_insensitive(self):
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0],
            cycle_counts=[1000.0],
            design_code="api",
            sn_curve_name="X",
        )
        result = fatigue_assessment(inp)
        assert result.utilisation_ratio >= 0.0

    def test_bs_case_insensitive(self):
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0],
            cycle_counts=[1000.0],
            design_code="bs",
            sn_curve_name="D",
        )
        result = fatigue_assessment(inp)
        assert result.utilisation_ratio >= 0.0

    def test_dnv_case_insensitive(self):
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0],
            cycle_counts=[1000.0],
            design_code="dnv",
            sn_curve_name="D",
        )
        result = fatigue_assessment(inp)
        assert result.utilisation_ratio >= 0.0


# ---------------------------------------------------------------------------
# 8. Validation errors
# ---------------------------------------------------------------------------

class TestValidationErrors:
    def test_empty_stress_ranges_raises(self):
        with pytest.raises(ValueError, match="empty"):
            fatigue_assessment(
                FatigueAssessmentInput(stress_ranges=[], cycle_counts=[])
            )

    def test_mismatched_lengths_raises(self):
        with pytest.raises(ValueError, match="same length"):
            fatigue_assessment(
                FatigueAssessmentInput(
                    stress_ranges=[100.0, 80.0],
                    cycle_counts=[1000.0],
                )
            )

    def test_invalid_sn_curve_name_raises(self):
        with pytest.raises(ValueError):
            fatigue_assessment(
                FatigueAssessmentInput(
                    stress_ranges=[100.0],
                    cycle_counts=[1000.0],
                    sn_curve_name="ZZZ",
                )
            )

    def test_invalid_design_code_raises(self):
        with pytest.raises(ValueError):
            fatigue_assessment(
                FatigueAssessmentInput(
                    stress_ranges=[100.0],
                    cycle_counts=[1000.0],
                    design_code="ISO",
                )
            )

    def test_sn_curve_valid_for_wrong_standard_raises(self):
        """F1 is a DNV class not present in API — should raise ValueError."""
        with pytest.raises(ValueError):
            fatigue_assessment(
                FatigueAssessmentInput(
                    stress_ranges=[100.0],
                    cycle_counts=[1000.0],
                    design_code="API",
                    sn_curve_name="F1",
                )
            )


# ---------------------------------------------------------------------------
# 9. Governing load case
# ---------------------------------------------------------------------------

class TestGoverningLoadCase:
    def test_governing_load_case_contains_mpa(self):
        result = fatigue_assessment(_simple_input())
        assert "MPa" in result.governing_load_case

    def test_governing_load_case_is_highest_damage_bin(self):
        """The 100 MPa bin produces far more damage per cycle than 40 MPa."""
        inp = FatigueAssessmentInput(
            stress_ranges=[100.0, 40.0],
            cycle_counts=[5000.0, 5000.0],
        )
        result = fatigue_assessment(inp)
        # Governing bin should be 100 MPa (highest damage contribution)
        assert "100.0" in result.governing_load_case


# ---------------------------------------------------------------------------
# 10. Offshore-realistic scenario
# ---------------------------------------------------------------------------

class TestOffshoreScenario:
    def test_offshore_riser_life_reasonable(self):
        """
        Simplified SCR fatigue histogram — result should give a finite life.
        """
        stress_ranges = [150.0, 100.0, 70.0, 50.0, 30.0]
        cycle_counts = [500.0, 5000.0, 50000.0, 200000.0, 1000000.0]
        inp = FatigueAssessmentInput(
            stress_ranges=stress_ranges,
            cycle_counts=cycle_counts,
            sn_curve_name="F",
            design_code="DNV",
        )
        result = fatigue_assessment(inp)
        # Damage should be positive and finite
        assert 0.0 < result.utilisation_ratio < 1e6
        assert math.isfinite(result.life_years)
        assert result.life_years > 0.0

    def test_high_cycle_low_stress_safe_result(self):
        """Many cycles but all below fatigue limit → infinite life."""
        inp = FatigueAssessmentInput(
            stress_ranges=[5.0, 10.0, 15.0],
            cycle_counts=[1e8, 1e8, 1e8],
            sn_curve_name="D",
            design_code="DNV",
        )
        result = fatigue_assessment(inp)
        assert result.utilisation_ratio == pytest.approx(0.0, abs=1e-12)
        assert math.isinf(result.life_years)
