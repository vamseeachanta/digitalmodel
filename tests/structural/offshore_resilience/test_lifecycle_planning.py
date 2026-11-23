"""
Tests for lifecycle planning: decommissioning-by-design flags,
tieback accommodation scoring, and the marginal field lifecycle score.
"""

import pytest

from digitalmodel.structural.offshore_resilience.lifecycle_planning import (
    DecomFlag,
    DecomFlagSeverity,
    PipelineLayout,
    decom_flags_for_design_choices,
    score_tieback_accommodation,
    marginal_field_lifecycle_score,
)


# ---------------------------------------------------------------------------
# Decommissioning-by-design flags
# ---------------------------------------------------------------------------

class TestDecomFlagsForDesignChoices:
    def test_grouted_connections_flagged(self):
        """Grouted pile/conductor connections raise a decom concern."""
        flags = decom_flags_for_design_choices(
            has_grouted_connections=True,
            pile_removal_strategy="cut",
            jacket_pieces=1,
            has_mechanical_connectors=False,
        )
        categories = [f.category for f in flags]
        assert "grouted_connections" in categories

    def test_no_grouted_connections_not_flagged(self):
        flags = decom_flags_for_design_choices(
            has_grouted_connections=False,
            pile_removal_strategy="cut",
            jacket_pieces=2,
            has_mechanical_connectors=True,
        )
        categories = [f.category for f in flags]
        assert "grouted_connections" not in categories

    def test_leave_in_situ_piles_flagged(self):
        """Piles left in situ trigger a regulatory complexity flag."""
        flags = decom_flags_for_design_choices(
            has_grouted_connections=False,
            pile_removal_strategy="leave_in_situ",
            jacket_pieces=2,
            has_mechanical_connectors=True,
        )
        categories = [f.category for f in flags]
        assert "pile_removal" in categories

    def test_single_piece_jacket_no_crane_flag(self):
        """Single-piece jacket is noted for heavy-lift requirement at decom."""
        flags = decom_flags_for_design_choices(
            has_grouted_connections=False,
            pile_removal_strategy="cut",
            jacket_pieces=1,
            has_mechanical_connectors=True,
        )
        categories = [f.category for f in flags]
        assert "jacket_lift" in categories

    def test_multi_piece_jacket_no_crane_flag(self):
        """Multi-piece jacket does not trigger heavy-lift decom flag."""
        flags = decom_flags_for_design_choices(
            has_grouted_connections=False,
            pile_removal_strategy="cut",
            jacket_pieces=3,
            has_mechanical_connectors=True,
        )
        categories = [f.category for f in flags]
        assert "jacket_lift" not in categories

    def test_flag_has_severity(self):
        flags = decom_flags_for_design_choices(
            has_grouted_connections=True,
            pile_removal_strategy="cut",
            jacket_pieces=1,
            has_mechanical_connectors=False,
        )
        for flag in flags:
            assert flag.severity in DecomFlagSeverity


# ---------------------------------------------------------------------------
# Tieback accommodation scoring
# ---------------------------------------------------------------------------

class TestScoreTiebackAccommodation:
    def test_high_score_for_well_designed_layout(self):
        """Ample manifold slots + loop pipeline + large mudmat => high score."""
        score = score_tieback_accommodation(
            manifold_future_slots=4,
            pipeline_layout=PipelineLayout.LOOP,
            mudmat_expansion_margin_pct=30.0,
        )
        assert score >= 0.75

    def test_low_score_for_constrained_layout(self):
        """No spare slots + single trunk + no mudmat margin => low score."""
        score = score_tieback_accommodation(
            manifold_future_slots=0,
            pipeline_layout=PipelineLayout.SINGLE_TRUNK,
            mudmat_expansion_margin_pct=0.0,
        )
        assert score <= 0.30

    def test_score_bounded_zero_to_one(self):
        for slots, layout, margin in [
            (0, PipelineLayout.SINGLE_TRUNK, 0.0),
            (6, PipelineLayout.LOOP, 50.0),
            (2, PipelineLayout.DAISY_CHAIN, 15.0),
        ]:
            s = score_tieback_accommodation(slots, layout, margin)
            assert 0.0 <= s <= 1.0

    def test_negative_margin_raises(self):
        with pytest.raises(ValueError, match="mudmat_expansion_margin_pct"):
            score_tieback_accommodation(
                manifold_future_slots=2,
                pipeline_layout=PipelineLayout.LOOP,
                mudmat_expansion_margin_pct=-5.0,
            )


# ---------------------------------------------------------------------------
# Marginal field lifecycle score (composite)
# ---------------------------------------------------------------------------

class TestMarginalFieldLifecycleScore:
    def test_returns_dict_with_required_keys(self):
        result = marginal_field_lifecycle_score(
            installation_difficulty=2,
            operational_flexibility=4,
            decommissioning_complexity=2,
            expansion_headroom=4,
            tieback_score=0.70,
        )
        assert "lifecycle_value_index" in result
        assert "tieback_combined" in result
        assert "recommendation" in result

    def test_high_scores_give_positive_recommendation(self):
        result = marginal_field_lifecycle_score(
            installation_difficulty=1,
            operational_flexibility=5,
            decommissioning_complexity=1,
            expansion_headroom=5,
            tieback_score=0.90,
        )
        assert result["recommendation"] in ("proceed", "strongly_recommended")

    def test_low_scores_give_cautious_recommendation(self):
        result = marginal_field_lifecycle_score(
            installation_difficulty=5,
            operational_flexibility=1,
            decommissioning_complexity=5,
            expansion_headroom=1,
            tieback_score=0.05,
        )
        assert result["recommendation"] in ("review", "caution")

    def test_tieback_score_out_of_range_raises(self):
        with pytest.raises(ValueError, match="tieback_score"):
            marginal_field_lifecycle_score(
                installation_difficulty=3,
                operational_flexibility=3,
                decommissioning_complexity=3,
                expansion_headroom=3,
                tieback_score=1.5,
            )
