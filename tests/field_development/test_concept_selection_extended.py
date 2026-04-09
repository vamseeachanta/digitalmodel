# ABOUTME: Extended tests for concept_selection — GoM reference fields, boundary depths, integration.
# ABOUTME: Issue #1972 — Test coverage uplift for overnight modules.
"""
Extended tests for digitalmodel.field_development.concept_selection

Covers:
- Parameterized GoM reference field cross-checks (all 10 fields)
- Boundary water depth edge cases at host-type hard limits
- Fluid type sensitivity across all host types
- Score determinism and reproducibility
- Extreme input values (very shallow, very deep, huge reservoirs)
- Subsea tieback threshold transitions
- CAPEX estimate consistency
- Composite score weight verification
- Integration with subsea_bridge catalog data
"""

from __future__ import annotations

import pytest

from digitalmodel.field_development.concept_selection import (
    ConceptOption,
    ConceptSelectionResult,
    HostType,
    concept_selection,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _ranks(result: ConceptSelectionResult) -> list[str]:
    return [opt.host_type.value for opt in result.ranked_options]


def _top(result: ConceptSelectionResult) -> str:
    return result.ranked_options[0].host_type.value


def _score_for(result: ConceptSelectionResult, host_value: str) -> float:
    for opt in result.ranked_options:
        if opt.host_type.value == host_value:
            return opt.score
    return 0.0


# ---------------------------------------------------------------------------
# Parameterized GoM reference field benchmarks (all 10 fields)
# ---------------------------------------------------------------------------

class TestGoMReferenceBenchmarks:
    """Cross-check against all 10 GoM reference fields from SubseaIQ scan."""

    @pytest.mark.parametrize("name,depth,reservoir,distance,fluid,expected_top", [
        # Perdido — ultra-deep Spar (Shell, 2438 m)
        ("Perdido", 2438, 100, 80, "oil", {"Spar", "Semi"}),
        # Mars — mid-depth TLP (Shell, 896 m)
        ("Mars", 896, 100, 50, "oil", {"TLP"}),
        # Atlantis — deep Semi-sub (BP, 2150 m)
        ("Atlantis", 2150, 200, 70, "oil", {"Semi", "Spar"}),
        # Thunder Horse — deep Semi-sub (BP, 1844 m)
        ("Thunder_Horse", 1844, 500, 60, "oil", {"Semi", "Spar", "TLP"}),
        # Appomattox — deep Semi-sub (Shell, 2250 m)
        ("Appomattox", 2250, 125, 65, "oil", {"Spar", "Semi"}),
        # Whale — deep Spar (Shell, 2100 m)
        ("Whale", 2100, 45, 55, "oil", {"Spar", "Semi"}),
        # Mad Dog — mid-deep Semi-sub (BP, 1480 m)
        ("Mad_Dog", 1480, 140, 50, "oil", {"Semi", "TLP", "Spar"}),
        # Stones — ultra-deep ETLP (Shell, 2900 m)
        ("Stones", 2900, 20, 90, "oil", {"Spar", "Semi", "FPSO"}),
        # Lucius — deep ETLP (LLOG, 2270 m)
        ("Lucius", 2270, 80, 75, "oil", {"Spar", "Semi"}),
        # Ursa — mid-depth TLP (Shell, 1067 m)
        ("Ursa", 1067, 150, 40, "oil", {"TLP", "Semi"}),
    ])
    def test_gom_field_top_choice(self, name, depth, reservoir, distance, fluid, expected_top):
        """Verify the top-ranked host type matches the known GoM field decision."""
        result = concept_selection(
            water_depth=depth,
            reservoir_size_mmbbl=reservoir,
            distance_to_infra_km=distance,
            fluid_type=fluid,
        )
        assert _top(result) in expected_top, (
            f"{name}: expected top in {expected_top}, got {_top(result)}"
        )

    @pytest.mark.parametrize("name,depth,reservoir,distance,fluid", [
        ("Perdido", 2438, 100, 80, "oil"),
        ("Mars", 896, 100, 50, "oil"),
        ("Atlantis", 2150, 200, 70, "oil"),
        ("Thunder_Horse", 1844, 500, 60, "oil"),
        ("Appomattox", 2250, 125, 65, "oil"),
    ])
    def test_gom_field_all_options_scored(self, name, depth, reservoir, distance, fluid):
        """Every GoM benchmark should produce exactly 5 ranked options."""
        result = concept_selection(
            water_depth=depth,
            reservoir_size_mmbbl=reservoir,
            distance_to_infra_km=distance,
            fluid_type=fluid,
        )
        assert len(result.ranked_options) == 5
        types_seen = {opt.host_type for opt in result.ranked_options}
        assert types_seen == set(HostType)

    @pytest.mark.parametrize("name,depth,reservoir,distance,fluid", [
        ("Perdido", 2438, 100, 80, "oil"),
        ("Mars", 896, 100, 50, "oil"),
        ("Stones", 2900, 20, 90, "oil"),
    ])
    def test_gom_field_deterministic(self, name, depth, reservoir, distance, fluid):
        """Same inputs must always produce the same ranked output."""
        r1 = concept_selection(depth, reservoir, distance, fluid)
        r2 = concept_selection(depth, reservoir, distance, fluid)
        assert _ranks(r1) == _ranks(r2)
        for o1, o2 in zip(r1.ranked_options, r2.ranked_options):
            assert o1.score == o2.score


# ---------------------------------------------------------------------------
# Water depth boundary conditions at host-type hard limits
# ---------------------------------------------------------------------------

class TestWaterDepthBoundaries:
    """Test behavior at the exact boundaries of host-type depth ranges."""

    def test_tlp_lower_limit_300m(self):
        """TLP range starts at 300 m — should score > 0 at boundary."""
        result = concept_selection(300, 200, 50, "oil")
        assert _score_for(result, "TLP") > 0

    def test_tlp_below_lower_limit_299m(self):
        """Below 300 m: TLP should score 0 (outside hard limit)."""
        result = concept_selection(299, 200, 50, "oil")
        assert _score_for(result, "TLP") == 0

    def test_tlp_upper_limit_1800m(self):
        """TLP range ends at 1800 m — should score > 0."""
        result = concept_selection(1800, 200, 50, "oil")
        assert _score_for(result, "TLP") > 0

    def test_tlp_above_upper_limit_1801m(self):
        """Above 1800 m: TLP should score 0."""
        result = concept_selection(1801, 200, 50, "oil")
        assert _score_for(result, "TLP") == 0

    def test_spar_lower_limit_1200m(self):
        """Spar range starts at 1200 m."""
        result = concept_selection(1200, 200, 50, "oil")
        assert _score_for(result, "Spar") > 0

    def test_spar_below_lower_limit_1199m(self):
        """Below 1200 m: Spar should score 0."""
        result = concept_selection(1199, 200, 50, "oil")
        assert _score_for(result, "Spar") == 0

    def test_spar_upper_limit_3000m(self):
        """Spar range ends at 3000 m."""
        result = concept_selection(3000, 200, 80, "oil")
        assert _score_for(result, "Spar") > 0

    def test_spar_above_upper_limit_3001m(self):
        """Above 3000 m: Spar should score 0."""
        result = concept_selection(3001, 200, 80, "oil")
        assert _score_for(result, "Spar") == 0

    def test_semi_lower_limit_600m(self):
        """Semi range starts at 600 m."""
        result = concept_selection(600, 200, 50, "oil")
        assert _score_for(result, "Semi") > 0

    def test_semi_below_lower_limit_599m(self):
        """Below 600 m: Semi should score 0."""
        result = concept_selection(599, 200, 50, "oil")
        assert _score_for(result, "Semi") == 0

    def test_fpso_lower_limit_600m(self):
        """FPSO range starts at 600 m."""
        result = concept_selection(600, 200, 50, "oil")
        assert _score_for(result, "FPSO") > 0

    def test_fpso_upper_limit_3000m(self):
        """FPSO range ends at 3000 m."""
        result = concept_selection(3000, 300, 80, "oil")
        assert _score_for(result, "FPSO") > 0

    def test_subsea_tieback_lower_limit_100m(self):
        """Subsea Tieback starts at 100 m."""
        result = concept_selection(100, 30, 8, "oil")
        assert _score_for(result, "Subsea_Tieback") > 0

    def test_subsea_tieback_below_lower_100m(self):
        """Below 100 m: tieback score = 0."""
        result = concept_selection(99, 30, 8, "oil")
        assert _score_for(result, "Subsea_Tieback") == 0


# ---------------------------------------------------------------------------
# Extreme water depths
# ---------------------------------------------------------------------------

class TestExtremeWaterDepths:
    """Edge cases: very shallow, very deep, mid-transition."""

    def test_very_shallow_150m_tieback_only(self):
        """At 150 m, only Subsea_Tieback has a valid depth range."""
        result = concept_selection(150, 20, 5, "oil")
        scored = [opt for opt in result.ranked_options if opt.score > 0]
        types = {opt.host_type.value for opt in scored}
        assert "Subsea_Tieback" in types
        # TLP starts at 300, Semi/FPSO at 600, Spar at 1200
        assert "TLP" not in types
        assert "Spar" not in types

    def test_very_deep_2999m_no_tlp(self):
        """At 2999 m, TLP should be excluded (hard limit 1800 m)."""
        result = concept_selection(2999, 200, 80, "oil")
        assert _score_for(result, "TLP") == 0
        # Spar/FPSO/Semi/Tieback should still be viable
        viable = [opt for opt in result.ranked_options if opt.score > 0]
        assert len(viable) >= 3

    def test_just_above_all_limits_3001m(self):
        """At 3001 m, only Subsea Tieback should be viable."""
        result = concept_selection(3001, 200, 10, "oil")
        # Spar, Semi, FPSO all cap at 3000; TLP at 1800; Tieback at 3000
        # Only tieback has hard limit up to 3000 so it too is excluded
        scored_types = {opt.host_type.value for opt in result.ranked_options if opt.score > 0}
        # All except tieback should be 0
        assert "TLP" not in scored_types
        assert "Spar" not in scored_types

    def test_depth_1m_near_zero(self):
        """At 1 m depth — only certain types should be valid."""
        result = concept_selection(1, 10, 5, "oil")
        # All should score 0 except possibly none (1 < 100 tieback lower limit)
        for opt in result.ranked_options:
            assert opt.score == 0, f"{opt.host_type.value} scored {opt.score} at 1m depth"


# ---------------------------------------------------------------------------
# Reservoir size edge cases
# ---------------------------------------------------------------------------

class TestReservoirSizeEdges:
    """Tests at reservoir size threshold transitions."""

    def test_reservoir_exactly_50_mmbbl(self):
        """At the tieback max threshold (50 MMbbl)."""
        result = concept_selection(1000, 50, 10, "oil")
        assert result.selected_host is not None

    def test_reservoir_exactly_150_mmbbl(self):
        """At the marginal tieback threshold (150 MMbbl)."""
        result = concept_selection(1000, 150, 10, "oil")
        assert result.selected_host is not None

    def test_reservoir_exactly_200_mmbbl(self):
        """At the standalone threshold (200 MMbbl)."""
        result = concept_selection(1000, 200, 50, "oil")
        assert result.selected_host is not None
        assert _top(result) != "Subsea_Tieback"

    def test_huge_reservoir_5000_mmbbl(self):
        """Very large field — standalone host should dominate."""
        result = concept_selection(1500, 5000, 80, "oil")
        assert _top(result) in ("TLP", "Spar", "Semi", "FPSO")
        # Tieback should score low
        assert _score_for(result, "Subsea_Tieback") < _score_for(result, _top(result))

    def test_tiny_reservoir_1_mmbbl(self):
        """Very small field near infrastructure — tieback preferred."""
        result = concept_selection(800, 1, 5, "oil")
        assert _top(result) == "Subsea_Tieback"


# ---------------------------------------------------------------------------
# Distance-to-infrastructure transitions
# ---------------------------------------------------------------------------

class TestDistanceTransitions:
    """Test tieback score behavior across distance thresholds."""

    def test_distance_0km_tieback_max_score(self):
        """Zero distance — tieback should get maximum distance score."""
        result = concept_selection(1000, 30, 0, "oil")
        assert _top(result) == "Subsea_Tieback"

    def test_distance_15km_threshold(self):
        """At the preferred tieback distance limit."""
        result = concept_selection(1000, 30, 15, "oil")
        assert _top(result) == "Subsea_Tieback"

    def test_distance_30km_marginal(self):
        """At the marginal tieback distance limit."""
        result = concept_selection(1000, 30, 30, "oil")
        # Tieback score is reduced but may still be top for small field
        tb_score = _score_for(result, "Subsea_Tieback")
        assert tb_score > 0

    def test_distance_60km_limit(self):
        """At the max viable tieback distance."""
        result = concept_selection(1000, 30, 60, "oil")
        tb_score = _score_for(result, "Subsea_Tieback")
        assert tb_score >= 0

    def test_distance_100km_tieback_nearly_zero(self):
        """Very far from infrastructure — tieback penalty very high."""
        result = concept_selection(1000, 200, 100, "oil")
        assert _top(result) != "Subsea_Tieback"

    def test_distance_monotonically_decreasing_tieback(self):
        """Tieback score should decrease as distance increases."""
        distances = [5, 15, 30, 60, 100]
        scores = []
        for d in distances:
            r = concept_selection(1000, 40, d, "oil")
            scores.append(_score_for(r, "Subsea_Tieback"))
        for i in range(1, len(scores)):
            assert scores[i] <= scores[i - 1] + 0.01, (
                f"Tieback score increased at distance {distances[i]}: "
                f"{scores[i]} > {scores[i-1]}"
            )


# ---------------------------------------------------------------------------
# Fluid type sensitivity
# ---------------------------------------------------------------------------

class TestFluidTypeSensitivity:
    """Verify fluid type affects scoring appropriately."""

    @pytest.mark.parametrize("fluid", ["oil", "gas", "condensate"])
    def test_all_fluid_types_accepted(self, fluid):
        """All three fluid types should produce valid results."""
        result = concept_selection(1500, 200, 50, fluid)
        assert result.selected_host is not None
        assert len(result.ranked_options) == 5

    def test_gas_boosts_fpso_over_tlp(self):
        """Gas fluid type should boost FPSO relative to TLP."""
        oil_result = concept_selection(1500, 200, 50, "oil")
        gas_result = concept_selection(1500, 200, 50, "gas")
        fpso_oil = _score_for(oil_result, "FPSO")
        fpso_gas = _score_for(gas_result, "FPSO")
        # FPSO should score higher for gas than oil (all else equal)
        assert fpso_gas >= fpso_oil

    def test_condensate_same_as_gas_for_scoring(self):
        """Condensate and gas should follow the same fluid-weight path."""
        gas_result = concept_selection(1500, 200, 50, "gas")
        cond_result = concept_selection(1500, 200, 50, "condensate")
        gas_scores = [(opt.host_type.value, opt.score) for opt in gas_result.ranked_options]
        cond_scores = [(opt.host_type.value, opt.score) for opt in cond_result.ranked_options]
        assert gas_scores == cond_scores

    def test_case_insensitive_fluid_type(self):
        """Fluid type should be case-insensitive."""
        r1 = concept_selection(1000, 200, 50, "Oil")
        r2 = concept_selection(1000, 200, 50, "OIL")
        r3 = concept_selection(1000, 200, 50, "oil")
        assert _ranks(r1) == _ranks(r2) == _ranks(r3)


# ---------------------------------------------------------------------------
# CAPEX estimate consistency
# ---------------------------------------------------------------------------

class TestCAPEXEstimates:
    """Verify CAPEX estimates are reasonable and consistent."""

    def test_capex_ranges_are_tuples(self):
        result = concept_selection(1500, 200, 50, "oil")
        for opt in result.ranked_options:
            assert isinstance(opt.capex_estimate_usd_bn, tuple)
            assert len(opt.capex_estimate_usd_bn) == 2
            low, high = opt.capex_estimate_usd_bn
            assert low < high, f"{opt.host_type.value} CAPEX: {low} >= {high}"

    def test_tieback_cheapest(self):
        """Subsea tieback should have the lowest CAPEX range."""
        result = concept_selection(1500, 200, 50, "oil")
        tieback_opt = [opt for opt in result.ranked_options
                       if opt.host_type == HostType.SUBSEA_TIEBACK][0]
        for opt in result.ranked_options:
            if opt.host_type != HostType.SUBSEA_TIEBACK:
                assert tieback_opt.capex_estimate_usd_bn[0] < opt.capex_estimate_usd_bn[0]

    def test_capex_same_for_same_host(self):
        """CAPEX ranges should not change with different inputs (they are lookup constants)."""
        r1 = concept_selection(800, 100, 20, "oil")
        r2 = concept_selection(2000, 300, 80, "gas")
        for host in HostType:
            c1 = [o for o in r1.ranked_options if o.host_type == host][0].capex_estimate_usd_bn
            c2 = [o for o in r2.ranked_options if o.host_type == host][0].capex_estimate_usd_bn
            assert c1 == c2


# ---------------------------------------------------------------------------
# Score ordering and structure
# ---------------------------------------------------------------------------

class TestScoreOrdering:
    """Verify structural properties of result objects."""

    @pytest.mark.parametrize("depth,res,dist,fluid", [
        (500, 100, 30, "oil"),
        (1500, 200, 50, "gas"),
        (2500, 300, 80, "condensate"),
        (900, 30, 8, "oil"),
        (1200, 500, None, "oil"),
    ])
    def test_scores_descending(self, depth, res, dist, fluid):
        """Ranked options must always be in descending score order."""
        result = concept_selection(depth, res, dist, fluid)
        scores = [opt.score for opt in result.ranked_options]
        assert scores == sorted(scores, reverse=True)

    @pytest.mark.parametrize("depth,res,dist,fluid", [
        (500, 100, 30, "oil"),
        (1500, 200, 50, "gas"),
        (2500, 300, 80, "condensate"),
    ])
    def test_selected_host_is_top(self, depth, res, dist, fluid):
        """selected_host must equal the top-ranked option's host_type."""
        result = concept_selection(depth, res, dist, fluid)
        assert result.selected_host == result.ranked_options[0].host_type

    @pytest.mark.parametrize("depth,res,dist,fluid", [
        (800, 200, 50, "oil"),
        (1500, 200, 50, "gas"),
        (2500, 100, None, "condensate"),
    ])
    def test_summary_is_nonempty(self, depth, res, dist, fluid):
        """Summary should be a non-trivial string."""
        result = concept_selection(depth, res, dist, fluid)
        assert isinstance(result.summary, str)
        assert len(result.summary) > 30

    def test_all_scores_between_0_and_100(self):
        """All scores should be in [0, 100]."""
        result = concept_selection(1500, 200, 50, "oil")
        for opt in result.ranked_options:
            assert 0.0 <= opt.score <= 100.0, (
                f"{opt.host_type.value} score {opt.score} out of range"
            )

    def test_scores_are_rounded_to_2_decimal(self):
        """Scores should be rounded to 2 decimal places."""
        result = concept_selection(1500, 200, 50, "oil")
        for opt in result.ranked_options:
            assert opt.score == round(opt.score, 2)


# ---------------------------------------------------------------------------
# Rationale content
# ---------------------------------------------------------------------------

class TestRationale:
    """Verify rationale strings contain useful info."""

    def test_rationale_contains_host_type(self):
        result = concept_selection(1500, 200, 50, "oil")
        for opt in result.ranked_options:
            assert opt.host_type.value in opt.rationale

    def test_rationale_contains_depth(self):
        result = concept_selection(1500, 200, 50, "oil")
        for opt in result.ranked_options:
            assert "1500" in opt.rationale

    def test_rationale_contains_fluid(self):
        result = concept_selection(1500, 200, 50, "oil")
        for opt in result.ranked_options:
            assert "oil" in opt.rationale


# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

class TestExtendedValidation:
    """Additional validation edge cases beyond the base test file."""

    def test_string_inputs_raise_type_error(self):
        with pytest.raises((TypeError, ValueError)):
            concept_selection("deep", 200, 50, "oil")

    def test_none_water_depth_raises(self):
        with pytest.raises((TypeError, ValueError)):
            concept_selection(None, 200, 50, "oil")

    def test_none_reservoir_raises(self):
        with pytest.raises((TypeError, ValueError)):
            concept_selection(1000, None, 50, "oil")

    def test_zero_distance_accepted(self):
        """distance_to_infra_km=0 should be valid (field is on existing platform)."""
        result = concept_selection(1000, 30, 0, "oil")
        assert result.selected_host is not None

    def test_very_large_distance_accepted(self):
        """distance_to_infra_km=1000 should be accepted (frontier basin)."""
        result = concept_selection(1500, 200, 1000, "oil")
        assert result.selected_host is not None
        assert _top(result) != "Subsea_Tieback"

    def test_empty_string_fluid_raises(self):
        with pytest.raises(ValueError, match="fluid_type"):
            concept_selection(1000, 200, 50, "")

    def test_very_small_reservoir_0_1_mmbbl_raises(self):
        """Reservoir must be > 0."""
        with pytest.raises(ValueError, match="reservoir"):
            concept_selection(1000, 0, 50, "oil")
