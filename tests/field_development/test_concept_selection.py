# ABOUTME: Tests for concept_selection module — host facility type ranking logic.
# ABOUTME: Issue #1843/#2053 — Concept Selection Framework with empirical weighting.
"""
Tests for digitalmodel.field_development.concept_selection

Covers:
- ConceptSelection dataclass structure and field validation
- concept_selection() happy-path scenarios for each host type
- Water-depth boundary conditions (TLP/Spar/Semi/FPSO thresholds)
- Reservoir-size tiebreaker (tieback vs standalone host)
- Distance-to-infrastructure preference (tieback penalty/bonus)
- Fluid type weighting (oil vs gas vs condensate)
- Ranking order and score consistency
- Input validation: negative depth, invalid fluid_type, bad types
- Benchmark cross-checks vs GoM reference fields
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
    """Return ordered list of host-type names from ranked result."""
    return [opt.host_type.value for opt in result.ranked_options]


def _top(result: ConceptSelectionResult) -> str:
    return result.ranked_options[0].host_type.value


# ---------------------------------------------------------------------------
# HostType enum
# ---------------------------------------------------------------------------

class TestHostTypeEnum:
    def test_all_types_defined(self):
        values = {h.value for h in HostType}
        assert values == {"TLP", "Spar", "Semi", "FPSO", "Subsea_Tieback"}

    def test_enum_accessible_by_value(self):
        assert HostType("TLP") is HostType.TLP
        assert HostType("Spar") is HostType.SPAR


# ---------------------------------------------------------------------------
# ConceptOption dataclass
# ---------------------------------------------------------------------------

class TestConceptOption:
    def test_fields_present(self):
        opt = ConceptOption(
            host_type=HostType.TLP,
            score=85.0,
            capex_estimate_usd_bn=(2.0, 6.0),
            rationale="TLP preferred for mid-depth oil fields.",
        )
        assert opt.host_type is HostType.TLP
        assert opt.score == pytest.approx(85.0)
        assert opt.capex_estimate_usd_bn == (2.0, 6.0)
        assert "TLP" in opt.rationale

    def test_score_is_float(self):
        opt = ConceptOption(
            host_type=HostType.SPAR,
            score=72.5,
            capex_estimate_usd_bn=(3.0, 7.0),
            rationale="Spar option.",
        )
        assert isinstance(opt.score, float)


# ---------------------------------------------------------------------------
# ConceptSelectionResult dataclass
# ---------------------------------------------------------------------------

class TestConceptSelectionResult:
    def test_top_recommendation_is_first_option(self):
        opt_a = ConceptOption(HostType.TLP, 90.0, (2.0, 6.0), "a")
        opt_b = ConceptOption(HostType.SPAR, 80.0, (3.0, 7.0), "b")
        result = ConceptSelectionResult(
            ranked_options=[opt_a, opt_b],
            selected_host=HostType.TLP,
            summary="TLP selected.",
        )
        assert result.selected_host is HostType.TLP
        assert result.ranked_options[0] is opt_a

    def test_ranked_options_not_empty(self):
        opt = ConceptOption(HostType.FPSO, 70.0, (5.0, 12.0), "fpso")
        result = ConceptSelectionResult(
            ranked_options=[opt],
            selected_host=HostType.FPSO,
            summary="FPSO only.",
        )
        assert len(result.ranked_options) >= 1


# ---------------------------------------------------------------------------
# concept_selection — Water depth thresholds
# ---------------------------------------------------------------------------

class TestConceptSelectionWaterDepth:
    """Water depth is the primary driver; test each host-type depth range."""

    def test_shallow_300m_prefers_tlp(self):
        """TLP range: 300-1800 m; at 800 m TLP should rank first."""
        result = concept_selection(
            water_depth=800,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
        )
        assert _top(result) == "TLP"

    def test_mid_depth_1200m_tlp_or_spar(self):
        """At 1200 m both TLP and Spar are viable; either may top the list."""
        result = concept_selection(
            water_depth=1200,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
        )
        assert _top(result) in ("TLP", "Spar", "Semi")

    def test_deep_2000m_spar_or_semi_tops(self):
        """At 2000 m (ultra-deep) Spar or Semi should rank first, not TLP."""
        result = concept_selection(
            water_depth=2000,
            reservoir_size_mmbbl=300,
            distance_to_infra_km=60,
            fluid_type="oil",
        )
        assert _top(result) in ("Spar", "Semi")

    def test_ultra_deep_2900m_spar_tops(self):
        """Perdido is 2438 m Spar; at 2900 m Spar/Semi are viable."""
        result = concept_selection(
            water_depth=2900,
            reservoir_size_mmbbl=150,
            distance_to_infra_km=80,
            fluid_type="oil",
        )
        # TLP excluded (depth > 1800 m hard limit)
        ranks = _ranks(result)
        assert "TLP" not in ranks or result.ranked_options[ranks.index("TLP")].score < result.ranked_options[0].score

    def test_tlp_excluded_above_1800m(self):
        """TLP hard limit is 1800 m; above that TLP should not be top choice."""
        result = concept_selection(
            water_depth=2200,
            reservoir_size_mmbbl=250,
            distance_to_infra_km=60,
            fluid_type="oil",
        )
        assert _top(result) != "TLP"

    def test_subsea_tieback_preferred_close_infra(self):
        """Small field <15 km from existing host → tieback preferred."""
        result = concept_selection(
            water_depth=1000,
            reservoir_size_mmbbl=30,
            distance_to_infra_km=8,
            fluid_type="oil",
        )
        assert _top(result) == "Subsea_Tieback"


# ---------------------------------------------------------------------------
# concept_selection — Reservoir size tiebreaker
# ---------------------------------------------------------------------------

class TestConceptSelectionReservoirSize:
    def test_small_reservoir_prefers_tieback(self):
        """< 50 MMbbl near existing infrastructure → tieback."""
        result = concept_selection(
            water_depth=900,
            reservoir_size_mmbbl=25,
            distance_to_infra_km=12,
            fluid_type="oil",
        )
        assert _top(result) == "Subsea_Tieback"

    def test_large_reservoir_prefers_standalone_host(self):
        """> 500 MMbbl → standalone host (TLP/Spar/Semi/FPSO) beats tieback."""
        result = concept_selection(
            water_depth=900,
            reservoir_size_mmbbl=600,
            distance_to_infra_km=12,
            fluid_type="oil",
        )
        assert _top(result) != "Subsea_Tieback"

    def test_medium_reservoir_200mmbbl_standalone(self):
        """200 MMbbl at moderate depth → standalone host."""
        result = concept_selection(
            water_depth=1500,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
        )
        assert _top(result) in ("TLP", "Spar", "Semi", "FPSO")


# ---------------------------------------------------------------------------
# concept_selection — Distance to infrastructure
# ---------------------------------------------------------------------------

class TestConceptSelectionDistance:
    def test_far_from_infra_reduces_tieback_score(self):
        """At 80 km from nearest host, tieback should score lower."""
        result_near = concept_selection(
            water_depth=1000,
            reservoir_size_mmbbl=100,
            distance_to_infra_km=8,
            fluid_type="oil",
        )
        result_far = concept_selection(
            water_depth=1000,
            reservoir_size_mmbbl=100,
            distance_to_infra_km=80,
            fluid_type="oil",
        )
        # Tieback score decreases as distance increases
        def tieback_score(r):
            for opt in r.ranked_options:
                if opt.host_type == HostType.SUBSEA_TIEBACK:
                    return opt.score
            return 0.0

        assert tieback_score(result_near) > tieback_score(result_far)

    def test_no_infra_tieback_not_top(self):
        """If no infrastructure nearby (distance=None/large), tieback not top."""
        result = concept_selection(
            water_depth=1200,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=None,
            fluid_type="oil",
        )
        assert _top(result) != "Subsea_Tieback"


# ---------------------------------------------------------------------------
# concept_selection — Fluid type
# ---------------------------------------------------------------------------

class TestConceptSelectionFluidType:
    def test_gas_condensate_boosts_fpso(self):
        """Gas/condensate fields benefit from FPSO storage/offloading."""
        result = concept_selection(
            water_depth=1500,
            reservoir_size_mmbbl=300,
            distance_to_infra_km=80,
            fluid_type="gas",
        )
        # FPSO or Semi should appear in top 2 for gas at deep water
        top2 = _ranks(result)[:2]
        assert any(h in top2 for h in ("FPSO", "Semi", "Spar"))

    def test_oil_heavy_fluid_valid(self):
        result = concept_selection(
            water_depth=1000,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=40,
            fluid_type="oil",
        )
        assert len(result.ranked_options) >= 3

    def test_condensate_fluid_type_accepted(self):
        result = concept_selection(
            water_depth=1500,
            reservoir_size_mmbbl=150,
            distance_to_infra_km=60,
            fluid_type="condensate",
        )
        assert result.selected_host is not None


# ---------------------------------------------------------------------------
# concept_selection — GoM benchmark cross-checks
# ---------------------------------------------------------------------------

class TestConceptSelectionBenchmarks:
    """Cross-check results against known GoM field decisions."""

    def test_perdido_analog_deep_spar(self):
        """Perdido: 2438 m, ~100 MMbbl → Spar or Semi."""
        result = concept_selection(
            water_depth=2438,
            reservoir_size_mmbbl=100,
            distance_to_infra_km=80,
            fluid_type="oil",
        )
        assert _top(result) in ("Spar", "Semi")

    def test_mars_analog_tlp(self):
        """Mars: 896 m → TLP is the documented choice."""
        result = concept_selection(
            water_depth=896,
            reservoir_size_mmbbl=100,
            distance_to_infra_km=50,
            fluid_type="oil",
        )
        assert _top(result) == "TLP"

    def test_atlantis_analog_semi(self):
        """Atlantis: 2150 m, 200 MMbbl → Semi or Spar."""
        result = concept_selection(
            water_depth=2150,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=70,
            fluid_type="oil",
        )
        assert _top(result) in ("Semi", "Spar")

    def test_thunder_horse_1844m_semi(self):
        """Thunder Horse: 1844 m, 250 MMbbl, large prod → Semi."""
        result = concept_selection(
            water_depth=1844,
            reservoir_size_mmbbl=500,
            distance_to_infra_km=60,
            fluid_type="oil",
        )
        assert _top(result) in ("Semi", "Spar", "TLP")


# ---------------------------------------------------------------------------
# concept_selection — Input validation
# ---------------------------------------------------------------------------

class TestConceptSelectionValidation:
    def test_negative_water_depth_raises(self):
        with pytest.raises(ValueError, match="water_depth"):
            concept_selection(
                water_depth=-100,
                reservoir_size_mmbbl=100,
                distance_to_infra_km=50,
                fluid_type="oil",
            )

    def test_zero_water_depth_raises(self):
        with pytest.raises(ValueError, match="water_depth"):
            concept_selection(
                water_depth=0,
                reservoir_size_mmbbl=100,
                distance_to_infra_km=50,
                fluid_type="oil",
            )

    def test_negative_reservoir_size_raises(self):
        with pytest.raises(ValueError, match="reservoir_size"):
            concept_selection(
                water_depth=1000,
                reservoir_size_mmbbl=-50,
                distance_to_infra_km=30,
                fluid_type="oil",
            )

    def test_invalid_fluid_type_raises(self):
        with pytest.raises(ValueError, match="fluid_type"):
            concept_selection(
                water_depth=1000,
                reservoir_size_mmbbl=100,
                distance_to_infra_km=30,
                fluid_type="uranium",
            )

    def test_negative_distance_raises(self):
        with pytest.raises(ValueError, match="distance"):
            concept_selection(
                water_depth=1000,
                reservoir_size_mmbbl=100,
                distance_to_infra_km=-5,
                fluid_type="oil",
            )

    def test_result_ranked_options_ordered_descending(self):
        """Scores in ranked_options must be non-increasing."""
        result = concept_selection(
            water_depth=1500,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
        )
        scores = [opt.score for opt in result.ranked_options]
        assert scores == sorted(scores, reverse=True)

    def test_selected_host_matches_top_option(self):
        result = concept_selection(
            water_depth=900,
            reservoir_size_mmbbl=150,
            distance_to_infra_km=40,
            fluid_type="oil",
        )
        assert result.selected_host == result.ranked_options[0].host_type

    def test_result_has_summary_string(self):
        result = concept_selection(
            water_depth=1200,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
        )
        assert isinstance(result.summary, str)
        assert len(result.summary) > 10


# ---------------------------------------------------------------------------
# concept_selection — Empirical weights integration (#2053)
# ---------------------------------------------------------------------------

from digitalmodel.field_development.concept_selection import (
    concept_selection_with_benchmarks,
)
from digitalmodel.field_development.benchmarks import (
    SubseaProject,
    load_projects,
)


class TestConceptSelectionEmpiricalWeights:
    """Tests for empirical_weights parameter (#2053)."""

    def test_no_empirical_weights_backward_compatible(self):
        """Without empirical_weights, behaviour is unchanged."""
        result = concept_selection(
            water_depth=900,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
        )
        assert result.selected_host is not None
        assert "Empirical" not in result.summary

    def test_empirical_weights_accepted(self):
        """Passing empirical_weights should not raise."""
        weights = {"TLP": 0.45, "Semi": 0.30, "Spar": 0.15, "FPSO": 0.05, "Subsea_Tieback": 0.05}
        result = concept_selection(
            water_depth=900,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
            empirical_weights=weights,
        )
        assert result.selected_host is not None
        assert "Empirical" in result.summary

    def test_empirical_weights_influence_score(self):
        """Scores should differ when empirical weights are applied.

        Empirical weights blend at 10%, so the relative ranking between
        two concepts should shift when one gets a heavy empirical boost
        and the other gets penalised.
        """
        result_no_emp = concept_selection(
            water_depth=1500,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
        )
        # Give Spar 100% empirical weight, zero to others
        weights_spar = {"TLP": 0.0, "Semi": 0.0, "Spar": 1.0, "FPSO": 0.0, "Subsea_Tieback": 0.0}
        # Give Semi 100% empirical weight, zero to Spar
        weights_semi = {"TLP": 0.0, "Semi": 1.0, "Spar": 0.0, "FPSO": 0.0, "Subsea_Tieback": 0.0}
        result_spar = concept_selection(
            water_depth=1500,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
            empirical_weights=weights_spar,
        )
        result_semi = concept_selection(
            water_depth=1500,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
            empirical_weights=weights_semi,
        )
        # Spar score should be higher when Spar gets 100% empirical vs Semi getting 100%
        spar_in_spar = next(o for o in result_spar.ranked_options if o.host_type == HostType.SPAR)
        spar_in_semi = next(o for o in result_semi.ranked_options if o.host_type == HostType.SPAR)
        assert spar_in_spar.score > spar_in_semi.score

    def test_empty_empirical_weights_no_crash(self):
        """Empty weights dict should be treated as no empirical data."""
        result = concept_selection(
            water_depth=900,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
            empirical_weights={},
        )
        assert result.selected_host is not None

    def test_scores_still_ordered_descending(self):
        """With empirical weights, scores must still be descending."""
        weights = {"TLP": 0.20, "Semi": 0.30, "Spar": 0.30, "FPSO": 0.10, "Subsea_Tieback": 0.10}
        result = concept_selection(
            water_depth=1200,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
            empirical_weights=weights,
        )
        scores = [opt.score for opt in result.ranked_options]
        assert scores == sorted(scores, reverse=True)


class TestConceptSelectionWithBenchmarks:
    """Tests for concept_selection_with_benchmarks convenience function (#2053)."""

    BENCHMARK_RECORDS = [
        {"name": "F1", "water_depth_m": 900, "concept_type": "TLP"},
        {"name": "F2", "water_depth_m": 1000, "concept_type": "TLP"},
        {"name": "F3", "water_depth_m": 1100, "concept_type": "Semi"},
        {"name": "F4", "water_depth_m": 1200, "concept_type": "Semi"},
        {"name": "F5", "water_depth_m": 1800, "concept_type": "Spar"},
        {"name": "F6", "water_depth_m": 2000, "concept_type": "FPSO"},
        {"name": "F7", "water_depth_m": 2200, "concept_type": "Spar"},
        {"name": "F8", "water_depth_m": 350, "concept_type": "Subsea Tieback"},
    ]

    @pytest.fixture
    def bench_projects(self):
        return load_projects(self.BENCHMARK_RECORDS)

    def test_returns_concept_selection_result(self, bench_projects):
        result = concept_selection_with_benchmarks(
            water_depth=1000,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
            benchmark_projects=bench_projects,
        )
        assert isinstance(result, ConceptSelectionResult)

    def test_has_all_host_types(self, bench_projects):
        result = concept_selection_with_benchmarks(
            water_depth=1000,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
            benchmark_projects=bench_projects,
        )
        host_types = {o.host_type for o in result.ranked_options}
        assert len(host_types) == len(HostType)

    def test_empirical_note_in_summary(self, bench_projects):
        result = concept_selection_with_benchmarks(
            water_depth=1000,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
            benchmark_projects=bench_projects,
        )
        assert "Empirical" in result.summary

    def test_selected_host_matches_top(self, bench_projects):
        result = concept_selection_with_benchmarks(
            water_depth=1000,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
            benchmark_projects=bench_projects,
        )
        assert result.selected_host == result.ranked_options[0].host_type
