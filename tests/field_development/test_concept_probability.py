# ABOUTME: Dedicated tests for concept probability matrix and decision tree predictor.
# ABOUTME: Issue #2053 — TDD tests for validate_against_cases and integration scenarios.
"""Tests for concept probability matrix, decision tree, and case validation (#2053).

This file supplements test_benchmarks.py with:
- validate_against_cases() accuracy checking
- Integration tests for concept_selection_with_benchmarks pipeline
- Edge cases for probability matrix behaviour
"""

import pytest

from digitalmodel.field_development.benchmarks import (
    SubseaProject,
    ConceptPrediction,
    load_projects,
    concept_probability_matrix,
    predict_concept_type,
    validate_against_cases,
    DEPTH_BANDS,
)
from digitalmodel.field_development.concept_selection import (
    concept_selection,
    concept_selection_with_benchmarks,
)


# ---------------------------------------------------------------------------
# Fixtures: GoM benchmark dataset for validation
# ---------------------------------------------------------------------------

# 15 real GoM fields with known concept decisions (public data)
GOM_BENCHMARK_RECORDS = [
    {"name": "Perdido", "water_depth_m": 2438, "concept_type": "Spar",
     "operator": "Shell", "region": "GoM"},
    {"name": "Mars", "water_depth_m": 896, "concept_type": "TLP",
     "operator": "Shell", "region": "GoM"},
    {"name": "Atlantis", "water_depth_m": 2150, "concept_type": "Semi",
     "operator": "BP", "region": "GoM"},
    {"name": "Thunder Horse", "water_depth_m": 1844, "concept_type": "Semi",
     "operator": "BP", "region": "GoM"},
    {"name": "Mad Dog", "water_depth_m": 1311, "concept_type": "Spar",
     "operator": "BP", "region": "GoM"},
    {"name": "Appomattox", "water_depth_m": 2195, "concept_type": "Semi",
     "operator": "Shell", "region": "GoM"},
    {"name": "Whale", "water_depth_m": 1372, "concept_type": "Semi",
     "operator": "Shell", "region": "GoM"},
    {"name": "Stones", "water_depth_m": 2900, "concept_type": "FPSO",
     "operator": "Shell", "region": "GoM"},
    {"name": "Lucius", "water_depth_m": 2164, "concept_type": "Spar",
     "operator": "Anadarko", "region": "GoM"},
    {"name": "Ursa", "water_depth_m": 1158, "concept_type": "TLP",
     "operator": "Shell", "region": "GoM"},
    {"name": "Na Kika", "water_depth_m": 1920, "concept_type": "Semi",
     "operator": "Shell", "region": "GoM"},
    {"name": "Holstein", "water_depth_m": 1325, "concept_type": "Spar",
     "operator": "Anadarko", "region": "GoM"},
    {"name": "Shenzi", "water_depth_m": 1311, "concept_type": "TLP",
     "operator": "BHP", "region": "GoM"},
    {"name": "Constitution", "water_depth_m": 1524, "concept_type": "Spar",
     "operator": "Anadarko", "region": "GoM"},
    {"name": "Great White", "water_depth_m": 2438, "concept_type": "Spar",
     "operator": "Shell", "region": "GoM"},
]

# Case studies: fields with known concept selection + field parameters
CASE_STUDIES = [
    {
        "name": "Mars-like",
        "water_depth": 896,
        "reservoir_size_mmbbl": 200,
        "distance_to_infra_km": 40,
        "expected_concept": "TLP",
    },
    {
        "name": "Perdido-like",
        "water_depth": 2438,
        "reservoir_size_mmbbl": 300,
        "distance_to_infra_km": 80,
        "expected_concept": "Spar",
    },
    {
        "name": "Small-tieback",
        "water_depth": 500,
        "reservoir_size_mmbbl": 20,
        "distance_to_infra_km": 5,
        "expected_concept": "Subsea Tieback",
    },
    {
        "name": "Atlantis-like",
        "water_depth": 2150,
        "reservoir_size_mmbbl": 400,
        "distance_to_infra_km": 60,
        "expected_concept": "Semi",
    },
]


@pytest.fixture
def gom_projects():
    return load_projects(GOM_BENCHMARK_RECORDS)


@pytest.fixture
def case_studies():
    return CASE_STUDIES


# ---------------------------------------------------------------------------
# validate_against_cases — accuracy checking
# ---------------------------------------------------------------------------

class TestValidateAgainstCases:
    """Tests for validate_against_cases() accuracy checker (#2053)."""

    def test_returns_dict_with_accuracy(self, gom_projects, case_studies):
        result = validate_against_cases(gom_projects, case_studies)
        assert isinstance(result, dict)
        assert "accuracy" in result
        assert "total" in result
        assert "correct" in result
        assert "details" in result

    def test_accuracy_is_float_between_0_and_1(self, gom_projects, case_studies):
        result = validate_against_cases(gom_projects, case_studies)
        assert 0.0 <= result["accuracy"] <= 1.0

    def test_total_matches_input_count(self, gom_projects, case_studies):
        result = validate_against_cases(gom_projects, case_studies)
        assert result["total"] == len(case_studies)

    def test_correct_plus_incorrect_equals_total(self, gom_projects, case_studies):
        result = validate_against_cases(gom_projects, case_studies)
        assert result["correct"] + result["incorrect"] == result["total"]

    def test_details_per_case(self, gom_projects, case_studies):
        result = validate_against_cases(gom_projects, case_studies)
        assert len(result["details"]) == len(case_studies)
        for detail in result["details"]:
            assert "name" in detail
            assert "expected" in detail
            assert "predicted" in detail
            assert "match" in detail
            assert "top_3" in detail

    def test_tieback_case_matches(self, gom_projects):
        """Small reservoir close to infra should reliably predict tieback."""
        cases = [{
            "name": "tieback-test",
            "water_depth": 400,
            "reservoir_size_mmbbl": 15,
            "distance_to_infra_km": 5,
            "expected_concept": "Subsea Tieback",
        }]
        result = validate_against_cases(gom_projects, cases)
        assert result["details"][0]["match"] is True

    def test_empty_cases_returns_zero_accuracy(self, gom_projects):
        result = validate_against_cases(gom_projects, [])
        assert result["accuracy"] == 0.0
        assert result["total"] == 0
        assert result["correct"] == 0

    def test_top_3_includes_predicted(self, gom_projects, case_studies):
        """The top 3 should always include the predicted concept."""
        result = validate_against_cases(gom_projects, case_studies)
        for detail in result["details"]:
            top_3_concepts = [entry[0] for entry in detail["top_3"]]
            assert detail["predicted"] in top_3_concepts

    def test_top_3_probabilities_descending(self, gom_projects, case_studies):
        result = validate_against_cases(gom_projects, case_studies)
        for detail in result["details"]:
            probs = [entry[1] for entry in detail["top_3"]]
            assert probs == sorted(probs, reverse=True)

    def test_accepts_flexible_expected_concepts(self, gom_projects):
        """expected_concept can be a list (accept multiple valid answers)."""
        cases = [{
            "name": "deep-field",
            "water_depth": 2000,
            "reservoir_size_mmbbl": 300,
            "distance_to_infra_km": 70,
            "expected_concept": ["Spar", "Semi", "FPSO"],
        }]
        result = validate_against_cases(gom_projects, cases)
        # Should match if predicted is any of the accepted concepts
        assert result["details"][0]["match"] is True


# ---------------------------------------------------------------------------
# Probability matrix edge cases
# ---------------------------------------------------------------------------

class TestProbabilityMatrixEdgeCases:
    """Additional edge cases for concept_probability_matrix (#2053)."""

    def test_all_same_concept_gives_100_percent(self):
        """If every project in a band is the same type, probability = 1.0."""
        records = [
            {"name": f"F{i}", "water_depth_m": 1000 + i * 10, "concept_type": "Semi"}
            for i in range(5)
        ]
        projects = load_projects(records)
        matrix = concept_probability_matrix(projects)
        deep = matrix["800-1500m"]
        assert deep == {"Semi": pytest.approx(1.0)}

    def test_two_concepts_equal_split(self):
        """Two concept types equally represented should each be 0.5."""
        records = [
            {"name": "A", "water_depth_m": 900, "concept_type": "TLP"},
            {"name": "B", "water_depth_m": 1000, "concept_type": "Semi"},
        ]
        projects = load_projects(records)
        matrix = concept_probability_matrix(projects)
        deep = matrix["800-1500m"]
        assert deep["TLP"] == pytest.approx(0.5)
        assert deep["Semi"] == pytest.approx(0.5)

    def test_bands_independent(self):
        """Projects in different bands should not affect each other's probabilities."""
        records = [
            {"name": "S1", "water_depth_m": 200, "concept_type": "Fixed Platform"},
            {"name": "D1", "water_depth_m": 1000, "concept_type": "TLP"},
        ]
        projects = load_projects(records)
        matrix = concept_probability_matrix(projects)
        assert matrix["0-300m"] == {"Fixed Platform": pytest.approx(1.0)}
        assert matrix["800-1500m"] == {"TLP": pytest.approx(1.0)}

    def test_many_concept_types_in_one_band(self):
        """Multiple concept types in one band: probabilities sum to 1.0."""
        records = [
            {"name": "A", "water_depth_m": 900, "concept_type": "TLP"},
            {"name": "B", "water_depth_m": 1000, "concept_type": "Semi"},
            {"name": "C", "water_depth_m": 1100, "concept_type": "Spar"},
            {"name": "D", "water_depth_m": 1200, "concept_type": "FPSO"},
            {"name": "E", "water_depth_m": 1300, "concept_type": "Subsea Tieback"},
        ]
        projects = load_projects(records)
        matrix = concept_probability_matrix(projects)
        deep = matrix["800-1500m"]
        assert len(deep) == 5
        assert sum(deep.values()) == pytest.approx(1.0)
        # Each should be 1/5 = 0.2
        for prob in deep.values():
            assert prob == pytest.approx(0.2)


# ---------------------------------------------------------------------------
# Decision tree predictor edge cases
# ---------------------------------------------------------------------------

class TestPredictConceptEdgeCases:
    """Edge cases for predict_concept_type (#2053)."""

    def test_no_infra_suppresses_tieback(self, gom_projects):
        """No infrastructure (None) should heavily penalise tieback."""
        result = predict_concept_type(
            projects=gom_projects,
            water_depth=1000,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=None,
        )
        # Tieback should be very low probability when no infra nearby
        assert result.probabilities.get("Subsea Tieback", 0.0) < 0.1

    def test_very_small_reservoir_boosts_tieback(self, gom_projects):
        """Tiny reservoir + close infra → tieback dominates."""
        result = predict_concept_type(
            projects=gom_projects,
            water_depth=500,
            reservoir_size_mmbbl=10,
            distance_to_infra_km=3,
        )
        assert result.probabilities.get("Subsea Tieback", 0.0) > 0.3

    def test_very_large_reservoir_suppresses_tieback(self, gom_projects):
        """Huge reservoir → standalone host dominates."""
        result = predict_concept_type(
            projects=gom_projects,
            water_depth=1500,
            reservoir_size_mmbbl=1000,
            distance_to_infra_km=50,
        )
        assert result.predicted_concept != "Subsea Tieback"

    def test_predicted_concept_in_probabilities(self, gom_projects):
        """The predicted concept must appear in the probabilities dict."""
        result = predict_concept_type(
            projects=gom_projects,
            water_depth=1500,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
        )
        assert result.predicted_concept in result.probabilities

    def test_predicted_concept_has_highest_probability(self, gom_projects):
        """The predicted concept should have the highest probability."""
        result = predict_concept_type(
            projects=gom_projects,
            water_depth=1500,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
        )
        max_prob = max(result.probabilities.values())
        assert result.probabilities[result.predicted_concept] == pytest.approx(max_prob)

    def test_rationale_includes_inputs(self, gom_projects):
        """Rationale string should mention depth, reservoir, and distance."""
        result = predict_concept_type(
            projects=gom_projects,
            water_depth=1500,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
        )
        assert "1500" in result.rationale
        assert "200" in result.rationale
        assert "50 km" in result.rationale

    def test_boundary_depth_300m(self, gom_projects):
        """300m is the boundary between shallow and mid-water."""
        result = predict_concept_type(
            projects=gom_projects,
            water_depth=300,
            reservoir_size_mmbbl=100,
            distance_to_infra_km=20,
        )
        assert result.depth_band == "300-800m"

    def test_boundary_depth_800m(self, gom_projects):
        """800m is the boundary between mid-water and deepwater."""
        result = predict_concept_type(
            projects=gom_projects,
            water_depth=800,
            reservoir_size_mmbbl=100,
            distance_to_infra_km=20,
        )
        assert result.depth_band == "800-1500m"

    def test_boundary_depth_1500m(self, gom_projects):
        """1500m is the boundary between deepwater and ultra-deep."""
        result = predict_concept_type(
            projects=gom_projects,
            water_depth=1500,
            reservoir_size_mmbbl=100,
            distance_to_infra_km=20,
        )
        assert result.depth_band == "1500m+"


# ---------------------------------------------------------------------------
# Integration: concept_selection_with_benchmarks
# ---------------------------------------------------------------------------

class TestConceptSelectionWithBenchmarksIntegration:
    """Integration tests for the full pipeline (#2053)."""

    def test_benchmark_weights_affect_result(self, gom_projects):
        """Result with benchmarks should differ from pure deterministic."""
        pure = concept_selection(
            water_depth=1200,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
        )
        weighted = concept_selection_with_benchmarks(
            water_depth=1200,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
            benchmark_projects=gom_projects,
        )
        # Scores should differ (empirical weights add data-driven adjustment)
        pure_scores = {o.host_type.value: o.score for o in pure.ranked_options}
        weighted_scores = {o.host_type.value: o.score for o in weighted.ranked_options}
        differences = [
            abs(pure_scores.get(k, 0) - weighted_scores.get(k, 0))
            for k in set(pure_scores) | set(weighted_scores)
        ]
        assert any(d > 0.01 for d in differences), (
            "Expected empirical weights to shift at least one score"
        )

    def test_benchmark_result_valid_structure(self, gom_projects):
        """Benchmark-weighted result should have same structure as pure."""
        result = concept_selection_with_benchmarks(
            water_depth=1200,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
            benchmark_projects=gom_projects,
        )
        assert result.selected_host is not None
        assert len(result.ranked_options) > 0
        assert result.summary is not None

    def test_benchmark_summary_mentions_empirical(self, gom_projects):
        """Summary should note that SubseaIQ weights were applied."""
        result = concept_selection_with_benchmarks(
            water_depth=1200,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
            fluid_type="oil",
            benchmark_projects=gom_projects,
        )
        assert "SubseaIQ" in result.summary or "mpirical" in result.summary
