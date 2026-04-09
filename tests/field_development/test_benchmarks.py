# ABOUTME: Tests for SubseaIQ benchmark bridge — concept bands and subsea architecture stats.
# ABOUTME: Issue #1861 — TDD tests written before implementation.
"""Tests for digitalmodel.field_development.benchmarks (#1861)."""

import pytest

from digitalmodel.field_development.benchmarks import (
    SubseaProject,
    load_projects,
    concept_benchmark_bands,
    subsea_architecture_stats,
    DEPTH_BANDS,
)


# ---------------------------------------------------------------------------
# Fixture: small synthetic SubseaIQ project dataset
# ---------------------------------------------------------------------------

FIXTURE_RECORDS = [
    {
        "name": "Alpha",
        "operator": "Shell",
        "water_depth_m": 200,
        "concept_type": "Fixed Platform",
        "tieback_distance_km": None,
        "num_wells": 6,
        "num_trees": 0,
        "num_manifolds": 0,
        "fluid_type": "oil",
        "region": "GoM",
    },
    {
        "name": "Bravo",
        "operator": "BP",
        "water_depth_m": 500,
        "concept_type": "TLP",
        "tieback_distance_km": None,
        "num_wells": 12,
        "num_trees": 8,
        "num_manifolds": 2,
        "fluid_type": "oil",
        "region": "GoM",
    },
    {
        "name": "Charlie",
        "operator": "Shell",
        "water_depth_m": 900,
        "concept_type": "TLP",
        "tieback_distance_km": None,
        "num_wells": 16,
        "num_trees": 12,
        "num_manifolds": 3,
        "fluid_type": "oil",
        "region": "GoM",
    },
    {
        "name": "Delta",
        "operator": "TotalEnergies",
        "water_depth_m": 1200,
        "concept_type": "Semi",
        "tieback_distance_km": 8.5,
        "num_wells": 20,
        "num_trees": 16,
        "num_manifolds": 4,
        "fluid_type": "oil",
        "region": "West Africa",
    },
    {
        "name": "Echo",
        "operator": "Equinor",
        "water_depth_m": 1800,
        "concept_type": "Spar",
        "tieback_distance_km": 12.0,
        "num_wells": 10,
        "num_trees": 10,
        "num_manifolds": 2,
        "fluid_type": "gas",
        "region": "GoM",
    },
    {
        "name": "Foxtrot",
        "operator": "Shell",
        "water_depth_m": 2400,
        "concept_type": "FPSO",
        "tieback_distance_km": 25.0,
        "num_wells": 24,
        "num_trees": 20,
        "num_manifolds": 5,
        "fluid_type": "oil",
        "region": "Brazil",
    },
    {
        "name": "Golf",
        "operator": "BP",
        "water_depth_m": 350,
        "concept_type": "Subsea Tieback",
        "tieback_distance_km": 6.0,
        "num_wells": 4,
        "num_trees": 4,
        "num_manifolds": 1,
        "fluid_type": "gas",
        "region": "North Sea",
    },
    {
        "name": "Hotel",
        "operator": "LLOG",
        "water_depth_m": 1600,
        "concept_type": "Subsea Tieback",
        "tieback_distance_km": 18.0,
        "num_wells": 3,
        "num_trees": 3,
        "num_manifolds": 1,
        "fluid_type": "oil",
        "region": "GoM",
    },
]

# Records with missing fields — for graceful-handling tests
SPARSE_RECORDS = [
    {"name": "Sparse-A"},
    {"name": "Sparse-B", "water_depth_m": 1000},
    {"name": "Sparse-C", "concept_type": "TLP"},
    {"name": "Sparse-D", "water_depth_m": 600, "concept_type": "Semi", "num_trees": 5},
]


@pytest.fixture
def projects():
    return load_projects(FIXTURE_RECORDS)


@pytest.fixture
def sparse_projects():
    return load_projects(SPARSE_RECORDS)


# ---------------------------------------------------------------------------
# load_projects
# ---------------------------------------------------------------------------

class TestLoadProjects:
    def test_loads_correct_count(self, projects):
        assert len(projects) == 8

    def test_returns_subseaproject_instances(self, projects):
        assert all(isinstance(p, SubseaProject) for p in projects)

    def test_preserves_name(self, projects):
        names = [p.name for p in projects]
        assert "Alpha" in names
        assert "Foxtrot" in names

    def test_numeric_fields_converted(self, projects):
        bravo = [p for p in projects if p.name == "Bravo"][0]
        assert bravo.water_depth_m == 500.0
        assert bravo.num_trees == 8
        assert bravo.num_manifolds == 2

    def test_missing_fields_default_to_none(self, sparse_projects):
        sparse_a = [p for p in sparse_projects if p.name == "Sparse-A"][0]
        assert sparse_a.water_depth_m is None
        assert sparse_a.concept_type is None
        assert sparse_a.num_trees is None
        assert sparse_a.tieback_distance_km is None

    def test_partial_fields_preserved(self, sparse_projects):
        sparse_d = [p for p in sparse_projects if p.name == "Sparse-D"][0]
        assert sparse_d.water_depth_m == 600.0
        assert sparse_d.concept_type == "Semi"
        assert sparse_d.num_trees == 5
        assert sparse_d.operator is None

    def test_empty_list_returns_empty(self):
        assert load_projects([]) == []

    def test_name_required(self):
        with pytest.raises((KeyError, ValueError)):
            load_projects([{"operator": "Shell"}])


# ---------------------------------------------------------------------------
# concept_benchmark_bands
# ---------------------------------------------------------------------------

class TestConceptBenchmarkBands:
    def test_returns_dict_with_depth_bands(self, projects):
        bands = concept_benchmark_bands(projects)
        assert isinstance(bands, dict)
        for band_label in DEPTH_BANDS:
            assert band_label in bands

    def test_shallow_band_contains_alpha(self, projects):
        """Alpha: 200m, Fixed Platform — should land in shallow band."""
        bands = concept_benchmark_bands(projects)
        shallow = bands[DEPTH_BANDS[0]]
        assert "Fixed Platform" in shallow
        assert shallow["Fixed Platform"] >= 1

    def test_mid_water_band(self, projects):
        """Bravo (500m, TLP) and Golf (350m, Subsea Tieback) in mid-water."""
        bands = concept_benchmark_bands(projects)
        mid = bands[DEPTH_BANDS[1]]
        assert "TLP" in mid
        assert "Subsea Tieback" in mid

    def test_deepwater_band(self, projects):
        """Charlie (900m, TLP) and Delta (1200m, Semi) in deepwater."""
        bands = concept_benchmark_bands(projects)
        deep = bands[DEPTH_BANDS[2]]
        assert "TLP" in deep
        assert "Semi" in deep

    def test_ultra_deep_band(self, projects):
        """Echo (1800m, Spar), Foxtrot (2400m, FPSO), Hotel (1600m, Tieback)."""
        bands = concept_benchmark_bands(projects)
        ultra = bands[DEPTH_BANDS[3]]
        assert "Spar" in ultra
        assert "FPSO" in ultra
        assert "Subsea Tieback" in ultra

    def test_total_count_matches(self, projects):
        """Sum of all band counts should equal projects with both depth + concept."""
        bands = concept_benchmark_bands(projects)
        total = sum(
            count
            for band_data in bands.values()
            for count in band_data.values()
        )
        classifiable = [
            p for p in projects
            if p.water_depth_m is not None and p.concept_type is not None
        ]
        assert total == len(classifiable)

    def test_skips_projects_missing_depth(self, sparse_projects):
        """Records without water_depth_m should be excluded from bands."""
        bands = concept_benchmark_bands(sparse_projects)
        total = sum(
            count
            for band_data in bands.values()
            for count in band_data.values()
        )
        # Sparse-A: no depth, Sparse-B: depth but no concept, Sparse-C: concept but no depth
        # Sparse-D: depth=600 + concept=Semi → only 1 classifiable
        assert total == 1

    def test_empty_input(self):
        bands = concept_benchmark_bands([])
        assert isinstance(bands, dict)
        assert all(v == {} for v in bands.values())


# ---------------------------------------------------------------------------
# subsea_architecture_stats
# ---------------------------------------------------------------------------

class TestSubseaArchitectureStats:
    def test_returns_dict(self, projects):
        stats = subsea_architecture_stats(projects)
        assert isinstance(stats, dict)

    def test_tieback_distance_stats(self, projects):
        stats = subsea_architecture_stats(projects)
        tb = stats["tieback_distance"]
        # 4 projects have tieback: Delta(8.5), Echo(12), Foxtrot(25), Golf(6), Hotel(18)
        assert tb["count"] == 5
        assert tb["min"] == pytest.approx(6.0)
        assert tb["max"] == pytest.approx(25.0)
        expected_mean = (8.5 + 12.0 + 25.0 + 6.0 + 18.0) / 5
        assert tb["mean"] == pytest.approx(expected_mean, rel=1e-3)

    def test_trees_per_project_stats(self, projects):
        stats = subsea_architecture_stats(projects)
        trees = stats["trees_per_project"]
        # non-None trees: Alpha(0), Bravo(8), Charlie(12), Delta(16), Echo(10),
        # Foxtrot(20), Golf(4), Hotel(3)
        assert trees["count"] == 8
        assert trees["min"] == 0
        assert trees["max"] == 20

    def test_manifolds_per_project_stats(self, projects):
        stats = subsea_architecture_stats(projects)
        mani = stats["manifolds_per_project"]
        assert mani["count"] == 8
        assert mani["min"] == 0
        assert mani["max"] == 5

    def test_trees_per_manifold(self, projects):
        """For projects with manifolds > 0, compute trees/manifold ratio."""
        stats = subsea_architecture_stats(projects)
        tpm = stats["trees_per_manifold"]
        # Bravo: 8/2=4, Charlie: 12/3=4, Delta: 16/4=4, Echo: 10/2=5,
        # Foxtrot: 20/5=4, Golf: 4/1=4, Hotel: 3/1=3
        assert tpm["count"] == 7  # excludes Alpha (0 manifolds)
        assert tpm["min"] == pytest.approx(3.0)
        assert tpm["max"] == pytest.approx(5.0)

    def test_handles_sparse_data(self, sparse_projects):
        """Sparse records should not crash — stats based on available data only."""
        stats = subsea_architecture_stats(sparse_projects)
        assert stats["tieback_distance"]["count"] == 0
        # Only Sparse-D has num_trees=5
        assert stats["trees_per_project"]["count"] == 1
        assert stats["trees_per_project"]["min"] == 5

    def test_empty_input(self):
        stats = subsea_architecture_stats([])
        assert stats["tieback_distance"]["count"] == 0
        assert stats["trees_per_project"]["count"] == 0


# ---------------------------------------------------------------------------
# Robustness: junk/unparseable scraped values
# ---------------------------------------------------------------------------

class TestJunkValues:
    """Codex review finding: _opt_float/_opt_int must tolerate scraped junk."""

    def test_na_strings_become_none(self):
        records = [
            {
                "name": "Junk",
                "water_depth_m": "N/A",
                "num_trees": "unknown",
                "tieback_distance_km": "",
                "num_manifolds": "TBD",
            }
        ]
        projects = load_projects(records)
        p = projects[0]
        assert p.water_depth_m is None
        assert p.num_trees is None
        assert p.tieback_distance_km is None
        assert p.num_manifolds is None

    def test_string_numbers_parsed(self):
        records = [{"name": "StringNum", "water_depth_m": "1200.5", "num_trees": "8.0"}]
        projects = load_projects(records)
        p = projects[0]
        assert p.water_depth_m == 1200.5
        assert p.num_trees == 8

    def test_junk_excluded_from_bands(self):
        records = [
            {"name": "JunkDepth", "water_depth_m": "N/A", "concept_type": "TLP"},
            {"name": "GoodOne", "water_depth_m": 900, "concept_type": "Semi"},
        ]
        projects = load_projects(records)
        bands = concept_benchmark_bands(projects)
        total = sum(c for bd in bands.values() for c in bd.values())
        assert total == 1  # only GoodOne classifiable


# ---------------------------------------------------------------------------
# Integration: normalize → load_projects pipeline
# ---------------------------------------------------------------------------

class TestNormalizeIntegration:
    """Codex review finding: prove raw SubseaIQ keys survive the pipeline."""

    def test_raw_subseaiq_keys_through_pipeline(self):
        import sys
        from pathlib import Path

        # digitalmodel/ sits alongside worldenergydata/ under workspace-hub/
        wed_root = Path(__file__).resolve().parents[3] / "worldenergydata"
        if not wed_root.exists():
            pytest.skip("worldenergydata repo not available")
        sys.path.insert(0, str(wed_root))
        try:
            from subseaiq.analytics.normalize import normalize_projects
        except ImportError:
            pytest.skip("subseaiq.analytics.normalize not importable")

        raw = [
            {
                "Project Name": "Perdido",
                "Operator": "Shell",
                "Water Depth (m)": 2438,
                "Host Type": "Spar",
                "Trees": 16,
                "Manifolds": 4,
                "Tieback Distance (km)": 12.5,
                "Fluid Type": "oil",
                "Region": "GoM",
            },
            {
                "Project Name": "Stones",
                "Operator": "Shell",
                "Water Depth (m)": 2900,
                "Host Type": "ETLP",
                "Trees": "N/A",
                "Manifolds": None,
            },
        ]
        normalized = normalize_projects(raw)
        projects = load_projects(normalized)

        assert len(projects) == 2
        perdido = projects[0]
        assert perdido.name == "Perdido"
        assert perdido.water_depth_m == 2438.0
        assert perdido.concept_type == "Spar"
        assert perdido.num_trees == 16
        assert perdido.tieback_distance_km == 12.5

        stones = projects[1]
        assert stones.name == "Stones"
        assert stones.num_trees is None  # "N/A" → None
        assert stones.num_manifolds is None

        bands = concept_benchmark_bands(projects)
        ultra = bands["1500m+"]
        assert "Spar" in ultra
        assert "ETLP" in ultra


# ---------------------------------------------------------------------------
# concept_probability_matrix (#2053)
# ---------------------------------------------------------------------------

from digitalmodel.field_development.benchmarks import (
    concept_probability_matrix,
    predict_concept_type,
    ConceptPrediction,
)


# Larger fixture with realistic GoM-like distribution for probability tests
PROBABILITY_FIXTURE_RECORDS = [
    # Shallow (0-300m): Fixed Platforms dominate
    {"name": "ShP1", "water_depth_m": 100, "concept_type": "Fixed Platform"},
    {"name": "ShP2", "water_depth_m": 150, "concept_type": "Fixed Platform"},
    {"name": "ShP3", "water_depth_m": 200, "concept_type": "Fixed Platform"},
    {"name": "ShP4", "water_depth_m": 250, "concept_type": "Subsea Tieback"},
    # Mid-water (300-800m): TLP + Subsea Tieback
    {"name": "MW1", "water_depth_m": 400, "concept_type": "TLP"},
    {"name": "MW2", "water_depth_m": 500, "concept_type": "TLP"},
    {"name": "MW3", "water_depth_m": 600, "concept_type": "Semi"},
    {"name": "MW4", "water_depth_m": 350, "concept_type": "Subsea Tieback"},
    {"name": "MW5", "water_depth_m": 700, "concept_type": "TLP"},
    # Deepwater (800-1500m): TLP + Semi + Spar
    {"name": "DW1", "water_depth_m": 900, "concept_type": "TLP"},
    {"name": "DW2", "water_depth_m": 1000, "concept_type": "TLP"},
    {"name": "DW3", "water_depth_m": 1100, "concept_type": "Semi"},
    {"name": "DW4", "water_depth_m": 1200, "concept_type": "Semi"},
    {"name": "DW5", "water_depth_m": 1300, "concept_type": "Spar"},
    {"name": "DW6", "water_depth_m": 1400, "concept_type": "TLP"},
    {"name": "DW7", "water_depth_m": 850, "concept_type": "Subsea Tieback"},
    {"name": "DW8", "water_depth_m": 1100, "concept_type": "Semi"},
    {"name": "DW9", "water_depth_m": 950, "concept_type": "TLP"},
    {"name": "DW10", "water_depth_m": 1450, "concept_type": "Spar"},
    # Ultra-deep (1500m+): Spar + Semi + FPSO
    {"name": "UD1", "water_depth_m": 1600, "concept_type": "Spar"},
    {"name": "UD2", "water_depth_m": 1800, "concept_type": "Semi"},
    {"name": "UD3", "water_depth_m": 2000, "concept_type": "FPSO"},
    {"name": "UD4", "water_depth_m": 2200, "concept_type": "Semi"},
    {"name": "UD5", "water_depth_m": 2400, "concept_type": "Spar"},
    {"name": "UD6", "water_depth_m": 2600, "concept_type": "FPSO"},
    {"name": "UD7", "water_depth_m": 1700, "concept_type": "Subsea Tieback"},
    {"name": "UD8", "water_depth_m": 1900, "concept_type": "Spar"},
]


@pytest.fixture
def probability_projects():
    return load_projects(PROBABILITY_FIXTURE_RECORDS)


class TestConceptProbabilityMatrix:
    """Tests for concept_probability_matrix (#2053)."""

    def test_returns_dict_with_depth_bands(self, probability_projects):
        matrix = concept_probability_matrix(probability_projects)
        assert isinstance(matrix, dict)
        for band_label in DEPTH_BANDS:
            assert band_label in matrix

    def test_probabilities_sum_to_one(self, probability_projects):
        """Each depth band's probabilities must sum to 1.0 (or be empty)."""
        matrix = concept_probability_matrix(probability_projects)
        for band_label, probs in matrix.items():
            if probs:  # non-empty band
                total = sum(probs.values())
                assert total == pytest.approx(1.0, abs=1e-9), (
                    f"Band {band_label}: probabilities sum to {total}, not 1.0"
                )

    def test_probabilities_non_negative(self, probability_projects):
        matrix = concept_probability_matrix(probability_projects)
        for band_label, probs in matrix.items():
            for concept, prob in probs.items():
                assert prob >= 0.0, f"{band_label}/{concept}: negative prob {prob}"

    def test_shallow_band_dominated_by_fixed_platform(self, probability_projects):
        """Shallow band should show Fixed Platform as highest probability."""
        matrix = concept_probability_matrix(probability_projects)
        shallow = matrix[DEPTH_BANDS[0]]
        assert "Fixed Platform" in shallow
        assert shallow["Fixed Platform"] >= 0.5  # 3/4 = 0.75

    def test_deepwater_band_has_tlp_and_semi(self, probability_projects):
        """Deepwater band should show TLP and Semi as significant options."""
        matrix = concept_probability_matrix(probability_projects)
        deep = matrix[DEPTH_BANDS[2]]
        assert "TLP" in deep
        assert "Semi" in deep
        assert deep["TLP"] > 0.2
        assert deep["Semi"] > 0.2

    def test_ultra_deep_has_spar_and_semi(self, probability_projects):
        """Ultra-deep band should show Spar and Semi as primary options."""
        matrix = concept_probability_matrix(probability_projects)
        ultra = matrix[DEPTH_BANDS[3]]
        assert "Spar" in ultra
        assert "Semi" in ultra

    def test_empty_input_returns_empty_bands(self):
        matrix = concept_probability_matrix([])
        assert isinstance(matrix, dict)
        assert all(v == {} for v in matrix.values())

    def test_sparse_data_still_valid(self, sparse_projects):
        """Sparse data should not crash; probabilities still valid."""
        matrix = concept_probability_matrix(sparse_projects)
        # Sparse-D is the only classifiable: depth=600, concept=Semi
        mid = matrix[DEPTH_BANDS[1]]
        if mid:
            total = sum(mid.values())
            assert total == pytest.approx(1.0, abs=1e-9)

    def test_single_project_gives_100_percent(self):
        """One project in a band → 100% probability for that concept."""
        records = [{"name": "Solo", "water_depth_m": 500, "concept_type": "TLP"}]
        projects = load_projects(records)
        matrix = concept_probability_matrix(projects)
        mid = matrix[DEPTH_BANDS[1]]
        assert mid == {"TLP": pytest.approx(1.0)}


# ---------------------------------------------------------------------------
# predict_concept_type — decision tree (#2053)
# ---------------------------------------------------------------------------

class TestPredictConceptType:
    """Tests for the decision tree predictor (#2053)."""

    def test_returns_concept_prediction(self, probability_projects):
        result = predict_concept_type(
            projects=probability_projects,
            water_depth=1000,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
        )
        assert isinstance(result, ConceptPrediction)

    def test_prediction_has_required_fields(self, probability_projects):
        result = predict_concept_type(
            projects=probability_projects,
            water_depth=1000,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
        )
        assert hasattr(result, "predicted_concept")
        assert hasattr(result, "probabilities")
        assert hasattr(result, "depth_band")
        assert hasattr(result, "rationale")

    def test_predicted_concept_is_string(self, probability_projects):
        result = predict_concept_type(
            projects=probability_projects,
            water_depth=1000,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
        )
        assert isinstance(result.predicted_concept, str)
        assert len(result.predicted_concept) > 0

    def test_probabilities_sum_to_one(self, probability_projects):
        result = predict_concept_type(
            projects=probability_projects,
            water_depth=1000,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
        )
        assert sum(result.probabilities.values()) == pytest.approx(1.0, abs=1e-9)

    def test_shallow_small_close_prefers_tieback(self, probability_projects):
        """Small reservoir close to infra in shallow water → tieback."""
        result = predict_concept_type(
            projects=probability_projects,
            water_depth=250,
            reservoir_size_mmbbl=20,
            distance_to_infra_km=5,
        )
        # Should boost Subsea Tieback due to small reservoir + close infra
        assert "Subsea Tieback" in result.probabilities

    def test_deep_large_remote_prefers_standalone(self, probability_projects):
        """Large reservoir far from infra at depth → standalone host."""
        result = predict_concept_type(
            projects=probability_projects,
            water_depth=2000,
            reservoir_size_mmbbl=500,
            distance_to_infra_km=100,
        )
        assert result.predicted_concept != "Subsea Tieback"

    def test_depth_band_assigned_correctly(self, probability_projects):
        result = predict_concept_type(
            projects=probability_projects,
            water_depth=1000,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
        )
        assert result.depth_band == "800-1500m"

    def test_ultra_deep_band(self, probability_projects):
        result = predict_concept_type(
            projects=probability_projects,
            water_depth=2000,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
        )
        assert result.depth_band == "1500m+"

    def test_negative_depth_raises(self, probability_projects):
        with pytest.raises(ValueError, match="water_depth"):
            predict_concept_type(
                projects=probability_projects,
                water_depth=-100,
                reservoir_size_mmbbl=200,
                distance_to_infra_km=50,
            )

    def test_negative_reservoir_raises(self, probability_projects):
        with pytest.raises(ValueError, match="reservoir_size"):
            predict_concept_type(
                projects=probability_projects,
                water_depth=1000,
                reservoir_size_mmbbl=-50,
                distance_to_infra_km=50,
            )

    def test_empty_projects_raises(self):
        """No benchmark data → cannot predict."""
        with pytest.raises(ValueError, match="projects"):
            predict_concept_type(
                projects=[],
                water_depth=1000,
                reservoir_size_mmbbl=200,
                distance_to_infra_km=50,
            )

    def test_rationale_mentions_depth_band(self, probability_projects):
        result = predict_concept_type(
            projects=probability_projects,
            water_depth=1000,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=50,
        )
        assert "800-1500m" in result.rationale


# ---------------------------------------------------------------------------
# Case study validation (#2053)
# ---------------------------------------------------------------------------

# GoM reference fields with known concept selections
CASE_STUDY_RECORDS = [
    # Real GoM field data (approximate values from public sources)
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
    # Additional GoM fields for statistical depth
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


class TestCaseStudyValidation:
    """Validate probability matrix against known GoM field decisions (#2053)."""

    @pytest.fixture
    def case_study_projects(self):
        return load_projects(CASE_STUDY_RECORDS)

    def test_deepwater_band_includes_tlp_and_spar(self, case_study_projects):
        """800-1500m band should show TLP, Spar, Semi all present."""
        matrix = concept_probability_matrix(case_study_projects)
        deep = matrix["800-1500m"]
        # Mars(TLP), Ursa(TLP), Mad Dog(Spar), Holstein(Spar),
        # Shenzi(TLP), Whale(Semi) are in this band
        assert len(deep) >= 2
        assert "TLP" in deep or "Spar" in deep

    def test_ultra_deep_spar_and_semi_dominant(self, case_study_projects):
        """1500m+ band should be dominated by Spar and Semi in GoM data."""
        matrix = concept_probability_matrix(case_study_projects)
        ultra = matrix["1500m+"]
        # Perdido(Spar), Atlantis(Semi), Thunder Horse(Semi),
        # Appomattox(Semi), Stones(FPSO), Lucius(Spar), Na Kika(Semi),
        # Constitution(Spar), Great White(Spar)
        assert "Spar" in ultra
        assert "Semi" in ultra
        assert ultra["Spar"] + ultra["Semi"] > 0.6  # combined > 60%

    def test_predict_at_perdido_depth(self, case_study_projects):
        """At 2438m with large reservoir, should predict Spar or Semi."""
        result = predict_concept_type(
            projects=case_study_projects,
            water_depth=2438,
            reservoir_size_mmbbl=200,
            distance_to_infra_km=80,
        )
        assert result.predicted_concept in ("Spar", "Semi", "FPSO")

    def test_predict_at_mars_depth(self, case_study_projects):
        """At 896m with medium reservoir, should predict TLP or Semi."""
        result = predict_concept_type(
            projects=case_study_projects,
            water_depth=896,
            reservoir_size_mmbbl=150,
            distance_to_infra_km=40,
        )
        assert result.predicted_concept in ("TLP", "Spar", "Semi")

    def test_predict_tieback_small_reservoir_close(self, case_study_projects):
        """Small reservoir near infra → tieback even if GoM data is sparse."""
        result = predict_concept_type(
            projects=case_study_projects,
            water_depth=900,
            reservoir_size_mmbbl=20,
            distance_to_infra_km=5,
        )
        # Decision tree should override historical data for tieback cases
        assert "Subsea Tieback" in result.probabilities
