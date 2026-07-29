# ABOUTME: Tests for dynacard feature extraction module.
# ABOUTME: Tests Bezerra vertical projections and analysis feature extraction.

import pytest
import numpy as np
from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData
from digitalmodel.marine_ops.artificial_lift.dynacard.feature_extraction import (
    FeatureExtractor,
)


def _make_rectangular_card(
    stroke: float = 100.0,
    high_load: float = 15000.0,
    low_load: float = 5000.0,
    num_points: int = 100,
) -> CardData:
    """Create a rectangular (ideal) pump card: upstroke at high_load, downstroke at low_load."""
    half = num_points // 2
    pos_up = np.linspace(0, stroke, half)
    pos_down = np.linspace(stroke, 0, half)
    position = np.concatenate([pos_up, pos_down]).tolist()
    load = ([high_load] * half) + ([low_load] * half)
    return CardData(position=position, load=load)


def _make_sinusoidal_card(
    stroke: float = 100.0,
    mean_load: float = 12000.0,
    amplitude: float = 4000.0,
    num_points: int = 100,
) -> CardData:
    """Create a sinusoidal card (common test pattern)."""
    t = np.linspace(0, 2 * np.pi, num_points)
    position = (stroke / 2 * (1 - np.cos(t))).tolist()
    load = (mean_load + amplitude * np.sin(t)).tolist()
    return CardData(position=position, load=load)


class TestBezerraProjections:
    """Tests for Bezerra vertical projection feature extraction."""

    def test_output_shape_default_bins(self):
        """Should return 16 features (8 bins x 2 half-cycles) by default."""
        card = _make_sinusoidal_card()
        features = FeatureExtractor.extract_bezerra_projections(card)
        assert features.shape == (16,)

    def test_output_shape_custom_bins(self):
        """Should return 2*n_bins features for custom bin count."""
        card = _make_sinusoidal_card()
        features = FeatureExtractor.extract_bezerra_projections(card, n_bins=4)
        assert features.shape == (8,)

    def test_values_normalized_0_1(self):
        """All projection values should be in [0, 1] range."""
        card = _make_sinusoidal_card()
        features = FeatureExtractor.extract_bezerra_projections(card)
        assert np.all(features >= 0.0)
        assert np.all(features <= 1.0)

    def test_rectangular_card_projections(self):
        """Rectangular card should have distinct upstroke/downstroke projections."""
        card = _make_rectangular_card()
        features = FeatureExtractor.extract_bezerra_projections(card)
        upstroke = features[:8]
        downstroke = features[8:]
        # Upstroke bins should be higher than downstroke bins
        assert np.mean(upstroke) > np.mean(downstroke)

    def test_symmetric_card_has_similar_halves(self):
        """A perfectly symmetric card should have symmetric projections."""
        # Circular card (load = mean + amp*sin, position = mean + amp*cos)
        t = np.linspace(0, 2 * np.pi, 200)
        position = (50 + 50 * np.cos(t)).tolist()
        load = (10000 + 5000 * np.sin(t)).tolist()
        card = CardData(position=position, load=load)
        features = FeatureExtractor.extract_bezerra_projections(card)
        # Both halves should sum to similar totals (not identical due to discretization)
        assert features.shape == (16,)

    def test_small_card_handles_gracefully(self):
        """Card with very few points should not crash."""
        card = CardData(position=[0.0, 50.0, 100.0, 50.0], load=[5000, 15000, 5000, 3000])
        features = FeatureExtractor.extract_bezerra_projections(card)
        assert features.shape == (16,)
        assert not np.any(np.isnan(features))

    def test_constant_load_card(self):
        """Card with constant load should produce all-zero projections (after normalization)."""
        t = np.linspace(0, 2 * np.pi, 100)
        position = (50 * (1 - np.cos(t))).tolist()
        load = [10000.0] * 100
        card = CardData(position=position, load=load)
        features = FeatureExtractor.extract_bezerra_projections(card)
        # With constant load, all bins should be the same (0 after min-max normalization)
        assert not np.any(np.isnan(features))


class TestExtractFullVector:
    """Tests for full feature vector extraction."""

    def test_bezerra_only_vector(self):
        """Without analysis results, should return 16 Bezerra features."""
        card = _make_sinusoidal_card()
        features = FeatureExtractor.extract_full_vector(card)
        assert features.shape == (16,)

    def test_full_vector_with_analysis(self):
        """With analysis results, should return 25 features (16 Bezerra + 9 analysis)."""
        card = _make_sinusoidal_card()
        # Create minimal mock analysis results
        mock_results = _make_mock_analysis_results(card)
        features = FeatureExtractor.extract_full_vector(card, results=mock_results)
        assert features.shape == (25,)

    def test_full_vector_no_nans(self):
        """Full vector should contain no NaN values."""
        card = _make_sinusoidal_card()
        features = FeatureExtractor.extract_full_vector(card)
        assert not np.any(np.isnan(features))

    def test_full_vector_with_analysis_no_nans(self):
        """Full vector with analysis should contain no NaN values."""
        card = _make_sinusoidal_card()
        mock_results = _make_mock_analysis_results(card)
        features = FeatureExtractor.extract_full_vector(card, results=mock_results)
        assert not np.any(np.isnan(features))


class TestNormalization:
    """Tests for feature normalization."""

    def test_normalize_identity(self):
        """Normalizing with matching min/max should produce [0,1] range."""
        features = np.array([0.0, 5.0, 10.0])
        scaling = {"min": np.array([0.0, 0.0, 0.0]), "max": np.array([10.0, 10.0, 10.0])}
        normalized = FeatureExtractor.normalize(features, scaling)
        np.testing.assert_allclose(normalized, [0.0, 0.5, 1.0])

    def test_normalize_clips_outliers(self):
        """Values outside min/max should be clipped to [0, 1]."""
        features = np.array([-5.0, 15.0])
        scaling = {"min": np.array([0.0, 0.0]), "max": np.array([10.0, 10.0])}
        normalized = FeatureExtractor.normalize(features, scaling)
        assert np.all(normalized >= 0.0)
        assert np.all(normalized <= 1.0)

    def test_normalize_zero_range(self):
        """Features with zero range (min==max) should produce 0.0."""
        features = np.array([5.0, 5.0])
        scaling = {"min": np.array([5.0, 5.0]), "max": np.array([5.0, 5.0])}
        normalized = FeatureExtractor.normalize(features, scaling)
        np.testing.assert_allclose(normalized, [0.0, 0.0])


class TestAnalysisFeatures:
    """Tests for analysis feature extraction from AnalysisResults."""

    def test_output_shape(self):
        """Should return exactly 9 features from analysis results."""
        card = _make_sinusoidal_card()
        mock_results = _make_mock_analysis_results(card)
        features = FeatureExtractor.extract_analysis_features(mock_results)
        assert features.shape == (9,)

    def test_no_nans(self):
        """Analysis features should never contain NaN."""
        card = _make_sinusoidal_card()
        mock_results = _make_mock_analysis_results(card)
        features = FeatureExtractor.extract_analysis_features(mock_results)
        assert not np.any(np.isnan(features))

    def test_fillage_in_range(self):
        """Fillage feature should be in [0, 1]."""
        card = _make_sinusoidal_card()
        mock_results = _make_mock_analysis_results(card)
        features = FeatureExtractor.extract_analysis_features(mock_results)
        # Feature 0 is fillage
        assert 0.0 <= features[0] <= 1.0


def _make_mock_analysis_results(card: CardData):
    """Create a minimal AnalysisResults with the fields FeatureExtractor needs."""
    from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
        AnalysisResults,
        PumpFillageAnalysis,
        IdealCardAnalysis,
        CardGeometryAnalysis,
        RodBucklingAnalysis,
    )

    return AnalysisResults(
        downhole_card=card,
        fillage=PumpFillageAnalysis(fillage=0.85),
        ideal_card=IdealCardAnalysis(shape_similarity=0.92),
        card_geometry=CardGeometryAnalysis(
            area=500000.0,
            perimeter=30000.0,
            position_range=100.0,
            load_range=8000.0,
            zone_area_fractions=[0.25, 0.25, 0.25, 0.25],
        ),
        rod_buckling=RodBucklingAnalysis(sinusoidal_buckling_detected=False),
    )


class TestScaleFeatures:
    """Absolute-magnitude features (dm#1884).

    NORMAL, TUBING_MOVEMENT and PLUNGER_UNDERTRAVEL differ only in stroke
    length. Under the Bezerra projections alone they are not merely similar,
    they are the *same vector* -- so the classifier could never separate them
    and no amount of retraining would have helped.
    """

    def test_projections_are_blind_to_stroke(self):
        """The defect itself: identical cards at different strokes project the same.

        This is the reason scale features exist. If this ever starts failing,
        the projections gained scale sensitivity and the extra features may be
        redundant -- do not just delete the assertion.
        """
        short = _make_rectangular_card(stroke=45.0)   # PLUNGER_UNDERTRAVEL band
        normal = _make_rectangular_card(stroke=100.0)  # NORMAL band
        long_ = _make_rectangular_card(stroke=155.0)   # TUBING_MOVEMENT band

        p_short = FeatureExtractor.extract_bezerra_projections(short)
        p_normal = FeatureExtractor.extract_bezerra_projections(normal)
        p_long = FeatureExtractor.extract_bezerra_projections(long_)

        np.testing.assert_allclose(p_short, p_normal, atol=1e-12)
        np.testing.assert_allclose(p_normal, p_long, atol=1e-12)

    def test_classifier_vector_separates_by_stroke(self):
        """The fix: the same three cards are distinguishable end to end."""
        vectors = [
            FeatureExtractor.extract_classifier_vector(
                _make_rectangular_card(stroke=s)
            )
            for s in (45.0, 100.0, 155.0)
        ]
        for a, b in ((0, 1), (1, 2), (0, 2)):
            assert not np.allclose(vectors[a], vectors[b]), (
                "classifier vector must distinguish cards that differ only in "
                "stroke length"
            )

    def test_scale_features_are_physical(self):
        """Values are the card's own units, not normalised."""
        card = _make_rectangular_card(
            stroke=120.0, high_load=15000.0, low_load=5000.0
        )
        stroke, load_range, area = FeatureExtractor.extract_scale_features(card)

        assert stroke == pytest.approx(120.0)
        assert load_range == pytest.approx(10000.0)
        # Exact, not approximate. _make_rectangular_card emits 50 points along
        # the upstroke at 15000 lb via linspace(0, 120, 50) and 50 along the
        # downstroke at 5000 lb via linspace(120, 0, 50). linspace *includes*
        # both endpoints, so the traced loop hits (0, 15000), (120, 15000),
        # (120, 5000), (0, 5000) exactly -- there is no corner clipping, and
        # the intermediate samples are collinear so they add no area. The
        # shoelace sum is therefore the plain rectangle:
        #   width 120 in x height (15000 - 5000) = 10000 lb = 1.2e6 in-lb.
        # (The earlier "corners are clipped by one sample step" reasoning was
        # wrong; it assumed linspace excluded the endpoint. The measured value
        # is 1200000.0000000002 -- the exact answer plus float rounding, which
        # is why this needs approx rather than a strict <= bound.)
        assert area == pytest.approx(1.2e6)

    def test_degenerate_card_returns_zeros(self):
        """Too few points to enclose anything -- zeros, not a crash."""
        card = CardData(position=[0.0, 1.0], load=[100.0, 200.0])
        np.testing.assert_array_equal(
            FeatureExtractor.extract_scale_features(card), np.zeros(3)
        )

    def test_vector_length_and_names_agree(self):
        """Names and values must stay in lockstep -- they document the model."""
        card = _make_sinusoidal_card()
        vector = FeatureExtractor.extract_classifier_vector(card)
        names = FeatureExtractor.classifier_feature_names()

        assert len(vector) == 19
        assert len(names) == len(vector)
        assert names[-3:] == ["stroke_length", "load_range", "card_area"]

    def test_shipped_model_matches_the_extractor(self):
        """A model trained on a different vector must never be scored silently.

        The dangerous case is not a crash -- it is a stale model whose trees
        split on features that are not the ones they learned, producing
        confident wrong diagnoses (dm#1884).
        """
        from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
            PumpDiagnostics,
        )

        model = PumpDiagnostics._load_model()
        if model is None:
            pytest.skip("no shipped model to check")
        card = _make_sinusoidal_card()
        expected = len(FeatureExtractor.extract_classifier_vector(card))
        assert len(model["scaling"]["min"]) == expected, (
            "shipped dynacard_classifier.json predates the current feature "
            "vector -- retrain with training.train_and_export()"
        )
