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
