# ABOUTME: Feature extraction for dynacard ML-based diagnostics.
# ABOUTME: Implements Bezerra vertical projections and analysis-based features.

from typing import Optional
import numpy as np

from .models import CardData, AnalysisResults


class FeatureExtractor:
    """Extracts feature vectors from dynacard data for ML classification.

    Two feature groups:
    - Group A: Bezerra vertical projections (16 features) -- card shape only
    - Group B: Analysis features (9 features) -- requires AnalysisResults
    """

    @staticmethod
    def extract_bezerra_projections(
        card: CardData,
        n_bins: int = 8,
    ) -> np.ndarray:
        """Extract Bezerra-style vertical projection features.

        Splits the card into ascending (upstroke) and descending (downstroke)
        half-cycles, then computes mean normalized load in evenly-spaced
        position bins for each half.

        Args:
            card: Downhole card data (position + load arrays).
            n_bins: Number of position bins per half-cycle.

        Returns:
            Array of shape (2 * n_bins,) with values in [0, 1].
        """
        pos = np.array(card.position, dtype=np.float64)
        load = np.array(card.load, dtype=np.float64)
        n = len(pos)

        if n < 4:
            return np.zeros(2 * n_bins, dtype=np.float64)

        # Normalize load to [0, 1]
        load_min = np.min(load)
        load_max = np.max(load)
        load_range = load_max - load_min
        if load_range < 1e-10:
            return np.zeros(2 * n_bins, dtype=np.float64)
        load_norm = (load - load_min) / load_range

        # Normalize position to [0, 1]
        pos_min = np.min(pos)
        pos_max = np.max(pos)
        pos_range = pos_max - pos_min
        if pos_range < 1e-10:
            return np.zeros(2 * n_bins, dtype=np.float64)
        pos_norm = (pos - pos_min) / pos_range

        # Split into ascending and descending halves by detecting direction change
        # Find the index of maximum position (approximate top of stroke)
        peak_idx = np.argmax(pos)

        # Ascending: indices 0..peak_idx, Descending: peak_idx..end
        if peak_idx < 2:
            asc_idx = np.arange(0, n // 2)
            desc_idx = np.arange(n // 2, n)
        elif peak_idx >= n - 2:
            asc_idx = np.arange(0, n // 2)
            desc_idx = np.arange(n // 2, n)
        else:
            asc_idx = np.arange(0, peak_idx + 1)
            desc_idx = np.arange(peak_idx, n)

        # Bin each half-cycle
        asc_features = _bin_half_cycle(pos_norm[asc_idx], load_norm[asc_idx], n_bins)
        desc_features = _bin_half_cycle(pos_norm[desc_idx], load_norm[desc_idx], n_bins)

        return np.concatenate([asc_features, desc_features])

    @staticmethod
    def extract_analysis_features(results: AnalysisResults) -> np.ndarray:
        """Extract features from existing analysis results.

        Features (9 total):
            0: fillage (0-1)
            1: shape_similarity (0-1)
            2-5: zone_area_fractions (4 quadrant fractions)
            6: area_ratio = actual_area / ideal_area
            7: perimeter_ratio = perimeter / (2*(pos_range + load_range))
            8: sinusoidal_buckling_detected (0 or 1)

        Args:
            results: AnalysisResults from the dynacard analysis pipeline.

        Returns:
            Array of shape (9,).
        """
        features = np.zeros(9, dtype=np.float64)

        # Feature 0: fillage
        if results.fillage is not None:
            features[0] = np.clip(results.fillage.fillage, 0.0, 1.0)

        # Feature 1: shape_similarity
        if results.ideal_card is not None:
            features[1] = np.clip(results.ideal_card.shape_similarity, 0.0, 1.0)

        # Features 2-5: zone_area_fractions
        if results.card_geometry is not None and len(results.card_geometry.zone_area_fractions) == 4:
            for i in range(4):
                features[2 + i] = results.card_geometry.zone_area_fractions[i]

        # Feature 6: area_ratio
        if (
            results.card_geometry is not None
            and results.ideal_card is not None
            and results.ideal_card.ideal_card_area > 0
        ):
            features[6] = results.card_geometry.area / results.ideal_card.ideal_card_area
        elif results.card_geometry is not None:
            features[6] = 1.0  # default when no ideal card

        # Feature 7: perimeter_ratio
        if results.card_geometry is not None:
            geom = results.card_geometry
            denom = 2.0 * (geom.position_range + geom.load_range)
            if denom > 0:
                features[7] = geom.perimeter / denom

        # Feature 8: sinusoidal_buckling_detected
        if results.rod_buckling is not None:
            features[8] = 1.0 if results.rod_buckling.sinusoidal_buckling_detected else 0.0

        return features

    @staticmethod
    def extract_full_vector(
        card: CardData,
        results: Optional[AnalysisResults] = None,
        n_bins: int = 8,
    ) -> np.ndarray:
        """Extract full feature vector (16 or 25 features).

        Args:
            card: Downhole card data.
            results: Optional analysis results for extended features.
            n_bins: Bins per half-cycle for Bezerra projections.

        Returns:
            Array of shape (16,) or (25,) depending on whether results provided.
        """
        bezerra = FeatureExtractor.extract_bezerra_projections(card, n_bins=n_bins)
        if results is None:
            return bezerra
        analysis = FeatureExtractor.extract_analysis_features(results)
        return np.concatenate([bezerra, analysis])

    @staticmethod
    def normalize(
        features: np.ndarray,
        scaling: dict,
    ) -> np.ndarray:
        """Min-max normalize features using stored scaling bounds.

        Args:
            features: Raw feature vector.
            scaling: Dict with 'min' and 'max' arrays.

        Returns:
            Normalized features clipped to [0, 1].
        """
        feat_min = np.asarray(scaling["min"], dtype=np.float64)
        feat_max = np.asarray(scaling["max"], dtype=np.float64)
        denom = feat_max - feat_min
        # Avoid division by zero
        denom = np.where(denom < 1e-10, 1.0, denom)
        normalized = (features - feat_min) / denom
        # Zero out features where range was zero
        zero_range = (feat_max - feat_min) < 1e-10
        normalized = np.where(zero_range, 0.0, normalized)
        return np.clip(normalized, 0.0, 1.0)


def _bin_half_cycle(
    pos_norm: np.ndarray,
    load_norm: np.ndarray,
    n_bins: int,
) -> np.ndarray:
    """Compute mean normalized load in evenly-spaced position bins.

    Args:
        pos_norm: Normalized position [0, 1].
        load_norm: Normalized load [0, 1].
        n_bins: Number of position bins.

    Returns:
        Array of shape (n_bins,).
    """
    bin_edges = np.linspace(0.0, 1.0, n_bins + 1)
    result = np.zeros(n_bins, dtype=np.float64)

    for i in range(n_bins):
        low = bin_edges[i]
        high = bin_edges[i + 1]
        if i == n_bins - 1:
            mask = (pos_norm >= low) & (pos_norm <= high)
        else:
            mask = (pos_norm >= low) & (pos_norm < high)

        if np.any(mask):
            result[i] = np.mean(load_norm[mask])

    return result
