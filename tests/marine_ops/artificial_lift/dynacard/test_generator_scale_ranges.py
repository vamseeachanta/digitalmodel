# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# ABOUTME: Detects fault labels encoded in synthetic scale ranges.
# ABOUTME: Compares every scale feature over a fixed seed set.
"""Absolute scale shall not become a synthetic fault label by construction.

Exceptions must name one feature and one mode pair, with a physical reason and
an allowed source that establishes the separation. A class name, an unverified
catalogue unit or a normalised plate shape does not establish absolute limits.
"""

from itertools import combinations

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    ALL_GENERATORS,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.feature_extraction import (
    FeatureExtractor,
)

# The seed set matches the default samples_per_mode in generate_training_dataset.
SEEDS = tuple(range(200))
FEATURES = ("stroke_length", "load_range", "card_area")

# (feature, alphabetically ordered mode pair) -> (physical reason, citation).
# No allowed dimensioned source currently establishes an exception.
PHYSICAL_EXCEPTIONS = {}


def measure_scale_ranges():
    """Read extrema from the same extractor used by the shipped classifier."""
    assert len(ALL_GENERATORS) >= 2, "at least two modes are required"
    ranges = {}
    for mode, generator in ALL_GENERATORS.items():
        values = np.array([
            FeatureExtractor.extract_scale_features(generator(seed=seed))
            for seed in SEEDS
        ])
        assert values.shape == (len(SEEDS), len(FEATURES))
        assert np.isfinite(values).all(), f"non-finite scale features: {mode}"
        ranges[mode] = (values.min(axis=0), values.max(axis=0))
    return ranges


@pytest.fixture(scope="module")
def scale_ranges():
    return measure_scale_ranges()


@pytest.mark.parametrize("feature_index,feature", enumerate(FEATURES))
def test_generator_scale_ranges_overlap(scale_ranges, feature_index, feature):
    """Every disjoint pair needs a dimensioned physical basis."""
    disjoint = []
    for first, second in combinations(sorted(scale_ranges), 2):
        first_min, first_max = scale_ranges[first]
        second_min, second_max = scale_ranges[second]
        if (first_max[feature_index] < second_min[feature_index]
                or second_max[feature_index] < first_min[feature_index]):
            key = (feature, first, second)
            if key in PHYSICAL_EXCEPTIONS:
                reason, citation = PHYSICAL_EXCEPTIONS[key]
                assert reason.strip() and citation.strip(), key
            else:
                disjoint.append((first, second))
    assert not disjoint, (
        f"{feature}: {len(disjoint)} disjoint mode pairs without a physical "
        f"reason and citation: {disjoint}"
    )
