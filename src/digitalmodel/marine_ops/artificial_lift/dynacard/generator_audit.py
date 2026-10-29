# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Report sampled scale envelopes and disjoint mode pairs, without retraining.

Intervals describe observed extrema, not qualified operating envelopes or proof
of population separation. Flagged pairs require domain review, not auto-rejection.
"""

from collections.abc import Callable, Mapping
from itertools import combinations

import numpy as np

from .card_generators import ALL_GENERATORS
from .feature_extraction import FeatureExtractor
from .models import CardData


def audit_generator_scale_ranges(
    samples_per_mode: int = 200,
    *,
    generators: Mapping[str, Callable[..., CardData]] | None = None,
) -> dict:
    """Sample seeds ``0..samples_per_mode-1`` and report each scale feature.

    Metrics use the existing extractor: max(position)-min(position) [in],
    max(load)-min(load) [lbf], and absolute shoelace area [in*lbf]. These are
    geometric identities, not standards-derived limits. Two closed sampled
    envelopes are disjoint iff one's maximum is strictly below the other's
    minimum. Boundary contact counts as overlap. The default sample count is
    the training generator's workload, not an engineering acceptance threshold.
    """
    if type(samples_per_mode) is not int or samples_per_mode < 1:
        raise ValueError("samples_per_mode must be a positive integer")
    selected = ALL_GENERATORS if generators is None else generators
    if not selected:
        raise ValueError("at least one generator is required")
    features = ("stroke_length", "load_range", "card_area")
    ranges = {}
    for mode, generator in sorted(selected.items()):
        values = np.array(
            [
                FeatureExtractor.extract_scale_features(generator(seed=seed))
                for seed in range(samples_per_mode)
            ]
        )
        if not np.isfinite(values).all():
            raise ValueError(f"nonfinite scale feature for {mode}")
        ranges[mode] = {
            name: {"min": float(values[:, i].min()), "max": float(values[:, i].max())}
            for i, name in enumerate(features)
        }
    disjoint = {name: [] for name in features}
    for first, second in combinations(ranges, 2):
        for name in features:
            a, b = ranges[first][name], ranges[second][name]
            if a["max"] < b["min"] or b["max"] < a["min"]:
                disjoint[name].append([first, second])
    return {
        "samples_per_mode": samples_per_mode,
        "ranges": ranges,
        "disjoint_pairs": disjoint,
    }
