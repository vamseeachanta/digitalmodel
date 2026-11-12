"""The documented tug-span check must report, not pass silently (#3987).

``ProjectInputSpec.validate_consistency`` lists "Tug positions relative to
pipeline length" among its checks but executed ``pass``. A tug spread ending
beyond 1.5 times the pipeline length (allowed, e.g. staged installation) now
raises a ``UserWarning`` naming both lengths.
"""

from __future__ import annotations

import warnings

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


def _spec(count: int, spacing: float, first_x: float = 0.0) -> dict:
    return {
        "metadata": {"name": "t", "description": "t", "structure": "pipeline",
                     "operation": "installation", "project": "T"},
        "environment": {"water": {"depth": 10, "density": 1.025},
                        "seabed": {"stiffness": {"normal": 100, "shear": 50}}},
        "pipeline": {
            "name": "pipe", "material": "X65",
            "dimensions": {"outer_diameter": 0.762, "wall_thickness": 0.0254},
            "coatings": {"corrosion": {"thickness": 0.003, "density": 1.1}},
            "segments": [{"type": "X65", "length": 1000, "segment_length": 5}],
        },
        "equipment": {"tugs": {"count": count, "spacing": spacing,
                               "first_position": [first_x, 50, 0],
                               "properties": {"mass": 30, "volume": 100}}},
    }


def test_tug_spread_beyond_one_and_a_half_lengths_warns():
    # Last tug at 0 + 4 x 400 = 1600 m > 1.5 x 1000 m.
    with pytest.warns(UserWarning, match=r"1600.*1000"):
        ProjectInputSpec(**_spec(count=5, spacing=400))


def test_tug_spread_within_limit_is_silent():
    # Last tug at 0 + 4 x 300 = 1200 m <= 1500 m.
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        ProjectInputSpec(**_spec(count=5, spacing=300))


def test_tug_spread_at_exactly_one_and_a_half_lengths_is_silent():
    # Last tug at 0 + 3 x 500 = 1500 m = 1.5 x 1000 m; the check is strict (>).
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        ProjectInputSpec(**_spec(count=4, spacing=500))
