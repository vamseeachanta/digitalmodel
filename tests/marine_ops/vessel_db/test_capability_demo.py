"""Smoke test for the confidence-weighted capability-score demo."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path


def _load_demo():
    path = (Path(__file__).resolve().parents[3]
            / "examples" / "demos" / "installation" / "vessel_capability_score.py")
    spec = importlib.util.spec_from_file_location("vessel_capability_score", path)
    mod = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = mod
    spec.loader.exec_module(mod)
    return mod


def test_score_fleet_ranks_and_flags():
    demo = _load_demo()
    rows = demo.score_fleet(lift_te=3000.0, radius_m=35.0)
    assert rows, "no vessels scored"
    # sorted by score descending
    scores = [cs.score for *_, cs in rows]
    assert scores == sorted(scores, reverse=True)
    # a vessel that comfortably meets the lift with cited data should be defensible
    assert any(cs.defensible for *_, cs in rows)
    # an under-capacity vessel should be flagged non-defensible
    assert any((not cs.defensible) for *_, cs in rows)


def test_undercapacity_is_not_defensible():
    demo = _load_demo()
    # absurd lift -> nobody is defensible
    rows = demo.score_fleet(lift_te=50000.0, radius_m=30.0)
    assert all(not cs.defensible for *_, cs in rows)
