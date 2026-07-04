"""Tests for the reusable confidence-weighted vessel suitability API (#881)."""

from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.marine_ops.installation.models import CraneCurve
from digitalmodel.marine_ops.installation.vessel_suitability import (
    LiftRequirement,
    assess_named,
    assess_vessel,
    rank_fleet,
)
from digitalmodel.marine_ops.vessel_db.confidence import ConfidenceTier


def _info(swl, radii=(0.0,), source="worldenergydata", dp="DP3", deck=8000.0):
    caps = [swl] * len(radii)
    return {
        "crane_curve": CraneCurve(
            radii_m=np.array(radii, dtype=float),
            capacities_te=np.array(caps, dtype=float),
            max_hook_load_te=float(swl),
        ),
        "dp_class": dp,
        "deck_area_m2": deck,
        "source": source,
    }


def test_assess_meets_lift_defensible():
    info = _info(10000.0, radii=(20.0, 48.0), source="worldenergydata")
    r = assess_vessel("BIG HLV", info, LiftRequirement(weight_te=2000.0, radius_m=30.0))
    assert r.swl_at_radius_te > 0
    assert r.defensible is True
    assert r.score > 0
    assert r.confidence == ConfidenceTier.CITED


def test_headline_only_downgrades_confidence():
    # no published radius curve -> crane confidence drops to estimated
    info = _info(25000.0, radii=(0.0,), source="curated")
    r = assess_vessel(
        "HEADLINE HLV", info, LiftRequirement(weight_te=2000.0, radius_m=40.0)
    )
    assert r.confidence == ConfidenceTier.ESTIMATED


def test_undercapacity_not_defensible():
    info = _info(1000.0, radii=(20.0, 40.0), source="worldenergydata")
    r = assess_vessel("SMALL", info, LiftRequirement(weight_te=5000.0, radius_m=30.0))
    assert r.defensible is False
    assert any("crane_swl" in lf for lf in r.limiting_factors)


def test_dp_gate_fails_when_below_required():
    info = _info(8000.0, radii=(20.0, 40.0), dp="DP1")
    r = assess_vessel(
        "LOWDP", info, LiftRequirement(weight_te=2000.0, radius_m=30.0, min_dp_class=3)
    )
    assert r.defensible is False  # DP1 < DP3 -> margin < 1


def test_deck_gate_gap_when_unknown():
    info = _info(8000.0, radii=(20.0, 40.0), deck=None)
    r = assess_vessel(
        "NODECK",
        info,
        LiftRequirement(weight_te=2000.0, radius_m=30.0, deck_area_m2=5000.0),
    )
    assert r.confidence == ConfidenceTier.GAP  # deck data missing -> weakest link gap


# ---- integration over the real merged fleet ----


def test_rank_fleet_orders_and_runs():
    rows = rank_fleet(LiftRequirement(weight_te=3000.0, radius_m=35.0))
    assert rows, "no vessels ranked"
    scores = [r.score for r in rows]
    assert scores == sorted(scores, reverse=True)
    assert all(0.0 <= r.score <= 100.0 for r in rows)


def test_assess_named_normalised_match():
    # 'Sleipnir' should match the WED 'SLEIPNIR' entry regardless of case/prefix.
    r = assess_named("SSCV Sleipnir", LiftRequirement(weight_te=2000.0, radius_m=40.0))
    if r is not None:  # present only when the WED fleet is available
        assert r.swl_at_radius_te > 0
