"""Tests for unified confidence tiers + capability scoring (#857)."""

from __future__ import annotations

import pytest

from digitalmodel.marine_ops.vessel_db.confidence import (
    ConfidenceTier,
    capability_score,
    field_confidence,
    record_confidence,
)
from digitalmodel.marine_ops.vessel_db.loader import Record


def _rec(fields, citations=None, vtype="fpso"):
    return Record(name="X", scope="floating", layer="particulars",
                  vessel_type=vtype, raw_fields=fields, citations=citations or [])


def test_tier_score_order():
    assert ConfidenceTier.MEASURED.score > ConfidenceTier.CITED.score
    assert ConfidenceTier.CITED.score > ConfidenceTier.BROCHURE.score
    assert ConfidenceTier.BROCHURE.score > ConfidenceTier.ESTIMATED.score
    assert ConfidenceTier.ESTIMATED.score > ConfidenceTier.GENERIC.score
    assert ConfidenceTier.GAP.score == 0.0


def test_estimated_marker_maps_to_estimated():
    rec = _rec({"beam_m": "estimated:kxx=0.35*beam"})
    assert field_confidence(rec, "beam") == ConfidenceTier.ESTIMATED


def test_gap_marker_maps_to_gap():
    rec = _rec({"beam_m": "gap"})
    assert field_confidence(rec, "beam") == ConfidenceTier.GAP


def test_missing_field_is_gap():
    assert field_confidence(_rec({}), "beam") == ConfidenceTier.GAP


def test_public_citation_is_cited():
    rec = _rec({"loa_m": 250.0},
               citations=[{"fields": ["loa_m"], "source": "Ship-Technology", "access": "public"}])
    assert field_confidence(rec, "loa") == ConfidenceTier.CITED


def test_brochure_access_is_brochure():
    rec = _rec({"loa_m": 250.0},
               citations=[{"fields": ["loa_m"], "source": "Heerema folder", "access": "brochure-pdf"}])
    assert field_confidence(rec, "loa") == ConfidenceTier.BROCHURE


def test_class_society_source_is_measured():
    rec = _rec({"loa_m": 250.0},
               citations=[{"fields": ["loa_m"], "source": "Equasis register", "access": "public"}])
    assert field_confidence(rec, "loa") == ConfidenceTier.MEASURED


def test_wed_generic_dimension_confidence():
    # worldenergydata blanket-cited row tagged generic.
    rec = _rec({"loa_m": 164.0, "dimension_confidence": "generic"},
               citations=[{"fields": ["all"], "source": "worldenergydata vessel_fleet (bsee_war)", "access": "public"}])
    assert field_confidence(rec, "loa") == ConfidenceTier.GENERIC


def test_uncited_number_is_generic():
    rec = _rec({"loa_m": 250.0})  # number with no citation
    assert field_confidence(rec, "loa") == ConfidenceTier.GENERIC


def test_record_confidence_rollup():
    rec = _rec(
        {"loa_m": 250.0, "beam_m": 40.0, "draft_m": 12.0, "displacement_t": 90000.0,
         "kxx_roll_m": "estimated:kxx=0.35*beam"},
        citations=[{"fields": ["loa_m", "beam_m", "draft_m", "displacement_t"],
                    "source": "Ship-Technology", "access": "public"}],
    )
    rc = record_confidence(rec)
    assert 0.0 < rc.score <= 1.0
    assert rc.per_field["loa"] == ConfidenceTier.CITED
    assert rc.per_field["kxx"] == ConfidenceTier.ESTIMATED
    assert rc.n_scored >= 5


def test_capability_score_defensible_when_strong():
    cs = capability_score(
        "heavy_lift",
        margins={"crane_swl": 2.0, "deck": 1.5},
        confidences={"crane_swl": ConfidenceTier.BROCHURE, "deck": ConfidenceTier.CITED},
    )
    assert cs.score > 0
    assert cs.defensible is True
    assert cs.confidence == ConfidenceTier.BROCHURE  # weakest link


def test_capability_score_not_defensible_on_gap_or_shortfall():
    cs = capability_score(
        "heavy_lift",
        margins={"crane_swl": 0.8, "deck": 1.2},                       # crane short
        confidences={"crane_swl": ConfidenceTier.GENERIC, "deck": ConfidenceTier.GAP},
    )
    assert cs.defensible is False
    assert any("crane_swl" in lf for lf in cs.limiting_factors)
    assert cs.confidence == ConfidenceTier.GAP


def test_capability_score_weakest_link_reported():
    cs = capability_score(
        "lift",
        margins={"a": 1.5, "b": 1.5},
        confidences={"a": ConfidenceTier.MEASURED, "b": ConfidenceTier.ESTIMATED},
    )
    assert cs.confidence == ConfidenceTier.ESTIMATED


# ---- integration with the real curated DB ----

def test_confidence_runs_on_real_records():
    from digitalmodel.marine_ops.vessel_db.loader import iter_records
    recs = iter_records("floating", "particulars")
    assert recs
    for rec in recs[:5]:
        rc = record_confidence(rec)
        assert 0.0 <= rc.score <= 1.0
