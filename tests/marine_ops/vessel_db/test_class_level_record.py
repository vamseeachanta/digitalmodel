"""Class-level vessel_db records: an explicit ``vessel_db_id`` (the id used by public models and
datasets, e.g. ``drill_<design>_<n>``) and public class particulars only - no owner, operator,
IMO number or individual vessel name."""

from __future__ import annotations

import json

from digitalmodel.marine_ops.vessel_db.loader import (
    Record,
    iter_records,
    validate_provenance,
    vessels_dir,
)

CLASS_ID = "drill_gustoprd12000_1"


def _raw():
    return json.loads((vessels_dir() / "raw" / "floating__particulars.json").read_text(encoding="utf-8"))


def test_explicit_vessel_db_id_overrides_the_slug():
    r = Record(name="Some class", scope="floating", layer="particulars", vessel_db_id="drill_x_1")
    assert r.vessel_id() == "drill_x_1"
    assert Record(name="Some class", scope="floating", layer="particulars").vessel_id() == "floating_some_class"


def test_class_level_drillship_record_resolves_by_its_id():
    recs = {r.vessel_id(): r for r in iter_records("floating", "particulars")}
    r = recs[CLASS_ID]
    assert r.record_level == "class"
    dims = r.canonical_dimensions()
    assert 180.0 < dims["loa"] < 195.0
    assert 38.0 < dims["beam"] < 40.0


def test_class_level_record_names_no_individual_vessel():
    raw = next(x for x in _raw()["records"] if x.get("vessel_db_id") == CLASS_ID)
    assert raw["record_level"] == "class"
    assert raw["owner_operator"] == "gap" and raw["year_built"] == "gap"
    assert "IMO" not in json.dumps(raw)
    for c in raw["citations"]:
        # sources are cited by public file or dataset, never by a vessel-specific page
        assert "vesselfinder" not in c["url"] and "marinetraffic" not in c["url"]


def test_class_level_record_provenance_is_clean():
    bad = [v for v in validate_provenance() if CLASS_ID in str(v) or "PRD12000" in str(v)]
    assert bad == []
