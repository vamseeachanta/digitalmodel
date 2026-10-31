# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Historical atlases remain loadable while defaults follow the current basis."""

import json
from pathlib import Path

import pytest

from digitalmodel.drilling_riser import operability_atlas
from digitalmodel.parametric import historical_provenance as history
from digitalmodel.parametric import query, refresh
from digitalmodel.parametric.atlas import Atlas

ROOT = Path(__file__).resolve().parents[2] / "atlases"


@pytest.mark.parametrize("atlas_id", ["229487678a1a", "bcc370f46b9c"])
def test_drilling_riser_operability_versions_load(atlas_id):
    atlas = Atlas.load(ROOT, "drilling_riser_operability", atlas_id)
    assert atlas.atlas_id == atlas_id


def test_default_points_at_current_version():
    atlas = Atlas.load(ROOT, "drilling_riser_operability")
    fingerprint = operability_atlas.content_fingerprint()
    assert atlas.atlas_id == fingerprint[:12]
    assert atlas.provenance["content_fingerprint"] == fingerprint
    assert atlas.validation["passes"] is True


def test_code_check_default_matches_current_basis():
    atlas = Atlas.load(ROOT, "code_check")
    fingerprint = refresh.content_fingerprint("api-rp-2rd-riser")
    assert atlas.atlas_id == fingerprint[:12]
    assert atlas.provenance["content_fingerprint"] == fingerprint
    assert atlas.validation["passes"] is True
    assert query._staleness(atlas) is None


def test_current_code_check_evidence_is_in_inventory():
    atlas = Atlas.load(ROOT, "code_check")
    manifest = f"atlases/code_check/{atlas.atlas_id}/manifest.yaml"
    inventory = json.loads((ROOT / "historical-provenance.json").read_text(
        encoding="utf-8"
    ))
    records = [row for row in inventory["atlases"] if row["manifest"] == manifest]
    assert len(records) == 1
    record = records[0]
    assert record["status"] == "VERIFIED"
    assert record["content_fingerprint"] == atlas.provenance["content_fingerprint"]
    history.verify_artifacts(record, ROOT.parent)
    assert history.verify_sources(
        record, history.archive_reader(ROOT.parent)
    ) == atlas.provenance["content_fingerprint"]


def test_historical_code_check_evidence_is_rejected(tmp_path):
    atlas = Atlas.load(ROOT, "code_check", "4b48e8c976cd")
    atlas.save(tmp_path / "atlases")
    point = {axis.name: atlas.grid.iloc[0][axis.name] for axis in atlas.axes}
    cfg = query.router({
        "parametric_query": {
            "atlas": "code_check",
            "atlas_root": str(tmp_path / "atlases"),
            "output_dir": str(tmp_path / "results"),
            "point": point,
        },
    })
    result = cfg["parametric_query"]["result"]
    assert result["stale"] is True
    assert result["in_range"] is False
    assert result["value"] is None
    assert "refresh required" in result["reason"]
