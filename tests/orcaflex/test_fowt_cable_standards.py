# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Regress publisher metadata and the scope of wind-farm cable citations.

Metadata oracles are the public DNV product pages, accessed 2026-11-05:
https://www.dnv.com/energy/standards-guidelines/dnv-rp-0360-subsea-power-cables-in-shallow-water/
https://www.dnv.com/energy/standards-guidelines/dnv-st-0359-subsea-power-cables-for-wind-power-plants/
No MBR value, factor or clause is established by those pages.
"""

import json
from pathlib import Path

import pytest

from digitalmodel.orcaflex.mooring_design_fowt import (
    DynamicCableConfig,
    check_watch_circle_vs_cable,
)

ROOT = Path(__file__).resolve().parents[2]


def test_dynamic_wind_cable_cites_wind_power_standard_scope(tmp_path):
    """A wind-cable scope reference must not assert an unverified MBR clause."""
    wiki_path = "wikis/engineering-standards/wiki/standards/dnv-st-0359.md"
    page = tmp_path / wiki_path
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: DNV-ST-0359\npublisher: DNV\nrevision: 2025-12\n---\n",
        encoding="utf-8",
        newline="\n",
    )
    cable = DynamicCableConfig(
        suspended_length=250.0,
        hang_off_elevation=80.0,
        nominal_horizontal_span=200.0,
        mbr_limit_m=50.0,
    )
    result = check_watch_circle_vs_cable(20.0, cable, repo_root=tmp_path)
    assert len(result.citations) == 1
    citation = result.citations[0]
    assert citation.code_id == "DNV-ST-0359"
    assert citation.revision == "2025-12"
    assert citation.wiki_path == wiki_path
    assert citation.section == "Scope (public product page)"
    assert "not established" in citation.note
    assert "caller" in citation.note


@pytest.mark.parametrize("relative", [
    "src/digitalmodel/orcaflex/mooring_design_fowt.py",
    "docs/plans/2026-05-05-issue-574-fowt-wiki-standards-pages.md",
    "docs/plans/2026-05-05-issue-576-fowt-watch-circle-cable-curvature.md",
])
def test_rp0360_metadata_distinguishes_static_shallow_water(relative):
    text = (ROOT / relative).read_text(encoding="utf-8")
    assert "2016-03" in text
    assert "2021-10" in text
    assert "static service in shallow water" in text
    assert "DNV-ST-0359" in text
    assert "2025-12" in text


def test_current_fowt_atlas_uses_wind_cable_scope_reference():
    from digitalmodel.parametric import refresh
    from digitalmodel.parametric.atlas import Atlas

    expected = [{"id": "DNV-ST-0359", "edition": "2025-12"}]
    assert refresh.STANDARDS["fowt_mooring"] == expected
    atlas = Atlas.load(ROOT / "atlases", "fowt_mooring")
    assert atlas.provenance["standards"] == expected
    assert not refresh.refresh_status("fowt-mooring")["stale"]


def test_historical_cable_metadata_has_explicit_correction():
    """Preserve hashed historical basis while recording corrected metadata."""
    inventory = json.loads(
        (ROOT / "atlases/historical-provenance.json").read_text(encoding="utf-8")
    )
    records = [r for r in inventory["atlases"] if r["workflow_id"] == "fowt-mooring"]
    assert records
    for record in records:
        if not any(s["id"] == "DNV-RP-0360" for s in record["basis"]["standards"]):
            continue
        correction = record["citation_correction"]
        assert correction["recorded_standard"]["edition"] == "2016-03"
        assert correction["recorded_standard"]["amended"] == "2021-10"
        assert correction["recorded_standard"]["scope"] == "static service in shallow water"
        assert correction["wind_cable_reference"] == {
            "id": "DNV-ST-0359", "edition": "2025-12",
        }
        assert "historical" in correction["note"]
