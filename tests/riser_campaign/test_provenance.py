# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Provenance evidence and reproducible synthetic motion contracts."""

import copy
import hashlib
import importlib
import json
from pathlib import Path

import pytest


CAMPAIGN = Path(__file__).resolve().parents[2] / "examples/riser-campaign"


def validator():
    return importlib.import_module("scripts.riser_campaign.validate_manifest")


def data():
    return json.loads((CAMPAIGN / "manifest.json").read_text(encoding="utf-8"))


def test_provenance_is_first_and_covers_every_input_group():
    manifest = data()
    assert next(iter(manifest)) == "data_provenance"
    table = {row["input_group"]: row for row in manifest["data_provenance"]}
    assert set(table) == {
        "field-development rows", "FPSO particulars", "semi-submersible particulars",
        "spar particulars", "TLP particulars", "floater RAOs", "metocean",
        "riser properties", "geometry and numerical settings",
    }
    assert table["field-development rows"]["status"] == "NOT_USED"
    assert table["field-development rows"]["sha256"] is None
    sources = {s["id"]: s for s in manifest["sources"]}
    for row in manifest["data_provenance"]:
        if row["status"] != "NOT_USED":
            assert row["status"] == "ASSUMED"
            source = sources[row["source_id"]]
            for field in ("sha256", "licence_or_terms", "date", "version"):
                assert row[field] == source[field]
    basis = (CAMPAIGN / "basis.md").read_bytes()
    assert hashlib.sha256(basis).hexdigest() == sources["screening-basis"]["sha256"]
    readme = (CAMPAIGN / "README.md").read_text(encoding="utf-8")
    assert readme.index("## Data provenance") < readme.index("## Revision history")
    for source in manifest["sources"]:
        assert source["sha256"] in readme


@pytest.mark.parametrize("mutation", [
    lambda d: d["data_provenance"].pop(),
    lambda d: d["data_provenance"].append(copy.deepcopy(d["data_provenance"][0])),
    lambda d: d["data_provenance"][1].update(sha256="0" * 64),
    lambda d: d["sources"][0].pop("licence_or_terms"),
    lambda d: d["sources"][0].update(date="invalid-date"),
    lambda d: d["sources"][-1].update(sha256="0" * 64),
    lambda d: d["rao_sets"][0]["generation"].update(tool_version=""),
    lambda d: d["rao_sets"][0]["rows"][0]["amplitudes"].__setitem__(0, 999),
    lambda d: d["rao_sets"][0]["rows"][0]["phases_deg"].__setitem__(0, 10),
])
def test_invalid_provenance_and_synthetic_raos_are_rejected(mutation):
    manifest = data()
    mutation(manifest)
    with pytest.raises(ValueError):
        validator().validate_manifest(manifest)


def test_changed_basis_is_rejected(tmp_path, monkeypatch):
    altered = tmp_path / "basis.md"
    original = (CAMPAIGN / "basis.md").read_bytes()
    altered.write_bytes(original + b"\nChanged assumption.\n")
    monkeypatch.setattr(validator(), "BASIS", altered)
    with pytest.raises(ValueError, match="basis.*digest"):
        validator().validate_manifest(data())
    assert (CAMPAIGN / "basis.md").read_bytes() == original


def test_synthetic_rao_tables_reproduce_from_recorded_inputs():
    for rao in data()["rao_sets"]:
        method = rao["generation"]
        assert method["tool"] == "explicit arithmetic table"
        assert method["tool_version"] == "synthetic-rao-v1"
        for row, factor in zip(rao["rows"], method["period_factors"]):
            expected = [a * factor * method["size_factor"] for a in method["base_amplitudes"]]
            assert row["amplitudes"] == pytest.approx(expected, rel=1e-12, abs=1e-12)
            assert row["phases_deg"] == [0] * 6


def test_campaign_tests_have_one_hosted_domain():
    root = CAMPAIGN.parents[1]
    rows = [line for line in (root / "tests/DOMAINS.md").read_text(encoding="utf-8").splitlines()
            if "`tests/riser_campaign/`" in line]
    assert len(rows) == 1
    assert rows[0].split("|")[1].strip() == "subsea"
