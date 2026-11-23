"""Public source receipts contain neutral ordinals and integrity metadata only."""

import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]


def test_public_source_manifest_is_neutral_and_preserves_probe_order():
    folder = ROOT / "docs/reports/dwg-conversion"
    manifest = json.loads((folder / "source-manifest.json").read_text())
    probe = json.loads((folder / "probe.json").read_text())
    assert set(manifest) == {"owner_repo", "issue", "card", "intended_use", "sources"}
    records = manifest["sources"]
    assert len(records) == len(probe["sources"]) == 6
    for ordinal, (record, inspected) in enumerate(zip(records, probe["sources"]), 1):
        assert set(record) == {"ordinal", "sha256", "size"}
        assert record["ordinal"] == ordinal
        assert record["sha256"] == inspected["source_sha256"]
        assert len(record["sha256"]) == 64
        assert set(record["sha256"]) <= set("0123456789abcdef")
        assert type(record["size"]) is int and record["size"] > 0
