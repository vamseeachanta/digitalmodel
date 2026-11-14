"""Provider data remains discoverable through both canonical routing surfaces."""

from pathlib import Path

import yaml


ROOT = Path(__file__).resolve().parents[2]


def test_provider_data_routes_to_source_tests_and_documentation():
    registry = yaml.safe_load(
        (ROOT / "docs/registry/module-routing.yaml").read_text(encoding="utf-8")
    )
    entries = [row for row in registry["modules"] if row["module"] == "provider_data"]
    assert len(entries) == 1
    entry = entries[0]
    assert entry["entry_point"] == "src/digitalmodel/provider_data/"
    assert entry["key_tests"] == ["tests/provider_data/"]
    assert entry["maturity"] == "development"
    assert entry["operator_map_row"].endswith("#provider_data")
    rows = [
        line
        for line in (ROOT / "docs/maps/digitalmodel-operator-map.md")
        .read_text(encoding="utf-8")
        .splitlines()
        if line.startswith("| `provider_data` |")
    ]
    assert len(rows) == 1
    for path in (entry["entry_point"], *entry["key_tests"], entry["owner_wiki"]):
        assert (ROOT / path).exists()
        assert f"`{path}`" in rows[0]
    assert "PyYAML" in rows[0]
