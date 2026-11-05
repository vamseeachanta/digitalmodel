# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Keep the discovery inventory complete without importing calculation code."""

import ast
import re
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
PACKAGE = ROOT / "src/digitalmodel/marine_ops"
INVENTORY = PACKAGE / "docs/method-inventory.md"
PACK = PACKAGE / "docs/agent-pack.md"


def definitions(path):
    """Include private functions, properties, class methods and nested helpers."""
    found = {}

    class Visitor(ast.NodeVisitor):
        scope = ()

        def visit_ClassDef(self, node):
            self.descend(node)

        def visit_FunctionDef(self, node):
            found[".".join((*self.scope, node.name))] = node.lineno
            self.descend(node)

        visit_AsyncFunctionDef = visit_FunctionDef

        def descend(self, node):
            previous = self.scope
            self.scope = (*previous, node.name)
            self.generic_visit(node)
            self.scope = previous

    Visitor().visit(ast.parse(path.read_text(encoding="utf-8")))
    return found


def rows(section):
    text = INVENTORY.read_text(encoding="utf-8")
    block = text.split(f"## {section}\n", 1)[1].split("\n## ", 1)[0]
    return [[cell.strip().strip("`") for cell in line.strip("|").split("|")]
            for line in block.splitlines() if line.startswith("| `")]


def test_inventory_enumerates_every_direct_file_and_definition():
    files = {row[0]: int(row[1]) for row in rows("Direct files")}
    actual = {path.name: definitions(path) for path in PACKAGE.glob("*.py")}
    assert files == {name: len(symbols) for name, symbols in actual.items()}
    listed = rows("Direct definitions")
    expected = {(name, symbol, str(line)) for name, symbols in actual.items()
                for symbol, line in symbols.items()}
    assert {(row[0], row[1], row[2]) for row in listed} == expected
    assert len(listed) == len(expected), "duplicate inventory entries"


def test_every_definition_has_a_relation_and_mapping_disposition():
    listed = rows("Direct definitions")
    assert listed
    for filename, symbol, line, family, relation, mapping in listed:
        assert family
        assert relation in {"helper", "wrapper", "independent"}
        assert mapping in {"wave-spectra / independent", "not mapped"}
    registry = yaml.safe_load(
        (ROOT / "docs/registry/implementation-map.yaml").read_text(encoding="utf-8")
    )
    mapped = {}
    for family in registry["families"]:
        for entry in family["implementations"]:
            path = Path(entry["path"])
            if path.parent == Path("src/digitalmodel/marine_ops"):
                mapped.setdefault((path.name, entry["symbol"]), set()).add(
                    f"{family['id']} / {entry['relation']}"
                )
    expected = {(row[0], row[1]): {row[5]} for row in listed if row[5] != "not mapped"}
    assert mapped == expected


def test_upstream_relations_resolve_to_real_definitions():
    upstream = rows("Upstream and adjacent relations")
    assert upstream
    for consumer, path, symbol, relation, consequence in upstream:
        assert symbol in definitions(ROOT / path)
        assert relation in {"delegates", "independent", "wrapper"}
        assert consequence
        module, name = consumer.split(":", 1)
        assert name in definitions(PACKAGE / module)
    targets = {row[2] for row in upstream}
    assert {"WaveSpectra.jonswap", "WaveSpectra._normalize_to_significant_height",
            "compute_response_spectrum", "spectral_moments", "significant_amplitude",
            "crane_tip_raos", "hs_limit_for_criterion"} <= targets


def test_published_documentation_locators_resolve_without_restricted_names():
    locators = rows("Public documentation locators")
    assert locators
    for purpose, locator, status in locators:
        path = ROOT / locator
        assert path.is_file(), locator
        assert status == "checked in repository"
    text = INVENTORY.read_text(encoding="utf-8")
    assert "Withheld discovery source: unresolved" in text
    assert "not a reconstruction of the withheld source" in text
    assert "method-inventory.md" in PACK.read_text(encoding="utf-8")


def test_historical_search_patterns_and_revisions_are_retained():
    pack = PACK.read_text(encoding="utf-8")
    inventory = INVENTORY.read_text(encoding="utf-8")
    for pin in ("7aad603be1c6136106dbb2381ce5096b050654f8",
                "3256bab95f84da3d9fadce8d5dcf8458d66d1eb4",
                "270c8759c374b67a7f032953e7fcdb14e0224461"):
        assert pin in pack
    assert "aa96c303e1971317995b0aed3727847942fd5d5a" in inventory
    assert "Inventory is not numerical qualification" in inventory
    # Compare the exact strings with the map's literal search signals, not a
    # normalized or independently invented approximation of those expressions.
    signals = re.search(r"```text\n(.*?)\n```", pack, re.DOTALL).group(1).splitlines()
    assert len(signals) == 3
    for signal in signals:
        assert signal in (ROOT / "docs/registry/implementation-map.yaml").read_text()
    assert "rg -n 'operation_envelope|operability envelope' scripts src --glob '*.py'" in pack
    assert "rg -l 'marine_ops|operation_envelope|OperationEnvelope' docs --glob '*.md'" in pack
