# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Discovery contracts; these tests do not qualify numerical methods.

The independent expected inventory is a manual reading of the direct package at
revision 272a424107ab9b2a3084bb61dada36dd66d601e8: eight numerical helpers and
fourteen envelope functions, methods and properties. Imports, generated dataclass
methods and sub-packages are excluded. No engineering acceptance limits are used.
"""

import ast
import hashlib
import json
import re
import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
PACKAGE = "src/digitalmodel/marine_ops/"
INVENTORY = ROOT / PACKAGE / "docs/method-inventory.json"
EXPECTED = {
    "envelope_numerics.py": {
        "positive",
        "finite_grid",
        "validate_operation",
        "validate_component",
        "integrate",
        "spectrum",
        "extrapolated_fraction",
        "heading_column",
    },
    "operation_envelope.py": {
        "available_operations",
        "resolve_operation",
        "EnvelopePoint.hs_forecast_limit_m",
        "EnvelopeResult.require_valid_points",
        "EnvelopeResult.require_evaluated",
        "EnvelopeResult.hs_limit_at",
        "EnvelopeResult.to_dict",
        "operation_envelope",
        "_evaluate_point",
        "envelope_for_operations",
        "plot_envelopes",
        "_component_for",
        "_rao_by_heading",
        "_significant_response",
    },
    "__init__.py": set(),
}


def definitions(source):
    """Read qualified definitions without importing any discovered module."""
    result = {}

    def walk(node, prefix=""):
        for child in ast.iter_child_nodes(node):
            if isinstance(child, (ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)):
                name = prefix + child.name
                if not isinstance(child, ast.ClassDef):
                    result[name] = [child.lineno, child.end_lineno]
                walk(child, name + ".")
            else:
                walk(child, prefix)

    walk(ast.parse(source))
    return result


def recorded_blob(inventory, path):
    """Read repository bytes at the observation pin, independent of checkout EOLs."""
    return subprocess.check_output(
        ["git", "show", f"{inventory['code_revision']}:{path}"], cwd=ROOT
    )


@pytest.fixture
def inventory():
    return json.loads(INVENTORY.read_text(encoding="utf-8"))


def test_direct_package_inventory_is_complete(inventory):
    """Check live symbols, plus historical definition ranges at the code pin."""
    actual_files = {p.name for p in (ROOT / PACKAGE).glob("*.py")}
    assert actual_files == set(EXPECTED)
    assert len(inventory["files"]) == len(EXPECTED)
    expected_keys = {
        (PACKAGE + name, symbol)
        for name, symbols in EXPECTED.items()
        for symbol in symbols
    }
    assert len(inventory["functions"]) == len(expected_keys)
    assert {
        (row["path"], row["symbol"]) for row in inventory["functions"]
    } == expected_keys
    assert {row["path"] for row in inventory["files"]} == {
        PACKAGE + name for name in EXPECTED
    }
    for name, expected in EXPECTED.items():
        actual = definitions((ROOT / PACKAGE / name).read_text(encoding="utf-8"))
        rows = [r for r in inventory["functions"] if r["path"] == PACKAGE + name]
        assert len(rows) == len(expected)  # Duplicate entries are not coverage.
        assert {r["symbol"] for r in rows} == expected == set(actual)
        recorded = definitions(recorded_blob(inventory, PACKAGE + name))
        assert set(recorded) == expected
        for row in rows:
            assert row["lines"] == recorded[row["symbol"]]


def test_inventory_source_fingerprints(inventory):
    assert inventory["code_revision"] == "272a424107ab9b2a3084bb61dada36dd66d601e8"
    assert inventory["map"]["observed_at_revision"] == inventory["code_revision"]
    for row in inventory["files"] + [inventory["map"]]:
        assert (
            hashlib.sha256(recorded_blob(inventory, row["path"])).hexdigest()
            == row["sha256"]
        )
    assert inventory["qualification"] == "NOT_EVALUATED"


def test_exact_search_signals_and_line_matches(inventory):
    """Compare all map signals, including those with no matches, case sensitively."""
    text = recorded_blob(inventory, inventory["map"]["path"]).decode("utf-8")
    assert f"\nrevision: {inventory['map']['data_revision']}\n" in text
    family = text.split("\n- id: wave-spectra\n", 1)[1].split("\n- id:", 1)[0]
    signals = re.findall(
        r"^  - \{search: (\w+), pattern: &\w+ (.*?)\}$", family, re.MULTILINE
    )
    assert signals == [(s["search"], s["pattern"]) for s in inventory["search_signals"]]
    assert inventory["search_mode"] == "case-sensitive re.search per source line"
    for signal in inventory["search_signals"]:
        actual = {}
        for row in inventory["files"]:
            lines = recorded_blob(inventory, row["path"]).decode("utf-8").splitlines()
            actual[row["path"]] = [
                number
                for number, line in enumerate(lines, start=1)
                if re.search(signal["pattern"], line)
            ]
        assert signal["matching_lines"] == actual


def test_each_function_has_a_bounded_relation(inventory):
    allowed = {
        "validation",
        "lookup",
        "serialization",
        "presentation",
        "orchestration",
        "numerical_helper",
        "delegation",
        "alias",
    }
    recorded = set()
    for row in inventory["functions"]:
        assert row["relation"] in allowed
        assert row["method"] and row["evidence"]
        assert row["map_status"] in {"recorded_independent", "not_recorded"}
        if row["map_status"] == "recorded_independent":
            recorded.add(row["symbol"])
    assert recorded == {"operation_envelope", "_significant_response"}
    assert inventory["remaining_gaps"]
    assert all(gap["status"] == "NOT_EVALUATED" for gap in inventory["remaining_gaps"])


def test_upstream_definition_locators_resolve(inventory):
    expected = {
        "WaveSpectra.jonswap",
        "compute_response_spectrum",
        "spectral_moments",
        "significant_amplitude",
    }
    rows = inventory["upstream"]
    assert len(rows) == len(expected)
    assert {row["symbol"] for row in rows} == expected
    for row in rows:
        source = recorded_blob(inventory, row["path"])
        assert definitions(source)[row["symbol"]] == row["lines"]
        assert hashlib.sha256(source).hexdigest() == row["sha256"]
        assert row["relation"] == "delegated_implementation"
        assert row["qualification"] == "NOT_EVALUATED"


def test_public_repository_documentation_locators(inventory):
    """Every published new documentation locator must resolve to its heading."""
    pack = ROOT / PACKAGE / "docs/agent-pack.md"
    content = pack.read_text(encoding="utf-8")
    assert "[method-inventory.json](method-inventory.json)" in content
    for row in inventory["documentation"]:
        path = Path(row["path"])
        assert not path.is_absolute() and ".." not in path.parts
        assert (ROOT / path).is_file()
        assert row["heading"] in (ROOT / path).read_text(encoding="utf-8").splitlines()


def test_historical_discovery_pins_are_retained(inventory):
    """Refresh supplements the original discovery evidence instead of repinning it."""
    pack = (ROOT / PACKAGE / "docs/agent-pack.md").read_text(encoding="utf-8")
    for key in ("code_revision", "map_revision", "map_data_revision"):
        assert inventory["historical_inventory"][key] in pack
    assert inventory["historical_inventory"]["code_revision"] == (
        "7aad603be1c6136106dbb2381ce5096b050654f8"
    )


@pytest.mark.parametrize("change", ["source_drift", "crlf"])
def test_recorded_evidence_survives_checkout_changes(inventory, monkeypatch, change):
    """Recorded evidence stays valid when checked-out files change or use CRLF."""
    paths = {
        ROOT / row["path"]
        for row in inventory["files"] + [inventory["map"]] + inventory["upstream"]
    }
    read_bytes, read_text = Path.read_bytes, Path.read_text

    def checkout_bytes(path):
        source = read_bytes(path)
        if path in paths:
            if change == "crlf":
                return source.replace(b"\n", b"\r\n")
            return b"# Later source revision\n" + source
        return source

    def checkout_text(path, *args, **kwargs):
        source = read_text(path, *args, **kwargs)
        if path in paths and change == "source_drift":
            return "# Later source revision\n" + source
        return source

    monkeypatch.setattr(Path, "read_bytes", checkout_bytes)
    monkeypatch.setattr(Path, "read_text", checkout_text)
    test_direct_package_inventory_is_complete(inventory)
    test_inventory_source_fingerprints(inventory)
    test_exact_search_signals_and_line_matches(inventory)
    test_upstream_definition_locators_resolve(inventory)


@pytest.mark.parametrize("section", ["files", "map", "upstream"])
def test_recorded_digest_corruption_is_rejected(inventory, section):
    """Historical fingerprints remain hard assertions for every source kind."""
    row = inventory[section] if section == "map" else inventory[section][0]
    row["sha256"] = "0" * 64
    check = (
        test_upstream_definition_locators_resolve
        if section == "upstream"
        else test_inventory_source_fingerprints
    )
    with pytest.raises(AssertionError):
        check(inventory)


@pytest.mark.parametrize("section", ["functions", "upstream"])
def test_recorded_locator_corruption_is_rejected(inventory, section):
    """Definition ranges must still resolve at the recorded revision."""
    inventory[section][0]["lines"] = [0, 0]
    check = (
        test_upstream_definition_locators_resolve
        if section == "upstream"
        else test_direct_package_inventory_is_complete
    )
    with pytest.raises(AssertionError):
        check(inventory)


def test_live_symbol_addition_requires_inventory_refresh(inventory, monkeypatch):
    """Pinning historical evidence must not hide new direct-package symbols."""
    read_text = Path.read_text
    path = ROOT / PACKAGE / "envelope_numerics.py"

    def additional_symbol(candidate, *args, **kwargs):
        source = read_text(candidate, *args, **kwargs)
        if candidate == path:
            source += "\ndef new_helper():\n    pass\n"
        return source

    monkeypatch.setattr(Path, "read_text", additional_symbol)
    with pytest.raises(AssertionError):
        test_direct_package_inventory_is_complete(inventory)
