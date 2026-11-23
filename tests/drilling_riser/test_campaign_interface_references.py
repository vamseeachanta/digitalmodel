# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline documentation contract for issue #2531; no solver module is imported.

Expected fields are transcribed from the cited public producer functions into
the inventory, independently of these AST readers. There are no engineering
equations, numeric acceptance limits or calculated reference values in this
documentation slice. These checks establish source agreement, not qualification.
"""

from __future__ import annotations

import ast
import json
from pathlib import Path
from types import SimpleNamespace

import pytest


ROOT = Path(__file__).resolve().parents[2]
DOCS = ROOT / "docs/domains/risers"
INVENTORY = DOCS / "campaign-interface-references.json"


@pytest.fixture(scope="module")
def inventory():
    return json.loads(INVENTORY.read_text(encoding="utf-8"))


def _function(source, symbol):
    tree = ast.parse((ROOT / source).read_text(encoding="utf-8"))
    for name in symbol.split("."):
        tree = next(node for node in tree.body if getattr(node, "name", None) == name)
    return tree


def _mapping(node):
    """Describe literal keys and unpacked calls without executing source."""
    assert isinstance(node, ast.Dict)
    return {
        "fields": sorted(key.value for key in node.keys if key is not None),
        "spreads": sorted(
            ast.unparse(value.func)
            for key, value in zip(node.keys, node.values)
            if key is None
        ),
    }


def test_producer_return_maps_match_public_source(inventory):
    for producer in inventory["producers"].values():
        function = _function(producer["source"], producer["symbol"])
        maps = [
            _mapping(node.value)
            for node in ast.walk(function)
            if isinstance(node, ast.Return) and isinstance(node.value, ast.Dict)
        ]
        # Compare as sets: AST traversal order is not the order of execution.
        actual = {json.dumps(item, sort_keys=True) for item in maps}
        expected = {
            json.dumps(
                {"fields": sorted(item["units"]), "spreads": item["spreads"]},
                sort_keys=True,
            )
            for item in producer["returns"]
        }
        assert actual == expected, producer["symbol"]


def test_adapter_assignments_match_public_source(inventory):
    for adapter in inventory["adapters"].values():
        function = _function(adapter["source"], adapter["symbol"])
        assigned = {}
        initializers = []
        for node in ast.walk(function):
            if not isinstance(node, (ast.Assign, ast.AnnAssign)):
                continue
            targets = node.targets if isinstance(node, ast.Assign) else [node.target]
            for target in targets:
                if isinstance(target, ast.Name) and target.id == "out":
                    if isinstance(node.value, ast.Dict):
                        initializers.append(_mapping(node.value))
                    else:
                        initializers.append({"call": ast.unparse(node.value)})
                if (
                    isinstance(target, ast.Subscript)
                    and isinstance(target.value, ast.Name)
                    and target.value.id == "out"
                ):
                    key = target.slice.value
                    assigned[key] = (
                        _mapping(node.value)["fields"]
                        if isinstance(node.value, ast.Dict)
                        else None
                    )
        assert assigned == adapter["assigned"], adapter["symbol"]
        assert initializers == adapter["initializers"], adapter["symbol"]
        direct_returns = [
            _mapping(node.value)
            for node in ast.walk(function)
            if isinstance(node, ast.Return) and isinstance(node.value, ast.Dict)
        ]
        assert direct_returns == adapter["direct_returns"]


def test_branch_field_coverage_and_units(inventory):
    """Each listed branch expands to unique fields with an explicit unit basis."""
    assert set(inventory["branches"]) == {
        "drilling_statics",
        "drilling_dynamics",
        "open_water_statics",
        "open_water_dynamics",
        "hang_off_statics",
        "hang_off_dynamics",
    }
    for branch in inventory["branches"].values():
        fields = dict(branch["additional_units"])
        for reference in branch["producer_returns"]:
            producer, label = reference.split(":")
            candidates = inventory["producers"][producer]["returns"]
            row = next(item for item in candidates if item["label"] == label)
            assert not fields.keys() & row["units"].keys()
            fields.update(row["units"])
        assert fields
        assert all(isinstance(unit, str) and unit.strip() for unit in fields.values())
        assert fields["w5"] == "structured; see w5_channels.UNITS"
        assert branch["consumer_evidence_ref"] is None
        assert branch["lesson_ref"] is None
        assert branch["status"] == "NOT_EVALUATED"


def test_pending_references_cannot_claim_private_readback(inventory):
    assert inventory["external_consumer_completeness"] == "NOT_EVALUATED"
    assert inventory["campaign_qualification"] == "NOT_EVALUATED"
    assert inventory["sidecar_entry_id"] is None
    assert inventory["private_readback"] == "NOT_EVALUATED"
    document = (DOCS / "campaign-interface-references.md").read_text(encoding="utf-8")
    for required in inventory["required_private_evidence"]:
        assert f"`{required}`" in document
    assert "campaign-interface-references.json" in document
    assert "NOT_EVALUATED" in document
    assert "Revision history" in document


def test_statistics_leaf_inventory(inventory):
    source = inventory["adapters"]["drilling"]["source"]
    function = _function(source, "_stats")
    returned = next(
        node.value for node in ast.walk(function) if isinstance(node, ast.Return)
    )
    assert _mapping(returned)["fields"] == inventory["series_statistics"]


class _RemoveImports(ast.NodeTransformer):
    """Remove dependencies from isolated adapter methods before compilation."""

    def visit_Import(self, node):
        return None

    def visit_ImportFrom(self, node):
        return None


@pytest.mark.parametrize(
    "branch_name",
    [
        "drilling_statics",
        "drilling_dynamics",
        "open_water_statics",
        "open_water_dynamics",
        "hang_off_statics",
        "hang_off_dynamics",
    ],
)
@pytest.mark.parametrize("options_enabled", [False, True])
def test_isolated_adapter_branch_keys(inventory, branch_name, options_enabled):
    """Exercise dispatch and optional fields with synthetic dependency responses.

    Expected fields come from the independently transcribed public inventory.
    Each response dependency returns symbolic values for those fields. A history
    contains a symbolic sample; its expected count is its literal cardinality.
    Option counts and spacing are synthetic routing inputs, not engineering
    values. No calculations or solver modules are executed: only the adapter
    methods are compiled, with imports removed and dependencies replaced.
    """
    kind, analysis = branch_name.rsplit("_", maxsplit=1)
    history = ["synthetic sample"]

    class OpenWaterSpec:
        pass

    class Model:
        def __getitem__(self, key):
            return self

        def TimeHistory(self, *args):
            return history

    def response(symbol, label):
        producer = next(
            item for item in inventory["producers"].values() if item["symbol"] == symbol
        )
        row = next(item for item in producer["returns"] if item["label"] == label)
        result = dict(row["units"])
        for spread in row["spreads"]:
            result.update(response(spread, label))
        return result

    def stub_response(symbol, label):
        return lambda *args: response(symbol, label)

    ofx = SimpleNamespace(oeEndA="end A", oeEndB="end B", Period=lambda *args: None)
    orun = SimpleNamespace(_api=lambda: ofx)
    for producer in inventory["producers"].values():
        if producer["symbol"] != "hang_off_summary":
            setattr(
                orun,
                producer["symbol"],
                stub_response(producer["symbol"], producer["returns"][0]["label"]),
            )
    orun.ring_static_z_m = lambda *args: "synthetic elevation"
    orun.main_period = lambda *args: None
    orun.riser_modal_periods = lambda *args, **kwargs: ["synthetic mode"]
    channels = SimpleNamespace(
        extract=lambda *args: {},
        extract_open_water=lambda *args: {},
        extract_hang_off=lambda *args: {},
        compact=lambda value: value,
        hang_off_summary=lambda model, spec, analysis, ofx: response(
            "hang_off_summary", "static" if analysis == "statics" else "dynamic"
        ),
    )
    namespace = {
        "Any": object,
        "OpenWaterRiserSpec": OpenWaterSpec,
        "orun": orun,
        "w5_channels": channels,
        "ofx": ofx,
        "_stats": lambda values: dict.fromkeys(
            inventory["series_statistics"], "symbolic"
        ),
        "fc": SimpleNamespace(
            extract=lambda *args, **kwargs: "synthetic fatigue",
            DEFAULT_SPACING_M=None,
            DEFAULT_BINS=None,
        ),
    }
    methods = [
        _RemoveImports().visit(_function(adapter["source"], adapter["symbol"]))
        for adapter in inventory["adapters"].values()
    ]
    tree = ast.fix_missing_locations(ast.Module(body=methods, type_ignores=[]))
    assert not any(
        isinstance(node, (ast.Import, ast.ImportFrom)) for node in ast.walk(tree)
    )
    exec(compile(tree, "<isolated public adapter methods>", "exec"), namespace)
    spec = (
        OpenWaterSpec()
        if kind == "open_water"
        else SimpleNamespace(hang_off=object() if kind == "hang_off" else None)
    )
    adapter = SimpleNamespace(spec=lambda case: spec)
    adapter._extract_open_water = namespace["_extract_open_water"]
    params = (
        {
            "modal_modes": len(["mode"]),
            "fatigue": {"spacing_m": len(["spacing"]), "n_bins": len(["bin"])},
        }
        if options_enabled
        else {}
    )
    case = {"analysis": analysis, "params": params}
    result = namespace["extract"](adapter, Model(), case)
    branch = inventory["branches"][branch_name]
    expected = set(branch["additional_units"])
    for reference in branch["producer_returns"]:
        producer, label = reference.split(":")
        expected.update(response(inventory["producers"][producer]["symbol"], label))
    if not options_enabled:
        expected.discard("modes")
    assert set(result) == expected
    if "series" in result:
        assert set(result["series"]) == set(inventory["adapters"][kind]["series_units"])
        assert result["sample_count"] == len(history)
    expect_fatigue = options_enabled and analysis == "dynamics" and kind != "hang_off"
    assert ("fatigue" in result["w5"]) == expect_fatigue
