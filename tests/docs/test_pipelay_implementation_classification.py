# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent, import-free inventory checks for the inspected pipelay call graph.

Expected members and edges are hand transcriptions of workflow.py and
orcaflex/pipelay_analysis.py, not discoveries from the map generator.
These checks establish inventory relationships only, not numerical qualification.
"""

import ast
import copy
import hashlib
from functools import lru_cache
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = "src/digitalmodel/pipelay/workflow.py"
ANALYSIS = "src/digitalmodel/orcaflex/pipelay_analysis.py"
CATENARY = "catenary-mooring-line-statics"
TENSION = "effective-tension-riser-stack-up"
BENDING = "pipelay-bending-screening"
OPERABILITY = "lay-rate-operability"

# Source inspection: workflow.run_jlay, run_reel_lay, _operability and router;
# pipelay_analysis.LayOperability.generate_operability_table.
EDGES = {
    (WORKFLOW, "run_jlay"): {
        (ANALYSIS, "JLayConfig.calculate_sagbend"), (WORKFLOW, "_operability"),
    },
    (WORKFLOW, "run_reel_lay"): {
        (WORKFLOW, "_reel_result"), (WORKFLOW, "_operability"),
    },
    (WORKFLOW, "_operability"): {
        (ANALYSIS, "LayOperability.generate_operability_table"),
    },
    (WORKFLOW, "router"): {
        (WORKFLOW, "run_jlay"), (WORKFLOW, "run_reel_lay"),
        (ANALYSIS, "calculate_stinger_radius"),
    },
    (ANALYSIS, "LayOperability.generate_operability_table"): {
        (ANALYSIS, "LayOperability.lay_rate_at_hs"),
    },
}
MEMBERS = {
    CATENARY: {(ANALYSIS, "JLayConfig.calculate_sagbend"), (WORKFLOW, "run_jlay")},
    TENSION: {(ANALYSIS, "JLayConfig.calculate_sagbend"), (WORKFLOW, "run_jlay")},
    BENDING: {(WORKFLOW, "_reel_result"), (WORKFLOW, "run_reel_lay"),
              (ANALYSIS, "calculate_stinger_radius"), (WORKFLOW, "router")},
    OPERABILITY: {(WORKFLOW, "_operability"),
                  (ANALYSIS, "LayOperability.generate_operability_table"),
                  (ANALYSIS, "LayOperability.lay_rate_at_hs")},
}
SYMBOLS = set.union(*MEMBERS.values())


@pytest.fixture(scope="module")
def document():
    return yaml.safe_load(
        (ROOT / "docs/registry/implementation-map.yaml").read_text(encoding="utf-8")
    )


@lru_cache(maxsize=None)
def source_index(path):
    """Resolve lexical names by a separate AST walk, without source imports."""
    tree = ast.parse((ROOT / path).read_text(encoding="utf-8"))
    nodes = {}

    def walk(parent, prefix=""):
        for child in ast.iter_child_nodes(parent):
            qualified = prefix
            if isinstance(child, (ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)):
                qualified = f"{prefix}.{child.name}" if prefix else child.name
                nodes[qualified] = child
            walk(child, qualified)

    walk(tree)
    return tree, nodes


def curated_rows(doc):
    """Require the complete expected membership, including repeated family rows."""
    actual = {}
    for family in doc["families"]:
        for row in family["implementations"]:
            if row["path"] in {WORKFLOW, ANALYSIS}:
                key = (family["id"], row["path"], row["symbol"])
                assert key not in actual, f"duplicate member: {key}"
                actual[key] = row
    expected = {(family, path, symbol)
                for family, members in MEMBERS.items() for path, symbol in members}
    assert set(actual) == expected, "missing or unexpected pipelay member"
    return actual


def resolved_calls(path, symbol, tree, nodes):
    """Bind direct calls, imported functions and constructed instance methods.

    Only the two inspected modules are resolved. Constructors and unrelated
    validation/output calls do not become calculation-delegation edges.
    """
    imports = {}
    for item in tree.body:
        if isinstance(item, ast.ImportFrom) and item.module == "digitalmodel.orcaflex.pipelay_analysis":
            imports.update({alias.asname or alias.name: (ANALYSIS, alias.name)
                            for alias in item.names})
    node = nodes[symbol]
    receivers = {}
    if "." in symbol:
        receivers["self"] = (path, symbol.rsplit(".", 1)[0])
    for item in ast.walk(node):
        if isinstance(item, ast.Assign) and isinstance(item.value, ast.Call):
            constructor = item.value.func
            if isinstance(constructor, ast.Name) and constructor.id in imports:
                for target in item.targets:
                    if isinstance(target, ast.Name):
                        receivers[target.id] = imports[constructor.id]
    calls = set()
    for item in ast.walk(node):
        if not isinstance(item, ast.Call):
            continue
        function = item.func
        if isinstance(function, ast.Name):
            target = imports.get(function.id, (path, function.id))
        elif (isinstance(function, ast.Attribute) and isinstance(function.value, ast.Name)
              and function.value.id in receivers):
            target_path, receiver = receivers[function.value.id]
            target = (target_path, f"{receiver}.{function.attr}")
        else:
            continue
        if target in SYMBOLS:
            calls.add(target)
    return calls


def validate_inventory(doc):
    """Reject missing symbols, wrong roles and unsupported delegation edges."""
    rows = curated_rows(doc)
    sources = {path: source_index(path) for path in {p for p, _ in SYMBOLS}}
    for (_, path, symbol), row in rows.items():
        tree, nodes = sources[path]
        assert symbol in nodes, f"unresolved symbol: {path}:{symbol}"
        expected = EDGES.get((path, symbol), set())
        assert row["relation"] == ("delegate" if expected else "independent"), symbol
        declared = {(target["path"], target["symbol"])
                    for target in row.get("delegates_to", [])}
        assert declared == expected, f"incorrect delegation: {symbol}"
        assert resolved_calls(path, symbol, tree, nodes) == expected, symbol


def test_pipelay_symbols_resolve_without_import(document):
    """Every curated row resolves independently, without loading solver packages."""
    for (_, path, symbol), row in curated_rows(document).items():
        _, nodes = source_index(path)
        assert isinstance(nodes[symbol], (ast.FunctionDef, ast.AsyncFunctionDef))
        assert row["line"] == nodes[symbol].lineno
        assert row["file_sha256"] == hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
        body = copy.deepcopy(nodes[symbol])
        for node in ast.walk(body):
            # Python 3.12 adds this field; empty parameters have no 3.11 AST form.
            if getattr(node, "type_params", None) == []:
                node._fields = tuple(field for field in node._fields
                                     if field != "type_params")
            statements = getattr(node, "body", None)
            if (isinstance(statements, list) and statements
                    and isinstance(statements[0], ast.Expr)
                    and isinstance(statements[0].value, ast.Constant)
                    and isinstance(statements[0].value.value, str)):
                del statements[0]
        assert row["body_sha256"] == hashlib.sha256(
            ast.dump(body, include_attributes=False).encode()
        ).hexdigest()


def test_pipelay_delegation_matches_calls(document):
    """Expected relationships must agree with the actual inspected call sites."""
    validate_inventory(document)


@pytest.mark.parametrize("mutation", ["missing", "nonexistent", "relationship", "target"])
def test_missing_or_misclassified_member_is_detected(document, mutation):
    """Document mutations must fail independently of generator validation."""
    changed = copy.deepcopy(document)
    family = next(f for f in changed["families"] if f["id"] == BENDING)
    row = next(r for r in family["implementations"] if r["symbol"] == "run_reel_lay")
    if mutation == "missing":
        family["implementations"].remove(row)
    elif mutation == "nonexistent":
        row["symbol"] = "nonexistent_pipelay_member"
    elif mutation == "relationship":
        row["relation"] = "independent"
    else:
        row["delegates_to"] = [{"path": ANALYSIS, "symbol": "calculate_stinger_radius"}]
    with pytest.raises(AssertionError):
        validate_inventory(changed)
