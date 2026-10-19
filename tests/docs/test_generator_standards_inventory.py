# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline contracts for the generator standards inventory.

Expected coverage comes from the four use sites listed in issue #2516 and
an independent AST inventory of the emitted fields. Bibliographic expectations
come from the public DNV product page, API publications catalogue and repository
materials references named in the inventory. No solver or engineering acceptance
calculation is performed; no numerical standard value is used as an oracle.
"""

import ast
import json
from pathlib import Path
from urllib.parse import urlsplit

import pytest

ROOT = Path(__file__).resolve().parents[2]
PACKAGE = ROOT / "src/digitalmodel/solvers/orcaflex/modular_generator"
INVENTORY = ROOT / "docs/domains/orcaflex/generator-standards.json"


@pytest.fixture
def inventory():
    return json.loads(INVENTORY.read_text(encoding="utf-8"))


def node_at(path, symbol):
    node = ast.parse((ROOT / path).read_text(encoding="utf-8"))
    for part in symbol.split("."):
        node = next(
            child
            for child in node.body
            if getattr(child, "name", None) == part
            or isinstance(child, ast.Assign)
            and any(isinstance(t, ast.Name) and t.id == part for t in child.targets)
            or isinstance(child, ast.AnnAssign)
            and isinstance(child.target, ast.Name)
            and child.target.id == part
        )
    return node


def dict_keys(node):
    return {
        key.value
        for child in ast.walk(node)
        if isinstance(child, ast.Dict)
        for key in child.keys
        if isinstance(key, ast.Constant) and isinstance(key.value, str)
    }


def use(inventory, identifier):
    return next(row for row in inventory["uses"] if row["id"] == identifier)


def test_issue_sites_have_explicit_unresolved_governing_locators(inventory):
    assert set(inventory["standards"]) == {
        "DNV-ST-F101",
        "DNV-OS-E302",
        "API RP 2SK",
        "API Spec 5L",
    }
    assert {row["path"] for row in inventory["uses"]} == {
        str(path.relative_to(ROOT))
        for path in (
            PACKAGE / "builders/linetype_builder.py",
            PACKAGE / "routers/mooring_router.py",
            PACKAGE / "schema/mooring.py",
            PACKAGE / "schema/pipeline.py",
        )
    }
    for standard in inventory["standards"].values():
        assert standard["publisher"] in {"DNV", "American Petroleum Institute"}
        assert standard["governing_edition"] is None
        assert standard["clause"] is None
        assert standard["clause_title"] is None
        assert standard["status"] == "NOT_EVALUATED"
        assert standard["limitation"].strip()
        assert standard["edition_difference"].strip()
        assert set(standard["sources"]) <= inventory["sources"].keys()
        assert standard["sources"]


def test_inventory_symbols_and_parameters_resolve_without_importing_solver(inventory):
    ids = [row["id"] for row in inventory["uses"]]
    assert len(ids) == len(set(ids))
    for row in inventory["uses"]:
        node_at(row["path"], row["symbol"])
        assert row["parameters"]
        assert row["status"] == "NOT_EVALUATED"
        assert row["limitation"].strip()
        assert set(row["standards"]) <= inventory["standards"].keys()
        assert row["standards"]


def test_all_emitted_dnv_inputs_are_inventoried(inventory):
    row = use(inventory, "pipe-code-inputs")
    emitted = {
        key
        for key in dict_keys(node_at(row["path"], row["symbol"]))
        if key.startswith("DNVSTF101")
    }
    assert set(row["parameters"]) == emitted
    assert emitted


def test_material_and_chain_defaults_are_distinguished_from_standard_rules(inventory):
    for identifier in ("pipe-material", "chain-defaults"):
        row = use(inventory, identifier)
        keys = dict_keys(node_at(row["path"], row["symbol"]))
        assert set(row["parameters"]) == keys - {
            "X52",
            "X60",
            "X65",
            "X70",
            "R3",
            "R3S",
            "R4",
            "R4S",
            "R5",
        }
        assert row["basis"] == "repository defaults; not qualified standard values"
    assert (
        use(inventory, "chain-breaking-load")["basis"]
        == "unresolved standard attribution"
    )


def test_catalogue_editions_do_not_select_the_governing_edition(inventory):
    """Metadata only: DNV product heading and API catalogue printed page 6."""
    f101 = inventory["standards"]["DNV-ST-F101"]
    assert f101["title"] == "Submarine pipeline systems"
    assert f101["catalogue_edition"] == "2021-08, amended 2021-12"
    api = inventory["standards"]["API RP 2SK"]
    assert api["title"] == (
        "Design and Analysis of Stationkeeping Systems for Floating Offshore Structures"
    )
    assert api["catalogue_edition"] == "4th edition, February 2024"
    assert inventory["standards"]["API Spec 5L"]["repository_edition"] == "46th edition"
    assert inventory["standards"]["DNV-OS-E302"]["title"] is None


def test_source_records_preserve_the_evidence_boundary(inventory):
    for source in inventory["sources"].values():
        assert source["supports"].strip()
        assert source["limitation"].strip()
        if "url" in source:
            parsed = urlsplit(source["url"])
            assert parsed.scheme == "https" and parsed.hostname
            assert parsed.username is None and parsed.password is None
            assert source["checked_on"]
            assert source["http_status"] in {200, 404}
        else:
            node_at(source["path"], source["symbol"])
    # An HTTP success serving an error page is not bibliographic evidence.
    assert inventory["sources"]["api-5l-unavailable"]["result"] == "error page"


def test_consumer_boundaries_record_missing_citation_sidecars(inventory):
    assert {row["symbol"] for row in inventory["boundaries"]} == {
        "MooringRouter.route",
        "ModularModelGenerator._generate",
        "ModularModelGenerator._generate_with_overrides",
        "write_variation_model",
    }
    for boundary in inventory["boundaries"]:
        node_at(boundary["path"], boundary["symbol"])
        assert boundary["citation_sidecar"] == "absent"
        assert boundary["status"] == "NOT_EVALUATED"
        assert boundary["required_before_qualification"]


def test_missing_sidecar_assessment_matches_current_package():
    """Reassess the inventory if the package starts carrying Citation objects."""
    for path in PACKAGE.rglob("*.py"):
        for node in ast.walk(ast.parse(path.read_text(encoding="utf-8"))):
            if isinstance(node, ast.Name):
                assert node.id not in {"Citation", "CitedValue"}, path
            if isinstance(node, ast.ImportFrom) and node.module:
                assert not node.module.startswith("digitalmodel.citations"), path
