"""Offline validation of the inventory transcribed from the issue research note."""

from collections import Counter
from copy import deepcopy
from pathlib import Path
from urllib.parse import urlsplit

import pytest
import yaml

INVENTORY = (
    Path(__file__).resolve().parents[2]
    / "docs/standards/hydrodynamics-inventory.yaml"
)
# The research note's own Counts table: C = 19, N = 11, U = 7.
COUNTS = {"currency-check": 19, "issuer-page": 11, "not-verified": 7}
STRING_FIELDS = (
    "code_id", "publisher", "revision", "title", "scope", "applicability",
    "group", "status", "edition_basis", "relation_to_neighbours",
    "date_checked", "references_text",
)
LIST_FIELDS = (
    "key_methods", "links_to", "unverified_statements", "also_known_as", "formerly",
)
REQUIRED = set(STRING_FIELDS + LIST_FIELDS) | {
    "edition_verified", "sources", "accepted_edition_by_regime",
}


def require(condition, message):
    """Reject invalid input without depending on optimizable assert statements."""
    if not condition:
        raise ValueError(message)


def validate_source(source):
    require(isinstance(source, dict), "source must be a mapping")
    require(set(source) == {"source_key", "urls", "remark"}, "source keys")
    require(isinstance(source["source_key"], str), "source key type")
    require(isinstance(source["remark"], str), "source remark type")
    require(isinstance(source["urls"], list), "source URLs type")
    for url in source["urls"]:
        require(isinstance(url, str), "URL type")
        parts = urlsplit(url)
        require(
            parts.scheme == "https" and bool(parts.hostname)
            and parts.username is None and parts.password is None
            and not any(char.isspace() for char in url),
            "source URL must be well-formed HTTPS",
        )
        try:
            parts.port
        except ValueError as error:
            raise ValueError("invalid URL port") from error


def validate_entry(entry, targets, sources, groups):
    require(isinstance(entry, dict), "entry must be a mapping")
    require(REQUIRED <= entry.keys(), "missing required field")
    for field in STRING_FIELDS:
        require(isinstance(entry[field], str), f"{field} must be text")
    for field in LIST_FIELDS:
        require(isinstance(entry[field], list), f"{field} must be a list")
        require(all(isinstance(item, str) for item in entry[field]), field)
    require(entry["edition_basis"] in COUNTS, "unknown edition basis")
    require(entry["group"] in groups, "unknown group")
    require(type(entry["edition_verified"]) is bool, "verification must be boolean")
    if entry["edition_basis"] == "not-verified" or "edition not verified" in entry["revision"]:
        require(entry["edition_verified"] is False, "unverified edition")
    require(entry["date_checked"] == "2026-10-11", "check date")
    require(set(entry["unverified_statements"]) <= entry.keys(), "unverified fields")
    require(set(entry["links_to"]) <= targets, "unresolved reference")
    require(isinstance(entry["sources"], list), "sources must be a list")
    for source in entry["sources"]:
        validate_source(source)
        require(sources.get(source["source_key"]) == source, "source mismatch")
    require(isinstance(entry["accepted_edition_by_regime"], list), "regimes type")
    for regime in entry["accepted_edition_by_regime"]:
        require(isinstance(regime, dict), "regime must be a mapping")
        require(set(regime) == {"regime", "edition_or_statement", "source_key"}, "regime keys")
        require(all(isinstance(value, str) and value for value in regime.values()), "regime types")
        require(regime["source_key"] in sources, "unresolved regime source")


def reject_pages(value):
    if isinstance(value, dict):
        require("pages_relevant" not in value, "pages_relevant is prohibited")
        for child in value.values():
            reject_pages(child)
    elif isinstance(value, list):
        for child in value:
            reject_pages(child)


def validate_inventory(inventory):
    require(isinstance(inventory, dict), "inventory must be a mapping")
    require(set(inventory) == {"meta", "standards"}, "top-level keys")
    meta, entries = inventory["meta"], inventory["standards"]
    require(isinstance(meta, dict), "meta must be a mapping")
    require(isinstance(entries, list) and len(entries) == 37, "expected 37 rows")
    for field in ("external_references", "groups", "source_catalog"):
        require(isinstance(meta.get(field), list), f"meta {field} type")
    require(all(isinstance(item, str) for item in meta["external_references"]), "external types")
    require(all(isinstance(item, str) for item in meta["groups"]), "group types")
    sources = {}
    for source in meta["source_catalog"]:
        validate_source(source)
        require(source["source_key"] not in sources, "duplicate source key")
        sources[source["source_key"]] = source
    require(all(isinstance(entry, dict) for entry in entries), "entry types")
    ids = [entry.get("code_id") for entry in entries]
    require(all(isinstance(code, str) for code in ids), "code_id types")
    require(len(set(ids)) == 37, "duplicate code_id")
    targets = set(ids) | set(meta["external_references"])
    for entry in entries:
        for field in ("also_known_as", "formerly"):
            aliases = entry.get(field)
            require(isinstance(aliases, list), "alias type")
            require(all(isinstance(alias, str) for alias in aliases), "alias values")
            targets.update(aliases)
    for entry in entries:
        validate_entry(entry, targets, sources, set(meta["groups"]))
    counts = Counter(entry["edition_basis"] for entry in entries)
    require(dict(counts) == COUNTS, "note Counts table mismatch")
    require(meta.get("counts") == {"total": 37, **COUNTS}, "meta counts mismatch")
    require(set(meta.get("edition_basis", {})) == set(COUNTS), "basis legend")
    reject_pages(inventory)


def load_inventory():
    return yaml.safe_load(INVENTORY.read_text(encoding="utf-8"))


def test_inventory_contract():
    validate_inventory(load_inventory())


@pytest.mark.parametrize("defect", [
    "missing", "wrong_type", "basis", "duplicate", "unverified", "url",
    "reference", "regime_key", "regime_source", "pages", "count", "group",
])
def test_validator_rejects_broken_in_memory_entry(defect):
    inventory = deepcopy(load_inventory())
    entry = inventory["standards"][0]
    if defect == "missing":
        del entry["publisher"]
    elif defect == "wrong_type":
        entry["key_methods"] = "not a list"
    elif defect == "basis":
        entry["edition_basis"] = "unknown"
    elif defect == "duplicate":
        entry["code_id"] = inventory["standards"][1]["code_id"]
    elif defect == "unverified":
        next(row for row in inventory["standards"] if row["edition_basis"] == "not-verified")["edition_verified"] = True
    elif defect == "url":
        entry["sources"][0]["urls"] = ["http://example.invalid/source"]
    elif defect == "reference":
        entry["links_to"] = ["unknown designation"]
    elif defect == "regime_key":
        entry["accepted_edition_by_regime"] = [{"regime": "example"}]
    elif defect == "regime_source":
        entry["accepted_edition_by_regime"] = [
            {"regime": "example", "edition_or_statement": "example", "source_key": "unknown"},
        ]
    elif defect == "pages":
        entry["pages_relevant"] = []
    elif defect == "count":
        entry["edition_basis"] = "issuer-page"
    elif defect == "group":
        entry["group"] = "unknown"
    with pytest.raises(ValueError):
        validate_inventory(inventory)
