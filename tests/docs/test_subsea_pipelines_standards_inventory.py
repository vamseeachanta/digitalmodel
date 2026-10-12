"""Offline validation of the issue research-note transcription, without shared schema."""

from collections import Counter
from copy import deepcopy
from pathlib import Path
from urllib.parse import urlsplit

import pytest
import yaml


INVENTORY = (
    Path(__file__).resolve().parents[2]
    / "docs/standards/subsea-pipelines-inventory.yaml"
)
# The research note's own Counts table: C = 22, N = 20, U = 3.
BASIS_COUNTS = {"currency-check": 22, "issuer-page": 20, "not-verified": 3}
BOUNDARIES = {"mooring", "risers-viv", "flexible-pipe", "subsea-production"}
GROUPS = {
    "A. Design codes",
    "B. On-bottom stability and pipe-soil interaction",
    "C. Free spans, trawl interference and third-party protection",
    "D. Global buckling and walking",
    "E. Integrity management, defect assessment and repair",
    "F. Materials and welding",
    "G. Corrosion control, cathodic protection and coatings",
    "H. Installation, testing and pre-commissioning",
    "I. Boundary rows (flexible pipe, risers, subsea production systems)",
}
FIELD_TYPES = {
    "code_id": str, "also_known_as": list, "designation": str,
    "publisher": str, "revision": str, "title": str, "scope": str,
    "key_methods": list, "applicability": str, "links_to": list,
    "references_statement": str, "group": str, "status": str,
    "edition_basis": str, "edition_verified": bool,
    "unverified_statements": list, "relation_to_neighbours": str,
    "boundary_flags": list, "boundary_reasons": dict, "sources": list,
    "date_checked": str, "accepted_edition_by_regime": list,
}


def require(condition, message):
    if not condition:
        raise ValueError(message)


def strings(value, name):
    require(isinstance(value, list), name)
    require(all(isinstance(item, str) and item for item in value), name)


def validate_source(source):
    require(isinstance(source, dict), "source mapping")
    require(isinstance(source.get("key"), str), "source key")
    require(isinstance(source.get("remark"), str), "source remark")
    if "urls" not in source:
        return  # A named site without an address is deliberately not completed.
    strings(source["urls"], "source urls")
    for url in source["urls"]:
        parsed = urlsplit(url)
        require(parsed.scheme == "https" and bool(parsed.hostname), "https URL")
        require(not parsed.username and not parsed.password, "URL credentials")
        require(not any(char.isspace() for char in url), "URL whitespace")
        require(parsed.port is None or 0 < parsed.port < 65536, "URL port")


def validate_entry(entry, targets, source_catalog):
    require(isinstance(entry, dict), "entry mapping")
    for field, field_type in FIELD_TYPES.items():
        require(type(entry.get(field)) is field_type, f"field type: {field}")
    for field in ("also_known_as", "key_methods", "links_to",
                  "unverified_statements", "boundary_flags"):
        strings(entry[field], field)
    require(entry["edition_basis"] in BASIS_COUNTS, "edition basis")
    require(entry["group"] in GROUPS, "group")
    require(set(entry["boundary_flags"]) <= BOUNDARIES, "boundary flag")
    require(set(entry["boundary_reasons"]) == set(entry["boundary_flags"]),
            "boundary reasons")
    require(all(isinstance(reason, str) and reason
                for reason in entry["boundary_reasons"].values()), "boundary reason")
    require(set(entry["unverified_statements"]) <= set(entry), "uncertain field")
    require(entry["date_checked"] == "2026-10-11", "check date")
    if entry["edition_basis"] == "not-verified" or "edition not verified" in entry["revision"]:
        require(entry["edition_verified"] is False, "unverified edition")
    require(set(entry["links_to"]) <= targets, "unresolved reference")
    for source in entry["sources"]:
        validate_source(source)
        require(source["key"] in source_catalog, "unresolved source")
        require(source == source_catalog[source["key"]], "source mismatch")
    validate_regimes(entry["accepted_edition_by_regime"], source_catalog)
    if "issue_designation_correction" in entry:
        correction = entry["issue_designation_correction"]
        require(isinstance(correction, list) and correction, "corrections")
        for item in correction:
            require(set(item) == {"issue_text", "issuer_page_today"}, "correction keys")
            require(all(isinstance(value, str) and value for value in item.values()),
                    "correction text")


def validate_regimes(regimes, source_catalog):
    for regime in regimes:
        require(isinstance(regime, dict), "regime mapping")
        require(set(regime) == {"regime", "edition_or_statement", "source_key"},
                "regime keys")
        require(all(isinstance(value, str) and value for value in regime.values()),
                "regime strings")
        require(regime["source_key"] in source_catalog, "regime source")


def forbid_pages(value):
    if isinstance(value, dict):
        require("pages_relevant" not in value, "forbidden page field")
        for child in value.values():
            forbid_pages(child)
    elif isinstance(value, list):
        for child in value:
            forbid_pages(child)


def validate_inventory(data):
    require(isinstance(data, dict) and set(data) == {"meta", "standards"}, "layout")
    meta, entries = data["meta"], data["standards"]
    require(isinstance(meta, dict) and isinstance(entries, list), "top-level types")
    require(len(entries) == 45, "row count")
    ids = [entry["code_id"] for entry in entries]
    require(len(set(ids)) == 45, "unique code ids")
    strings(meta["external_references"], "external references")
    targets = set(ids) | set(meta["external_references"])
    for entry in entries:
        targets.update(entry["also_known_as"])
    require(isinstance(meta["source_keys"], list), "source catalog")
    catalog = {source["key"]: source for source in meta["source_keys"]}
    require(len(catalog) == len(meta["source_keys"]), "unique source keys")
    for source in catalog.values():
        validate_source(source)
    for entry in entries:
        validate_entry(entry, targets, catalog)
    require(Counter(entry["edition_basis"] for entry in entries) == BASIS_COUNTS,
            "note basis counts")
    require(meta["counts"] == {**BASIS_COUNTS, "total": 45}, "metadata counts")
    require(set(meta["edition_basis_legend"]) == set(BASIS_COUNTS), "basis legend")
    require(meta["date_checked"] == "2026-10-11", "metadata date")
    strings(meta["acceptance_criteria_not_met"], "unmet criteria")
    strings(meta["cross_reference_edges"], "graph edges")
    forbid_pages(data)


@pytest.fixture
def inventory():
    return yaml.safe_load(INVENTORY.read_text(encoding="utf-8"))


def test_inventory_schema_and_integrity(inventory):
    validate_inventory(inventory)


@pytest.mark.parametrize("mutation", [
    "duplicate", "type", "basis", "boundary", "uncertain", "edition",
    "url", "reference", "regime-source", "pages", "count", "source-mismatch",
])
def test_validator_rejects_broken_in_memory_inventory(inventory, mutation):
    broken = deepcopy(inventory)
    row = broken["standards"][0]
    if mutation == "duplicate":
        row["code_id"] = broken["standards"][1]["code_id"]
    elif mutation == "type":
        row["key_methods"] = "not a list"
    elif mutation == "basis":
        row["edition_basis"] = "unknown"
    elif mutation == "boundary":
        row["boundary_flags"] = ["unknown"]
    elif mutation == "uncertain":
        row["unverified_statements"] = ["missing-field"]
    elif mutation == "edition":
        next(item for item in broken["standards"]
             if item["edition_basis"] == "not-verified")["edition_verified"] = True
    elif mutation == "url":
        broken["meta"]["source_keys"][0]["urls"] = ["http://example.invalid/"]
    elif mutation == "reference":
        row["links_to"] = ["unknown"]
    elif mutation == "regime-source":
        row["accepted_edition_by_regime"][0]["source_key"] = "unknown"
    elif mutation == "pages":
        row["pages_relevant"] = []
    elif mutation == "count":
        broken["standards"].pop()
    elif mutation == "source-mismatch":
        row["sources"][0]["remark"] = "changed"
    with pytest.raises(ValueError):
        validate_inventory(broken)
