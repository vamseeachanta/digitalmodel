# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline evidence contract; public responses are inspected outside pytest."""

from copy import deepcopy
from datetime import date
from hashlib import sha256
from pathlib import Path
import re
from urllib.parse import urlsplit

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
INVENTORY = ROOT / "docs/standards/hydrodynamics-inventory.yaml"
SUPPLEMENT = ROOT / "docs/standards/hydrodynamics-edition-verification.yaml"
PIN = "73bdbcbf0c28e1103fc0fb0464b1ad1177b46bffa984b8a464fd34fc1ee79076"
FIELDS = {"title", "edition", "amendment_currency", "supersession", "applicability"}
HOSTS = {"DNV": {"www.dnv.com", "rules.dnv.com"},
         "ABS": {"ww2.eagle.org"}, "IMO": {"www.imo.org"}}


def require(condition, message):
    if not condition:
        raise ValueError(message)


def text(value):
    return isinstance(value, str) and bool(value.strip())


def iso_date(value):
    require(text(value) and re.fullmatch(r"\d{4}-\d{2}-\d{2}", value), "ISO date")
    return date.fromisoformat(value)


def validate(document, inventory_bytes):
    """Fail closed on unsupported facts, unknown fields and changed source bytes."""
    require(isinstance(document, dict) and set(document) == {"meta", "records"}, "root keys")
    meta = document["meta"]
    require(isinstance(meta, dict) and set(meta) == {
        "inventory_sha256", "observed_on", "limitation"}, "meta keys")
    require(meta["inventory_sha256"] == PIN == sha256(inventory_bytes).hexdigest(), "digest")
    iso_date(meta["observed_on"])
    require(text(meta["limitation"]), "meta limitation")
    historical = yaml.safe_load(inventory_bytes)
    targets = {row["code_id"]: row for row in historical["standards"]
               if row["edition_basis"] == "not-verified"}
    records = document["records"]
    require(isinstance(records, list) and len(records) == 7, "seven records")
    seen = set()
    for row in records:
        require(isinstance(row, dict) and set(row) == FIELDS | {
            "code_id", "publisher", "observed_on", "evidence", "verification_scope",
            "limitation", "next_checkpoint"}, "record keys")
        code = row["code_id"]
        require(text(code) and code in targets and code not in seen, "record identity")
        seen.add(code)
        require(row["publisher"] == targets[code]["publisher"], "publisher")
        iso_date(row["observed_on"])
        require(row["observed_on"] == meta["observed_on"], "observation date")
        for key in ("verification_scope", "limitation", "next_checkpoint"):
            require(text(row[key]), key)
        evidence = row["evidence"]
        require(isinstance(evidence, dict) and set(evidence) == {
            "url", "locator", "outcome", "supports"}, "evidence keys")
        for key in ("url", "locator", "outcome"):
            require(text(evidence[key]), key)
        url = urlsplit(evidence["url"])
        require(url.scheme == "https" and url.hostname in HOSTS[row["publisher"]]
                and url.username is None and url.password is None
                and url.port in (None, 443)
                and not any(c.isspace() for c in evidence["url"]), "issuer HTTPS URL")
        supports = evidence["supports"]
        require(isinstance(supports, list) and all(text(v) for v in supports), "supports")
        require(len(set(supports)) == len(supports) and set(supports) <= FIELDS, "supported fields")
        for field in FIELDS:
            fact = row[field]
            require(isinstance(fact, dict) and set(fact) == {"state", "value"}, "field keys")
            require(fact["state"] in ("verified", "partial", "unresolved"), "field state")
            if fact["state"] == "unresolved":
                require(fact["value"] is None and field not in supports, "unresolved value")
            else:
                require(text(fact["value"]) and field in supports, "field evidence")
                require(evidence["outcome"].startswith("HTTP 200;"), "successful evidence")
            # Currency wording in a value must have dated, field-specific evidence.
            if isinstance(fact["value"], str) and re.search(
                    r"\b(current|latest|in force)\b", fact["value"], re.I):
                require(field in supports and text(row["observed_on"]), "currency evidence")
    require(seen == set(targets), "target set")


def load():
    return yaml.safe_load(SUPPLEMENT.read_text(encoding="utf-8"))


def test_supplement_contract():
    validate(load(), INVENTORY.read_bytes())


def test_historical_bytes_remain_pinned():
    assert sha256(INVENTORY.read_bytes()).hexdigest() == PIN


def test_bounded_observations_and_unresolved_remainders():
    rows = {row["code_id"]: row for row in load()["records"]}
    expected = {
        "DNV-ST-0119": ("Floating wind turbines", "2026-10", "verified"),
        "IMO MODU Code": ("2009 MODU Code", "2020", "partial"),
        "ABS Guidance Notes on Air Gap and Wave Impact Analysis for Semi-Submersibles": (
            "Guidance Notes on Air Gap and Wave Impact Analysis for Semi-Submersibles",
            "May 2020", "partial"),
        "ABS Guidance Notes on Global Performance and Integrated Load Analysis for Offshore Wind Turbines": (
            "Guidance Notes on Global Performance and Integrated Load Analysis for Offshore Wind Turbines",
            "August 2023", "partial"),
    }
    for code, row in rows.items():
        if code in expected:
            title, edition, state = expected[code]
            assert row["title"]["value"] == title
            assert row["edition"] == {"state": state, "value": edition}
        else:
            assert row["title"] == row["edition"] == {"state": "unresolved", "value": None}
        for field in ("amendment_currency", "supersession", "applicability"):
            assert row[field] == {"state": "unresolved", "value": None}
    assert '"ABS Guidance Notes on Wave Loads"' in rows


@pytest.mark.parametrize(("path", "value"), [
    (("meta", "inventory_sha256"), "0" * 64),
    (("meta", "observed_on"), "2026-02-30"),
    (("meta", "observed_on"), "20261113"),
    (("meta", "observed_on"), date(2026, 11, 13)),
    (("records", 0, "code_id"), "unknown"),
    (("records", 0, "publisher"), "ABS"),
    (("records", 0, "observed_on"), "2026-11-12"),
    (("records", 0, "title", "value"), "invented"),
    (("records", 0, "title", "state"), "assumed"),
    (("records", 0, "evidence", "locator"), ""),
    (("records", 0, "evidence", "url"), "http://rules.dnv.com/"),
    (("records", 0, "evidence", "url"), "https://example.invalid/"),
    (("records", 0, "evidence", "url"), "https://rules.dnv.com:bad/"),
    (("records", 0, "evidence", "outcome"), ""),
    (("records", 0, "evidence", "supports"), ["title"]),
    (("records", 0, "limitation"), ""),
    (("records", 0, "next_checkpoint"), ""),
])
def test_rejects_malformed_metadata(path, value):
    document = deepcopy(load())
    target = document
    for key in path[:-1]:
        target = target[key]
    target[path[-1]] = value
    with pytest.raises(ValueError):
        validate(document, INVENTORY.read_bytes())


@pytest.mark.parametrize("defect", [
    "duplicate", "missing_record", "missing_evidence", "field_evidence", "failed_access",
    "currency_source", "currency_date", "formulas", "pages_relevant", "normative_text",
])
def test_rejects_unsupported_claims(defect):
    document = deepcopy(load())
    row = next(r for r in document["records"] if r["code_id"] == "DNV-ST-0119")
    if defect == "duplicate":
        document["records"][0] = deepcopy(row)
    elif defect == "missing_record":
        document["records"].pop()
    elif defect == "missing_evidence":
        del row["evidence"]
    elif defect == "field_evidence":
        row["evidence"]["supports"] = ["title"]
    elif defect == "failed_access":
        row["evidence"]["outcome"] = "HTTP 403; inaccessible"
    elif defect == "currency_source":
        row["edition"]["value"] = "latest edition"
        row["evidence"]["supports"] = []
    elif defect == "currency_date":
        row["edition"]["value"] = "currently in force"
        row["observed_on"] = None
    else:
        row[defect] = "unsupported content"
    with pytest.raises(ValueError):
        validate(document, INVENTORY.read_bytes())


def test_rejects_changed_historical_input():
    with pytest.raises(ValueError, match="digest"):
        validate(load(), INVENTORY.read_bytes() + b"\n")
