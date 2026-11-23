# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline structural validation of the single-note mooring survey."""

from copy import deepcopy
from pathlib import Path
import re
from urllib.parse import urlsplit

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
DOCUMENT = ROOT / "docs/research/mooring-industry-practice.md"
MANIFEST = DOCUMENT.with_suffix(".sources.yaml")
RECORDS = DOCUMENT.with_suffix(".records.yaml")
HEADER_KEYS = {"schema_version", "document", "issue", "source_of_entries", "checked"}
# Five register-flagged passages are paraphrased; exact note wording stays in YAML.
PARAPHRASE_COUNT = 5
# Known note anomalies: section 11 omits M46; preserve that numbering gap.
# No read source is uncited outside section 11 in the research note.
SOURCE_IDS = [f"M{i}" for i in range(1, 49) if i != 46]
UNCITED_SOURCE_IDS = set()
# The note has four italic unverified tags, including its section 0 legend.
UNVERIFIED_TAG_COUNT = 4


def require(condition, message):
    if not condition:
        raise ValueError(message)


def fields(entry, required):
    require(isinstance(entry, dict) and required <= entry.keys(), "missing fields")


def validate_url(value):
    require(isinstance(value, str) and not re.search(r"\s", value), "URL text")
    parts = urlsplit(value)
    require(parts.scheme in {"http", "https"} and bool(parts.hostname), "URL host/scheme")
    require(parts.username is None and parts.password is None, "URL credentials")
    try:
        parts.port
    except ValueError as error:
        raise ValueError("URL port") from error


def section(text, heading):
    match = re.search(rf"^## {re.escape(heading)}\n(.*?)(?=^## |\Z)", text, re.M | re.S)
    require(match is not None, f"missing heading: {heading}")
    return match.group(1)


def bullets(text):
    return [line[2:] for line in text.splitlines() if line.startswith("- ")]


def table_rows(text):
    lines = [line for line in text.splitlines() if line.startswith("|")]
    return [[cell.strip() for cell in line.strip("|").split("|")] for line in lines[2:]]


def validate_sources(data, text):
    sources = data["sources"]
    ids = [entry["id"] for entry in sources]
    require(ids == SOURCE_IDS and len(set(ids)) == 47, "source id order/count")
    listed = section(text, "Sources read (all checked 2026-10-11)")
    expected = [f"[{e['id']}] {e.get('as_written_in_document', e['citation'])} {e['url']}" for e in sources]
    require(bullets(listed) == expected, "source list transcription")
    outside = text.replace(listed, "", 1)
    require(set(re.findall(r"\[(M\d+)\]", text)) <= set(ids), "unresolved citation")
    cited = set(re.findall(r"\[(M\d+)\]", outside))
    require(set(ids) - cited == UNCITED_SOURCE_IDS, "uncited source")
    for entry in sources:
        fields(entry, {"id", "citation", "url", "status"})
        require(entry["status"] == "read" and bool(entry["citation"]), "source status/text")
        validate_url(entry["url"])


def validate_located(data, text):
    entries = data["located_not_read"]
    listed = bullets(section(text, "Located, not read (kept out of conclusions)"))
    require(len(entries) == 16 and listed == [e.get("as_written_in_document", e["text"]) for e in entries], "located list")
    read_urls = {entry["url"] for entry in data["sources"]}
    for index, entry in enumerate(entries, 1):
        fields(entry, {"id", "text", "urls", "status"})
        require(entry["id"] == f"L{index}", "located id order")
        require(entry["status"] == "located_not_read", "located status")
        require(isinstance(entry["urls"], list), "located URL list")
        printed = re.findall(r"https?://[^\s)]+", entry["text"])
        require(entry["urls"] == printed, "located URLs differ from printed text")
        require(not read_urls.intersection(entry["urls"]), "located URL marked read")
        for url in entry["urls"]:
            validate_url(url)


def validate_properties(data, text):
    entries = data["open_substitute_properties"]
    require(bool(entries), "missing properties")
    body = section(text, "2. Vendor capability matrix (published properties and the page they come from)")
    body = body.split("**Open substitutes for vendor tables.**", 1)[1]
    for entry in entries:
        fields(entry, {"item", "quantity", "variant", "value_as_given", "unit_as_given",
                       "source_id", "caveat"})
        require(entry["source_id"] in SOURCE_IDS, "property source")
        require(entry["item"] and entry["quantity"] and entry["caveat"], "property description")
        for key in ("variant", "value_as_given", "unit_as_given"):
            require(entry[key] is None or isinstance(entry[key], str), f"property {key}")
        if entry["value_as_given"] is not None:
            require(entry["value_as_given"] in body, "missing verbatim property value")
        if entry["unit_as_given"] is not None:
            require(entry["unit_as_given"] in body, "missing verbatim property unit")


def validate_gaps(data, text):
    rows = table_rows(section(text, "9. Gap analysis for a small consultancy with strong open analysis capability"))
    entries = data["gap_analysis"]
    require(len(rows) == len(entries) == 7, "gap row count")
    for row, entry in zip(rows, entries):
        fields(entry, {"area", "verdict", "basis_source_ids"})
        verdict = entry.get("as_written_in_document", entry["verdict"])
        require(entry["verdict"] and row[:2] == [entry["area"], f"**{verdict}**"],
                "gap area/verdict")
        require(entry["basis_source_ids"] == re.findall(r"\[(M\d+)\]", row[2]), "gap basis")
        require(set(entry["basis_source_ids"]) <= set(SOURCE_IDS), "unresolved gap basis")


def validate_document(data, text):
    fields(data, {"schema_version", "document", "issue", "source_of_entries", "checked",
                  "sources", "located_not_read", "open_substitute_properties", "gap_analysis", "counts"})
    require(data["schema_version"] == 1 and data["issue"] == 1807, "schema/issue")
    require(data["document"] == "docs/research/mooring-industry-practice.md", "document")
    require(data["source_of_entries"] == "research note on the issue", "single source")
    require(data["checked"] == "2026-10-11", "checked date")
    require(data["counts"] == {"sources_read": 47, "located_not_read_documents": "about 27 documents",
                               "located_not_read_groups": 16}, "note counts")
    count_text = section(text, "Counts and acceptance-criteria status")
    require("Sources read: **47**" in count_text and "**about 27 documents** in 16 groups" in count_text,
            "document counts")
    require(len(re.findall(r"\*\([^\n]*?unverified[^\n]*?\)\*", text)) == UNVERIFIED_TAG_COUNT,
            "unverified tag count")
    validate_paraphrases(data, text)
    validate_sources(data, text)
    validate_located(data, text)
    validate_properties(data, text)
    validate_gaps(data, text)


def validate_paraphrases(data, text):
    changed = []
    for key, original in (("sources", "citation"), ("located_not_read", "text"),
                          ("gap_analysis", "verdict")):
        for entry in data[key]:
            if "as_written_in_document" in entry:
                value = entry["as_written_in_document"]
                require(isinstance(value, str) and bool(value) and value in text,
                        "missing paraphrase")
                require(isinstance(entry[original], str) and bool(entry[original]),
                        "empty original wording")
                changed.append(entry)
    require(len(changed) == PARAPHRASE_COUNT, "paraphrase count")


def validate_files(first, second, file_texts):
    fields(first, HEADER_KEYS | {"companion_file", "sources"})
    fields(second, HEADER_KEYS | {"companion_file", "located_not_read",
                               "open_substitute_properties", "gap_analysis", "counts"})
    require(all(first[k] == second[k] for k in HEADER_KEYS), "header disagreement")
    require(first["companion_file"] == RECORDS.name and
            second["companion_file"] == MANIFEST.name, "companion file")
    shared = HEADER_KEYS | {"companion_file"}
    require(not (first.keys() - shared) & (second.keys() - shared), "duplicate payload key")
    ids = [{entry["id"] for value in part.values() if isinstance(value, list)
            for entry in value if isinstance(entry, dict) and "id" in entry}
           for part in (first, second)]
    require(not ids[0] & ids[1], "duplicate cross-file id")
    return {**first, **second}


class UniqueKeyLoader(yaml.SafeLoader):
    """Reject repeated mapping keys before parsing loses their evidence."""


def unique_mapping(loader, node):
    pairs = loader.construct_pairs(node)
    require(len({key for key, _ in pairs}) == len(pairs), "duplicate YAML key")
    return dict(pairs)


UniqueKeyLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, unique_mapping)


def load_survey():
    texts = [path.read_text(encoding="utf-8") for path in (MANIFEST, RECORDS)]
    parts = [yaml.load(text, Loader=UniqueKeyLoader) for text in texts]
    return validate_files(*parts, texts), DOCUMENT.read_text(encoding="utf-8")


def test_survey():
    validate_document(*load_survey())


@pytest.mark.parametrize("kind", ["duplicate_id", "located_read", "bad_url", "missing_value",
                                  "bad_verdict", "missing_field", "unknown_citation", "lost_tag"])
def test_validator_rejects_broken_survey(kind):
    data, text = load_survey()
    data = deepcopy(data)
    if kind == "duplicate_id":
        data["sources"][1]["id"] = "M1"
    elif kind == "located_read":
        data["located_not_read"][0]["status"] = "read"
    elif kind == "bad_url":
        data["sources"][0]["url"] = "https:///missing-host"
    elif kind == "missing_value":
        data["open_substitute_properties"][0]["value_as_given"] = "invented value"
    elif kind == "bad_verdict":
        data["gap_analysis"][0]["verdict"] = ""
    elif kind == "missing_field":
        del data["sources"][0]["citation"]
    elif kind == "unknown_citation":
        text += "\n[M46]\n"
    elif kind == "lost_tag":
        text = text.replace("*(unverified)*", "unverified", 1)
    with pytest.raises((ValueError, KeyError)):
        validate_document(data, text)


@pytest.mark.parametrize("kind", ["header", "payload", "id", "companion"])
def test_validator_rejects_broken_file_pair(kind):
    texts = [path.read_text(encoding="utf-8") for path in (MANIFEST, RECORDS)]
    first, second = [yaml.load(text, Loader=UniqueKeyLoader) for text in texts]
    if kind == "header":
        second["checked"] = "different"
    elif kind == "payload":
        second["sources"] = first["sources"]
    elif kind == "id":
        second["located_not_read"][0]["id"] = "M1"
    elif kind == "companion":
        first["companion_file"] = "wrong.yaml"
    with pytest.raises(ValueError):
        validate_files(first, second, texts)


def test_duplicate_yaml_key_rejected():
    with pytest.raises(ValueError, match="duplicate YAML key"):
        yaml.load("key: 1\nkey: 2\n", Loader=UniqueKeyLoader)


@pytest.mark.parametrize("kind", ["missing", "original", "count", "gap"])
def test_validator_rejects_broken_paraphrase(kind):
    data, text = load_survey()
    data = deepcopy(data)
    entry = next(e for e in data["sources"] if "as_written_in_document" in e)
    if kind == "missing":
        entry["as_written_in_document"] = "absent paraphrase"
    elif kind == "original":
        entry["citation"] = ""
    elif kind == "count":
        del entry["as_written_in_document"]
    elif kind == "gap":
        entry = data["gap_analysis"][0]
        text = text.replace(entry["as_written_in_document"], entry["verdict"])
    with pytest.raises(ValueError):
        validate_document(data, text)
