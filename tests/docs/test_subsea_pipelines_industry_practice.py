"""Offline validation of the industry-practice research-note transcription."""

from copy import deepcopy
from pathlib import Path
import re
from urllib.parse import urlsplit

import pytest
import yaml

ROOT = Path(__file__).parents[2]
DOCUMENT = ROOT / "docs/research/subsea-pipelines-industry-practice.md"
SOURCE_LIST = DOCUMENT.with_suffix(".sources.yaml")
SOURCE_IDS = [f"P{i}" for i in range(1, 35)]
# The note has five tags beginning *(unverified, including qualified tags.
NOTE_UNVERIFIED_TAG_COUNT = 5
# Explicit exceptions for sources never cited outside section 11 in the note.
# None: all 34 source IDs occur outside the note's Sources read list.
KNOWN_NOTE_UNCITED_SOURCES = []
SOURCE_HEADING = "Sources read"
LOCATED_HEADING = "Located, not read (kept out of conclusions)"
MATRIX_HEADING = "4. Contractor and vessel capability matrix"
GAP_HEADING = "10. Gap analysis for a small consultancy with strong open analysis capability"


def section(text, heading, level=2):
    marker = "#" * level + " "
    parts = text.split(marker + heading + "\n")
    assert len(parts) == 2, f"missing or repeated heading: {heading}"
    return parts[1].split("\n" + marker, 1)[0]


def table_rows(text):
    lines = [line for line in text.splitlines() if line.startswith("|")]
    assert len(lines) >= 3, "missing table"
    rows = [[cell.strip() for cell in line.strip("|").split("|")] for line in lines]
    assert all(re.fullmatch(r":?-+:?", cell) for cell in rows[1])
    assert all(len(row) == len(rows[0]) for row in rows)
    return rows[2:]


def source_ids(text):
    return re.findall(r"\[(P\d+)\]", text)


def validate_url(value):
    assert isinstance(value, str) and value and not re.search(r"\s", value)
    address = urlsplit(value)
    assert address.scheme in {"http", "https"} and address.hostname
    assert not address.username and not address.password
    assert address.port is None or 0 < address.port < 65536


def require_fields(entry, fields):
    assert isinstance(entry, dict) and set(fields) <= entry.keys()
    for field in fields:
        assert entry[field] is not None, f"null field: {field}"
    for field in set(fields) - {"urls", "basis_source_ids"}:
        assert isinstance(entry[field], str) and entry[field]


def validate_metadata(data):
    assert isinstance(data, dict)
    required = {"schema_version", "document", "issue", "source_of_entries", "checked",
                "sources", "located_not_read", "gap_analysis", "counts"}
    assert required <= data.keys()
    assert data["schema_version"] == 1
    assert data["document"] == "docs/research/subsea-pipelines-industry-practice.md"
    assert data["issue"] == 1815
    assert data["source_of_entries"] == "research note on the issue"
    assert data["checked"] == "2026-10-11"
    assert data["counts"] == {"sources_read": 34,
                              "located_not_read_documents": "about 18 documents",
                              "located_not_read_groups": 10}
    for field in ("sources", "located_not_read", "gap_analysis"):
        assert isinstance(data[field], list) and data[field]


def validate_sources(text, data):
    entries = data["sources"]
    assert [entry["id"] for entry in entries] == SOURCE_IDS
    assert len(entries) == data["counts"]["sources_read"]
    read_list = section(text, SOURCE_HEADING)
    parsed = re.findall(r"^- \[(P\d+)\] (.+?) (https?://\S+)$", read_list, re.M)
    assert len(parsed) == len(entries)
    outside = text.replace("## " + SOURCE_HEADING + "\n" + read_list, "", 1)
    used = set(source_ids(outside))
    assert set(source_ids(text)) <= set(SOURCE_IDS)
    assert set(KNOWN_NOTE_UNCITED_SOURCES) == set(SOURCE_IDS) - used
    for entry, record in zip(entries, parsed):
        require_fields(entry, ("id", "citation", "url", "status"))
        assert entry["status"] == "read"
        validate_url(entry["url"])
        assert record == (entry["id"], entry["citation"], entry["url"])
        assert entry["id"] in used or entry["id"] in KNOWN_NOTE_UNCITED_SOURCES


def validate_located(text, data):
    entries = data["located_not_read"]
    assert [entry["id"] for entry in entries] == [f"L{i}" for i in range(1, 11)]
    assert len(entries) == data["counts"]["located_not_read_groups"]
    located = section(text, LOCATED_HEADING)
    bullets = re.findall(r"^- (.+)$", located, re.M)
    assert len(bullets) == len(entries)
    read_urls = {entry["url"] for entry in data["sources"]}
    for entry, bullet in zip(entries, bullets):
        require_fields(entry, ("id", "text", "urls", "status"))
        assert entry["status"] == "located_not_read"
        assert entry["text"] == bullet
        assert isinstance(entry["urls"], list)
        assert entry["urls"] == re.findall(r"https?://[^\s)]+", bullet)
        assert text.count(bullet) == 1, "located item repeated outside its list"
        for url in entry["urls"]:
            validate_url(url)
            assert url not in read_urls
            assert text.count(url) == 1, "located URL repeated outside its list"


def validate_matrix(text):
    matrix = section(text, MATRIX_HEADING)
    read = table_rows(section(matrix, "Rows sourced to pages read", level=3))
    unread = table_rows(section(matrix, "Unverified or not reached (kept out of conclusions)", 3))
    assert len(read) == 4 and len(unread) == 3
    for row in read:
        joined = " | ".join(row)
        assert "unverified" not in joined and "not reached" not in joined
        assert source_ids(row[-1]) and set(source_ids(joined)) <= set(SOURCE_IDS)
    assert [row[-1] for row in unread] == ["section 12", "section 12", "section 13"]
    for row in unread:
        joined = " | ".join(row)
        assert "*(unverified" in joined or "not reached" in joined
        assert row[-1] in {"section 12", "section 13"}
        assert not source_ids(joined)


def validate_gaps(text, data):
    rows = table_rows(section(text, GAP_HEADING))
    entries = data["gap_analysis"]
    assert len(rows) == len(entries) == 9
    for entry, row in zip(entries, rows):
        require_fields(entry, ("area", "verdict", "basis_source_ids"))
        assert row[0] == entry["area"]
        assert row[1] == "**" + entry["verdict"] + "**"
        assert isinstance(entry["basis_source_ids"], list)
        assert source_ids(row[2]) == entry["basis_source_ids"]
        assert set(entry["basis_source_ids"]) <= set(SOURCE_IDS)


def validate_document(text, data):
    validate_metadata(data)
    validate_sources(text, data)
    validate_located(text, data)
    validate_matrix(text)
    validate_gaps(text, data)
    assert text.count("*(unverified") == NOTE_UNVERIFIED_TAG_COUNT
    for url in re.findall(r"https?://[^\s)]+", text):
        validate_url(url)


def test_industry_practice_document():
    validate_document(DOCUMENT.read_text(encoding="utf-8"),
                      yaml.safe_load(SOURCE_LIST.read_text(encoding="utf-8")))


@pytest.mark.parametrize("field,value", [
    ("id", "P99"), ("citation", ""), ("url", "https:///missing-host"),
    ("status", "located_not_read"),
])
def test_validator_rejects_broken_source(field, value):
    data = deepcopy(yaml.safe_load(SOURCE_LIST.read_text(encoding="utf-8")))
    data["sources"][0][field] = value
    with pytest.raises(AssertionError):
        validate_document(DOCUMENT.read_text(encoding="utf-8"), data)


@pytest.mark.parametrize("part,field,value", [
    ("located_not_read", "status", "read"),
    ("located_not_read", "urls", ["https:///missing-host"]),
    ("gap_analysis", "verdict", ""),
    ("gap_analysis", "basis_source_ids", ["P99"]),
])
def test_validator_rejects_broken_auxiliary_entry(part, field, value):
    data = deepcopy(yaml.safe_load(SOURCE_LIST.read_text(encoding="utf-8")))
    data[part][0][field] = value
    with pytest.raises(AssertionError):
        validate_document(DOCUMENT.read_text(encoding="utf-8"), data)


@pytest.mark.parametrize("old,new", [
    ("[P1]", "[P99]"),
    ("Rows sourced to pages read", "Rows missing"),
    ("*(unverified, PDF data sheet)*", "verified"),
    ("**Differentiated offering plausible**", "**Different verdict**"),
    ("section 13 |", "section 12 |"),
])
def test_validator_rejects_broken_markdown(old, new):
    text = DOCUMENT.read_text(encoding="utf-8")
    assert old in text
    data = yaml.safe_load(SOURCE_LIST.read_text(encoding="utf-8"))
    with pytest.raises(AssertionError):
        validate_document(text.replace(old, new, 1), data)
