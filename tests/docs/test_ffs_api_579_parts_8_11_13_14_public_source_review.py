"""Offline validation of the issue research note and its source sidecar."""

from copy import deepcopy
from pathlib import Path
import re
from urllib.parse import urlsplit

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
SLUG = "ffs-api-579-parts-8-11-13-14-public-source-review"
DOCUMENT = ROOT / "docs" / "research" / f"{SLUG}.md"
SIDECAR = DOCUMENT.with_suffix(".sources.yaml")
ENTRY_FIELDS = {"id", "citation", "urls", "access_note"}
REQUIRED_FIELDS = {
    "schema_version", "document", "issue", "source_of_entries", "checked",
    "sources", "could_not_verify", "bottom_line", "unverified_mark_count",
}
# The note lists nine sources, in this order, and cites each outside that list.
SOURCE_IDS = [f"S{number}" for number in range(1, 10)]
KNOWN_UNCITED_NOTE_SOURCES = []  # No uncited-source anomalies in this note.
HEADINGS = [
    "1. Bottom line", "2. Part titles as published", "3. Part by part",
    "4. What needs the licensed standard", "5. Suggested order, with reasons",
    "Sources", "Could not verify",
]
MARK = re.compile(r"\*\*(?:unverified|not opened|not verified)\*\*", re.I)
URL = re.compile(r"https?://\S+")


def section(text, heading):
    return text.split(f"## {heading}\n", 1)[1].split("\n## ", 1)[0]


def bullets(text):
    return re.findall(r"^- (.*)$", text, re.M)


def source_ids(text):
    brackets = re.findall(r"\[S\d+[^\]\n]*\]", text)
    return [value for bracket in brackets for value in re.findall(r"\bS\d+\b", bracket)]


def parse_source_line(line):
    match = re.fullmatch(r"\[(S\d+)\] (.+)", line)
    assert match, "unrecognised source entry"
    entry_id, content = match.groups()
    access = re.search(r"\s+\(([^()]*)\)$", content)
    access_note = access.group(1) if access else None
    citation_and_urls = content[:access.start()] if access else content
    urls = URL.findall(citation_and_urls)
    citation = URL.sub("", citation_and_urls).strip()
    return {"id": entry_id, "citation": citation, "urls": urls,
            "access_note": access_note}


def validate_entry(entry):
    assert isinstance(entry, dict) and set(entry) == ENTRY_FIELDS
    assert isinstance(entry["id"], str) and re.fullmatch(r"S[1-9]\d*", entry["id"])
    assert isinstance(entry["citation"], str) and entry["citation"]
    assert isinstance(entry["urls"], list)
    assert entry["access_note"] is None or (
        isinstance(entry["access_note"], str) and entry["access_note"]
    )
    for url in entry["urls"]:
        assert isinstance(url, str) and not re.search(r"\s", url)
        parsed = urlsplit(url)
        assert parsed.scheme in {"http", "https"} and parsed.hostname
        assert not parsed.username and not parsed.password
        assert parsed.port is None or 0 < parsed.port < 65536


def validate_sidecar(data):
    assert isinstance(data, dict) and set(data) == REQUIRED_FIELDS
    assert data["schema_version"] == 1
    assert data["document"] == DOCUMENT.relative_to(ROOT).as_posix()
    assert data["issue"] == 2203
    assert data["source_of_entries"] == "research note on the issue"
    assert data["checked"] == "2026-10-11"
    assert isinstance(data["sources"], list)
    for entry in data["sources"]:
        validate_entry(entry)
    ids = [entry["id"] for entry in data["sources"]]
    assert len(ids) == 9  # Nine numbered entries in the note's Sources section.
    assert len(set(ids)) == len(ids) and ids == SOURCE_IDS
    for field, count in (("could_not_verify", 4), ("bottom_line", 4)):
        assert isinstance(data[field], list) and len(data[field]) == count
        assert all(isinstance(value, str) and value for value in data[field])
    assert type(data["unverified_mark_count"]) is int
    assert data["unverified_mark_count"] == 3  # Two body marks and one legend mark.


def tables(text):
    result, current = [], []
    for line in text.splitlines() + [""]:
        if line.startswith("|"):
            current.append([cell.strip() for cell in line.strip("|").split("|")])
        elif current:
            assert len(current) >= 2
            assert all(re.fullmatch(r":?-+:?", cell) for cell in current[1])
            assert all(len(row) == len(current[0]) for row in current)
            result.append(current)
            current = []
    return result


def validate_document(text, data):
    validate_sidecar(data)
    assert re.findall(r"^## (.+)$", text, re.M) == HEADINGS
    # The note has no deeper headings; only the added document title is allowed.
    headings = re.findall(r"^(#+) (.+)$", text, re.M)
    assert headings[0][0] == "#" and len(headings) == len(HEADINGS) + 1
    body = text.split("## 1. Bottom line\n", 1)[1]
    legend = text.split("## 1. Bottom line\n", 1)[0]
    assert len(MARK.findall(legend)) == 1  # Note's opening legend: unverified.
    assert len(MARK.findall(body)) == 2  # One unverified and one not verified.
    assert len(MARK.findall(body)) + len(MARK.findall(legend)) == data["unverified_mark_count"]
    assert set(source_ids(text)) <= set(SOURCE_IDS)
    source_text = section(text, "Sources")
    entries = [parse_source_line(line) for line in bullets(source_text)]
    assert entries == data["sources"], "source text, URL or access-note mismatch"
    outside = text.split("## Sources\n", 1)[0] + section(text, "Could not verify")
    uncited = [value for value in SOURCE_IDS if value not in source_ids(outside)]
    assert uncited == KNOWN_UNCITED_NOTE_SOURCES
    assert bullets(section(text, "Could not verify")) == data["could_not_verify"]
    assert bullets(section(text, "1. Bottom line")) == data["bottom_line"]
    assert all(value in text for value in data["bottom_line"])
    # The note contains one table, six data rows, and five columns (row + parts).
    parsed_tables = tables(body)
    assert [len(table) - 2 for table in parsed_tables] == [6]
    assert len(parsed_tables[0][0]) == 5


def load_sidecar():
    return yaml.safe_load(SIDECAR.read_text(encoding="utf-8"))


def test_research_screen():
    validate_document(DOCUMENT.read_text(encoding="utf-8"), load_sidecar())


@pytest.mark.parametrize("field,value", [
    ("id", "S0"), ("citation", None), ("urls", ["https:///missing-host"]),
    ("urls", ["ftp://invalid"]), ("urls", "https://invalid"),
    ("access_note", 0),
])
def test_validator_rejects_broken_entry(field, value):
    entry = deepcopy(load_sidecar()["sources"][0])
    entry[field] = value
    with pytest.raises(AssertionError):
        validate_entry(entry)


def test_validator_rejects_missing_field():
    entry = deepcopy(load_sidecar()["sources"][0])
    del entry["urls"]
    with pytest.raises(AssertionError):
        validate_entry(entry)


@pytest.mark.parametrize("field", ["sources", "bottom_line", "could_not_verify"])
def test_validator_rejects_missing_list_item(field):
    data = deepcopy(load_sidecar())
    data[field].pop()
    with pytest.raises(AssertionError):
        validate_sidecar(data)


@pytest.mark.parametrize("old,new", [
    ("[S1]", "[S99]"),
    ("**not verified**", "not verified"),
    ("## Could not verify", "## Missing heading"),
    ("## Sources", "## Invented heading"),
    ("## 3. Part by part", "### 3. Part by part"),
])
def test_validator_rejects_broken_document(old, new):
    text = DOCUMENT.read_text(encoding="utf-8")
    assert old in text
    with pytest.raises(AssertionError):
        validate_document(text.replace(old, new, 1), load_sidecar())


def test_validator_rejects_duplicate_source_id():
    data = deepcopy(load_sidecar())
    data["sources"][1]["id"] = data["sources"][0]["id"]
    with pytest.raises(AssertionError):
        validate_sidecar(data)
