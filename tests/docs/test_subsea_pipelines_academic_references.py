"""Offline structural validation of the issue-note transcription."""

from collections import Counter
from copy import deepcopy
from pathlib import Path
import re
from urllib.parse import urlsplit

import pytest
import yaml

DOCUMENT = Path(__file__).parents[2] / "docs/research/subsea-pipelines-academic-references.md"
FIELDS = ("id", "key", "citation", "identifier", "identifier_status", "access", "pointers", "use")
STATUSES = {
    "[CR]", "[OL]", "[OL, LCCN 2007050317]", "[OL, LCCN 80029039]",
    "[OL, LCCN 2010016934]", "no identifier",
}
# No note identifiers fail the format/check-digit checks. Exceptions, if needed,
# must name the entry and exact identifier; transcription must never repair them.
KNOWN_NOTE_ANOMALIES = set()
DOI = re.compile(r"10\.\d{4,9}/[^\s]+")
ISBN = re.compile(r"[\d-]+[\dXx]")
URL = re.compile(r"https?://[^\s]+")


def parse_document(text):
    """Read the fixed YAML records, rejecting duplicate fields and stray records."""
    blocks = re.findall(r"^### (P\d+) `([a-z0-9]+)`\n\n```yaml\n(.*?)\n```", text, re.M | re.S)
    assert len(blocks) == text.count("```yaml"), "unrecognised record layout"
    entries = []
    for entry_id, key, body in blocks:
        names = re.findall(r"^([a-z_]+):", body, re.M)
        assert tuple(names) == FIELDS, "missing, duplicate or unordered fields"
        entry = yaml.safe_load(body)
        assert entry["id"] == entry_id and entry["key"] == key
        entries.append(entry)
    return entries


def valid_isbn(value):
    if not ISBN.fullmatch(value):
        return False
    digits = value.replace("-", "").upper()
    if len(digits) == 13 and digits.isdigit():
        return sum(int(c) * (1 if i % 2 == 0 else 3)
                   for i, c in enumerate(digits)) % 10 == 0
    if len(digits) == 10 and digits[:9].isdigit() and digits[-1] in "0123456789X":
        values = [int(c) for c in digits[:9]] + [10 if digits[-1] == "X" else int(digits[-1])]
        return sum(v * (10 - i) for i, v in enumerate(values)) % 11 == 0
    return False


def validate_entry(entry):
    assert tuple(entry) == FIELDS
    for field in ("id", "key", "citation", "access", "use"):
        assert isinstance(entry[field], str) and entry[field]
    assert re.fullmatch(r"P[1-9]\d*", entry["id"])
    assert re.fullmatch(r"[a-z][a-z0-9]*", entry["key"])
    assert entry["pointers"] is None or isinstance(entry["pointers"], str)
    identifiers, statuses = entry["identifier"], entry["identifier_status"]
    assert isinstance(identifiers, list) and identifiers
    assert isinstance(statuses, list) and statuses and set(statuses) <= STATUSES
    absent = identifiers == ["no identifier"]
    assert absent == (statuses == ["no identifier"])
    assert absent == ("no identifier" in identifiers)
    for value in identifiers:
        assert isinstance(value, str)
        if value == "no identifier":
            continue
        if (entry["id"], value) in KNOWN_NOTE_ANOMALIES:
            continue
        assert DOI.fullmatch(value) if value.startswith("10.") else valid_isbn(value)
    assert entry["access"].startswith(("free ", "no free access"))
    for value in URL.findall(entry["access"]):
        address = urlsplit(value)
        assert address.scheme in {"http", "https"} and address.hostname
        assert not address.username and not address.password
        assert address.port is None or 0 < address.port < 65536


def section(text, heading):
    return text.split("## " + heading + "\n", 1)[1].split("\n## ", 1)[0]


def validate_document(text):
    entries = parse_document(text)
    assert [e["id"] for e in entries] == [f"P{i}" for i in range(1, 41)]
    assert len({e["key"] for e in entries}) == 40
    for entry in entries:
        validate_entry(entry)
    counts = Counter(e["identifier_status"][0].split(",")[0].rstrip("]")
                     for e in entries)
    assert counts == {"[CR": 35, "[OL": 4, "no identifier": 1}
    summary = section(text, "Counts")
    match = re.search(r"(\d+) entries: (\d+) with identifiers confirmed .*?\[CR\]` (\d+), `\[OL\]` (\d+).*?; (\d+) identifiers carried unverified; (\d+) with no identifier \((P\d+)\)", summary)
    assert match and match.groups() == ("40", "39", "35", "4", "0", "1", "P17")
    free_ids = [e["id"] for e in entries if e["access"].startswith("free ")]
    assert free_ids == ["P17", "P27", "P38"]
    assert all("(200" in e["access"] for e in entries if e["id"] in free_ids)
    assert "P17, P27, P38 (3)" in summary
    table = section(text, "Topic-to-source cross-reference").split("\nSee also", 1)[0]
    rows = [line for line in table.splitlines() if line.startswith("|")]
    assert len(rows) == 14
    references = re.findall(r"\bP\d+\b", table)
    assert references and set(references) <= {e["id"] for e in entries}
    corrections = section(text, "Corrections to the issue's target list")
    assert len(re.findall(r"^- ", corrections, re.M)) == 6
    for value in URL.findall(text):
        address = urlsplit(value)
        assert address.scheme in {"http", "https"} and address.hostname
    return entries


def test_reference_document():
    validate_document(DOCUMENT.read_text(encoding="utf-8"))


@pytest.mark.parametrize("field,value", [
    ("identifier", ["10.invalid/broken"]),
    ("identifier", ["978-0-08-044566-4"]),
    ("identifier", ["no identifier"]),
    ("identifier_status", ["unverified"]),
    ("key", "UpperCase"),
    ("access", "free — https:///missing-host"),
])
def test_validator_rejects_broken_entry(field, value):
    entry = deepcopy(parse_document(DOCUMENT.read_text(encoding="utf-8"))[0])
    entry[field] = value
    with pytest.raises(AssertionError):
        validate_entry(entry)


@pytest.mark.parametrize("old,new", [
    ("### P40", "### P41"),
    ("P18, P1, P2", "P99, P1, P2"),
    ("40 entries: 39", "40 entries: 38"),
    ("id: P1\n", "id: P1\nid: P1\n"),
])
def test_validator_rejects_broken_document(old, new):
    text = DOCUMENT.read_text(encoding="utf-8")
    assert old in text
    with pytest.raises(AssertionError):
        validate_document(text.replace(old, new, 1))


@pytest.mark.parametrize("value", ["978-0-08-044566-3", "0000000000"])
def test_isbn_check_digit_forms(value):
    # Arithmetic fixtures only, not bibliographic claims or additional sources.
    assert valid_isbn(value)


@pytest.mark.parametrize("value", ["978-0-08-044566-4", "0000000001", "123", "978000000000X"])
def test_isbn_rejects_invalid_forms(value):
    assert not valid_isbn(value)
