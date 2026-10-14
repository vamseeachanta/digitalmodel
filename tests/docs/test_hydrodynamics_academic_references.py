"""Offline structure checks for the issue research note's transcription."""

import copy
import json
import re
from collections import Counter
from pathlib import Path
from urllib.parse import urlsplit

import pytest

ROOT = Path(__file__).resolve().parents[2]
REFERENCE = ROOT / "docs/research/hydrodynamics-academic-references.md"
FIELDS = ("id", "key", "citation", "identifier", "identifier_status",
          "access", "pointers", "use")
STATUSES = {"[CR]", "[OL]", "[OL, LCCN 80020237]",
            "[OL, LCCN 87016845]", "no identifier"}
# The research note's own Counts section, checked 2026-10-11.
NOTE_COUNTS = {"[CR]": 26, "[OL]": 6, "no identifier": 8}
NO_IDENTIFIER_IDS = {"H1", "H17", "H18", "H19", "H21", "H22", "H27", "H28"}
RESOLVED_FREE_IDS = {"H1", "H17", "H18", "H19", "H20", "H22", "H28", "H33"}
LISTED_IDS = {"H3", "H27"}
# Known note anomalies: exact invalid identifier tokens only, never entire entries.
# Empty unless a format/check-digit failure is observed in the source note.
KNOWN_NOTE_ANOMALIES = {}


def unique_object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"duplicate field: {key}")
        result[key] = value
    return result


def parse_document(text):
    blocks = re.findall(r"^```json\n(.*?)\n```$", text, flags=re.M | re.S)
    entries = [json.loads(block, object_pairs_hook=unique_object) for block in blocks]
    table = text.split("## Topic-to-source cross-reference\n", 1)[1]
    table = table.split("\nSee also", 1)[0]
    rows = [line for line in table.splitlines() if line.startswith("|")]
    if rows[:2] != ["| Topic | Primary | Supporting |", "|---|---|---|"]:
        raise ValueError("invalid cross-reference header")
    references = []
    for row in rows[2:]:
        cells = [cell.strip() for cell in row.strip("|").split("|")]
        if len(cells) != 3 or not cells[0]:
            raise ValueError("invalid cross-reference row")
        for cell in cells[1:]:
            ids = cell.split(", ")
            if not all(re.fullmatch(r"H[1-9][0-9]*", item) for item in ids):
                raise ValueError("invalid cross-reference id")
            references.extend(ids)
    if not references:
        raise ValueError("empty cross-reference")
    return entries, references


def identifiers(entry):
    statement = entry["identifier"]
    dois = re.findall(r"(?<!no )\bDOI\s+([^\s,]+)", statement)
    isbns = re.findall(r"(?:\bISBN\s+| / )([0-9Xx-]+)", statement)
    return dois, isbns


def valid_isbn(token):
    digits = token.replace("-", "")
    if re.fullmatch(r"[0-9]{13}", digits):
        return sum(int(digit) * (1 if i % 2 == 0 else 3)
                   for i, digit in enumerate(digits)) % 10 == 0
    if re.fullmatch(r"[0-9]{9}[0-9X]", digits):
        values = [10 if digit == "X" else int(digit) for digit in digits]
        return sum((10 - i) * value for i, value in enumerate(values)) % 11 == 0
    return False


def entry_urls(entry):
    urls = re.findall(r"\b[A-Za-z][A-Za-z0-9+.-]*://[^\s]+", entry["access"])
    result = []
    for url in urls:
        # Prose may close a parenthesis around a URL; DOI parentheses are retained.
        while url.endswith(")") and url.count(")") > url.count("("):
            url = url[:-1]
        result.append(url)
    return result


def validate_entry(entry):
    if tuple(entry) != FIELDS:
        raise ValueError("missing, extra or reordered fields")
    for field in FIELDS:
        value = entry[field]
        if field == "pointers" and value is None:
            continue
        if not isinstance(value, str) or not value.strip():
            raise ValueError(f"empty or invalid {field}")
    if not re.fullmatch(r"H[1-9][0-9]*", entry["id"]):
        raise ValueError("invalid entry id")
    if not re.fullmatch(r"[a-z][a-z0-9_]*", entry["key"]):
        raise ValueError("invalid BibTeX key")
    if entry["identifier_status"] not in STATUSES:
        raise ValueError("unknown identifier status")
    validate_identifiers(entry)
    access = entry["access"]
    if not access.startswith(("free", "no free access", "repository record ")):
        raise ValueError("unknown access statement")
    for url in entry_urls(entry):
        parsed = urlsplit(url)
        if (parsed.scheme not in {"http", "https"} or not parsed.hostname
                or not re.fullmatch(r"[A-Za-z0-9.-]+", parsed.hostname)
                or parsed.username or parsed.password or parsed.port == 0
                or re.search(r"[\s<>]", url)
                or re.search(r"%(?![0-9a-fA-F]{2})", url)):
            raise ValueError("invalid URL")
    if entry["pointers"] is not None and not re.search(
            r"\bpp\. [0-9]+–[0-9]+", entry["pointers"]):
        raise ValueError("pointer has no page range")


def validate_identifiers(entry):
    dois, isbns = identifiers(entry)
    absent = entry["identifier"].startswith("no identifier")
    if absent != (not dois and not isbns):
        raise ValueError("no identifier contradicts DOI/ISBN")
    if absent != (entry["identifier_status"] == "no identifier"):
        raise ValueError("identifier status contradicts identifier")
    marks = re.findall(r"\[(?:CR|OL)[^\]]*\]", entry["identifier"])
    if not absent and (not marks or set(marks) != {entry["identifier_status"]}):
        raise ValueError("identifier marks contradict identifier status")
    anomalies = set(KNOWN_NOTE_ANOMALIES.get(entry["id"], ()))
    failed = {token for token in dois
              if not re.fullmatch(r"10\.[0-9]{4,9}/\S+", token)}
    failed.update(token for token in isbns if not valid_isbn(token))
    if failed != anomalies:
        raise ValueError(f"invalid identifiers or stale anomaly list: {failed}")


def validate_document(entries, references):
    if [entry.get("id") for entry in entries] != [f"H{i}" for i in range(1, 41)]:
        raise ValueError("expected exactly H1 to H40 in order")
    for entry in entries:
        validate_entry(entry)
    if len({entry["key"] for entry in entries}) != 40:
        raise ValueError("duplicate key")
    counts = Counter("[OL]" if entry["identifier_status"].startswith("[OL")
                     else entry["identifier_status"] for entry in entries)
    if counts != NOTE_COUNTS:
        raise ValueError("status counts differ from note")
    ids = {entry["id"] for entry in entries}
    if not set(references) <= ids:
        raise ValueError("unknown cross-reference id")
    if {entry["id"] for entry in entries
            if entry["identifier_status"] == "no identifier"} != NO_IDENTIFIER_IDS:
        raise ValueError("no-identifier ids differ from note")
    resolved = {entry["id"] for entry in entries
                if entry["access"].startswith("free") and "(200)" in entry["access"]}
    listed = {entry["id"] for entry in entries if "(listed" in entry["access"]}
    if resolved != RESOLVED_FREE_IDS or listed != LISTED_IDS:
        raise ValueError("access counts differ from note")


def test_reference_document():
    assert REFERENCE.is_file(), "reference transcription is absent"
    text = REFERENCE.read_text(encoding="utf-8")
    entries, references = parse_document(text)
    validate_document(entries, references)
    assert "40 entries: 32 with identifiers confirmed (`[CR]` 26, `[OL]` 6)" in text
    assert "0 identifiers carried unverified; 8 with no identifier" in text
    assert "H1, H17, H18, H19, H20, H22, H28, H33 (8)" in text
    assert "Listed but not machine-checked: H3, H27." in text
    assert re.findall(r"^## ([A-F])\.", text, flags=re.M) == list("ABCDEF")


@pytest.mark.parametrize("digits,valid", [
    ("978-0-521-37285-5", True),
    ("978-0-521-37285-4", False),
    ("978-0-521-37285", False),
])
def test_isbn_check_digits(digits, valid):
    assert valid_isbn(digits) is valid


@pytest.mark.parametrize("field,value", [
    ("key", "Bad-Key"), ("identifier_status", "unverified"),
    ("identifier", "no identifier DOI 10.1017/9781009198059"),
    ("identifier", "DOI broken [CR]"),
    ("identifier", "ISBN 978-0-521-37285-4 [CR]"),
    ("access", "free — https:///missing-host (200)"),
    ("access", "free — https://bad)host/ (200)"),
    ("access", "free — ftp://example.org/ (200)"),
    ("pointers", "no page range"), ("citation", ""),
])
def test_validator_rejects_broken_entry(field, value):
    entry = dict(zip(FIELDS, ("H1", "example2001", "Example citation",
                             "DOI 10.1017/9781009198059 [CR]", "[CR]",
                             "no free access", None, "Example use")))
    entry[field] = value
    with pytest.raises(ValueError):
        validate_entry(entry)


@pytest.mark.parametrize("defect", ["duplicate_key", "order", "unknown_reference"])
def test_validator_rejects_broken_document(defect):
    entries, references = parse_document(REFERENCE.read_text(encoding="utf-8"))
    entries = copy.deepcopy(entries)
    if defect == "duplicate_key":
        entries[1]["key"] = entries[0]["key"]
    elif defect == "order":
        entries[0], entries[1] = entries[1], entries[0]
    else:
        references.append("H41")
    with pytest.raises(ValueError):
        validate_document(entries, references)
