"""Offline parser and validation for the mooring research-note transcription."""

from collections import Counter
from copy import deepcopy
import json
from pathlib import Path
import re
from urllib.parse import urlsplit

import pytest

DOCUMENT = Path(__file__).resolve().parents[2] / "docs/research/mooring-academic-references.md"
FIELDS = {"id", "key", "citation", "identifier", "identifier_status", "access", "pointers", "use"}
COUNTS = {"[CR]": 30, "[OL]": 3, "unverified": 3, "no identifier": 4}
STATUSES = set(COUNTS) | {"[OL, LCCN 2010016934]"}
UNVERIFIED = {"M4", "M7", "M8"}
NO_IDENTIFIER = {"M9", "M23", "M34", "M36"}
# Known note anomalies are populated only for identifiers failing offline checks.
KNOWN_NOTE_ANOMALIES = set()


def require(condition, message):
    if not condition:
        raise ValueError(message)


def unique_object(pairs):
    result = {}
    for key, value in pairs:
        require(key not in result, f"duplicate field: {key}")
        result[key] = value
    return result


def parse_document(text):
    blocks = re.findall(r"^```json\n(.*?)\n```$", text, re.M | re.S)
    entries = [json.loads(block, object_pairs_hook=unique_object) for block in blocks]
    require("## Topic-to-source cross-reference\n" in text, "missing cross-reference")
    table = text.split("## Topic-to-source cross-reference\n", 1)[1].split("See also", 1)[0]
    rows = [line for line in table.splitlines() if line.startswith("|")]
    require(len(rows) == 16, "cross-reference row count")
    references = re.findall(r"\bM\d+\b", "\n".join(rows[2:]))
    return entries, references


def isbn_valid(value):
    digits = value.replace("-", "")
    if re.fullmatch(r"\d{13}", digits):
        return sum(int(digit) * (1 if i % 2 == 0 else 3)
                   for i, digit in enumerate(digits)) % 10 == 0
    if re.fullmatch(r"\d{9}[\dX]", digits):
        return sum((10 - i) * (10 if digit == "X" else int(digit))
                   for i, digit in enumerate(digits)) % 11 == 0
    return False


def identifiers(text):
    dois = re.findall(r"\bDOI\s+(\S+)", text)
    # Include multiple editions and ISBNs retained inside the full citation.
    isbns = re.findall(r"\bISBN\s+([0-9X-]+)", text)
    isbns += re.findall(r"\s/\s+([0-9X-]+)", text)
    return [doi.rstrip(";,") for doi in dois], isbns


def urls(text):
    return re.findall(r"https?://[^\s]+", text)


def validate_url(url):
    parts = urlsplit(url)
    require(parts.scheme in {"http", "https"} and bool(parts.hostname), "invalid URL")
    require(parts.username is None and parts.password is None, "URL credentials")
    require(not any(char.isspace() for char in url), "URL whitespace")
    try:
        parts.port
    except ValueError as error:
        raise ValueError("invalid URL port") from error


def validate_entry(entry):
    require(isinstance(entry, dict) and set(entry) == FIELDS, "entry fields")
    for field in FIELDS - {"pointers"}:
        require(isinstance(entry[field], str) and bool(entry[field]), f"invalid {field}")
    require(entry["pointers"] is None or isinstance(entry["pointers"], str), "pointers type")
    require(re.fullmatch(r"[a-z][a-z0-9]*", entry["key"]), "invalid key")
    require(entry["identifier_status"] in STATUSES, "unknown status")
    dois, isbns = identifiers(entry["citation"] + " " + entry["identifier"])
    absent = "no identifier" in entry["identifier"]
    require(absent == (not dois and not isbns), "identifier absence mismatch")
    require(absent == (entry["identifier_status"] == "no identifier"), "absence status")
    for doi in dois:
        require(re.fullmatch(r"10\.\d{4,9}/\S+", doi), "invalid DOI")
    for isbn in isbns:
        require(isbn_valid(isbn) or (entry["id"], isbn) in KNOWN_NOTE_ANOMALIES,
                f"invalid ISBN: {entry['id']} {isbn}")
    for url in urls(entry["access"]):
        validate_url(url)


def validate_document(entries, references):
    require(len(entries) == 40, "entry count")
    require([entry["id"] for entry in entries] == [f"M{i}" for i in range(1, 41)], "id order")
    require(len({entry["key"] for entry in entries}) == 40, "duplicate keys")
    for entry in entries:
        validate_entry(entry)
    # M5 carries its printed ISBN mark verbatim; its additional CR mark remains
    # in identifier. Count the entry as OL, matching the note's Counts section.
    statuses = ("[OL]" if e["identifier_status"].startswith("[OL")
                else e["identifier_status"] for e in entries)
    require(Counter(statuses) == COUNTS, "status counts")
    for status, expected in (("unverified", UNVERIFIED), ("no identifier", NO_IDENTIFIER)):
        require({e["id"] for e in entries if e["identifier_status"] == status} == expected,
                f"wrong {status} entries")
    require(set(references) <= {entry["id"] for entry in entries}, "unknown cross-reference")
    resolved = {e["id"] for e in entries if "(200)" in e["access"]}
    listed = {e["id"] for e in entries if "(listed)" in e["access"]}
    require(resolved == {"M13", "M36"}, "resolved access counts")
    require(listed == {"M8", "M9", "M17"}, "listed access counts")


def test_mooring_references():
    validate_document(*parse_document(DOCUMENT.read_text(encoding="utf-8")))


@pytest.mark.parametrize("field,value", [
    ("key", "Bad Key"), ("identifier_status", "verified"),
    ("identifier", "no identifier"), ("identifier", "DOI 9.1234/bad"),
    ("identifier", "ISBN 978-0-12-818551-4 [CR]"),
    ("access", "free — https:///missing-host"),
])
def test_validator_rejects_broken_entry(field, value):
    entries, _ = parse_document(DOCUMENT.read_text(encoding="utf-8"))
    broken = deepcopy(entries[0])
    broken[field] = value
    with pytest.raises(ValueError):
        validate_entry(broken)


def test_validator_rejects_unknown_reference():
    entries, references = parse_document(DOCUMENT.read_text(encoding="utf-8"))
    with pytest.raises(ValueError, match="unknown cross-reference"):
        validate_document(entries, references + ["M41"])
