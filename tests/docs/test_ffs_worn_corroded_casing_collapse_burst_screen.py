"""Offline structural and verbatim checks for the issue research transcription."""

import copy
import hashlib
import re
from pathlib import Path
from urllib.parse import urlsplit

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
SLUG = "ffs-worn-corroded-casing-collapse-burst-screen"
MARK = re.compile(r"\*\*(?:unverified|not opened|not verified)\*\*", re.I)
# Note inventory: 3 sources, two tables with 4 data rows each.
SOURCE_IDS = ["S1", "S2", "S3"]
TABLE_ROWS = [4, 4]
# The note cites all three sources outside its Sources list; no known anomalies.
KNOWN_UNCITED_NOTE_ANOMALIES = []
HEADINGS = ['1. Bottom line', '2. Damage mechanisms', '3. Governing standards and technical reports (designations only)', '4. Assessment methods and levels in use', '5. Input data', '6. Acceptance criteria in outline', '7. Published validation and benchmark material', '8. Who buys the work, and when', '9. Fit assessment and next reading', 'Sources', 'Could not verify']
BODY_SHA256 = '24babdeb783d468fe14de38620b6dfda10fa7391b0627383cd04e3ffdab77512'


def table_rows(text):
    """Count data rows for each contiguous Markdown table."""
    tables = re.findall(r"(?:^\|.*\|\n?)+", text, re.M)
    return [len(table.splitlines()) - 2 for table in tables]


def section(text, heading):
    """Read a level-two section without changing its contents."""
    return text.split("## " + heading + "\n", 1)[1].split("\n## ", 1)[0]


def validate_entries(data):
    """Reject incomplete sources, altered ordering and malformed URLs."""
    required = {"id", "citation", "urls", "access_note"}
    entries = data["sources"]
    assert len(entries) == 3  # Three entries in the issue note, not inferred.
    assert [entry["id"] for entry in entries] == SOURCE_IDS
    for entry in entries:
        assert required <= entry.keys()
        assert isinstance(entry["citation"], str) and entry["citation"]
        assert isinstance(entry["urls"], list)
        assert entry["access_note"] is None or isinstance(entry["access_note"], str)
        for url in entry["urls"]:
            parsed = urlsplit(url)
            assert parsed.scheme in {"http", "https"} and parsed.netloc
            assert parsed.hostname and not parsed.username and not parsed.password
            assert not re.search(r"\s", url)
            assert parsed.port is None or 0 < parsed.port <= 65535


@pytest.fixture
def research():
    text = (ROOT / "docs/research" / (SLUG + ".md")).read_text()
    data = yaml.safe_load((ROOT / "docs/research" / (SLUG + ".sources.yaml")).read_text())
    return text, data


def test_schema(research):
    _, data = research
    assert {"schema_version", "document", "issue", "source_of_entries", "checked",
            "sources", "could_not_verify", "bottom_line", "unverified_mark_count"} <= data.keys()
    assert data["schema_version"] == 1
    assert data["document"] == SLUG + ".md"
    assert data["issue"] == 2201
    assert data["checked"] == "2026-10-11"
    assert data["source_of_entries"] == "research note on the issue"
    validate_entries(data)


def test_sources(research):
    text, data = research
    source_list = section(text, "Sources")
    outside = text.split("## Sources\n")[0] + section(text, "Could not verify")
    # Covers qualified citations such as [S2, section 2, opened].
    used = re.findall(r"\[(S\d+)(?:\]|,)", text)
    assert set(used) <= set(SOURCE_IDS)
    lines = re.findall(r"^- \[S\d+\] .+$", source_list, re.M)
    assert len(lines) == len(data["sources"])
    for entry, line in zip(data["sources"], lines):
        expected = "- [" + entry["id"] + "] " + entry["citation"]
        expected += "".join(" " + url for url in entry["urls"])
        if entry["access_note"] is not None:
            expected += " (" + entry["access_note"] + ")"
        assert line == expected
        if entry["id"] not in KNOWN_UNCITED_NOTE_ANOMALIES:
            assert re.search(r"\[" + entry["id"] + r"(?:\]|,)", outside)


def test_lists_marks_tables_and_headings(research):
    text, data = research
    body = text.split("## 1. Bottom line\n", 1)[1]
    assert re.findall(r"^## (.+)$", text, re.M) == HEADINGS
    assert table_rows(text) == TABLE_ROWS
    missing = re.findall(r"^- (.+)$", section(text, "Could not verify"), re.M)
    assert missing == data["could_not_verify"]
    assert len(missing) == 3  # Three bullets in the note's final section.
    bottom = re.findall(r"^- (.+)$", section(text, "1. Bottom line"), re.M)
    assert bottom == data["bottom_line"] and len(bottom) == 3
    for item in data["bottom_line"]:
        assert item in text
    assert len(MARK.findall(body)) == data["unverified_mark_count"] == 6
    assert len(MARK.findall(text.split("## 1. Bottom line")[0])) == 1
    # The entire section body is pinned to the note, including short quotations.
    complete_body = "## 1. Bottom line\n" + body
    assert hashlib.sha256(complete_body.encode()).hexdigest() == BODY_SHA256


def test_validator_rejects_broken_entry(research):
    _, data = research
    broken = copy.deepcopy(data)
    broken["sources"][0]["urls"] = ["file:///invalid"]
    with pytest.raises(AssertionError):
        validate_entries(broken)
