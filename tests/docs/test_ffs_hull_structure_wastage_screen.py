"""Offline structural checks pinned to the issue's research note."""

import copy
import re
from pathlib import Path
from urllib.parse import urlsplit

import pytest
import yaml


ROOT = Path(__file__).resolve().parents[2]
SLUG = "ffs-hull-structure-wastage-screen"
DOC = ROOT / "docs" / "research" / f"{SLUG}.md"
SOURCES = DOC.with_suffix(".sources.yaml")
MARKS = r"\*\*(?:unverified|not opened|not verified)\*\*"
# The note lists eight sources, all cited outside its Sources list.
SOURCE_IDS = [f"S{number}" for number in range(1, 9)]
KNOWN_NOTE_ANOMALIES = []  # No uncited source entries in this note.
HEADINGS = [
    "## 1. Bottom line",
    "## 2. Damage mechanisms",
    "## 3. What the governing texts actually say",
    "### 3.1 Common structural rules for bulk carriers and oil tankers, Part 1 Chapter 13 (class-society mirror; page footer reads 01 JUL 2025) [S1, opened]",
    "### 3.2 Ships not built to those rules",
    "### 3.3 Hull-girder limit for oil tankers in service",
    "### 3.4 Pitting, grooving, edge corrosion",
    "## 4. Assessment methods and levels in use",
    "## 5. Input data and where it comes from",
    "## 6. Acceptance criteria in outline",
    "## 7. Published validation and benchmark material",
    "## 8. Who buys the work, and when",
    "## 9. Fit assessment for a small analysis-led consultancy",
    "## 10. Consequences for this issue's objective",
    "## Sources (all checked 2026-10-11)",
    "## Could not verify",
]


def source_rows(markdown):
    section = markdown.split("## Sources (all checked 2026-10-11)\n\n")[1]
    section = section.split("\n\n## Could not verify")[0]
    rows = []
    for line in section.splitlines():
        match = re.fullmatch(r"- \[(S\d+)\] (.*)", line)
        assert match, "Malformed source entry"
        text = match[2]
        access = re.search(r" \(([^()]*)\)$", text)
        citation = text[:access.start()] if access else text
        urls = re.findall(r"https?://\S+", citation)
        for url in urls:
            citation = citation.replace(url, "")
        rows.append({"id": match[1], "citation": citation.strip(),
                     "urls": urls, "access_note": access[1] if access else None})
    return rows


def validate_sources(data):
    required = {"schema_version", "document", "issue", "source_of_entries",
                "checked", "sources", "could_not_verify", "bottom_line",
                "unverified_mark_count"}
    assert required <= data.keys()
    assert data["schema_version"] == 1
    assert data["document"] == DOC.name
    assert data["issue"] == 2202
    assert data["source_of_entries"] == "research note on the issue"
    assert data["checked"] == "2026-10-11"
    assert [entry["id"] for entry in data["sources"]] == SOURCE_IDS
    assert len(set(entry["id"] for entry in data["sources"])) == 8
    for entry in data["sources"]:
        assert {"id", "citation", "urls", "access_note"} == entry.keys()
        assert isinstance(entry["citation"], str) and entry["citation"]
        assert isinstance(entry["urls"], list)
        assert entry["access_note"] is None or isinstance(entry["access_note"], str)
        for url in entry["urls"]:
            parsed = urlsplit(url)
            assert parsed.scheme in {"http", "https"} and parsed.hostname
            assert not re.search(r"\s", url)
            assert parsed.port is None or 0 < parsed.port < 65536
    for key, count in (("bottom_line", 3), ("could_not_verify", 3)):
        assert len(data[key]) == count  # Three bullets each in the note.
        assert all(isinstance(item, str) and item for item in data[key])
    assert type(data["unverified_mark_count"]) is int
    assert data["unverified_mark_count"] == 5  # Four in body, one in legend.


def validate_markdown(markdown, data):
    body = markdown.split("## 1. Bottom line", 1)[1]
    outside_sources = body.split("## Sources (all checked 2026-10-11)")[0]
    outside_sources += body.split("## Could not verify")[1]
    ids = set(re.findall(r"\bS\d+\b", " ".join(re.findall(r"\[([^\]]+)\]", markdown))))
    assert ids <= set(SOURCE_IDS)
    for source_id in SOURCE_IDS:
        if source_id not in KNOWN_NOTE_ANOMALIES:
            assert re.search(rf"\[[^\]]*\b{source_id}\b[^\]]*\]", outside_sources)
    assert source_rows(markdown) == data["sources"]
    could = body.split("## Could not verify")[1]
    assert re.findall(r"^- (.+)$", could, re.M) == data["could_not_verify"]
    bottom = body.split("## 2. Damage mechanisms")[0]
    assert re.findall(r"^- (.+)$", bottom, re.M) == data["bottom_line"]
    assert all(item in markdown for item in data["bottom_line"])
    body_marks = len(re.findall(MARKS, body, re.I))
    legend_marks = len(re.findall(MARKS, markdown.split("## 1.")[0], re.I))
    assert body_marks == 4 and legend_marks == 1  # Pinned separately to note.
    assert body_marks + legend_marks == data["unverified_mark_count"]
    assert re.findall(r"^#{2,} .+$", markdown, re.M) == HEADINGS
    tables = re.findall(r"(?:^\|.*\|\n?)+", markdown, re.M)
    assert [len(table.strip().splitlines()) - 2 for table in tables] == [4]
    # The note contains one Markdown table, with four data rows.


def test_research_document_and_source_list():
    data = yaml.safe_load(SOURCES.read_text(encoding="utf-8"))
    validate_sources(data)
    validate_markdown(DOC.read_text(encoding="utf-8"), data)


def test_validator_rejects_broken_entry():
    data = copy.deepcopy(yaml.safe_load(SOURCES.read_text(encoding="utf-8")))
    data["sources"][0]["urls"] = ["not-a-url"]
    with pytest.raises(AssertionError):
        validate_sources(data)
