"""Offline structure checks pinned to the issue research note checked 2026-10-11."""

import copy
import re
from pathlib import Path
from urllib.parse import urlsplit

import pytest
import yaml

SLUG = "ffs-unbonded-flexible-pipe-integrity-screen"
ROOT = Path(__file__).resolve().parents[2] / "docs/research"
HEADINGS = ['1. Bottom line', '2. Damage mechanisms and their published frequency', '3. Governing standards and recommended practices (designations only)', '4. Assessment methods and levels in use', '5. Input data and where it comes from', '6. Acceptance criteria in outline', '7. Published validation and case-history material', '8. Who buys the work, and when', '9. Fit assessment for a small analysis-led consultancy', "10. Consequences for this issue's objective", 'Sources', 'Could not verify']
# Five entries, in the note's Sources section order.
SOURCE_IDS = ["S1", "S2", "S3", "S4", "S5"]
# The note lists S5 but never cites it outside Sources.
KNOWN_NOTE_ANOMALIES = ["S5"]
# Three note tables: frequency, designations, input evidence.
TABLE_ROWS = [16, 10, 7]
MARKS = re.compile(r"\*\*(?:unverified|not opened|not verified)\*\*", re.I)


def bullets(text):
    return re.findall(r"^- (.+)$", text, re.M)


def table_rows(text):
    tables = re.findall(r"(?:^\|.*\|\n)+", text, re.M)
    return [len(table.splitlines()) - 2 for table in tables]


def validate_entries(data):
    required = {"schema_version", "document", "issue", "source_of_entries",
                "checked", "sources", "could_not_verify", "bottom_line",
                "unverified_mark_count"}
    if not required <= data.keys():
        raise ValueError("missing document field")
    ids = [entry.get("id") for entry in data["sources"]]
    if ids != SOURCE_IDS or len(set(ids)) != len(ids):
        raise ValueError("source sequence differs from note")
    for entry in data["sources"]:
        if not {"id", "citation", "urls", "access_note"} <= entry.keys():
            raise ValueError("missing source field")
        if not isinstance(entry["citation"], str) or not entry["citation"]:
            raise ValueError("missing citation")
        if not isinstance(entry["urls"], list):
            raise ValueError("URLs must be a list")
        for url in entry["urls"]:
            parsed = urlsplit(url)
            if (parsed.scheme not in {"http", "https"} or not parsed.hostname
                    or parsed.username or parsed.password
                    or re.search(r"[\s<>]", url)):
                raise ValueError("invalid URL")
        if entry["access_note"] is not None and not isinstance(entry["access_note"], str):
            raise ValueError("invalid access note")


def load_data():
    return yaml.safe_load((ROOT / (SLUG + ".sources.yaml")).read_text(encoding="utf-8"))


def test_research_note_structure_and_content():
    data = load_data()
    validate_entries(data)
    text = (ROOT / (SLUG + ".md")).read_text(encoding="utf-8")
    body = text[text.index("## 1. Bottom line"):]
    before_sources, tail = body.split("## Sources\n", 1)
    sources_text, gaps = tail.split("## Could not verify\n", 1)
    assert data["schema_version"] == 1
    assert data["document"] == SLUG + ".md"
    assert data["issue"] == 2200
    assert data["source_of_entries"] == "research note on the issue"
    assert data["checked"] == "2026-10-11"
    assert re.findall(r"^#{2,6} (.+)$", body, re.M) == HEADINGS
    used_ids = re.findall(r"\[(S\d+)(?=[\], ])", text)
    assert set(used_ids) <= set(SOURCE_IDS)
    entries = bullets(sources_text)
    assert len(entries) == 5  # Five source bullets in the note.
    for entry, line in zip(data["sources"], entries):
        expected = f"[{entry['id']}] {entry['citation']}"
        if entry["urls"]:
            expected += " " + " ".join(entry["urls"])
        if entry["access_note"] is not None:
            expected += " (" + entry["access_note"] + ")"
        assert line == expected
        cited = entry["id"] in re.findall(r"\[(S\d+)(?=[\], ])", before_sources + gaps)
        assert cited == (entry["id"] not in KNOWN_NOTE_ANOMALIES)
    assert bullets(gaps) == data["could_not_verify"]
    assert len(data["could_not_verify"]) == 4  # Four note gap bullets.
    bottom = before_sources.split("## 2.", 1)[0]
    assert bullets(bottom) == data["bottom_line"]
    assert len(data["bottom_line"]) == 3  # Three note bottom-line bullets.
    assert data["unverified_mark_count"] == 2  # One body mark and one separately pinned legend mark.
    assert len(MARKS.findall(body)) == data["unverified_mark_count"] - 1
    assert len(MARKS.findall(text[:text.index("## 1. Bottom line")])) == 1
    assert table_rows(body) == TABLE_ROWS


@pytest.mark.parametrize("defect", ["missing_field", "duplicate_id", "bad_url"])
def test_validator_rejects_broken_in_memory_entry(defect):
    data = copy.deepcopy(load_data())
    if defect == "missing_field":
        del data["sources"][0]["citation"]
    elif defect == "duplicate_id":
        data["sources"][1]["id"] = "S1"
    else:
        data["sources"][0]["urls"] = ["https:///"]
    with pytest.raises(ValueError):
        validate_entries(data)
