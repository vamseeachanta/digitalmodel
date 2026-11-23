"""Offline checks for the issue research-note transcription."""

from copy import deepcopy
from pathlib import Path
import re
from urllib.parse import urlsplit

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
DOCUMENT = "docs/research/ffs-mooring-chain-degradation-screen.md"
SOURCE_LIST = ROOT / DOCUMENT.replace(".md", ".sources.yaml")
# The note has nine sources, in this order; each is cited outside the list.
SOURCE_IDS = [f"S{i}" for i in range(1, 10)]
KNOWN_UNCITED_NOTE_ANOMALIES = []
# The note has three tables with 6, 8 and 6 data rows, respectively.
TABLE_ROWS = [6, 8, 6]
HEADINGS = [
    "1. Bottom line",
    "2. Damage mechanisms that drive the work",
    "3. Governing standards and recommended practices (designations only)",
    "4. Assessment methods and levels in use",
    "5. Input data and where it comes from",
    "6. Acceptance criteria in outline",
    "7. Published validation and benchmark material",
    "8. Who buys the work, and when",
    "9. Fit assessment for a small analysis-led consultancy",
    "10. Consequences for this issue's acceptance criteria",
    "Sources",
    "Could not verify",
]
MARKS = re.compile(r"\*\*(?:unverified|not opened|not verified)\*\*", re.I)
URLS = re.compile(r"https?://[^\s]+")


def require(condition, message):
    if not condition:
        raise ValueError(message)


def validate_entry(entry):
    require(isinstance(entry, dict), "entry mapping")
    require(set(entry) == {"id", "citation", "urls", "access_note"}, "entry fields")
    require(isinstance(entry["id"], str), "source id type")
    require(re.fullmatch(r"S[1-9]\d*", entry["id"]), "source id")
    require(isinstance(entry["citation"], str) and entry["citation"], "citation")
    require(entry["access_note"] is None or isinstance(entry["access_note"], str),
            "access note")
    require(isinstance(entry["urls"], list), "URL list")
    for url in entry["urls"]:
        require(isinstance(url, str) and not re.search(r"\s", url), "URL string")
        parsed = urlsplit(url)
        require(parsed.scheme in {"http", "https"} and parsed.hostname, "URL scheme/host")
        require(parsed.username is None and parsed.password is None, "URL credentials")
        try:
            parsed.port
        except ValueError as error:
            raise ValueError("URL port") from error


def bullets(section):
    return re.findall(r"^- (.*)$", section, re.M)


def validate_catalogue(data, markdown):
    fields = {"schema_version", "document", "issue", "source_of_entries", "checked",
              "sources", "could_not_verify", "bottom_line", "unverified_mark_count"}
    require(set(data) == fields, "catalogue fields")
    require(data["schema_version"] == 1 and data["document"] == DOCUMENT, "schema/document")
    require(data["issue"] == 2199 and data["checked"] == "2026-10-11", "issue/date")
    require(data["source_of_entries"] == "research note on the issue", "single source")
    entries = data["sources"]
    require(len(entries) == 9, "note source count")
    for entry in entries:
        validate_entry(entry)
    ids = [entry["id"] for entry in entries]
    require(ids == SOURCE_IDS and len(set(ids)) == len(ids), "source order/uniqueness")
    require(set(re.findall(r"\[(S\d+)\]", markdown)) <= set(ids), "unknown citation")
    validate_body(data, markdown)


def validate_body(data, markdown):
    require(re.findall(r"^#{1,6} (.*)$", markdown, re.M)[1:] == HEADINGS, "note headings")
    body = markdown.split("## 1. Bottom line\n", 1)[1]
    content, sources = body.split("## Sources\n", 1)
    source_list, gaps = sources.split("## Could not verify\n", 1)
    lines = bullets(source_list)
    require(re.findall(r"^- \[(S\d+)\]", source_list, re.M) == SOURCE_IDS, "source list")
    for entry, line in zip(data["sources"], lines):
        require(URLS.findall(line) == entry["urls"], "source URLs")
        require(f"[{entry['id']}]" in content
                or entry["id"] in KNOWN_UNCITED_NOTE_ANOMALIES, "uncited source")
    require(bullets(gaps) == data["could_not_verify"], "gap bullets")
    require(len(data["could_not_verify"]) == 3, "note gap count")
    bottom = content.split("## 2. Damage mechanisms", 1)[0]
    require(bullets(bottom) == data["bottom_line"], "bottom-line bullets")
    require(len(data["bottom_line"]) == 3, "note bottom-line count")
    # Six body marks: **unverified** and **Unverified**. No bold Not opened
    # or not verified occurs in this note. The legend adds one **unverified**.
    require(type(data["unverified_mark_count"]) is int, "mark count type")
    require(len(MARKS.findall(body)) == data["unverified_mark_count"] == 6, "body marks")
    require(len(MARKS.findall(markdown.split("## 1.", 1)[0])) == 1, "legend mark")
    tables = re.findall(r"(?:^\|.*\n)+", body, re.M)
    require([len(table.splitlines()) - 2 for table in tables] == TABLE_ROWS, "table rows")


def test_research_transcription():
    data = yaml.safe_load(SOURCE_LIST.read_text(encoding="utf-8"))
    validate_catalogue(data, (ROOT / DOCUMENT).read_text(encoding="utf-8"))


def test_validator_rejects_broken_entry():
    broken = deepcopy({"id": "S1", "citation": "citation", "urls": ["relative/path"],
                       "access_note": None})
    with pytest.raises(ValueError, match="URL scheme/host"):
        validate_entry(broken)
