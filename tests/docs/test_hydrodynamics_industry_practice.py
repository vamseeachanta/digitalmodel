"""Offline validation of the issue research note's survey transcription."""

import copy
import re
from pathlib import Path
from urllib.parse import urlsplit

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
DOCUMENT = ROOT / "docs/research/hydrodynamics-industry-practice.md"
SOURCES = DOCUMENT.with_suffix(".sources.yaml")
# The note has three literal tags, including one in Located, not read.
NOTE_UNVERIFIED_COUNT = 3
# Known note anomalies: no uncited read sources were found in the note.
KNOWN_UNCITED_NOTE_SOURCES = []


def section(text, heading):
    match = re.search(rf"^## {re.escape(heading)}\n(.*?)(?=^## |\Z)",
                      text, re.M | re.S)
    if match is None:
        raise ValueError(f"missing heading: {heading}")
    return match.group(1)


def table_rows(text):
    lines = [line for line in text.splitlines() if line.startswith("|")]
    return [[cell.strip() for cell in line.strip("|").split("|")]
            for line in lines[2:]]


def require_fields(entry, fields):
    if not isinstance(entry, dict) or any(field not in entry for field in fields):
        raise ValueError("missing required field")
    if any(entry[field] in (None, "") for field in fields):
        raise ValueError("empty required field")


def validate_url(url):
    parsed = urlsplit(url)
    if (parsed.scheme not in {"http", "https"} or not parsed.hostname
            or parsed.username or parsed.password or re.search(r"[\s<>]", url)
            or not re.fullmatch(r"[A-Za-z0-9.-]+", parsed.hostname)
            or re.search(r"%(?![0-9a-fA-F]{2})", url)):
        raise ValueError("invalid HTTP(S) URL")
    _ = parsed.port


def validate_sources(data, text):
    require_fields(data, ("schema_version", "document", "issue", "source_of_entries",
                          "checked", "sources", "located_not_read", "gap_analysis", "counts"))
    if (data["schema_version"] != 1 or data["issue"] != 1800
            or data["document"] != DOCUMENT.relative_to(ROOT).as_posix()
            or data["source_of_entries"] != "research note on the issue"
            or data["checked"] != "2026-10-11"):
        raise ValueError("invalid document provenance")
    expected = [f"H{i}" for i in range(1, 38)]
    if [entry.get("id") for entry in data["sources"]] != expected:
        raise ValueError("source sequence differs from note H1 to H37")
    if data["counts"] != {"sources_read": 37, "located_not_read_documents":
                           "about 20 documents", "located_not_read_groups": 12}:
        raise ValueError("counts differ from note")
    read_list = section(text, "Sources read")
    outside = text.replace(read_list, "")
    read_urls = set()
    for entry in data["sources"]:
        require_fields(entry, ("id", "citation", "url", "status"))
        if entry["status"] != "read":
            raise ValueError("read status changed")
        validate_url(entry["url"])
        read_urls.add(entry["url"])
        line = f"- [{entry['id']}] {entry['citation']} {entry['url']}"
        if line not in read_list.splitlines():
            raise ValueError("source list entry differs")
        uncited = f"[{entry['id']}]" not in outside
        if uncited != (entry["id"] in KNOWN_UNCITED_NOTE_SOURCES):
            raise ValueError("uncited source or stale anomaly")
    if len(re.findall(r"^- \[H\d+\]", read_list, re.M)) != 37:
        raise ValueError("source list count differs")
    if not set(re.findall(r"\[(H\d+)\]", text)) <= set(expected):
        raise ValueError("unresolved source reference")
    validate_located(data["located_not_read"], text, read_urls)
    validate_gaps(data["gap_analysis"], text, set(expected))
    if text.count("*(unverified)*") != NOTE_UNVERIFIED_COUNT:
        raise ValueError("unverified tag count differs from note")


def validate_located(entries, text, read_urls):
    block = section(text, "Located, not read (kept out of conclusions)")
    bullets = re.findall(r"^- (.+)$", block, re.M)
    if len(bullets) != len(entries) or len(entries) != 12:
        raise ValueError("located count differs")
    for number, (entry, bullet) in enumerate(zip(entries, bullets), 1):
        require_fields(entry, ("id", "text", "urls", "status"))
        if (entry["id"] != f"L{number}" or entry["text"] != bullet
                or entry["status"] != "located_not_read"
                or entry["urls"] != re.findall(r"https?://\S+", bullet)):
            raise ValueError("located entry differs")
        for url in entry["urls"]:
            validate_url(url)
            if url in read_urls:
                raise ValueError("located URL promoted to read")


def validate_gaps(entries, text, source_ids):
    rows = table_rows(section(
        text, "10. Gap analysis for a small consultancy with strong open analysis capability"))
    if len(rows) != len(entries) or len(entries) != 8:
        raise ValueError("gap row count differs")
    for entry, row in zip(entries, rows):
        require_fields(entry, ("area", "verdict", "basis_source_ids"))
        if (len(row) != 3 or row[0] != entry["area"]
                or row[1] != f"**{entry['verdict']}**"
                or re.findall(r"\[(H\d+)\]", row[2]) != entry["basis_source_ids"]
                or not set(entry["basis_source_ids"]) <= source_ids):
            raise ValueError("gap analysis differs")


def load_artifacts():
    return yaml.safe_load(SOURCES.read_text(encoding="utf-8")), DOCUMENT.read_text(
        encoding="utf-8")


def test_survey_transcription():
    validate_sources(*load_artifacts())


@pytest.mark.parametrize("defect", ["missing", "order", "url", "status", "gap",
                                    "reference", "tag", "located"])
def test_validator_rejects_broken_entry(defect):
    data, text = load_artifacts()
    data = copy.deepcopy(data)
    if defect == "missing":
        del data["sources"][0]["citation"]
    elif defect == "order":
        data["sources"][1]["id"] = "H1"
    elif defect == "url":
        data["sources"][0]["url"] = "https:///missing-host"
    elif defect == "status":
        data["located_not_read"][0]["status"] = "read"
    elif defect == "gap":
        data["gap_analysis"][0]["verdict"] = ""
    elif defect == "reference":
        text += "\n[H38]\n"
    elif defect == "tag":
        text += "\n*(unverified)*\n"
    else:
        data["located_not_read"][0]["urls"] = [data["sources"][0]["url"]]
    with pytest.raises(ValueError):
        validate_sources(data, text)
