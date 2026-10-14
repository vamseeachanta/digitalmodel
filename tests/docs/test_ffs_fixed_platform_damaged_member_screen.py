"""Offline structural validation of the issue's single research note."""

import copy
import re
from pathlib import Path
from urllib.parse import urlsplit

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
SLUG = "ffs-fixed-platform-damaged-member-screen"
DOCUMENT = f"docs/research/{SLUG}.md"
CATALOGUE = ROOT / f"docs/research/{SLUG}.sources.yaml"
# The note has nine sources, in this order, and no uncited-source anomalies.
SOURCE_IDS = [f"S{number}" for number in range(1, 10)]
KNOWN_UNCITED_NOTE_ANOMALIES = []
# The note's two tables have three standards rows and six benchmark rows.
TABLE_ROWS = [3, 6]
# The opening legend has one bold unverified mark; the body has five.
LEGEND_MARKS = 1
BODY_MARKS = 5
MARK = re.compile(r"\*\*(?:unverified|not opened|not verified)\*\*", re.I)
HEADINGS = [
    "1. Bottom line",
    "2. Damage mechanisms",
    "3. Governing standards and recommended practices (designations only)",
    "4. Assessment methods and levels in use",
    "5. Input data",
    "6. Published validation and benchmark material",
    "7. Who buys the work, and when",
    "8. Fit assessment for a small analysis-led consultancy",
    "9. What a reviewer should open next",
    "Sources",
    "Could not verify",
]


def load_artifacts():
    return (
        yaml.safe_load(CATALOGUE.read_text(encoding="utf-8")),
        (ROOT / DOCUMENT).read_text(encoding="utf-8"),
    )


def section(markdown, heading):
    pattern = rf"^## {re.escape(heading)}\n(.*?)(?=^## |\Z)"
    match = re.search(pattern, markdown, re.M | re.S)
    if match is None:
        raise ValueError(f"missing heading: {heading}")
    return match.group(1)


def bullets(text):
    return re.findall(r"^- (.*)$", text, re.M)


def table_row_counts(markdown):
    tables = re.findall(r"(?:^\|.*\|\n)+", markdown, re.M)
    counts = []
    for table in tables:
        rows = table.splitlines()
        if len(rows) < 2 or not re.fullmatch(r"[| :\-]+", rows[1]):
            raise ValueError("malformed table")
        width = len(rows[0].split("|"))
        if any(len(row.split("|")) != width for row in rows):
            raise ValueError("inconsistent table width")
        counts.append(len(rows) - 2)
    return counts


def require(condition, message):
    if not condition:
        raise ValueError(message)


def validate_url(url):
    require(isinstance(url, str), "URL must be a string")
    parsed = urlsplit(url)
    require(
        parsed.scheme in {"http", "https"}
        and parsed.hostname
        and not parsed.username
        and not parsed.password
        and not re.search(r"[\s<>]", url),
        "invalid URL",
    )
    try:
        parsed.port
    except ValueError as error:
        raise ValueError("invalid URL port") from error


def validate_entries(data):
    required = {"id", "citation", "urls", "access_note"}
    require(isinstance(data["sources"], list), "sources must be a list")
    ids = []
    for entry in data["sources"]:
        require(isinstance(entry, dict) and required <= entry.keys(), "missing source field")
        require(isinstance(entry["citation"], str) and entry["citation"], "empty citation")
        require(isinstance(entry["urls"], list), "URLs must be a list")
        require(entry["access_note"] is None or isinstance(entry["access_note"], str),
                "invalid access note")
        ids.append(entry["id"])
        for url in entry["urls"]:
            validate_url(url)
    require(ids == SOURCE_IDS and len(set(ids)) == 9, "source sequence mismatch")
    # S4 prints no URL or closing access parenthesis; its status stays in its citation.
    require(data["sources"][3]["urls"] == [], "S4 has no printed URL")
    require(data["sources"][3]["access_note"] is None, "S4 has no closing access note")
    require(data["sources"][3]["citation"].endswith(": not opened."), "S4 status lost")


def validate_source_list(data, markdown):
    entries = bullets(section(markdown, "Sources"))
    expected = []
    for source in data["sources"]:
        text = f"[{source['id']}] {source['citation']}"
        if source["urls"]:
            text += " " + " ".join(source["urls"])
        if source["access_note"] is not None:
            text += f" ({source['access_note']})"
        expected.append(text)
    require(entries == expected, "source list differs from YAML")
    refs = set()
    for bracket in re.findall(r"\[([^]\n]+)\]", markdown):
        refs.update(re.findall(r"\bS\d+\b", bracket))
    require(refs <= set(SOURCE_IDS), "unresolved source citation")
    outside = markdown.split("## Sources\n", 1)[0]
    cited = set()
    for bracket in re.findall(r"\[([^]\n]+)\]", outside):
        cited.update(re.findall(r"\bS\d+\b", bracket))
    require(set(SOURCE_IDS) - cited == set(KNOWN_UNCITED_NOTE_ANOMALIES),
            "unexpected uncited source")


def validate_document(data, markdown):
    required = {"schema_version", "document", "issue", "source_of_entries", "checked",
                "sources", "could_not_verify", "bottom_line", "unverified_mark_count"}
    require(isinstance(data, dict) and required <= data.keys(), "missing catalogue field")
    require(data["schema_version"] == 1 and data["document"] == DOCUMENT, "schema mismatch")
    require(data["issue"] == 2198 and data["checked"] == "2026-10-11", "provenance mismatch")
    require(data["source_of_entries"] == "research note on the issue", "source mismatch")
    validate_entries(data)
    body = markdown.split("## 1. Bottom line", 1)[1]
    body = "## 1. Bottom line" + body
    require(re.findall(r"^(#{1,6}) (.*)$", body, re.M)
            == [("##", heading) for heading in HEADINGS], "heading mismatch")
    validate_source_list(data, markdown)
    gaps = bullets(section(markdown, "Could not verify"))
    require(len(gaps) == 4 and gaps == data["could_not_verify"], "verification gaps mismatch")
    bottom = bullets(section(markdown, "1. Bottom line"))
    require(len(bottom) == 3 and bottom == data["bottom_line"], "bottom line mismatch")
    legend, body = markdown.split("## 1. Bottom line", 1)
    require(len(MARK.findall(legend)) == LEGEND_MARKS, "legend marks mismatch")
    require(type(data["unverified_mark_count"]) is int
            and data["unverified_mark_count"] == BODY_MARKS
            and len(MARK.findall(body)) == BODY_MARKS, "body marks mismatch")
    require(table_row_counts(markdown) == TABLE_ROWS, "table counts mismatch")


def test_research_document_and_sources():
    validate_document(*load_artifacts())


@pytest.mark.parametrize("defect", [
    "missing_field", "duplicate_id", "invalid_url", "access_note", "bottom_line",
    "verification_gap", "mark_count",
])
def test_validator_rejects_broken_entry(defect):
    data, markdown = load_artifacts()
    data = copy.deepcopy(data)
    if defect == "missing_field":
        del data["sources"][0]["citation"]
    elif defect == "duplicate_id":
        data["sources"][1]["id"] = "S1"
    elif defect == "invalid_url":
        data["sources"][0]["urls"] = ["https:///"]
    elif defect == "access_note":
        data["sources"][0]["access_note"] = "not opened"
    elif defect == "bottom_line":
        data["bottom_line"][0] = "broken"
    elif defect == "verification_gap":
        data["could_not_verify"].pop()
    else:
        data["unverified_mark_count"] = 0
    with pytest.raises(ValueError):
        validate_document(data, markdown)


@pytest.mark.parametrize("defect", ["citation", "table", "mark", "heading"])
def test_validator_rejects_broken_markdown(defect):
    data, markdown = load_artifacts()
    if defect == "citation":
        markdown = markdown.replace("[S1]", "[S99]", 1)
    elif defect == "table":
        markdown = re.sub(r"^\| API RP 2A .*\n", "", markdown, flags=re.M)
    elif defect == "mark":
        markdown = markdown.replace("**Not opened**", "Not opened")
    else:
        markdown += "\n## Extra heading\n"
    with pytest.raises(ValueError):
        validate_document(data, markdown)
