"""Offline validation of the source register transcribed from the issue note."""
from collections import Counter
from copy import deepcopy
from pathlib import Path
import re
from urllib.parse import urlsplit

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
REGISTER = ROOT / "docs/domains/hull_library/public-benchmark-hull-sources.yaml"
PAGE = REGISTER.with_suffix(".md")
IDS = [f"{group}{number}" for group in ("S", "O") for number in range(1, 10)]
# The research note's own section 3, Counts, governs grades, not Status cells.
GRADE_IDS = {
    "verified": ["S1", "S2", "S3", "S4", "S5", "O1", "O3", "O4", "O5", "O6"],
    "partial": ["S6", "S7", "O2", "O7", "O8", "O9"],
    "unverified": ["S8", "S9"],
}
# Only explicit source-note gaps belong here; no inferred URLs or regrading.
KNOWN_NOTE_GAPS = ()
REQUIRED = {
    "id": str, "name": str, "hull_type": str, "group": str,
    "particulars": list, "particulars_source": (str, type(None)),
    "geometry": list, "pages": list, "benchmark_data": str,
    "reuse_terms": str, "fitness_note": str, "date_checked": str,
    "grade": str, "status_note": str, "benchmark_pages": list,
    "geometry_available": str, "particulars_as_reported": str,
    "geometry_as_reported": str,
}


def valid_url(value):
    """Parse source URLs without sending any requests."""
    if not isinstance(value, str) or re.search(r"\s|[<>`\\]", value):
        return False
    try:
        parsed = urlsplit(value)
        return (parsed.scheme in {"http", "https"} and bool(parsed.hostname)
                and parsed.username is None and parsed.password is None
                and (parsed.port is None or 0 < parsed.port < 65536))
    except ValueError:
        return False


def geometry_reaches_url(entry, rows, seen=frozenset()):
    """Follow geometry references only; page URLs cannot qualify a row."""
    if entry["id"] in seen:
        return False
    seen = seen | {entry["id"]}
    for location in entry["geometry"]:
        if valid_url(location.get("url")):
            return True
        target = rows.get(location.get("same_as"))
        if target is not None and geometry_reaches_url(target, rows, seen):
            return True
    return False


def validate_entry(entry, rows=None):
    """Reject malformed rows and unsupported verified-source claims."""
    for key, kind in REQUIRED.items():
        if key not in entry or not isinstance(entry[key], kind):
            raise ValueError(f"Missing or mistyped {key}")
    if entry["grade"] not in GRADE_IDS:
        raise ValueError("Unknown grade")
    if entry["group"] not in {"ships", "offshore and box-type forms"}:
        raise ValueError("Unknown group")
    for particular in entry["particulars"]:
        for key in ("name", "value", "unit", "note"):
            if key not in particular or not isinstance(particular[key], (str, type(None))):
                raise ValueError(f"Missing or mistyped particular {key}")
        if particular.get("verified") not in (None, False, True):
            raise ValueError("Particular verification must be boolean")
        if "unverified" in (particular["note"] or "").lower():
            if particular.get("verified") is not False:
                raise ValueError("Unverified particular must carry verified: false")
    for location in entry["geometry"]:
        for key in ("format", "url", "host", "size_as_reported", "note"):
            if key not in location or not isinstance(location[key], (str, type(None))):
                raise ValueError(f"Missing or mistyped geometry {key}")
        if location["url"] is not None and not valid_url(location["url"]):
            raise ValueError("Malformed geometry URL")
        if "same_as" in location:
            reference = location["same_as"]
            if not isinstance(reference, str) or reference == entry["id"]:
                raise ValueError("Invalid geometry same-as reference")
    if not all(valid_url(url) for url in entry["pages"] + entry["benchmark_pages"]):
        raise ValueError("Malformed page URL")
    # Listed case folders do not establish that their contents were opened.
    if entry["geometry_available"] not in {
        "yes", "no", "dimensions only", "listed, not opened",
    }:
        raise ValueError("Unknown geometry availability")
    if entry["grade"] == "verified" and (
        not geometry_reaches_url(entry, rows or {}) or not entry["reuse_terms"].strip()
    ):
        raise ValueError("Verified row requires geometry URL and terms text")


def load_register():
    assert REGISTER.is_file(), "Source register is missing"
    return yaml.safe_load(REGISTER.read_text(encoding="utf-8"))


def test_structure_order_and_counts():
    data = load_register()
    assert isinstance(data, dict) and set(data) == {"meta", "hulls"}
    assert isinstance(data["meta"], dict)
    rows = data["hulls"]
    assert isinstance(rows, list) and len(rows) == 18
    assert [row["id"] for row in rows] == IDS
    assert len({row["id"] for row in rows}) == 18
    assert Counter(row["grade"] for row in rows) == {
        "verified": 10, "partial": 6, "unverified": 2,
    }
    assert data["meta"]["counts"] == {
        "verified": 10, "partial": 6, "unverified": 2,
    }
    for grade, ids in GRADE_IDS.items():
        assert [row["id"] for row in rows if row["grade"] == grade] == ids
        assert data["meta"]["grade_ids"][grade] == ids


def test_rows_and_known_note_gaps():
    data = load_register()
    gaps = []
    rows = {row["id"]: row for row in data["hulls"]}
    for row in data["hulls"]:
        try:
            validate_entry(row, rows)
        except ValueError as error:
            assert str(error) == "Verified row requires geometry URL and terms text"
            gaps.append(row["id"])
        assert row["date_checked"] == "2026-10-11"
    assert tuple(gaps) == KNOWN_NOTE_GAPS
    assert not gaps, "Source note gaps require PM resolution before acceptance"
    mismatches = [row["id"] for row in data["hulls"]
                  if row["status_note"].split()[0].lower() != row["grade"]]
    assert mismatches == [item["id"] for item in data["meta"]["note_inconsistencies"]]


def all_strings(value):
    if isinstance(value, str):
        yield value
    elif isinstance(value, dict):
        for key, item in value.items():
            yield from all_strings(key)
            yield from all_strings(item)
    elif isinstance(value, list):
        for item in value:
            yield from all_strings(item)


def test_every_url_is_parsed_offline():
    for text in all_strings(load_register()):
        for url in re.findall(r"https?://[^\s`\"<>]+", text):
            assert valid_url(url.rstrip(".,);")), url


def test_unverified_particulars_are_not_promoted():
    rows = {row["id"]: row for row in load_register()["hulls"]}
    for row_id in ("S6", "S8", "S9", "O2"):
        assert rows[row_id]["particulars"]
        assert all(item.get("verified") is False for item in rows[row_id]["particulars"])
    for row_id, names in {
        "O1": ["Draft", "displaced volume", "water depth"], "O3": ["Draft"],
    }.items():
        for name in names:
            item = next(p for p in rows[row_id]["particulars"] if p["name"] == name)
            assert item["verified"] is False


def test_markdown_table_matches_yaml():
    data = load_register()
    text = PAGE.read_text(encoding="utf-8")
    table = [line.split("|")[1:-1] for line in text.splitlines()
             if re.match(r"\| [SO][1-9] \|", line)]
    assert [[cell.strip() for cell in row] for row in table] == [
        [row["id"], row["name"], row["grade"], row["geometry_available"]]
        for row in data["hulls"]
    ]
    assert data["meta"]["suggested_use"] in text
    assert data["meta"]["terms_legend"] in text
    for legend in data["meta"]["grade_legend"].values():
        assert legend in text


@pytest.mark.parametrize("mutation, message", [
    ("no_geometry", "Verified row requires geometry URL and terms text"),
    ("bad_grade", "Unknown grade"),
    ("no_terms", "Verified row requires geometry URL and terms text"),
    ("bad_url", "Malformed geometry URL"),
    ("promoted_particular", "Unverified particular must carry verified: false"),
])
def test_validator_rejects_broken_in_memory_entry(mutation, message):
    row = deepcopy(load_register()["hulls"][0])
    if mutation == "no_geometry":
        row["geometry"] = []  # A URL in pages must not satisfy geometry.
    elif mutation == "bad_grade":
        row["grade"] = "confirmed"
    elif mutation == "no_terms":
        row["reuse_terms"] = ""
    elif mutation == "bad_url":
        row["geometry"][0]["url"] = "file:relative"
    else:
        row["particulars"][0]["note"] = "unverified"
        row["particulars"][0]["verified"] = True
    with pytest.raises(ValueError, match=message):
        validate_entry(row)


@pytest.mark.parametrize("target, accepted", [
    ("missing", False), ("no_url", False), ("self", False),
    ("cycle", False), ("direct", True), ("chain", True),
])
def test_geometry_same_as_resolution(target, accepted):
    rows = {row["id"]: deepcopy(row) for row in load_register()["hulls"]}
    row = rows["O5"]
    reference = {
        "format": None, "url": None, "host": None, "size_as_reported": None,
        "note": row["geometry_as_reported"], "same_as": "O4",
    }
    row["geometry"] = [reference]
    if target == "missing":
        del rows["O4"]
    elif target == "no_url":
        rows["O4"]["geometry"] = []
    elif target == "self":
        reference["same_as"] = "O5"
    elif target == "cycle":
        rows["O4"]["geometry"] = [dict(reference, same_as="O5")]
    elif target == "chain":
        rows["O4"]["geometry"] = [dict(reference, same_as="O6")]
    if accepted:
        validate_entry(row, rows)
    else:
        with pytest.raises(ValueError, match="geometry"):
            validate_entry(row, rows)


def test_listed_geometry_availability_retains_note_qualification():
    row = next(row for row in load_register()["hulls"] if row["id"] == "O7")
    assert "Contents were listed, not opened." in row["fitness_note"]
    assert row["geometry_available"] == "listed, not opened"
