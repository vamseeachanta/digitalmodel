"""Offline validation of the issue research-note transcription."""

from copy import deepcopy
from datetime import date
from pathlib import Path
import re
from urllib.parse import urlparse

import pytest
import yaml

DATA_PATH = (
    Path(__file__).resolve().parents[2]
    / "data/drilling_rigs/udw_rig_fleet_external.yml"
)
RIG_FIELDS = {
    "name", "owner_manager", "rig_type", "design_as_published",
    "year_entered_service", "water_depth_designed_ft",
    "water_depth_outfitted_ft", "drilling_depth_ft", "bop_stacks",
    "hookload_short_tons", "generation", "other_items",
}
CONTRACT_FIELDS = {
    "rig", "customer", "location", "status", "start", "end",
    "dayrate_usd", "source_keys", "as_published_on", "footnote_1",
}


def iso_date(value):
    assert isinstance(value, str) and re.fullmatch(r"\d{4}-\d{2}-\d{2}", value)
    date.fromisoformat(value)


def positive_integer(value):
    assert type(value) is int and value > 0


def validate_value(value, sources):
    assert isinstance(value, dict) and "value" in value
    assert set(value) <= {
        "value", "unit", "source_key", "source_keys", "as_published_on",
        "as_published", "note", "sic", "status",
    }
    assert value.get("source_key") in sources
    for key in value.get("source_keys", []):
        assert key in sources
    if value["source_key"] == "S5":
        published = value["as_published_on"]
        assert published == "undated" or published.startswith("file dated "), \
            "shared-source field requires a per-rig document date"
        if published != "undated":
            iso_date(published.removeprefix("file dated "))
    else:
        assert value["as_published_on"] == sources[value["source_key"]]["published"]
    assert not {"not_verified", "not_verified_ref", "unverified_ref"} & value.keys()


def validate_shared_source_dates(rig, source):
    fields = [value for value in rig.values()
              if isinstance(value, dict) and value.get("source_key") == source["key"]]
    for value in fields:
        validate_value(value, {source["key"]: source})
    dates = {value["as_published_on"] for value in fields}
    assert len(dates) == 1, "shared-source dates differ within rig"
    published = dates.pop()
    name = rig["name"]["value"]
    dated_sheets = dict(re.findall(
        r"(\w+) (?:file dated )?(\d{4}-\d{2}-\d{2})", source["published"]
    ))
    rig_date = dated_sheets.get(name.removeprefix("Deepwater "))
    expected = f"file dated {rig_date}" if rig_date else "undated"
    assert published == expected, "document date belongs to another rig"
    if rig_date:
        assert rig_date in source["published"]
    assert rig.get("source_date_remark") == source["published"], \
        "source_date_remark must preserve the shared-source published text"


def validate_rigs(rigs, sources):
    assert len(rigs) == 9
    names = [rig["name"]["value"] for rig in rigs]
    assert len(set(names)) == 9
    high_pressure = set()
    for rig in rigs:
        shared = [value for value in rig.values()
                  if isinstance(value, dict) and value.get("source_key") == "S5"]
        expected_fields = RIG_FIELDS | {"source_date_remark"} if shared else RIG_FIELDS
        assert set(rig) == expected_fields
        assert rig["generation"] is None
        for field, value in rig.items():
            if field not in {"generation", "source_date_remark"}:
                validate_value(value, sources)
        if shared:
            validate_shared_source_dates(rig, sources["S5"])
        for stack in rig["bop_stacks"]["value"]:
            assert set(stack) == {"rating_psi", "rams", "count", "maker"}
            for field in ("rating_psi", "rams", "count"):
                positive_integer(stack[field])
            assert stack["maker"] is None or isinstance(stack["maker"], str)
            if stack["rating_psi"] == 20000:
                high_pressure.add(rig["name"]["value"])
    assert high_pressure == {"Deepwater Titan", "Deepwater Atlas"}


def validate_contract(row, sources, rig_names):
    assert CONTRACT_FIELDS <= row.keys()
    assert set(row) <= CONTRACT_FIELDS | {"previous_report"}
    assert row["rig"] in rig_names
    keys = row["source_keys"]
    assert keys and len(keys) == len(set(keys))
    assert all(key in sources for key in keys)
    iso_date(row["as_published_on"])
    assert row["as_published_on"] == sources[keys[0]]["published"]
    assert type(row["footnote_1"]) is bool
    dayrate = row["dayrate_usd"]
    if dayrate is None:
        assert row["status"] == "Idle"
        assert row["rig"] == "Deepwater Proteus"
        assert keys == ["S2"] and row["start"] == "Apr-26"
        assert row["customer"] is None and row["end"] is None
    elif dayrate != "not_disclosed":
        positive_integer(dayrate)
    if "previous_report" in row:
        previous = row["previous_report"]
        assert previous["source_key"] in keys
        iso_date(previous["as_published_on"])
        assert previous["as_published_on"] == sources[previous["source_key"]]["published"]


def validate_data(data):
    assert list(data)[:5] == [
        "schema_version", "data_class", "source_of_entries", "checked", "limits",
    ]
    assert data["data_class"] == "REAL-EXTERNAL"
    assert data["source_of_entries"] == "research note on the issue"
    assert data["checked"] == "2026-10-11"
    limits = "\n".join(data["limits"])
    assert "Other contractors: zero verified rig rows." in limits
    assert '"Only two 20K rigs worldwide" is not proven by this note' in limits
    assert "Semisubmersibles: zero verified rows." in limits
    assert "must not be read as a census" in limits
    assert "no trend or forecast is implied" in limits
    keys = [source["key"] for source in data["sources"]]
    assert keys == [f"S{i}" for i in range(1, 8)]
    sources = {source["key"]: source for source in data["sources"]}
    for source in sources.values():
        parsed = urlparse(source["url"])
        assert parsed.scheme in {"http", "https"} and parsed.netloc
        assert not re.search(r"\s", source["url"])
        assert source["checked"] == data["checked"]
    validate_rigs(data["rigs"], sources)
    assert len(data["contracts"]) == 18
    names = {rig["name"]["value"] for rig in data["rigs"]}
    for row in data["contracts"]:
        validate_contract(row, sources, names)
    generation = [item for item in data["not_verified"]
                  if item["status"] == "secondary_unverified"]
    assert len(generation) == 1
    assert all(word in generation[0]["statement"] for word in (
        '"8th generation"', '"7th generation"', "Atlas", "Titan",
    ))
    assert all(item["status"] in {"unverified", "secondary_unverified"}
               for item in data["not_verified"])
    assert all(set(item) == {"statement", "status"} for item in data["not_verified"])
    for item in data["secondary_reports"] + data["narrative_items"]:
        assert item["source_key"] in sources


@pytest.fixture
def data():
    return yaml.safe_load(DATA_PATH.read_text(encoding="utf-8"))


def test_research_note_dataset(data):
    validate_data(data)


@pytest.mark.parametrize("defect", ["missing_source", "malformed_date"])
def test_invalid_contract_rejected(data, defect):
    broken = deepcopy(data)
    row = broken["contracts"][0]
    if defect == "missing_source":
        row.pop("source_keys")
    else:
        row["as_published_on"] = "2026-99-05"
    with pytest.raises((AssertionError, ValueError)):
        validate_data(broken)


@pytest.mark.parametrize("defect", ["unknown_rig_source", "generation", "well_link"])
def test_unsourced_rig_or_contract_link_rejected(data, defect):
    broken = deepcopy(data)
    if defect == "unknown_rig_source":
        broken["rigs"][0]["bop_stacks"]["source_key"] = "unknown"
    elif defect == "generation":
        broken["rigs"][0]["generation"] = "8th generation"
    else:
        broken["contracts"][0]["well"] = "unverified"
    with pytest.raises(AssertionError):
        validate_data(broken)


@pytest.mark.parametrize("rig_index", range(2, 9))
def test_sister_rig_document_dates(data, rig_index):
    source = next(source for source in data["sources"] if source["key"] == "S5")
    validate_shared_source_dates(data["rigs"][rig_index], source)


@pytest.mark.parametrize("defect", [
    "combined_remark", "another_rig_date", "dated_rig_undated",
    "inconsistent_dates", "missing_remark", "altered_remark",
])
def test_invalid_sister_rig_document_dates_rejected(data, defect):
    rig = deepcopy(next(rig for rig in data["rigs"]
                        if rig["name"]["value"] == "Deepwater Conqueror"))
    source = next(source for source in data["sources"] if source["key"] == "S5")
    if defect in {"combined_remark", "another_rig_date", "dated_rig_undated"}:
        invictus_date = re.search(r"Invictus (\d{4}-\d{2}-\d{2})",
                                 source["published"])[1]
        published = {
            "combined_remark": source["published"],
            "another_rig_date": f"file dated {invictus_date}",
            "dated_rig_undated": "undated",
        }[defect]
        for value in rig.values():
            if isinstance(value, dict) and value.get("source_key") == "S5":
                value["as_published_on"] = published
    elif defect == "inconsistent_dates":
        rig["owner_manager"]["as_published_on"] = "undated"
    elif defect == "missing_remark":
        rig.pop("source_date_remark", None)
    else:
        rig["source_date_remark"] = "undated"
    with pytest.raises(AssertionError):
        validate_shared_source_dates(rig, source)
