"""Offline vessel-spec contracts using synthetic, hand-written records."""

from copy import deepcopy
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "assets" / "data"
URL = "https://example.org/vessel"
DATE = "2026-10-11T12:00:00Z"


@pytest.fixture
def validator():
    module = ROOT / "src/digitalmodel/data_systems/vessel_specs.py"
    assert module.exists(), "Vessel-spec validator is required"
    from digitalmodel.data_systems.vessel_specs import validate_rows

    return validate_rows


@pytest.fixture
def manifest():
    return {
        "sources": [
            {
                "source_url": URL,
                "retrieved_utc": DATE,
                "status": "retrieved",
                "sha256": "a" * 64,
                "size_bytes": 42,
                "licence_terms": "Synthetic test source; no real vessel.",
            }
        ]
    }


@pytest.fixture
def row():
    return {
        "vessel_name": "Example Lift",
        "owner": "Example Operator",
        "crane_capacity_tonnes": 100,
        "crane_capacity_basis": "combined",
        "deck_area_m2": None,
        "dp_class": None,
        "bollard_pull_tonnes": None,
        "source_url": URL,
        "retrieved_utc": DATE,
        "field_sources": {
            "vessel_name": URL,
            "owner": URL,
            "crane_capacity_tonnes": URL,
            "crane_capacity_basis": URL,
        },
    }


@pytest.mark.parametrize("kind", ["hlv", "pipelay"])
def test_committed_datasets_validate(kind):
    from importlib import import_module

    path = DATA / f"vessel_specs_{kind}.json"
    assert path.exists(), "Sourced dataset is required"
    module = import_module("digitalmodel.data_systems.vessel_specs")
    rows = module.read_json(path)
    assert rows, "Dataset must include sourced vessels"
    module.validate_dataset(DATA, kind)
    for row in rows:
        assert row["source_url"].startswith("https://")
        assert row["retrieved_utc"].endswith(("Z", "+00:00"))


@pytest.mark.parametrize(
    "field,value",
    [
        ("source_url", None),
        ("source_url", "http://example.org/vessel"),
        ("source_url", "https://"),
        ("source_url", "https://bad host/vessel"),
        ("crane_capacity_tonnes", -1),
        ("crane_capacity_tonnes", 0),
        ("crane_capacity_tonnes", True),
        ("retrieved_utc", "2026-02-30T00:00:00Z"),
        ("retrieved_utc", "2026-10-11"),
        ("retrieved_utc", "2099-01-01T00:00:00Z"),
        ("deck_area_m2", float("nan")),
    ],
)
def test_rejects_invalid_fields(validator, manifest, row, field, value):
    row[field] = value
    with pytest.raises(ValueError):
        validator([row], DATA / "vessel_specs_hlv.schema.json", manifest)


def test_rejects_missing_source_url(validator, manifest, row):
    del row["source_url"]
    with pytest.raises(ValueError):
        validator([row], DATA / "vessel_specs_hlv.schema.json", manifest)


def test_rejects_duplicate_names(validator, manifest, row):
    duplicate = deepcopy(row)
    duplicate["vessel_name"] = " example lift "
    with pytest.raises(ValueError, match="Duplicate"):
        validator([row, duplicate], DATA / "vessel_specs_hlv.schema.json", manifest)


@pytest.mark.parametrize("mutation", ["missing", "unknown", "refused", "date"])
def test_rejects_untraceable_fields(validator, manifest, row, mutation):
    if mutation == "missing":
        del row["field_sources"]["crane_capacity_tonnes"]
    elif mutation == "unknown":
        row["field_sources"]["crane_capacity_tonnes"] = "https://example.org/other"
    elif mutation == "refused":
        manifest["sources"][0]["status"] = "refused"
    else:
        row["retrieved_utc"] = "2026-10-10T12:00:00Z"
    with pytest.raises(ValueError):
        validator([row], DATA / "vessel_specs_hlv.schema.json", manifest)


def test_accepts_known_values_and_null_gaps(validator, manifest, row):
    validator([row], DATA / "vessel_specs_hlv.schema.json", manifest)


@pytest.mark.parametrize("text", ['{"x":1,"x":2}', '{"x":NaN}'])
def test_rejects_ambiguous_json(text):
    assert (ROOT / "src/digitalmodel/data_systems/vessel_specs.py").exists()
    from digitalmodel.data_systems.vessel_specs import parse_json

    with pytest.raises(ValueError):
        parse_json(text)


@pytest.mark.parametrize(
    "field,value",
    [
        ("water_depth_min_m", 200),
        ("water_depth_max_m", 50),
    ],
)
def test_rejects_reversed_operating_depths(validator, manifest, field, value):
    row = {
        "vessel_name": "Example Lay",
        "owner": "Example Operator",
        "dp_class": None,
        "source_url": URL,
        "retrieved_utc": DATE,
        "lay_methods": ["S-lay"],
        "tensioner_capacity_tonnes": 100,
        "max_pipe_diameter_inches": 10,
        "water_depth_min_m": 100,
        "water_depth_max_m": 150,
        "record_water_depth_m": None,
    }
    row[field] = value
    row["field_sources"] = {
        key: URL
        for key, value in row.items()
        if value is not None and key not in ("source_url", "retrieved_utc")
    }
    with pytest.raises(ValueError, match="reversed"):
        validator([row], DATA / "vessel_specs_pipelay.schema.json", manifest)


def test_rejects_capacity_without_basis(validator, manifest, row):
    row["crane_capacity_basis"] = None
    del row["field_sources"]["crane_capacity_basis"]
    with pytest.raises(ValueError, match="basis"):
        validator([row], DATA / "vessel_specs_hlv.schema.json", manifest)


@pytest.mark.parametrize(
    "field,value", [("sha256", "bad"), ("size_bytes", -1), ("licence_terms", "")]
)
def test_rejects_incomplete_manifest(validator, manifest, row, field, value):
    manifest["sources"][0][field] = value
    with pytest.raises(ValueError):
        validator([row], DATA / "vessel_specs_hlv.schema.json", manifest)
