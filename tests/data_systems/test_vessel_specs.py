# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline vessel-spec contracts using synthetic, hand-written records."""

import json
import re
from copy import deepcopy
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "assets" / "data"
URL = "https://example.org/vessel"
DATE = "2026-10-11T12:00:00Z"
PIPELAY_QUEUE = (
    ("DB50", "McDermott", ("db50", "derrickbarge50")),
    ("DB30", "McDermott", ("db30", "derrickbarge30")),
    ("Seven Borealis", "Subsea7", ("sevenborealis",)),
    ("Castorone", "Saipem", ("castorone",)),
)
HLV_QUEUE = (
    ("Bokalift 2", "Boskalis", ("bokalift2",)),
    ("Bokalift 1", "Boskalis", ("bokalift1",)),
    ("Les Alizés", "Jan De Nul", ("lesalizés", "lesalizes")),
    ("Orion", "DEME", ("orion",)),
    ("Gulliver", "DEME", ("gulliver",)),
    ("Svanen", "Van Oord", ("svanen",)),
    ("DB50", "McDermott", ("db50", "derrickbarge50")),
)


def test_committed_datasets_cover_batch_two():
    readme = (DATA / "vessel_specs_README.md").read_text(encoding="utf-8")
    manifest = json.loads(
        (DATA / "vessel_specs_provenance.json").read_text(encoding="utf-8")
    )
    retrieved = {
        s["source_url"] for s in manifest["sources"] if s["status"] == "retrieved"
    }
    reasons = {
        "refused",
        "not published",
        "conflicting values",
        "name differs",
        "no headline value",
        "batch cap",
    }
    counts, owners, picked, available = [], set(), set(), {}
    for kind, queue in (("pipelay", PIPELAY_QUEUE), ("hlv", HLV_QUEUE)):
        rows = json.loads(
            (DATA / f"vessel_specs_{kind}.json").read_text(encoding="utf-8")
        )
        matches, gaps = [], []
        for name, owner, keys in queue:
            found = [
                row
                for row in rows
                if "".join(c for c in row["vessel_name"].casefold() if c.isalnum())
                in keys
            ]
            assert len(found) <= 1, f"Duplicate batch row: {name} ({kind})"
            gap = re.findall(
                rf"^- {re.escape(name)} \({kind}\): (.+)$", readme, re.MULTILINE
            )
            assert len(gap) <= 1
            reason = gap[0].split(";", 1)[0] if gap else None
            matches.append(bool(found))
            gaps.append(reason)
            assert not (found and gap), f"Row also listed as gap: {name} ({kind})"
            if not found:
                assert reason in reasons, f"Missing batch gap: {name} ({kind})"
                continue
            assert name.casefold() in readme.casefold(), f"Missing locator: {name}"
            row = found[0]
            assert row["owner"] == owner
            owners.add(owner)
            assert row["source_url"] in retrieved
            if kind == "hlv":
                assert row["crane_capacity_tonnes"] is not None
                assert row["crane_capacity_basis"] in ("individual", "combined")
            else:
                assert any(
                    row[field]
                    for field in (
                        "tensioner_capacity_tonnes",
                        "max_pipe_diameter_inches",
                        "lay_methods",
                    )
                )
        count = sum(matches)
        assert 1 <= count <= 4, f"New {kind} rows: {count}"
        counts.append(count)
        last = max(i for i, matched in enumerate(matches) if matched)
        assert all(
            matched or (reason is not None and reason != "batch cap")
            for matched, reason in zip(matches[:last], gaps[:last], strict=True)
        ), f"Queue entry skipped for batch cap before last {kind} row"
        picked.update((kind, entry[0]) for entry, yes in zip(queue, matches) if yes)
        available[kind] = [
            entry[0]
            for entry, yes, reason in zip(queue, matches, gaps, strict=True)
            if yes or reason == "batch cap"
        ]
    assert 4 <= sum(counts) <= 5
    assert len(owners) >= 2
    expected = []
    for preferred in ("pipelay", "hlv", "pipelay", "hlv", "hlv"):
        alternate = "hlv" if preferred == "pipelay" else "pipelay"
        for kind in (preferred, alternate):
            if available[kind] and sum(k == kind for k, _ in expected) < 4:
                expected.append((kind, available[kind].pop(0)))
                break
    assert picked == set(expected[: sum(counts)]), "Batch selection must alternate"


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
        "dp_notation_source": None,
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
        "dp_notation_source": None,
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


def test_rejects_mixed_positioning_vocabulary(validator, manifest, row):
    row["dp_class"] = "NMD Class III"
    row["field_sources"]["dp_class"] = URL
    with pytest.raises(ValueError, match="Schema"):
        validator([row], DATA / "vessel_specs_hlv.schema.json", manifest)


def test_source_notation_retains_only_normalised_class_and_locator(
    validator, manifest, row
):
    row["dp_notation_source"] = "DP-3; printed page 3"
    row["field_sources"]["dp_notation_source"] = URL
    validator([row], DATA / "vessel_specs_hlv.schema.json", manifest)
    for wording in ("DP3", "NMD Class III", "DP-3", "Unmapped notation"):
        row["dp_notation_source"] = wording
        with pytest.raises(ValueError, match="Schema"):
            validator([row], DATA / "vessel_specs_hlv.schema.json", manifest)
