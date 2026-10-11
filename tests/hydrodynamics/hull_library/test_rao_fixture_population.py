"""Offline behaviour checks derived only from solver spreadsheet fixtures."""

from copy import deepcopy
from datetime import date, timedelta
import json
from pathlib import Path
import socket

import numpy as np
import openpyxl
import pytest

from digitalmodel.hydrodynamics.hull_library.rao_database import RAODatabase
from digitalmodel.hydrodynamics.hull_library.rao_extractor import (
    populate_database_from_xlsx,
    xlsx_to_rao_data,
)

ROOT = Path(__file__).resolve().parents[3]
FIXTURES = ROOT / "tests/fixtures/solver"
STEMS = (
    "test01_unit_box", "ellipsoid", "L00_test01",
    "L01_001_ship_raos", "L02_OC4_semi_sub",
)
RETRIEVED_ON = date.fromisoformat("2026-10-11")  # Issue dispatch date.
FIELDS = ("frequencies", "directions", "amplitudes", "phases")


@pytest.fixture(autouse=True)
def deny_network(monkeypatch):
    def blocked(*args, **kwargs):
        raise AssertionError("Fixture population tests shall run offline")
    monkeypatch.setattr(socket.socket, "connect", blocked)
    monkeypatch.setattr(socket, "create_connection", blocked)


@pytest.fixture
def extracted_entries():
    entries = {}
    for stem in STEMS:
        entries[stem] = populate_database_from_xlsx(
            RAODatabase(), FIXTURES / f"{stem}.xlsx", stem, {},
            metadata={"basis": {"qualification": "fixture only"}},
        )
    return entries


def test_identical_datasets_collapse_across_ids(extracted_entries):
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import (
        store_fixture_entry,
    )
    original = extracted_entries[STEMS[0]]
    alias = deepcopy(original)
    alias.variation_id = "identical_export"
    db = RAODatabase()
    store_fixture_entry(db, original)
    retained = store_fixture_entry(db, alias)
    assert list(db.query({})) == [original]
    assert retained.variation_id == original.variation_id


def test_distinct_datasets_remain_distinct(extracted_entries):
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import (
        store_fixture_entry,
    )
    db = RAODatabase()
    for stem in STEMS[:2]:
        store_fixture_entry(db, extracted_entries[stem])
    assert {entry.variation_id for entry in db.query({})} == set(STEMS[:2])


def test_different_basis_does_not_collapse(extracted_entries):
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import (
        store_fixture_entry,
    )
    original = extracted_entries[STEMS[0]]
    changed = deepcopy(original)
    changed.variation_id = "different_basis"
    changed.metadata["basis"]["qualification"] = "different reference"
    db = RAODatabase()
    store_fixture_entry(db, original)
    store_fixture_entry(db, changed)
    assert {entry.variation_id for entry in db.query({})} == {
        original.variation_id, changed.variation_id,
    }


def test_box_exports_compare_at_source_precision(extracted_entries):
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_comparison import (
        compare_box_exports,
    )
    comparison = compare_box_exports(
        extracted_entries[STEMS[0]].rao_data,
        extracted_entries[STEMS[2]].rao_data,
        FIXTURES / f"{STEMS[0]}.xlsx", FIXTURES / f"{STEMS[2]}.xlsx",
    )
    assert comparison["equivalent"]
    assert comparison["directions_exact"]
    for field in ("frequencies", "amplitudes", "phases"):
        assert comparison["components"][field]["within_bounds"]


def test_changed_box_response_fails_alias_comparison(extracted_entries):
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_comparison import (
        compare_box_exports,
    )
    original = extracted_entries[STEMS[0]].rao_data
    changed = deepcopy(extracted_entries[STEMS[2]].rao_data)
    # A distinct value from the same fixture supplies the perturbation.
    changed.amplitudes.flat[0] = original.amplitudes.max()
    assert not compare_box_exports(
        original, changed, FIXTURES / f"{STEMS[0]}.xlsx",
        FIXTURES / f"{STEMS[2]}.xlsx",
    )["equivalent"]


def test_nonfinite_dataset_is_rejected(extracted_entries):
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import (
        store_fixture_entry,
    )
    entry = deepcopy(extracted_entries[STEMS[0]])
    entry.rao_data.amplitudes.flat[0] = np.nan
    with pytest.raises(ValueError, match="finite"):
        store_fixture_entry(RAODatabase(), entry)


def test_ship_periods_convert_and_responses_stay_aligned():
    path = FIXTURES / "L01_001_ship_raos.xlsx"
    wb = openpyxl.load_workbook(path, read_only=True, data_only=True)
    rows = list(wb["Displacement RAOs"].iter_rows(values_only=True))
    wb.close()
    # The fixture identifies the axis as Period (s) in rows 7 and 9.
    assert rows[6][1] == "Period" and rows[8][1] == "s"
    heading = min(row[0] for row in rows if isinstance(row[0], (int, float)))
    block = [row for row in rows if row[0] == heading]
    # omega*T=2*pi, also implemented by orcawave_to_orcaflex.py.
    expected = np.array([2 * np.pi / row[1] for row in block])
    order = np.argsort(expected)
    actual = xlsx_to_rao_data(path)
    np.testing.assert_array_equal(actual.frequencies, expected[order])
    np.testing.assert_array_equal(
        actual.amplitudes[:, 0, :],
        np.abs(np.array([row[2:14:2] for row in block], dtype=float)[order]),
    )
    np.testing.assert_array_equal(
        actual.phases[:, 0, :],
        np.array([row[3:14:2] for row in block], dtype=float)[order],
    )


def test_expected_set_and_all_sources_are_retained():
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import (
        populate_fixture_database,
    )
    db = RAODatabase()
    populate_fixture_database(db, ROOT, RETRIEVED_ON.isoformat())
    assert {entry.variation_id for entry in db.query({})} == set(STEMS) - {STEMS[2]}
    sources = [source for entry in db.query({}) for source in entry.metadata["sources"]]
    assert {source["path"] for source in sources} == {
        f"tests/fixtures/solver/{stem}.xlsx" for stem in STEMS
    }
    for entry in db.query({}):
        assert entry.hull_params == {}
        assert not Path(entry.metadata["source_file"]).is_absolute()
        for source in entry.metadata["sources"]:
            assert source["sha256"] and source["revision"]
            assert source["url"] == f"../../{source['revision']}/{source['path']}"
    alias = db.get_by_id(STEMS[0])
    assert alias.metadata["box_comparison"]["equivalent"]


def test_population_idempotent_even_across_dates():
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import (
        populate_fixture_database,
    )
    db = RAODatabase()
    populate_fixture_database(db, ROOT, RETRIEVED_ON.isoformat())
    original = deepcopy(list(db.query({})))
    later = (RETRIEVED_ON + timedelta(days=1)).isoformat()
    populate_fixture_database(db, ROOT, later)
    repeated = list(db.query({}))
    assert [entry.variation_id for entry in repeated] == [entry.variation_id for entry in original]
    for old, new in zip(original, repeated):
        assert old.metadata == new.metadata
        for field in FIELDS:
            np.testing.assert_array_equal(getattr(old.rao_data, field), getattr(new.rao_data, field))


def test_occupied_id_with_different_data_is_rejected(extracted_entries):
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import (
        store_fixture_entry,
    )
    original = extracted_entries[STEMS[0]]
    changed = deepcopy(original)
    changed.rao_data.amplitudes.flat[0] = original.rao_data.amplitudes.max()
    db = RAODatabase()
    store_fixture_entry(db, original)
    with pytest.raises(ValueError, match="occupied"):
        store_fixture_entry(db, changed)
    np.testing.assert_array_equal(db.get_by_id(original.variation_id).rao_data.amplitudes, original.rao_data.amplitudes)


def test_regeneration_readback_and_no_unrelated_overwrite(tmp_path):
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import (
        regenerate_fixture_database,
    )
    output = tmp_path / "rao_database.parquet"
    manifest = tmp_path / "rao_database.manifest.json"
    db = regenerate_fixture_database(ROOT, output, manifest, RETRIEVED_ON.isoformat())
    restored = RAODatabase()
    restored.load_from_disk(output)
    for entry in db.query({}):
        persisted = restored.get_by_id(entry.variation_id)
        assert persisted.metadata == entry.metadata
        for field in FIELDS:
            np.testing.assert_array_equal(getattr(entry.rao_data, field), getattr(persisted.rao_data, field))
    regenerate_fixture_database(ROOT, output, manifest, RETRIEVED_ON.isoformat())
    extra = deepcopy(restored.get_by_id(STEMS[0]))
    extra.variation_id = "unrelated_entry"
    restored.store(extra)
    restored.save_to_disk(output)
    before = output.read_bytes()
    with pytest.raises(ValueError, match="unrelated"):
        regenerate_fixture_database(ROOT, output, manifest, RETRIEVED_ON.isoformat())
    assert output.read_bytes() == before


def test_manifest_command_tracks_retained_source_date(tmp_path):
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import (
        regenerate_fixture_database,
    )
    output = tmp_path / "rao_database.parquet"
    manifest = tmp_path / "rao_database.manifest.json"
    source_date = (RETRIEVED_ON - timedelta(days=1)).isoformat()
    regenerate_fixture_database(ROOT, output, manifest, source_date)
    original_manifest = manifest.read_bytes()
    payload = json.loads(original_manifest)
    assert payload["regenerate"].endswith(f"--retrieved-on {source_date}")
    assert {source["retrieved_on"] for entry in payload["entries"]
            for source in entry["sources"]} == {source_date}
    regenerate_fixture_database(ROOT, output, manifest, RETRIEVED_ON.isoformat())
    assert manifest.read_bytes() == original_manifest
