"""Source-cell qualification and repository-artifact regression checks."""

from pathlib import Path
from copy import deepcopy
import hashlib
import json
import os
import subprocess

import numpy as np

import pytest

from digitalmodel.hydrodynamics.hull_library.rao_database import RAODatabase
from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import (
    populate_fixture_database,
)

ROOT = Path(__file__).resolve().parents[3]
MISSING = "not stated in source"
DOFS = ("surge", "sway", "heave", "roll", "pitch", "yaw")


@pytest.fixture(scope="module")
def qualified_database():
    return populate_fixture_database(RAODatabase(), ROOT, "2026-10-11")


def test_each_source_carries_its_own_unit_labels(qualified_database):
    sources = [source for entry in qualified_database.query({})
               for source in entry.metadata["sources"]]
    assert len(sources) == 5
    for source in sources:
        units = source["units"]
        if source["format"] == "native":
            assert units["directions"]["source_unit"] == "deg"
            assert units["phases"]["source_unit"] == "deg"
            assert units["frequencies"]["stored_unit"] == "rad/s"
            for dof, cell in zip(DOFS, ("C9", "E9", "G9", "I9", "K9", "M9")):
                expected = "m/m" if dof in DOFS[:3] else "rad/m"
                assert units[dof]["source_unit"] == expected
                assert units[dof]["stored_unit"] == expected
                assert units[dof]["labels"] == [{
                    "text": expected, "sheet": "Displacement RAOs", "cell": cell,
                }]
                assert units[dof]["conversion"] == "none"
        else:
            assert units["frequencies"]["labels"] == [{
                "text": "Frequency (rad/s)", "sheet": "RAOs", "cell": "A1",
            }]
            for dof in (*DOFS, "phases"):
                assert units[dof]["source_unit"] == MISSING
                assert units[dof]["stored_unit"] == MISSING
                assert units[dof]["labels"]  # Column names, without unit claims.
        entry = next(entry for entry in qualified_database.query({})
                     if source in entry.metadata["sources"])
        assert entry.metadata["units"][source["path"]] == units
    ship = qualified_database.get_by_id("L01_001_ship_raos")
    axis = next(iter(ship.metadata["units"].values()))["frequencies"]
    assert axis["source_unit"] == "s"
    assert axis["conversion"] == "omega = 2*pi/T"
    semi = qualified_database.get_by_id("L02_OC4_semi_sub")
    assert next(iter(semi.metadata["units"].values()))["directions"]["source_unit"] == MISSING


def test_source_and_loading_statements_remain_source_specific(qualified_database):
    for entry in qualified_database.query({}):
        for source in entry.metadata["sources"]:
            assert source["conventions"] == {
                "phase_convention": MISSING, "response_reference_point": MISSING,
                "heading_convention": MISSING,
            }
            for field in ("conventions", "source", "loading_condition"):
                assert entry.metadata[field][source["path"]] == source[field]
            description = source["source"]
            loading = source["loading_condition"]
            if source["format"] == "pipeline":
                assert description["solver"]["statement"] == "OrcaWave / OrcFxAPI.Diffraction"
                assert description["solver"]["sheet"] == "Summary"
                assert description["solver"]["cells"] == ["A3", "B3"]
                assert description["version"] == MISSING
                assert description["date"] == MISSING
                assert set(loading.values()) == {MISSING}
            else:
                assert description["solver"]["statement"] == "OrcaFlex"
                assert description["version"]["statement"] == "11.6c"
                assert description["date"]["statement"] == "9/24/2026"
                assert loading["draft"] == MISSING
                assert loading["displacement"] == MISSING
                assert loading["mass"][0]["statement"] == "Mass (te)"
    ship = qualified_database.get_by_id("L01_001_ship_raos").metadata["sources"][0]
    loading = ship["loading_condition"]
    assert loading["water_depth"] == [{
        "statement": "Water depth (m)", "sheet": "Summary",
        "cells": ["A41", "B41"], "values": [[30]],
    }]
    assert loading["centre_of_mass"] == [
        {"statement": "Centre of mass (m)", "sheet": "Summary",
         "cells": ["A101", "B101", "C101", "D101"], "values": [[2.53, 0, -1.974]]},
        {"statement": "Centre of mass (m)", "sheet": "Hydrostatics",
         "cells": ["A14", "B14", "C14", "D14"], "values": [[2.53, 0, -1.474]]},
    ]
    box = qualified_database.get_by_id("test01_unit_box").metadata["sources"][1]
    assert box["loading_condition"]["radii_of_gyration"][0]["values"] == [
        [1, 0, 0], [0, 1, 0], [0, 0, 1],
    ]


def test_descriptive_evidence_does_not_change_dataset_identity(qualified_database):
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import store_fixture_entry

    original = deepcopy(qualified_database.get_by_id("L01_001_ship_raos"))
    alias = deepcopy(original)
    alias.variation_id = "same_data_other_evidence"
    alias.metadata["basis"]["unit_evidence"] = "other descriptive evidence"
    source = alias.metadata["sources"][0]
    source["path"] = "tests/fixtures/solver/other_export.xlsx"
    source["conventions"]["phase_convention"] = "other recorded statement"
    source["source"]["date"] = MISSING
    source["loading_condition"]["draft"] = "other recorded statement"
    db = RAODatabase()
    store_fixture_entry(db, original)
    store_fixture_entry(db, alias)
    assert len(list(db.query({}))) == 1
    retained = db.get_by_id(original.variation_id)
    assert len(retained.metadata["sources"]) == 2
    assert retained.metadata["conventions"][source["path"]] == source["conventions"]
    distinct = deepcopy(alias)
    distinct.variation_id = "same_data_different_stored_unit"
    distinct.metadata["basis"]["rotations"] = "deg/m"
    store_fixture_entry(db, distinct)
    assert len(list(db.query({}))) == 2


@pytest.fixture(scope="module")
def repository_pair(tmp_path_factory):
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import regenerate_fixture_database

    scratch = tmp_path_factory.mktemp("repository_pair")
    fresh_path = scratch / "rao_database.parquet"
    fresh_manifest_path = scratch / "rao_database.manifest.json"
    committed_path = ROOT / "data/rao_database.parquet"
    manifest = json.loads((ROOT / "data/rao_database.manifest.json").read_text())
    observed = manifest["entries"][0]["sources"][0]["retrieved_on"]
    fresh = regenerate_fixture_database(ROOT, fresh_path, fresh_manifest_path, observed)
    committed = RAODatabase()
    committed.load_from_disk(committed_path)
    return committed, fresh, committed_path, manifest, json.loads(fresh_manifest_path.read_text())


def test_committed_database_matches_fixtures_and_manifest(repository_pair):
    committed, fresh, path, manifest, fresh_manifest = repository_pair
    names = {"test01_unit_box", "ellipsoid", "L01_001_ship_raos", "L02_OC4_semi_sub"}
    assert {e.variation_id for e in committed.query({})} == names
    assert {e.variation_id for e in fresh.query({})} == names
    assert hashlib.sha256(path.read_bytes()).hexdigest() == manifest["database_sha256"]
    assert path.stat().st_size == manifest["database_bytes"]
    for entry in committed.query({}):
        generated = fresh.get_by_id(entry.variation_id)
        for field in ("frequencies", "directions", "amplitudes", "phases"):
            np.testing.assert_array_equal(getattr(entry.rao_data, field),
                                          getattr(generated.rao_data, field))
        for field in ("units", "conventions", "source", "loading_condition", "basis"):
            assert entry.metadata[field] == generated.metadata[field]
        old_sources, new_sources = deepcopy(entry.metadata["sources"]), deepcopy(generated.metadata["sources"])
        for source in old_sources + new_sources:
            assert source["url"] == f"../../{source['revision']}/{source['path']}"
            source.pop("revision")
            source.pop("url")  # This locator contains the separately checked revision.
        assert old_sources == new_sources
        saved = next(e for e in manifest["entries"] if e["variation_id"] == entry.variation_id)
        assert saved["sources"] == entry.metadata["sources"]
        rebuilt = next(e for e in fresh_manifest["entries"] if e["variation_id"] == entry.variation_id)
        for field in ("units", "conventions", "source", "loading_condition", "basis"):
            assert saved[field] == rebuilt[field] == entry.metadata[field]
    assert sum(e.rao_data.amplitudes.size for e in committed.query({})) == 3516


def test_committed_source_revisions_match_history(repository_pair):
    committed, fresh, _, _, _ = repository_pair
    env = {k: v for k, v in os.environ.items()
           if k not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    shallow = subprocess.run(["git", "rev-parse", "--is-shallow-repository"],
                             cwd=ROOT, env=env, check=True, capture_output=True, text=True)
    if shallow.stdout.strip() == "true":
        pytest.skip("Only source revision equality is skipped: shallow history cannot resolve fixture last commits")
    for entry in committed.query({}):
        sources = fresh.get_by_id(entry.variation_id).metadata["sources"]
        assert [s["revision"] for s in entry.metadata["sources"]] == [s["revision"] for s in sources]


def test_metadata_refresh_refuses_divergent_existing_arrays(tmp_path, qualified_database):
    from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import regenerate_fixture_database

    existing = deepcopy(qualified_database)
    existing.get_by_id("ellipsoid").rao_data.amplitudes.flat[0] += 1
    output, manifest = tmp_path / "rao.parquet", tmp_path / "manifest.json"
    existing.save_to_disk(output)
    original = output.read_bytes()
    with pytest.raises(ValueError, match="occupied"):
        regenerate_fixture_database(ROOT, output, manifest, "2026-10-11")
    assert output.read_bytes() == original
    assert not manifest.exists()
