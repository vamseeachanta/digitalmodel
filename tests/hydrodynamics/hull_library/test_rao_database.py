"""
ABOUTME: Tests for RAO database — Phase 3 (WRK-043).

Tests are written FIRST before implementation (TDD).
Covers RAODatabaseEntry, RAODatabase storage, querying, persistence.
"""

from __future__ import annotations

import pytest
import numpy as np
import tempfile
from pathlib import Path


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_rao_data(vessel_name: str = "test_vessel",
                   n_freq: int = 10, n_dir: int = 3):
    """Create synthetic RAOData for testing."""
    from digitalmodel.hydrodynamics.models import RAOData

    frequencies = np.linspace(0.1, 2.0, n_freq)
    directions = np.array([0.0, 90.0, 180.0])[:n_dir]
    amplitudes = np.ones((n_freq, n_dir, 6)) * 0.5
    phases = np.zeros((n_freq, n_dir, 6))
    return RAOData(
        frequencies=frequencies,
        directions=directions,
        amplitudes=amplitudes,
        phases=phases,
        vessel_name=vessel_name,
    )


# ---------------------------------------------------------------------------
# RAODatabaseEntry tests
# ---------------------------------------------------------------------------


class TestRAODatabaseEntry:
    def test_create_entry(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabaseEntry,
        )

        rao = _make_rao_data()
        entry = RAODatabaseEntry(
            variation_id="hull_001",
            hull_params={"length_scale": 1.0, "beam_scale": 1.0},
            rao_data=rao,
        )
        assert entry.variation_id == "hull_001"
        assert entry.hull_params["length_scale"] == 1.0

    def test_entry_has_metadata(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabaseEntry,
        )

        rao = _make_rao_data()
        entry = RAODatabaseEntry(
            variation_id="hull_002",
            hull_params={"draft_scale": 0.8},
            rao_data=rao,
            metadata={"source": "synthetic", "version": "1.0"},
        )
        assert entry.metadata["source"] == "synthetic"

    def test_entry_default_metadata_empty(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabaseEntry,
        )

        rao = _make_rao_data()
        entry = RAODatabaseEntry(
            variation_id="hull_003",
            hull_params={},
            rao_data=rao,
        )
        assert isinstance(entry.metadata, dict)

    def test_entry_stores_rao_data_arrays(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabaseEntry,
        )

        rao = _make_rao_data(n_freq=20)
        entry = RAODatabaseEntry(
            variation_id="hull_004",
            hull_params={"length_scale": 1.5},
            rao_data=rao,
        )
        assert entry.rao_data.amplitudes.shape[0] == 20


# ---------------------------------------------------------------------------
# RAODatabase storage and retrieval tests
# ---------------------------------------------------------------------------


class TestRAODatabaseStoreAndRetrieve:
    def test_store_and_get_by_id(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase, RAODatabaseEntry,
        )

        db = RAODatabase()
        entry = RAODatabaseEntry(
            variation_id="v001",
            hull_params={"length_scale": 1.0},
            rao_data=_make_rao_data(),
        )
        db.store(entry)
        retrieved = db.get_by_id("v001")
        assert retrieved.variation_id == "v001"
        assert retrieved.hull_params["length_scale"] == 1.0

    def test_get_by_id_missing_raises(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase,
        )

        db = RAODatabase()
        with pytest.raises(KeyError):
            db.get_by_id("nonexistent")

    def test_store_multiple_entries(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase, RAODatabaseEntry,
        )

        db = RAODatabase()
        for i in range(5):
            e = RAODatabaseEntry(
                variation_id=f"v{i:03d}",
                hull_params={"length_scale": 1.0 + i * 0.1},
                rao_data=_make_rao_data(),
            )
            db.store(e)
        assert len(list(db.query({}))) == 5

    def test_store_overwrites_same_id(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase, RAODatabaseEntry,
        )

        db = RAODatabase()
        e1 = RAODatabaseEntry(
            variation_id="dup",
            hull_params={"length_scale": 1.0},
            rao_data=_make_rao_data("vessel_a"),
        )
        e2 = RAODatabaseEntry(
            variation_id="dup",
            hull_params={"length_scale": 2.0},
            rao_data=_make_rao_data("vessel_b"),
        )
        db.store(e1)
        db.store(e2)
        retrieved = db.get_by_id("dup")
        assert retrieved.hull_params["length_scale"] == 2.0


# ---------------------------------------------------------------------------
# RAODatabase query tests
# ---------------------------------------------------------------------------


class TestRAODatabaseQuery:
    def _populated_db(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase, RAODatabaseEntry,
        )

        db = RAODatabase()
        params_list = [
            {"length_scale": 1.0, "beam_scale": 0.8},
            {"length_scale": 1.5, "beam_scale": 0.8},
            {"length_scale": 2.0, "beam_scale": 0.8},
            {"length_scale": 1.0, "beam_scale": 1.0},
            {"length_scale": 1.5, "beam_scale": 1.0},
        ]
        for i, params in enumerate(params_list):
            e = RAODatabaseEntry(
                variation_id=f"v{i:03d}",
                hull_params=params,
                rao_data=_make_rao_data(),
            )
            db.store(e)
        return db

    def test_query_empty_filter_returns_all(self):
        db = self._populated_db()
        results = list(db.query({}))
        assert len(results) == 5

    def test_query_exact_match(self):
        db = self._populated_db()
        results = list(db.query({"beam_scale": (0.8, 0.8)}))
        assert len(results) == 3

    def test_query_range_filter(self):
        db = self._populated_db()
        # length_scale between 1.0 and 1.5 inclusive
        results = list(db.query({"length_scale": (1.0, 1.5)}))
        assert len(results) == 4  # entries with 1.0 or 1.5 length_scale

    def test_query_combined_filters(self):
        db = self._populated_db()
        results = list(
            db.query({"length_scale": (1.4, 2.0), "beam_scale": (0.8, 0.8)})
        )
        # length_scale in [1.5, 2.0] AND beam_scale == 0.8 -> 2 entries
        assert len(results) == 2

    def test_query_no_match_returns_empty(self):
        db = self._populated_db()
        results = list(db.query({"length_scale": (9.0, 10.0)}))
        assert len(results) == 0


# ---------------------------------------------------------------------------
# RAODatabase list_parameters tests
# ---------------------------------------------------------------------------


class TestRAODatabaseListParameters:
    def test_list_parameters_returns_unique_values(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase, RAODatabaseEntry,
        )

        db = RAODatabase()
        for ls in [1.0, 1.5, 2.0]:
            e = RAODatabaseEntry(
                variation_id=f"v_{ls}",
                hull_params={"length_scale": ls},
                rao_data=_make_rao_data(),
            )
            db.store(e)

        params = db.list_parameters()
        assert "length_scale" in params
        values = sorted(params["length_scale"])
        assert values == pytest.approx([1.0, 1.5, 2.0])

    def test_list_parameters_multiple_dims(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase, RAODatabaseEntry,
        )

        db = RAODatabase()
        for i, (ls, bs) in enumerate([(1.0, 0.8), (1.5, 1.0), (2.0, 1.2)]):
            e = RAODatabaseEntry(
                variation_id=f"v{i}",
                hull_params={"length_scale": ls, "beam_scale": bs},
                rao_data=_make_rao_data(),
            )
            db.store(e)

        params = db.list_parameters()
        assert set(params.keys()) >= {"length_scale", "beam_scale"}
        assert len(params["length_scale"]) == 3

    def test_list_parameters_empty_db(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase,
        )

        db = RAODatabase()
        params = db.list_parameters()
        assert isinstance(params, dict)
        assert len(params) == 0


# ---------------------------------------------------------------------------
# RAODatabase persistence tests (save/load round-trip)
# ---------------------------------------------------------------------------


class TestRAODatabasePersistence:
    def _make_entry(self, vid: str, params: dict):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabaseEntry,
        )

        return RAODatabaseEntry(
            variation_id=vid,
            hull_params=params,
            rao_data=_make_rao_data(n_freq=15),
        )

    def test_save_creates_file(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase,
        )

        db = RAODatabase()
        db.store(self._make_entry("v001", {"length_scale": 1.0}))

        with tempfile.TemporaryDirectory() as tmpdir:
            save_path = Path(tmpdir) / "rao_db.parquet"
            db.save_to_disk(save_path)
            assert save_path.exists()

    def test_round_trip_variation_ids(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase,
        )

        db = RAODatabase()
        for i in range(3):
            db.store(self._make_entry(f"v{i:03d}", {"length_scale": 1.0 + i}))

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "db.parquet"
            db.save_to_disk(path)

            db2 = RAODatabase()
            db2.load_from_disk(path)

        ids = {e.variation_id for e in db2.query({})}
        assert ids == {"v000", "v001", "v002"}

    def test_round_trip_hull_params(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase,
        )

        db = RAODatabase()
        db.store(self._make_entry("p001", {"length_scale": 1.75,
                                           "beam_scale": 0.9}))

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "db.parquet"
            db.save_to_disk(path)
            db2 = RAODatabase()
            db2.load_from_disk(path)

        e = db2.get_by_id("p001")
        assert e.hull_params["length_scale"] == pytest.approx(1.75)
        assert e.hull_params["beam_scale"] == pytest.approx(0.9)

    def test_round_trip_rao_amplitudes_preserved(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase,
        )

        db = RAODatabase()
        orig_entry = self._make_entry("amp001", {"length_scale": 1.0})
        # Set a recognizable amplitude pattern
        orig_entry.rao_data.amplitudes[:, :, 2] = 0.123456
        db.store(orig_entry)

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "db.parquet"
            db.save_to_disk(path)
            db2 = RAODatabase()
            db2.load_from_disk(path)

        e = db2.get_by_id("amp001")
        assert np.allclose(e.rao_data.amplitudes[:, :, 2], 0.123456,
                           atol=1e-5)

    def test_round_trip_frequencies_preserved(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase,
        )

        db = RAODatabase()
        entry = self._make_entry("freq001", {"draft_scale": 0.5})
        db.store(entry)
        orig_freqs = entry.rao_data.frequencies.copy()

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "db.parquet"
            db.save_to_disk(path)
            db2 = RAODatabase()
            db2.load_from_disk(path)

        e2 = db2.get_by_id("freq001")
        assert np.allclose(e2.rao_data.frequencies, orig_freqs, atol=1e-9)

    def test_save_to_missing_dir_creates_parents(self):
        from digitalmodel.hydrodynamics.hull_library.rao_database import (
            RAODatabase,
        )

        db = RAODatabase()
        db.store(self._make_entry("v001", {"length_scale": 1.0}))

        with tempfile.TemporaryDirectory() as tmpdir:
            nested_path = Path(tmpdir) / "deep" / "nested" / "db.parquet"
            db.save_to_disk(nested_path)
            assert nested_path.exists()
