"""Tests for panel catalog data models.

ABOUTME: Unit tests for PanelFormat, PanelCatalogEntry, PanelCatalog — the
data models for the hull panel inventory and catalog system.
"""

import csv
import io
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.hull_library.panel_catalog import (
    PanelCatalog,
    PanelCatalogEntry,
    PanelFormat,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import HullType


# ---------------------------------------------------------------------------
# PanelFormat enum
# ---------------------------------------------------------------------------


class TestPanelFormat:
    def test_gdf_value(self):
        assert PanelFormat.GDF.value == "gdf"

    def test_aqwa_dat_value(self):
        assert PanelFormat.AQWA_DAT.value == "aqwa_dat"

    def test_owd_value(self):
        assert PanelFormat.OWD.value == "owd"

    def test_obj_value(self):
        assert PanelFormat.OBJ.value == "obj"

    def test_rhino_3dm_value(self):
        assert PanelFormat.RHINO_3DM.value == "rhino_3dm"

    def test_orcaflex_yaml_value(self):
        assert PanelFormat.ORCAFLEX_YAML.value == "orcaflex_yaml"

    def test_yaml_profile_value(self):
        assert PanelFormat.YAML_PROFILE.value == "yaml_profile"

    def test_all_formats_are_strings(self):
        for fmt in PanelFormat:
            assert isinstance(fmt.value, str)


# ---------------------------------------------------------------------------
# PanelCatalogEntry
# ---------------------------------------------------------------------------


def _make_entry(**overrides) -> PanelCatalogEntry:
    """Helper to create a valid entry with sensible defaults."""
    defaults = {
        "hull_id": "test_barge_001",
        "hull_type": HullType.BARGE,
        "name": "Test Barge",
        "source": "test-configs",
        "panel_format": PanelFormat.GDF,
        "file_path": "specs/modules/orcawave/test-configs/geometry/barge.gdf",
    }
    defaults.update(overrides)
    return PanelCatalogEntry(**defaults)


class TestPanelCatalogEntry:
    def test_minimal_entry_valid(self):
        entry = _make_entry()
        assert entry.hull_id == "test_barge_001"
        assert entry.hull_type == HullType.BARGE
        assert entry.panel_count is None
        assert entry.tags == []

    def test_full_entry_valid(self):
        entry = _make_entry(
            panel_count=500,
            vertex_count=502,
            symmetry="y",
            length_m=100.0,
            beam_m=20.0,
            draft_m=10.0,
            displacement_t=20500.0,
            description="Standard test barge",
            loading_condition="ballast",
            tags=["validation", "benchmark"],
        )
        assert entry.panel_count == 500
        assert entry.length_m == 100.0
        assert entry.tags == ["validation", "benchmark"]

    def test_hull_id_required(self):
        with pytest.raises(Exception):
            PanelCatalogEntry(
                hull_type=HullType.BARGE,
                name="No ID",
                source="test",
                panel_format=PanelFormat.GDF,
                file_path="foo.gdf",
            )

    def test_to_dict_round_trip(self):
        entry = _make_entry(panel_count=100, length_m=50.0)
        d = entry.model_dump()
        restored = PanelCatalogEntry(**d)
        assert restored.hull_id == entry.hull_id
        assert restored.panel_count == entry.panel_count

    def test_new_hull_types(self):
        """Verify the new hull types work in entries."""
        for ht in [
            HullType.SPAR,
            HullType.CYLINDER,
            HullType.SPHERE,
            HullType.ELLIPSOID,
            HullType.FPSO,
            HullType.LNGC,
        ]:
            entry = _make_entry(hull_id=f"test_{ht.value}", hull_type=ht)
            assert entry.hull_type == ht


# ---------------------------------------------------------------------------
# PanelCatalog
# ---------------------------------------------------------------------------


def _make_catalog(n_entries: int = 3) -> PanelCatalog:
    """Helper to create a catalog with n distinct entries."""
    entries = []
    types = [HullType.BARGE, HullType.SPAR, HullType.SHIP]
    for i in range(n_entries):
        entries.append(
            _make_entry(
                hull_id=f"hull_{i:03d}",
                hull_type=types[i % len(types)],
                name=f"Hull {i}",
            )
        )
    return PanelCatalog(entries=entries)


class TestPanelCatalog:
    def test_creation(self):
        cat = _make_catalog()
        assert cat.version == "1.0"
        assert len(cat.entries) == 3

    def test_hull_ids_unique(self):
        cat = _make_catalog()
        ids = [e.hull_id for e in cat.entries]
        assert len(ids) == len(set(ids))

    def test_duplicate_hull_id_raises(self):
        with pytest.raises(ValueError, match="Duplicate hull_id"):
            PanelCatalog(
                entries=[
                    _make_entry(hull_id="dup"),
                    _make_entry(hull_id="dup"),
                ]
            )

    def test_yaml_round_trip(self, tmp_path: Path):
        cat = _make_catalog(5)
        yaml_path = tmp_path / "catalog.yaml"
        cat.to_yaml(yaml_path)

        loaded = PanelCatalog.from_yaml(yaml_path)
        assert len(loaded.entries) == 5
        assert loaded.entries[0].hull_id == cat.entries[0].hull_id
        assert loaded.version == cat.version

    def test_csv_export(self, tmp_path: Path):
        cat = _make_catalog(3)
        csv_path = tmp_path / "catalog.csv"
        cat.to_csv(csv_path)

        with open(csv_path) as f:
            reader = csv.DictReader(f)
            rows = list(reader)
        assert len(rows) == 3
        assert "hull_id" in rows[0]
        assert "hull_type" in rows[0]
        assert "panel_count" in rows[0]

    def test_yaml_file_is_valid_yaml(self, tmp_path: Path):
        cat = _make_catalog(2)
        yaml_path = tmp_path / "catalog.yaml"
        cat.to_yaml(yaml_path)

        with open(yaml_path) as f:
            data = yaml.safe_load(f)
        assert "version" in data
        assert "entries" in data
        assert len(data["entries"]) == 2

    def test_empty_catalog(self):
        cat = PanelCatalog(entries=[])
        assert len(cat.entries) == 0

    def test_filter_by_type(self):
        cat = _make_catalog(6)
        barges = [e for e in cat.entries if e.hull_type == HullType.BARGE]
        assert len(barges) == 2  # indices 0, 3

    def test_csv_handles_none_values(self, tmp_path: Path):
        cat = PanelCatalog(
            entries=[_make_entry(panel_count=None, length_m=None)]
        )
        csv_path = tmp_path / "catalog.csv"
        cat.to_csv(csv_path)

        with open(csv_path) as f:
            reader = csv.DictReader(f)
            rows = list(reader)
        assert rows[0]["panel_count"] == ""
