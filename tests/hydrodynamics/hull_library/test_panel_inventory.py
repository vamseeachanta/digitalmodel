"""Panel inventory scanner tests.

ABOUTME: Tests for panel_inventory.py — the scanner functions that discover hull
panel meshes across GDF directories, AQWA .dat files, OrcaFlex vessel YAMLs,
and metadata-only hull directories, then assembles them into a unified
PanelCatalog. Uses real GDF files from specs/modules/orcawave/test-configs/
where available and tmp_path fixtures for synthetic test data.
"""

from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.hull_library.panel_catalog import (
    PanelCatalog,
    PanelCatalogEntry,
    PanelFormat,
)
from digitalmodel.hydrodynamics.hull_library.panel_inventory import (
    build_full_catalog,
    scan_aqwa_dat_directory,
    scan_gdf_directory,
    scan_metadata_hulls,
    scan_orcaflex_vessels,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import HullType

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

GDF_DIR = Path(
    "/mnt/local-analysis/workspace-hub/digitalmodel"
    "/specs/modules/orcawave/test-configs/geometry"
)


@pytest.fixture
def gdf_dir() -> Path:
    """Path to the real GDF test-configs directory."""
    return GDF_DIR


@pytest.fixture
def orcaflex_vessel_dir(tmp_path: Path) -> Path:
    """Create a tmp directory with a minimal OrcaFlex vessel YAML."""
    vessel_yaml = tmp_path / "test_fpso.yml"
    vessel_yaml.write_text(
        "ObjectType: Vessel Type\n"
        "Name: TestFPSO\n"
        "Connection: Fixed\n"
        "Length: 200.0\n"
    )
    return tmp_path


@pytest.fixture
def metadata_hull_dir(tmp_path: Path) -> Path:
    """Create a tmp directory with fake metadata hull files."""
    (tmp_path / "hull_A.3dm").write_bytes(b"\x00" * 64)
    (tmp_path / "hull_B.xlsx").write_bytes(b"\x00" * 64)
    return tmp_path


# ---------------------------------------------------------------------------
# GDF scanner tests
# ---------------------------------------------------------------------------


class TestScanGdfDirectory:
    """Tests for scan_gdf_directory."""

    def test_scan_gdf_directory_with_real_file(self, gdf_dir: Path) -> None:
        """A real GDF file should yield entries with panel and vertex counts."""
        entries = scan_gdf_directory(gdf_dir, source_id="test-gdf")
        barge_entries = [e for e in entries if "barge" in e.file_path.lower()]
        assert len(barge_entries) >= 1
        entry = barge_entries[0]
        assert entry.panel_count is not None and entry.panel_count > 0
        assert entry.vertex_count is not None and entry.vertex_count > 0

    def test_scan_gdf_directory_multiple_files(self, gdf_dir: Path) -> None:
        """All five GDF files in the test directory should be discovered."""
        entries = scan_gdf_directory(gdf_dir, source_id="test-gdf")
        assert len(entries) == 5

    def test_scan_gdf_directory_missing_dir(self) -> None:
        """A non-existent directory should return an empty list, not raise."""
        missing = Path("/tmp/does_not_exist_panel_inv_test")
        entries = scan_gdf_directory(missing, source_id="missing")
        assert entries == []

    def test_scan_gdf_entry_has_dimensions(self, gdf_dir: Path) -> None:
        """GDF entries should carry bounding-box dimensions."""
        entries = scan_gdf_directory(gdf_dir, source_id="test-gdf")
        barge_entries = [e for e in entries if "barge" in e.file_path.lower()]
        assert len(barge_entries) >= 1
        entry = barge_entries[0]
        assert entry.length_m is not None and entry.length_m > 0
        assert entry.beam_m is not None and entry.beam_m > 0
        assert entry.draft_m is not None and entry.draft_m > 0

    def test_gdf_entry_format_is_gdf(self, gdf_dir: Path) -> None:
        """Every entry from the GDF scanner must have panel_format GDF."""
        entries = scan_gdf_directory(gdf_dir, source_id="test-gdf")
        assert len(entries) > 0
        for entry in entries:
            assert entry.panel_format == PanelFormat.GDF

    def test_gdf_entries_have_source_id(self, gdf_dir: Path) -> None:
        """All GDF entries should carry the provided source_id."""
        entries = scan_gdf_directory(gdf_dir, source_id="my-source")
        for entry in entries:
            assert entry.source == "my-source"


# ---------------------------------------------------------------------------
# Metadata hull scanner tests
# ---------------------------------------------------------------------------


class TestScanMetadataHulls:
    """Tests for scan_metadata_hulls."""

    def test_scan_metadata_hulls(self, metadata_hull_dir: Path) -> None:
        """Fake .3dm and .xlsx files should produce entries with no panel count."""
        entries = scan_metadata_hulls(metadata_hull_dir, source_id="meta")
        assert len(entries) == 2
        for entry in entries:
            assert entry.panel_count is None

    def test_scan_metadata_hulls_empty_dir(self, tmp_path: Path) -> None:
        """An empty directory should return an empty list."""
        entries = scan_metadata_hulls(tmp_path, source_id="empty")
        assert entries == []


# ---------------------------------------------------------------------------
# OrcaFlex vessel scanner tests
# ---------------------------------------------------------------------------


class TestScanOrcaflexVessels:
    """Tests for scan_orcaflex_vessels."""

    def test_scan_orcaflex_vessels(self, orcaflex_vessel_dir: Path) -> None:
        """A minimal OrcaFlex vessel YAML should produce an entry."""
        entries = scan_orcaflex_vessels(
            orcaflex_vessel_dir, source_id="ofx"
        )
        assert len(entries) == 1
        entry = entries[0]
        assert entry.panel_format == PanelFormat.ORCAFLEX_YAML
        assert entry.length_m == pytest.approx(200.0)

    def test_scan_orcaflex_vessels_missing_dir(self) -> None:
        """A non-existent directory should return an empty list."""
        missing = Path("/tmp/does_not_exist_orcaflex_inv_test")
        entries = scan_orcaflex_vessels(missing, source_id="missing")
        assert entries == []


# ---------------------------------------------------------------------------
# AQWA .dat scanner tests
# ---------------------------------------------------------------------------


class TestScanAqwaDatDirectory:
    """Tests for scan_aqwa_dat_directory."""

    def test_scan_aqwa_dat_directory_missing_dir(self) -> None:
        """A non-existent directory should return an empty list."""
        missing = Path("/tmp/does_not_exist_aqwa_inv_test")
        entries = scan_aqwa_dat_directory(missing, source_id="missing")
        assert entries == []


# ---------------------------------------------------------------------------
# Full catalog builder tests
# ---------------------------------------------------------------------------


class TestBuildFullCatalog:
    """Tests for build_full_catalog."""

    def test_build_full_catalog(
        self, gdf_dir: Path, tmp_path: Path
    ) -> None:
        """Catalog built from real GDF dir and empty tmp dirs should work."""
        source_config = {
            "gdf_dirs": [{"path": str(gdf_dir), "source_id": "gdf-test"}],
            "aqwa_dirs": [],
            "orcaflex_dirs": [],
            "metadata_dirs": [
                {"path": str(tmp_path), "source_id": "meta-empty"}
            ],
        }
        catalog = build_full_catalog(source_config)
        assert isinstance(catalog, PanelCatalog)
        assert len(catalog.entries) >= 5  # at least the 5 GDF files

        # Hull IDs must be unique (PanelCatalog validator enforces this,
        # but verify explicitly)
        hull_ids = [e.hull_id for e in catalog.entries]
        assert len(hull_ids) == len(set(hull_ids))

    def test_hull_ids_unique_across_sources(
        self, gdf_dir: Path, orcaflex_vessel_dir: Path
    ) -> None:
        """Hull IDs from different scanners must not collide."""
        gdf_entries = scan_gdf_directory(gdf_dir, source_id="gdf")
        ofx_entries = scan_orcaflex_vessels(
            orcaflex_vessel_dir, source_id="ofx"
        )
        all_ids = [e.hull_id for e in gdf_entries + ofx_entries]
        assert len(all_ids) == len(set(all_ids)), (
            f"Duplicate hull_ids across sources: {all_ids}"
        )

    def test_build_full_catalog_empty_config(self) -> None:
        """An empty source config should produce an empty catalog."""
        source_config = {
            "gdf_dirs": [],
            "aqwa_dirs": [],
            "orcaflex_dirs": [],
            "metadata_dirs": [],
        }
        catalog = build_full_catalog(source_config)
        assert isinstance(catalog, PanelCatalog)
        assert len(catalog.entries) == 0
