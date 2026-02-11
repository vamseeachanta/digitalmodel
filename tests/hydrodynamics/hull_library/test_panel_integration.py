"""Integration tests for hull panel catalog system.

ABOUTME: End-to-end tests verifying the generated catalog YAML loads correctly,
all GDF files in panels/ are parseable, hull_ids are unique, and major hull
types are represented.
"""

from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.hull_library.panel_catalog import (
    PanelCatalog,
    PanelCatalogEntry,
    PanelFormat,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import HullType

# Path to the generated catalog and panel files
REPO_ROOT = Path(__file__).resolve().parents[3]
CATALOG_YAML = REPO_ROOT / "data" / "hull_library" / "catalog" / "hull_panel_catalog.yaml"
CATALOG_CSV = REPO_ROOT / "data" / "hull_library" / "catalog" / "hull_panel_catalog.csv"
PANELS_DIR = REPO_ROOT / "data" / "hull_library" / "panels"


@pytest.fixture
def catalog() -> PanelCatalog:
    """Load the generated catalog."""
    assert CATALOG_YAML.exists(), f"Catalog YAML not found: {CATALOG_YAML}"
    return PanelCatalog.from_yaml(CATALOG_YAML)


class TestCatalogYaml:
    def test_catalog_yaml_loads(self, catalog: PanelCatalog) -> None:
        """Generated catalog is valid YAML and loads into PanelCatalog."""
        assert isinstance(catalog, PanelCatalog)
        assert len(catalog.entries) > 0

    def test_catalog_yaml_is_valid_yaml(self) -> None:
        """Raw YAML file parses without errors."""
        with open(CATALOG_YAML) as f:
            data = yaml.safe_load(f)
        assert "version" in data
        assert "entries" in data
        assert isinstance(data["entries"], list)

    def test_catalog_has_version(self, catalog: PanelCatalog) -> None:
        assert catalog.version == "1.0"


class TestGdfEntries:
    def test_all_gdf_entries_have_panel_count(
        self, catalog: PanelCatalog
    ) -> None:
        """Every GDF entry has non-null panel_count."""
        gdf_entries = [
            e for e in catalog.entries
            if e.panel_format == PanelFormat.GDF
        ]
        assert len(gdf_entries) > 0
        for entry in gdf_entries:
            assert entry.panel_count is not None, (
                f"{entry.hull_id} has null panel_count"
            )
            assert entry.panel_count > 0, (
                f"{entry.hull_id} has panel_count={entry.panel_count}"
            )

    def test_all_gdf_entries_have_vertex_count(
        self, catalog: PanelCatalog
    ) -> None:
        """Every GDF entry has non-null vertex_count."""
        gdf_entries = [
            e for e in catalog.entries
            if e.panel_format == PanelFormat.GDF
        ]
        for entry in gdf_entries:
            assert entry.vertex_count is not None
            assert entry.vertex_count > 0


class TestCopiedFiles:
    def test_copied_gdf_files_exist(self, catalog: PanelCatalog) -> None:
        """Every entry pointing to panels/ has a real file."""
        panels_str = str(PANELS_DIR)
        for entry in catalog.entries:
            if panels_str in entry.file_path or "data/hull_library/panels" in entry.file_path:
                # Resolve both absolute and relative paths
                p = Path(entry.file_path)
                if not p.is_absolute():
                    p = REPO_ROOT / p
                assert p.exists(), f"File missing for {entry.hull_id}: {p}"

    def test_gdf_files_parseable(self) -> None:
        """Every GDF in panels/ parses via GDFHandler."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh.gdf_handler import (
            GDFHandler,
        )

        handler = GDFHandler()
        gdf_files = list(PANELS_DIR.rglob("*.gdf"))
        assert len(gdf_files) > 0, "No GDF files found in panels/"

        for gdf_path in gdf_files:
            mesh = handler.read(gdf_path)
            assert mesh.n_panels > 0, f"{gdf_path.name}: no panels"
            assert mesh.n_vertices > 0, f"{gdf_path.name}: no vertices"


class TestCatalogQuality:
    def test_catalog_hull_ids_unique(self, catalog: PanelCatalog) -> None:
        """No duplicate hull_ids."""
        ids = [e.hull_id for e in catalog.entries]
        assert len(ids) == len(set(ids)), (
            f"Duplicate hull_ids: {[x for x in ids if ids.count(x) > 1]}"
        )

    def test_at_least_one_hull_per_major_type(
        self, catalog: PanelCatalog
    ) -> None:
        """Catalog covers barge, spar, semi_pontoon, ship."""
        types_present = {e.hull_type for e in catalog.entries}
        for required in [
            HullType.BARGE,
            HullType.SPAR,
            HullType.SEMI_PONTOON,
            HullType.SHIP,
        ]:
            assert required in types_present, (
                f"Missing hull type: {required.value}"
            )

    def test_at_least_11_entries(self, catalog: PanelCatalog) -> None:
        """Catalog has at least the 11 copied GDF files."""
        assert len(catalog.entries) >= 11

    def test_no_legal_violations(self, catalog: PanelCatalog) -> None:
        """No deny-listed terms in catalog YAML."""
        with open(CATALOG_YAML) as f:
            content = f.read().lower()

        # These terms must never appear in the catalog
        deny_terms = [
            "yellowtail", "b1522", "ctr-7", "0113", "orc dr",
        ]
        for term in deny_terms:
            assert term not in content, (
                f"Legal violation: '{term}' found in catalog YAML"
            )


class TestCsvOutput:
    def test_csv_exists(self) -> None:
        assert CATALOG_CSV.exists(), f"CSV not found: {CATALOG_CSV}"

    def test_csv_has_rows(self) -> None:
        import csv as csv_mod

        with open(CATALOG_CSV) as f:
            reader = csv_mod.DictReader(f)
            rows = list(reader)
        assert len(rows) >= 11
        assert "hull_id" in rows[0]
