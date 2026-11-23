"""Tests for WRK-110: expanded hull library with FST and LNGC forms.

Validates the four new hull profiles (fst_barge_250m, fst_ship_330m,
lngc_qflex_315m, lngc_qmax_345m), their generated GDF meshes, mesh
dimensions, and catalog entries.
"""

from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullType,
)
from digitalmodel.hydrodynamics.bemrosetta.mesh.gdf_handler import GDFHandler
from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
    HullMeshGenerator,
    MeshGeneratorConfig,
)

# ---------------------------------------------------------------------------
# Path helpers
# ---------------------------------------------------------------------------

PROJECT_ROOT = Path(__file__).resolve().parents[3]
PROFILES_DIR = PROJECT_ROOT / "data" / "hull_library" / "profiles"
PANELS_DIR = PROJECT_ROOT / "data" / "hull_library" / "panels"
CATALOG_PATH = (
    PROJECT_ROOT / "data" / "hull_library" / "catalog" / "hull_panel_catalog.yaml"
)

# ---------------------------------------------------------------------------
# Test data: expected properties for each new hull
# ---------------------------------------------------------------------------

NEW_HULLS = {
    "fst_barge_250m": {
        "profile_path": PROFILES_DIR / "fst_barge_250m.yaml",
        "gdf_path": PANELS_DIR / "fpso" / "fst_barge_250m.gdf",
        "length_bp": 250.0,
        "beam": 46.0,
        "draft": 14.0,
        "hull_type": HullType.FPSO,
        "catalog_hull_id": "canonical_fst_barge_250m",
        "catalog_hull_type": "fpso",
    },
    "fst_ship_330m": {
        "profile_path": PROFILES_DIR / "fst_ship_330m.yaml",
        "gdf_path": PANELS_DIR / "fpso" / "fst_ship_330m.gdf",
        "length_bp": 330.0,
        "beam": 60.0,
        "draft": 22.0,
        "hull_type": HullType.FPSO,
        "catalog_hull_id": "canonical_fst_ship_330m",
        "catalog_hull_type": "fpso",
    },
    "lngc_qflex_315m": {
        "profile_path": PROFILES_DIR / "lngc_qflex_315m.yaml",
        "gdf_path": PANELS_DIR / "lngc" / "lngc_qflex_315m.gdf",
        "length_bp": 315.0,
        "beam": 50.0,
        "draft": 12.0,
        "hull_type": HullType.LNGC,
        "catalog_hull_id": "canonical_lngc_qflex_315m",
        "catalog_hull_type": "lngc",
    },
    "lngc_qmax_345m": {
        "profile_path": PROFILES_DIR / "lngc_qmax_345m.yaml",
        "gdf_path": PANELS_DIR / "lngc" / "lngc_qmax_345m.gdf",
        "length_bp": 345.0,
        "beam": 55.0,
        "draft": 12.0,
        "hull_type": HullType.LNGC,
        "catalog_hull_id": "canonical_lngc_qmax_345m",
        "catalog_hull_type": "lngc",
    },
}

HULL_NAMES = list(NEW_HULLS.keys())


# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def catalog_data():
    """Load the hull panel catalog YAML once for the module."""
    with open(CATALOG_PATH) as f:
        data = yaml.safe_load(f)
    return data


@pytest.fixture(scope="module")
def catalog_entries_by_id(catalog_data):
    """Index catalog entries by hull_id for fast lookup."""
    return {entry["hull_id"]: entry for entry in catalog_data["entries"]}


@pytest.fixture(scope="module")
def gdf_handler():
    """Shared GDFHandler instance."""
    return GDFHandler()


# ---------------------------------------------------------------------------
# 1. Profile Loading (4 tests)
# ---------------------------------------------------------------------------


class TestProfileLoading:
    """Verify each new YAML profile loads into a valid HullProfile."""

    @pytest.mark.parametrize("hull_name", HULL_NAMES)
    def test_profile_loads_with_correct_dimensions(self, hull_name):
        """Load profile YAML and verify principal dimensions and hull_type."""
        spec = NEW_HULLS[hull_name]
        profile = HullProfile.load_yaml(spec["profile_path"])

        assert profile.name == hull_name
        assert profile.hull_type == spec["hull_type"]
        assert profile.length_bp == spec["length_bp"]
        assert profile.beam == spec["beam"]
        assert profile.draft == spec["draft"]
        assert len(profile.stations) >= 2


# ---------------------------------------------------------------------------
# 2. Mesh File Existence (4 tests)
# ---------------------------------------------------------------------------


class TestMeshFiles:
    """Verify each generated GDF file exists and has non-zero size."""

    @pytest.mark.parametrize("hull_name", HULL_NAMES)
    def test_gdf_file_exists_and_nonempty(self, hull_name):
        """GDF file should exist on disk with non-zero byte size."""
        gdf_path = NEW_HULLS[hull_name]["gdf_path"]
        assert gdf_path.exists(), f"GDF file missing: {gdf_path}"
        assert gdf_path.stat().st_size > 0, f"GDF file is empty: {gdf_path}"


# ---------------------------------------------------------------------------
# 3. GDF Readability (4 tests)
# ---------------------------------------------------------------------------


class TestGDFReadability:
    """Verify GDFHandler can parse each mesh with panels and vertices."""

    @pytest.mark.parametrize("hull_name", HULL_NAMES)
    def test_gdf_reads_with_panels_and_vertices(self, hull_name, gdf_handler):
        """GDFHandler.read should produce a PanelMesh with n_panels > 0
        and n_vertices > 0."""
        gdf_path = NEW_HULLS[hull_name]["gdf_path"]
        mesh = gdf_handler.read(gdf_path)
        assert mesh.n_panels > 0, f"No panels parsed from {gdf_path.name}"
        assert mesh.n_vertices > 0, f"No vertices parsed from {gdf_path.name}"


# ---------------------------------------------------------------------------
# 4. Mesh Dimension Validation (4 tests)
# ---------------------------------------------------------------------------


class TestMeshDimensions:
    """Verify bounding box extents match expected hull dimensions within 5%."""

    @pytest.mark.parametrize("hull_name", HULL_NAMES)
    def test_bounding_box_matches_dimensions(self, hull_name, gdf_handler):
        """Mesh bounding box length, half-beam, and draft should match the
        profile dimensions within 5% relative tolerance.

        The mesh uses marine z-convention (z=0 at waterline, z<0 below)
        and is Y-symmetric (starboard only), so max_y ~ beam/2.
        """
        spec = NEW_HULLS[hull_name]
        mesh = gdf_handler.read(spec["gdf_path"])

        bb_min, bb_max = mesh.bounding_box

        mesh_length = bb_max[0] - bb_min[0]
        mesh_half_beam = bb_max[1]  # starboard only, min_y ~ 0
        mesh_draft = abs(bb_min[2])  # z<0 below waterline

        expected_length = spec["length_bp"]
        expected_half_beam = spec["beam"] / 2.0
        expected_draft = spec["draft"]

        assert mesh_length == pytest.approx(expected_length, rel=0.05), (
            f"Length mismatch: {mesh_length:.1f} vs {expected_length:.1f}"
        )
        assert mesh_half_beam == pytest.approx(expected_half_beam, rel=0.05), (
            f"Half-beam mismatch: {mesh_half_beam:.1f} vs {expected_half_beam:.1f}"
        )
        assert mesh_draft == pytest.approx(expected_draft, rel=0.05), (
            f"Draft mismatch: {mesh_draft:.1f} vs {expected_draft:.1f}"
        )


# ---------------------------------------------------------------------------
# 5. Panel Count Range (4 tests)
# ---------------------------------------------------------------------------


class TestPanelCountRange:
    """Verify each mesh has a reasonable panel count (1500-3500)."""

    @pytest.mark.parametrize("hull_name", HULL_NAMES)
    def test_panel_count_in_range(self, hull_name, gdf_handler):
        """Each new hull mesh should have between 1500 and 3500 panels."""
        gdf_path = NEW_HULLS[hull_name]["gdf_path"]
        mesh = gdf_handler.read(gdf_path)
        assert 1500 <= mesh.n_panels <= 3500, (
            f"{hull_name}: panel count {mesh.n_panels} outside [1500, 3500]"
        )


# ---------------------------------------------------------------------------
# 6. Catalog Entries (4 tests)
# ---------------------------------------------------------------------------


class TestCatalogEntries:
    """Verify each new hull has a correct entry in hull_panel_catalog.yaml."""

    @pytest.mark.parametrize("hull_name", HULL_NAMES)
    def test_catalog_entry_exists_with_correct_fields(
        self, hull_name, catalog_entries_by_id
    ):
        """Catalog should contain an entry with correct hull_type, length_m,
        beam_m, draft_m, and non-null panel_count."""
        spec = NEW_HULLS[hull_name]
        hull_id = spec["catalog_hull_id"]

        assert hull_id in catalog_entries_by_id, (
            f"Catalog missing entry for {hull_id}"
        )

        entry = catalog_entries_by_id[hull_id]
        assert entry["hull_type"] == spec["catalog_hull_type"]
        assert entry["length_m"] == spec["length_bp"]
        assert entry["beam_m"] == spec["beam"]
        assert entry["draft_m"] == spec["draft"]
        assert entry.get("panel_count") is not None and entry["panel_count"] > 0


# ---------------------------------------------------------------------------
# 7. Catalog Completeness (1 test)
# ---------------------------------------------------------------------------


class TestCatalogCompleteness:
    """Verify the catalog has at least 27 entries (23 original + 4 new)."""

    def test_total_catalog_entries_at_least_27(self, catalog_data):
        """Total catalog entries should be >= 27."""
        total = len(catalog_data["entries"])
        assert total >= 27, (
            f"Expected >= 27 catalog entries, got {total}"
        )


# ---------------------------------------------------------------------------
# 8. Mesh Generation Round-Trip (1 test)
# ---------------------------------------------------------------------------


class TestMeshGenerationRoundTrip:
    """Load a profile, generate a mesh, and verify it produces valid output."""

    def test_generate_mesh_from_fst_barge_profile(self):
        """Load fst_barge_250m profile, generate mesh with target_panels=500,
        verify it produces a PanelMesh with n_panels > 0 and correct name."""
        profile_path = NEW_HULLS["fst_barge_250m"]["profile_path"]
        profile = HullProfile.load_yaml(profile_path)

        generator = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=500)
        mesh = generator.generate(profile, config)

        assert mesh.n_panels > 0, "Generated mesh has no panels"
        assert mesh.n_vertices > 0, "Generated mesh has no vertices"
        assert mesh.name == "fst_barge_250m_mesh", (
            f"Expected mesh name 'fst_barge_250m_mesh', got '{mesh.name}'"
        )
