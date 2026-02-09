"""Tests for hull catalog and analysis chain."""

import pytest
import numpy as np
from pathlib import Path
import tempfile


DATA_DIR = Path(__file__).resolve().parents[3] / "data" / "hull_library" / "profiles"


def _make_simple_rao_data():
    """Create simple synthetic RAO data for testing."""
    from digitalmodel.hydrodynamics.models import RAOData

    frequencies = np.linspace(0.1, 2.0, 20)  # rad/s
    directions = np.array([0.0, 90.0, 180.0])  # degrees
    n_freq, n_dir = len(frequencies), len(directions)
    # Simple constant RAOs for testing
    amplitudes = np.ones((n_freq, n_dir, 6)) * 0.5
    phases = np.zeros((n_freq, n_dir, 6))
    # Heave RAO peaks at low frequency
    amplitudes[:, :, 2] = 1.0  # heave = 1 m/m (unity at all freqs for simplicity)
    return RAOData(
        frequencies=frequencies,
        directions=directions,
        amplitudes=amplitudes,
        phases=phases,
        vessel_name="test",
    )


class TestSeaStateDefinition:
    def test_valid_sea_state(self):
        from digitalmodel.hydrodynamics.hull_library.catalog import (
            SeaStateDefinition,
        )

        ss = SeaStateDefinition(significant_height=3.0, peak_period=10.0)
        assert ss.significant_height == 3.0
        assert ss.spectrum_type == "jonswap"
        assert ss.heading == 0.0

    def test_custom_heading(self):
        from digitalmodel.hydrodynamics.hull_library.catalog import (
            SeaStateDefinition,
        )

        ss = SeaStateDefinition(
            significant_height=2.0, peak_period=8.0, heading=90.0
        )
        assert ss.heading == 90.0


class TestHullCatalogEntry:
    def test_entry_creation(self):
        from digitalmodel.hydrodynamics.hull_library.catalog import (
            HullCatalogEntry,
        )
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )

        profile = HullProfile(
            name="test",
            hull_type=HullType.BARGE,
            stations=[
                HullStation(
                    x_position=0.0,
                    waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
                ),
                HullStation(
                    x_position=50.0,
                    waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
                ),
            ],
            length_bp=50.0,
            beam=10.0,
            draft=4.0,
            depth=6.0,
            source="test",
        )
        entry = HullCatalogEntry(hull_id="test", profile=profile)
        assert entry.hull_id == "test"
        assert entry.variations == []


class TestHullCatalog:
    def test_empty_catalog(self):
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

        cat = HullCatalog()
        assert cat.list_hulls() == []

    def test_register_hull(self):
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )

        cat = HullCatalog()
        profile = HullProfile(
            name="box",
            hull_type=HullType.BARGE,
            stations=[
                HullStation(
                    x_position=0.0,
                    waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
                ),
                HullStation(
                    x_position=50.0,
                    waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
                ),
            ],
            length_bp=50.0,
            beam=10.0,
            draft=4.0,
            depth=6.0,
            source="test",
        )
        entry = cat.register_hull(profile)
        assert "box" in cat.list_hulls()
        assert cat.get_hull("box").hull_id == "box"

    def test_get_nonexistent_hull_raises(self):
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

        cat = HullCatalog()
        with pytest.raises(KeyError):
            cat.get_hull("nonexistent")

    def test_load_from_directory(self):
        """Catalog loads all YAML profiles from a directory."""
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

        if not DATA_DIR.exists():
            pytest.skip("Seed data directory not found")
        cat = HullCatalog(profiles_dir=DATA_DIR)
        hulls = cat.list_hulls()
        assert "unit_box" in hulls
        assert "generic_barge" in hulls
        assert "generic_tanker" in hulls

    def test_generate_mesh(self):
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            MeshGeneratorConfig,
        )

        if not DATA_DIR.exists():
            pytest.skip("Seed data directory not found")
        cat = HullCatalog(profiles_dir=DATA_DIR)
        config = MeshGeneratorConfig(target_panels=100)
        mesh = cat.generate_mesh("unit_box", config)
        assert mesh.n_panels > 0

    def test_compute_motions(self):
        from digitalmodel.hydrodynamics.hull_library.catalog import (
            HullCatalog,
            SeaStateDefinition,
        )

        if not DATA_DIR.exists():
            pytest.skip("Seed data directory not found")
        cat = HullCatalog(profiles_dir=DATA_DIR)
        sea_state = SeaStateDefinition(significant_height=3.0, peak_period=10.0)
        rao_data = _make_simple_rao_data()
        response = cat.compute_motions("unit_box", sea_state, rao_data)
        # Should have significant values for each DOF
        assert "heave_sig" in response.significant_values
        assert response.significant_values["heave_sig"] > 0

    def test_compute_accelerations(self):
        from digitalmodel.hydrodynamics.hull_library.catalog import (
            HullCatalog,
            SeaStateDefinition,
        )

        if not DATA_DIR.exists():
            pytest.skip("Seed data directory not found")
        cat = HullCatalog(profiles_dir=DATA_DIR)
        sea_state = SeaStateDefinition(significant_height=3.0, peak_period=10.0)
        rao_data = _make_simple_rao_data()
        # Point at bow, 20m above keel
        acc = cat.compute_accelerations(
            "unit_box",
            sea_state,
            rao_data,
            point=(100.0, 0.0, 10.0),
            cog=(50.0, 0.0, 5.0),
        )
        assert "vertical" in acc
        assert "lateral" in acc
        assert "longitudinal" in acc
        assert acc["vertical"] > 0

    def test_accelerations_increase_with_distance_from_cog(self):
        """Point further from COG should have higher accelerations."""
        from digitalmodel.hydrodynamics.hull_library.catalog import (
            HullCatalog,
            SeaStateDefinition,
        )

        if not DATA_DIR.exists():
            pytest.skip("Seed data directory not found")
        cat = HullCatalog(profiles_dir=DATA_DIR)
        sea_state = SeaStateDefinition(significant_height=3.0, peak_period=10.0)
        rao_data = _make_simple_rao_data()
        cog = (50.0, 0.0, 5.0)
        acc_near = cat.compute_accelerations(
            "unit_box",
            sea_state,
            rao_data,
            point=(55.0, 0.0, 6.0),
            cog=cog,
        )
        acc_far = cat.compute_accelerations(
            "unit_box",
            sea_state,
            rao_data,
            point=(100.0, 0.0, 20.0),
            cog=cog,
        )
        # Far point should have higher vertical accelerations due to pitch coupling
        assert acc_far["vertical"] >= acc_near["vertical"]
