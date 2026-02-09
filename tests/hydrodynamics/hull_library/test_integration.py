"""Integration tests: hull profile -> mesh -> GDF/DAT export -> reload -> validate.

Verifies that the hull mesh generator produces PanelMesh instances compatible
with the BEMRosetta-backed MeshPipeline, and that meshes survive a full
write-to-disk / read-from-disk round-trip in both GDF (WAMIT) and DAT
(AQWA/NEMOH) formats.
"""

import pytest
import numpy as np
import tempfile
from pathlib import Path


DATA_DIR = Path(__file__).resolve().parents[3] / "data" / "hull_library" / "profiles"


class TestMeshPipelineIntegration:
    """Test that generated hull meshes integrate with the diffraction mesh pipeline."""

    def test_box_mesh_validates(self):
        """Generated box mesh passes MeshPipeline validation."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )
        from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline

        profile = HullProfile.load_yaml(DATA_DIR / "unit_box.yaml")
        gen = HullMeshGenerator()
        mesh = gen.generate(profile, MeshGeneratorConfig(target_panels=100))

        pipeline = MeshPipeline()
        report = pipeline.validate(mesh)
        assert report.is_valid
        assert report.n_panels > 0

    def test_box_mesh_to_gdf_roundtrip(self):
        """Box mesh exports to GDF and reloads with same panel count."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )
        from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline

        profile = HullProfile.load_yaml(DATA_DIR / "unit_box.yaml")
        gen = HullMeshGenerator()
        mesh = gen.generate(profile, MeshGeneratorConfig(target_panels=100))

        pipeline = MeshPipeline()
        with tempfile.TemporaryDirectory() as tmpdir:
            gdf_path = pipeline.convert_by_name(
                mesh, "gdf", Path(tmpdir) / "box.gdf"
            )
            assert gdf_path.exists()
            assert gdf_path.stat().st_size > 0

            loaded = pipeline.load(gdf_path)
            # Panel count must survive the round-trip exactly
            assert loaded.n_panels == mesh.n_panels
            # Vertex count may differ because the GDF reader deduplicates
            # vertices independently.  Instead verify bounding box is close.
            orig_min, orig_max = mesh.bounding_box
            load_min, load_max = loaded.bounding_box
            np.testing.assert_allclose(orig_min, load_min, atol=0.01)
            np.testing.assert_allclose(orig_max, load_max, atol=0.01)

    def test_box_mesh_to_dat_roundtrip(self):
        """Box mesh exports to DAT (AQWA/NEMOH format) and reloads."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )
        from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline

        profile = HullProfile.load_yaml(DATA_DIR / "unit_box.yaml")
        gen = HullMeshGenerator()
        mesh = gen.generate(profile, MeshGeneratorConfig(target_panels=100))

        pipeline = MeshPipeline()
        with tempfile.TemporaryDirectory() as tmpdir:
            dat_path = pipeline.convert_by_name(
                mesh, "dat", Path(tmpdir) / "box.dat"
            )
            assert dat_path.exists()
            assert dat_path.stat().st_size > 0

            loaded = pipeline.load(dat_path)
            assert loaded.n_panels == mesh.n_panels

    def test_tanker_mesh_validates(self):
        """Tanker mesh (complex hull form) passes validation."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )
        from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline

        profile = HullProfile.load_yaml(DATA_DIR / "generic_tanker.yaml")
        gen = HullMeshGenerator()
        mesh = gen.generate(profile, MeshGeneratorConfig(target_panels=200))

        pipeline = MeshPipeline()
        report = pipeline.validate(mesh)
        assert report.is_valid
        assert report.quality_score > 50

    def test_tanker_gdf_roundtrip(self):
        """Tanker mesh survives GDF round-trip."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )
        from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline

        profile = HullProfile.load_yaml(DATA_DIR / "generic_tanker.yaml")
        gen = HullMeshGenerator()
        mesh = gen.generate(profile, MeshGeneratorConfig(target_panels=200))

        pipeline = MeshPipeline()
        with tempfile.TemporaryDirectory() as tmpdir:
            gdf_path = pipeline.convert_by_name(
                mesh, "gdf", Path(tmpdir) / "tanker.gdf"
            )
            loaded = pipeline.load(gdf_path)
            # Panel count should match exactly
            assert loaded.n_panels == mesh.n_panels
            # Bounding boxes should be very close
            orig_min, orig_max = mesh.bounding_box
            load_min, load_max = loaded.bounding_box
            np.testing.assert_allclose(orig_min, load_min, atol=0.01)
            np.testing.assert_allclose(orig_max, load_max, atol=0.01)

    def test_adaptive_mesh_validates(self):
        """Adaptive-density mesh passes validation."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )
        from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline

        profile = HullProfile.load_yaml(DATA_DIR / "generic_tanker.yaml")
        gen = HullMeshGenerator()
        mesh = gen.generate(
            profile,
            MeshGeneratorConfig(target_panels=200, adaptive_density=True),
        )

        pipeline = MeshPipeline()
        report = pipeline.validate(mesh)
        assert report.is_valid

    def test_coarsened_mesh_validates(self):
        """Coarsened mesh passes validation."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
            coarsen_mesh,
        )
        from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline

        profile = HullProfile.load_yaml(DATA_DIR / "unit_box.yaml")
        gen = HullMeshGenerator()
        fine = gen.generate(profile, MeshGeneratorConfig(target_panels=500))
        coarse = coarsen_mesh(fine, target_panels=100)

        pipeline = MeshPipeline()
        report = pipeline.validate(coarse)
        assert report.is_valid
        assert report.n_panels < fine.n_panels


class TestCatalogFullChain:
    """End-to-end: load catalog -> generate mesh -> export -> compute motions."""

    def test_catalog_to_gdf_to_motions(self):
        """Full chain: catalog -> mesh -> GDF -> validate -> compute motions."""
        from digitalmodel.hydrodynamics.hull_library.catalog import (
            HullCatalog,
            SeaStateDefinition,
        )
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            MeshGeneratorConfig,
        )
        from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline
        from digitalmodel.hydrodynamics.models import RAOData

        cat = HullCatalog(profiles_dir=DATA_DIR)

        # Generate mesh
        mesh = cat.generate_mesh(
            "unit_box", MeshGeneratorConfig(target_panels=100)
        )

        # Validate through pipeline
        pipeline = MeshPipeline()
        report = pipeline.validate(mesh)
        assert report.is_valid

        # Export to GDF
        with tempfile.TemporaryDirectory() as tmpdir:
            gdf_path = pipeline.convert_by_name(
                mesh, "gdf", Path(tmpdir) / "box.gdf"
            )
            assert gdf_path.exists()

        # Compute motions with synthetic RAOs
        frequencies = np.linspace(0.1, 2.0, 20)
        directions = np.array([0.0, 90.0, 180.0])
        amplitudes = np.ones((20, 3, 6)) * 0.5
        amplitudes[:, :, 2] = 1.0  # heave
        phases = np.zeros((20, 3, 6))
        rao_data = RAOData(
            frequencies=frequencies,
            directions=directions,
            amplitudes=amplitudes,
            phases=phases,
            vessel_name="box",
        )

        sea_state = SeaStateDefinition(
            significant_height=3.0, peak_period=10.0
        )
        response = cat.compute_motions("unit_box", sea_state, rao_data)
        assert response.significant_values["heave_sig"] > 0

        # Compute point accelerations
        acc = cat.compute_accelerations(
            "unit_box",
            sea_state,
            rao_data,
            point=(100.0, 0.0, 10.0),
            cog=(50.0, 0.0, 5.0),
        )
        assert acc["vertical"] > 0
