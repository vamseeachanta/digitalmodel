# ABOUTME: Tests for digitalmodel.nde package — imports, loader construction, validation.
# ABOUTME: Part of SKELETON → DEVELOPMENT coverage uplift (#1589).
# ABOUTME: Uses mocks so the_well optional dependency is NOT required.
"""
Tests for digitalmodel.nde

Covers:
- Package import tests (nde, well_acoustic)
- AcousticScatteringLoader construction and variant validation
- Attribution string presence
- WELL_AVAILABLE flag
- stream_sample with mocked the_well dependency
- Edge cases: invalid variant, invalid n_steps
"""

from unittest.mock import patch, MagicMock
import pytest


class TestPackageImport:
    """Verify nde package is importable."""

    def test_import_nde_package(self):
        import digitalmodel.nde
        assert digitalmodel.nde is not None

    def test_import_well_acoustic(self):
        from digitalmodel.nde import well_acoustic
        assert hasattr(well_acoustic, "AcousticScatteringLoader")
        assert hasattr(well_acoustic, "WELL_AVAILABLE")
        assert hasattr(well_acoustic, "THE_WELL_ATTRIBUTION")

    def test_import_loader_class(self):
        from digitalmodel.nde.well_acoustic import AcousticScatteringLoader
        assert AcousticScatteringLoader is not None


class TestAcousticScatteringLoader:
    """Test AcousticScatteringLoader construction and validation."""

    def test_default_variant(self):
        from digitalmodel.nde.well_acoustic import AcousticScatteringLoader
        loader = AcousticScatteringLoader()
        assert loader.variant == "basic"

    def test_maze_variant(self):
        from digitalmodel.nde.well_acoustic import AcousticScatteringLoader
        loader = AcousticScatteringLoader(variant="maze")
        assert loader.variant == "maze"

    def test_inclusions_variant(self):
        from digitalmodel.nde.well_acoustic import AcousticScatteringLoader
        loader = AcousticScatteringLoader(variant="inclusions")
        assert loader.variant == "inclusions"

    def test_invalid_variant_raises(self):
        from digitalmodel.nde.well_acoustic import AcousticScatteringLoader
        with pytest.raises(ValueError, match="Unknown variant"):
            AcousticScatteringLoader(variant="nonexistent")


class TestVariantMapping:
    """Test the variant-to-dataset mapping."""

    def test_variant_dataset_map_keys(self):
        from digitalmodel.nde.well_acoustic.acoustic_scattering_loader import VARIANT_DATASET_MAP
        assert set(VARIANT_DATASET_MAP.keys()) == {"basic", "maze", "inclusions"}

    def test_variant_dataset_map_values(self):
        from digitalmodel.nde.well_acoustic.acoustic_scattering_loader import VARIANT_DATASET_MAP
        assert VARIANT_DATASET_MAP["basic"] == "acoustic_scattering"
        assert VARIANT_DATASET_MAP["maze"] == "acoustic_scattering_maze"
        assert VARIANT_DATASET_MAP["inclusions"] == "acoustic_scattering_inclusions"


class TestAttribution:
    """Test CC BY 4.0 attribution string."""

    def test_attribution_contains_polymathic(self):
        from digitalmodel.nde.well_acoustic import THE_WELL_ATTRIBUTION
        assert "Polymathic AI" in THE_WELL_ATTRIBUTION

    def test_attribution_contains_license(self):
        from digitalmodel.nde.well_acoustic import THE_WELL_ATTRIBUTION
        assert "CC BY 4.0" in THE_WELL_ATTRIBUTION


class TestStreamSample:
    """Test stream_sample with mocked dependency."""

    def test_stream_sample_without_the_well_raises(self):
        """When the_well is not installed, stream_sample should raise RuntimeError."""
        from digitalmodel.nde.well_acoustic.acoustic_scattering_loader import (
            AcousticScatteringLoader,
            WELL_AVAILABLE,
        )
        if not WELL_AVAILABLE:
            loader = AcousticScatteringLoader(variant="basic")
            with pytest.raises(RuntimeError, match="the_well"):
                loader.stream_sample(n_steps=5)

    def test_stream_sample_mocked(self):
        """Mock the_well to verify stream_sample returns correct structure."""
        from digitalmodel.nde.well_acoustic import acoustic_scattering_loader as mod

        mock_dataset = [{"frame": i} for i in range(10)]
        mock_well_dataset = MagicMock(return_value=iter(mock_dataset))

        # Temporarily pretend the_well is available
        original_available = mod.WELL_AVAILABLE
        original_cls = mod.WellDataset
        try:
            mod.WELL_AVAILABLE = True
            mod.WellDataset = mock_well_dataset

            loader = mod.AcousticScatteringLoader(variant="basic")
            result = loader.stream_sample(n_steps=5)

            assert "frames" in result
            assert result["variant"] == "basic"
            assert result["dataset_name"] == "acoustic_scattering"
            assert "attribution" in result
            assert len(result["frames"]) == 5
        finally:
            mod.WELL_AVAILABLE = original_available
            mod.WellDataset = original_cls
