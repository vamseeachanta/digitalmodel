#!/usr/bin/env python3
"""
ABOUTME: Tests for ShearFlowLoader — The Well shear_flow dataset integration.
Tests cover construction, stream_sample(), WELL_AVAILABLE flag, and
mock-based behaviour when the_well is not installed.

Attribution: The Well dataset — CC BY 4.0, Polymathic AI
"""

import sys
from types import ModuleType
from unittest.mock import MagicMock, patch

import numpy as np
import pytest

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Fake velocity/vorticity arrays reused across tests
_FAKE_VELOCITY = np.random.rand(2, 64, 64).astype(np.float32)
_FAKE_VORTICITY = np.random.rand(1, 64, 64).astype(np.float32)


def _make_fake_dataset_cls():
    """Return a FakeDataset class that mimics the_well.data.WellDataset."""

    class FakeDataset:
        def __init__(self, *args, **kwargs):
            pass

        def as_torch_dataset(self, *args, **kwargs):
            return self

        def __iter__(self):
            for _ in range(20):
                yield {
                    "velocity": _FAKE_VELOCITY.copy(),
                    "vorticity": _FAKE_VORTICITY.copy(),
                }

        def __len__(self):
            return 20

    return FakeDataset


def _inject_well_mock():
    """Insert a minimal the_well mock into sys.modules and return it."""
    well_mod = ModuleType("the_well")
    well_data_mod = ModuleType("the_well.data")
    well_data_mod.WellDataset = _make_fake_dataset_cls()
    well_mod.data = well_data_mod
    sys.modules["the_well"] = well_mod
    sys.modules["the_well.data"] = well_data_mod
    return well_mod, well_data_mod


def _remove_well_mock():
    """Remove the the_well mock from sys.modules."""
    sys.modules.pop("the_well", None)
    sys.modules.pop("the_well.data", None)


# Pre-load the loader module once at collection time so that scipy is never
# re-imported during the test run.  We then patch module-level attributes
# directly on the already-loaded module object.
import digitalmodel.hydrodynamics.well_datasets.shear_flow_loader as _sfl_mod  # noqa: E402
import digitalmodel.hydrodynamics.well_datasets as _well_pkg  # noqa: E402


# ---------------------------------------------------------------------------
# Tests: WELL_AVAILABLE flag
# ---------------------------------------------------------------------------

class TestWellAvailableFlag:
    """Test the module-level WELL_AVAILABLE guard."""

    def test_attribution_constant_present(self):
        """THE_WELL_ATTRIBUTION constant exists and mentions CC BY 4.0."""
        assert hasattr(_sfl_mod, "THE_WELL_ATTRIBUTION")
        assert "CC BY 4.0" in _sfl_mod.THE_WELL_ATTRIBUTION

    def test_well_available_is_bool(self):
        """WELL_AVAILABLE is a boolean."""
        assert isinstance(_sfl_mod.WELL_AVAILABLE, bool)

    def test_well_available_false_when_patched_off(self):
        """WELL_AVAILABLE can be False (simulated via patch)."""
        with patch.object(_sfl_mod, "WELL_AVAILABLE", False):
            assert _sfl_mod.WELL_AVAILABLE is False

    def test_well_available_true_when_patched_on(self):
        """WELL_AVAILABLE can be True (simulated via patch)."""
        with patch.object(_sfl_mod, "WELL_AVAILABLE", True):
            assert _sfl_mod.WELL_AVAILABLE is True


# ---------------------------------------------------------------------------
# Tests: ShearFlowLoader construction
# ---------------------------------------------------------------------------

class TestShearFlowLoaderConstruction:
    """Test ShearFlowLoader can be constructed with various arguments."""

    def test_construction_default_args(self):
        """ShearFlowLoader constructs with no arguments."""
        loader = _sfl_mod.ShearFlowLoader()
        assert loader is not None

    def test_construction_with_split(self):
        """ShearFlowLoader accepts a split argument."""
        loader = _sfl_mod.ShearFlowLoader(split="train")
        assert loader.split == "train"

    def test_construction_with_data_root(self, tmp_path):
        """ShearFlowLoader stores the data_root path."""
        loader = _sfl_mod.ShearFlowLoader(data_root=str(tmp_path))
        assert loader.data_root == str(tmp_path)

    def test_construction_default_split_is_train(self):
        """Default split is 'train'."""
        loader = _sfl_mod.ShearFlowLoader()
        assert loader.split == "train"

    def test_construction_data_root_defaults_to_none(self):
        """Default data_root is None."""
        loader = _sfl_mod.ShearFlowLoader()
        assert loader.data_root is None


# ---------------------------------------------------------------------------
# Tests: stream_sample — with mocked the_well
# ---------------------------------------------------------------------------

class TestStreamSampleWithMock:
    """Test stream_sample() when the_well is mocked via module attribute patching."""

    @pytest.fixture(autouse=True)
    def patch_well_available(self):
        """Patch WELL_AVAILABLE=True and WellDataset on the loader module."""
        FakeDataset = _make_fake_dataset_cls()
        with (
            patch.object(_sfl_mod, "WELL_AVAILABLE", True),
            patch.object(_sfl_mod, "WellDataset", FakeDataset),
        ):
            yield

    def test_stream_sample_returns_list(self):
        """stream_sample returns a list."""
        loader = _sfl_mod.ShearFlowLoader()
        result = loader.stream_sample(n_steps=3)
        assert isinstance(result, list)

    def test_stream_sample_length_matches_n_steps(self):
        """stream_sample returns exactly n_steps snapshots."""
        loader = _sfl_mod.ShearFlowLoader()
        result = loader.stream_sample(n_steps=5)
        assert len(result) == 5

    def test_stream_sample_default_n_steps_is_ten(self):
        """Default n_steps is 10."""
        loader = _sfl_mod.ShearFlowLoader()
        result = loader.stream_sample()
        assert len(result) == 10

    def test_stream_sample_each_item_has_velocity_key(self):
        """Each snapshot dict contains a 'velocity' key."""
        loader = _sfl_mod.ShearFlowLoader()
        for snap in loader.stream_sample(n_steps=3):
            assert "velocity" in snap

    def test_stream_sample_each_item_has_vorticity_key(self):
        """Each snapshot dict contains a 'vorticity' key."""
        loader = _sfl_mod.ShearFlowLoader()
        for snap in loader.stream_sample(n_steps=3):
            assert "vorticity" in snap

    def test_stream_sample_velocity_is_numpy_array(self):
        """Velocity values are numpy arrays."""
        loader = _sfl_mod.ShearFlowLoader()
        result = loader.stream_sample(n_steps=1)
        assert isinstance(result[0]["velocity"], np.ndarray)

    def test_stream_sample_vorticity_is_numpy_array(self):
        """Vorticity values are numpy arrays."""
        loader = _sfl_mod.ShearFlowLoader()
        result = loader.stream_sample(n_steps=1)
        assert isinstance(result[0]["vorticity"], np.ndarray)

    def test_stream_sample_velocity_shape_has_spatial_dims(self):
        """Velocity array has at least 2 dimensions."""
        loader = _sfl_mod.ShearFlowLoader()
        result = loader.stream_sample(n_steps=1)
        assert result[0]["velocity"].ndim >= 2

    def test_stream_sample_vorticity_shape_has_spatial_dims(self):
        """Vorticity array has at least 2 dimensions."""
        loader = _sfl_mod.ShearFlowLoader()
        result = loader.stream_sample(n_steps=1)
        assert result[0]["vorticity"].ndim >= 2

    def test_stream_sample_n_steps_one(self):
        """stream_sample(n_steps=1) returns exactly one snapshot."""
        loader = _sfl_mod.ShearFlowLoader()
        result = loader.stream_sample(n_steps=1)
        assert len(result) == 1

    def test_stream_sample_velocity_matches_fake_shape(self):
        """Velocity shape matches the fake dataset's (2, 64, 64)."""
        loader = _sfl_mod.ShearFlowLoader()
        result = loader.stream_sample(n_steps=1)
        assert result[0]["velocity"].shape == (2, 64, 64)

    def test_stream_sample_vorticity_matches_fake_shape(self):
        """Vorticity shape matches the fake dataset's (1, 64, 64)."""
        loader = _sfl_mod.ShearFlowLoader()
        result = loader.stream_sample(n_steps=1)
        assert result[0]["vorticity"].shape == (1, 64, 64)


# ---------------------------------------------------------------------------
# Tests: stream_sample — without the_well (graceful degradation)
# ---------------------------------------------------------------------------

class TestStreamSampleWithoutTheWell:
    """Test behaviour when the_well is not available."""

    @pytest.fixture(autouse=True)
    def patch_well_unavailable(self):
        """Force WELL_AVAILABLE=False."""
        with patch.object(_sfl_mod, "WELL_AVAILABLE", False):
            yield

    def test_well_not_available_flag_is_false(self):
        """WELL_AVAILABLE is False when patched off."""
        assert _sfl_mod.WELL_AVAILABLE is False

    def test_stream_sample_raises_import_error_without_well(self):
        """stream_sample raises ImportError when the_well is absent."""
        loader = _sfl_mod.ShearFlowLoader()
        with pytest.raises(ImportError, match="the_well"):
            loader.stream_sample(n_steps=3)

    def test_construction_still_works_without_well(self):
        """ShearFlowLoader can be constructed even when the_well is absent."""
        loader = _sfl_mod.ShearFlowLoader()
        assert loader is not None


# ---------------------------------------------------------------------------
# Tests: public package re-export
# ---------------------------------------------------------------------------

class TestPackageExport:
    """ShearFlowLoader is importable from the well_datasets package."""

    def test_import_from_well_datasets_package(self):
        """Can import ShearFlowLoader from well_datasets __init__."""
        from digitalmodel.hydrodynamics.well_datasets import ShearFlowLoader
        assert ShearFlowLoader is not None

    def test_well_available_exported_from_package(self):
        """WELL_AVAILABLE is re-exported from well_datasets package."""
        assert hasattr(_well_pkg, "WELL_AVAILABLE")

    def test_attribution_exported_from_package(self):
        """THE_WELL_ATTRIBUTION is re-exported from well_datasets package."""
        assert hasattr(_well_pkg, "THE_WELL_ATTRIBUTION")
        assert "CC BY 4.0" in _well_pkg.THE_WELL_ATTRIBUTION

    def test_shear_flow_loader_same_object_as_submodule(self):
        """Package ShearFlowLoader is the same class as the submodule one."""
        from digitalmodel.hydrodynamics.well_datasets import ShearFlowLoader
        assert ShearFlowLoader is _sfl_mod.ShearFlowLoader
