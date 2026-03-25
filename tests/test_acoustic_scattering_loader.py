# ABOUTME: Unit tests for AcousticScatteringLoader — The Well acoustic_scattering datasets
# ABOUTME: for subsea NDE validation. Tests construction, stream_sample, variant coverage,
# ABOUTME: and attribution. Uses mocks so the_well is not required to run the suite.

"""
Tests for AcousticScatteringLoader in digitalmodel.nde.well_acoustic.

The Well acoustic scattering datasets (CC BY 4.0, Polymathic AI):
  - acoustic_scattering          (basic variant)
  - acoustic_scattering_maze     (maze variant)
  - acoustic_scattering_inclusions (inclusions variant)

These simulations model acoustic wave propagation around obstacles —
directly relevant to ultrasonic/acoustic NDE of subsea structures.
"""

from __future__ import annotations

import sys
import types
from unittest.mock import MagicMock, patch

import pytest


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _make_mock_well_module() -> types.ModuleType:
    """Build a minimal fake the_well package for isolated unit tests."""
    well_mod = types.ModuleType("the_well")
    well_data_mod = types.ModuleType("the_well.data")

    class _FakeWellDataset:
        """Minimal stub for WellDataset."""

        DATASET_NAME_MAP = {
            "acoustic_scattering": "acoustic_scattering",
            "acoustic_scattering_maze": "acoustic_scattering_maze",
            "acoustic_scattering_inclusions": "acoustic_scattering_inclusions",
        }

        def __init__(self, dataset_name: str, split: str = "train", **kwargs):
            self.dataset_name = dataset_name
            self.split = split

        def __iter__(self):
            for i in range(20):
                frame = MagicMock()
                frame.shape = (64, 64)
                yield frame

    well_data_mod.WellDataset = _FakeWellDataset
    well_mod.data = well_data_mod

    sys.modules["the_well"] = well_mod
    sys.modules["the_well.data"] = well_data_mod
    return well_mod


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture(autouse=True)
def mock_the_well():
    """Inject a fake the_well module for every test in this module."""
    mock_mod = _make_mock_well_module()
    yield mock_mod
    # Teardown: remove injected mocks so they don't pollute other test modules
    sys.modules.pop("the_well", None)
    sys.modules.pop("the_well.data", None)


# ---------------------------------------------------------------------------
# Import under test (done here, after mocks are in place)
# ---------------------------------------------------------------------------

@pytest.fixture()
def loader_class():
    """Import AcousticScatteringLoader freshly (respects mock fixture ordering)."""
    # Remove cached module so re-import picks up the mock
    for key in list(sys.modules.keys()):
        if "digitalmodel.nde.well_acoustic" in key:
            del sys.modules[key]
    from digitalmodel.nde.well_acoustic.acoustic_scattering_loader import (
        AcousticScatteringLoader,
    )
    return AcousticScatteringLoader


# ---------------------------------------------------------------------------
# Construction tests
# ---------------------------------------------------------------------------

class TestAcousticScatteringLoaderConstruction:
    """AcousticScatteringLoader can be constructed for all valid variants."""

    def test_construct_basic_variant(self, loader_class):
        loader = loader_class(variant="basic")
        assert loader.variant == "basic"

    def test_construct_maze_variant(self, loader_class):
        loader = loader_class(variant="maze")
        assert loader.variant == "maze"

    def test_construct_inclusions_variant(self, loader_class):
        loader = loader_class(variant="inclusions")
        assert loader.variant == "inclusions"

    def test_invalid_variant_raises_value_error(self, loader_class):
        with pytest.raises(ValueError, match="variant"):
            loader_class(variant="unknown_variant")

    def test_default_variant_is_basic(self, loader_class):
        loader = loader_class()
        assert loader.variant == "basic"


# ---------------------------------------------------------------------------
# stream_sample structure tests
# ---------------------------------------------------------------------------

class TestStreamSample:
    """stream_sample returns a dict with the expected keys and shapes."""

    def test_stream_sample_returns_dict(self, loader_class):
        loader = loader_class(variant="basic")
        result = loader.stream_sample(n_steps=10)
        assert isinstance(result, dict)

    def test_stream_sample_has_frames_key(self, loader_class):
        loader = loader_class(variant="basic")
        result = loader.stream_sample(n_steps=10)
        assert "frames" in result

    def test_stream_sample_has_variant_key(self, loader_class):
        loader = loader_class(variant="basic")
        result = loader.stream_sample(n_steps=10)
        assert "variant" in result
        assert result["variant"] == "basic"

    def test_stream_sample_has_n_steps_key(self, loader_class):
        loader = loader_class(variant="basic")
        result = loader.stream_sample(n_steps=10)
        assert "n_steps" in result

    def test_stream_sample_n_steps_respected(self, loader_class):
        loader = loader_class(variant="basic")
        result = loader.stream_sample(n_steps=5)
        assert result["n_steps"] == 5
        assert len(result["frames"]) == 5

    def test_stream_sample_default_n_steps_is_10(self, loader_class):
        loader = loader_class(variant="basic")
        result = loader.stream_sample()
        assert result["n_steps"] == 10
        assert len(result["frames"]) == 10

    def test_stream_sample_has_dataset_name_key(self, loader_class):
        loader = loader_class(variant="basic")
        result = loader.stream_sample(n_steps=10)
        assert "dataset_name" in result

    def test_stream_sample_has_attribution_key(self, loader_class):
        loader = loader_class(variant="basic")
        result = loader.stream_sample(n_steps=10)
        assert "attribution" in result


# ---------------------------------------------------------------------------
# All 3 variants produce valid samples
# ---------------------------------------------------------------------------

class TestAllVariants:
    """All three Well acoustic scattering variants stream samples correctly."""

    @pytest.mark.parametrize("variant", ["basic", "maze", "inclusions"])
    def test_variant_streams_10_steps(self, loader_class, variant):
        loader = loader_class(variant=variant)
        result = loader.stream_sample(n_steps=10)
        assert result["n_steps"] == 10
        assert len(result["frames"]) == 10
        assert result["variant"] == variant

    @pytest.mark.parametrize("variant", ["basic", "maze", "inclusions"])
    def test_variant_dataset_name_correct(self, loader_class, variant):
        loader = loader_class(variant=variant)
        result = loader.stream_sample(n_steps=1)
        expected_names = {
            "basic": "acoustic_scattering",
            "maze": "acoustic_scattering_maze",
            "inclusions": "acoustic_scattering_inclusions",
        }
        assert result["dataset_name"] == expected_names[variant]


# ---------------------------------------------------------------------------
# Attribution constant
# ---------------------------------------------------------------------------

class TestAttribution:
    """THE_WELL_ATTRIBUTION constant is present and references CC BY 4.0."""

    def test_attribution_constant_exists(self):
        for key in list(sys.modules.keys()):
            if "digitalmodel.nde.well_acoustic" in key:
                del sys.modules[key]
        from digitalmodel.nde.well_acoustic.acoustic_scattering_loader import (
            THE_WELL_ATTRIBUTION,
        )
        assert THE_WELL_ATTRIBUTION is not None

    def test_attribution_contains_cc_by(self):
        for key in list(sys.modules.keys()):
            if "digitalmodel.nde.well_acoustic" in key:
                del sys.modules[key]
        from digitalmodel.nde.well_acoustic.acoustic_scattering_loader import (
            THE_WELL_ATTRIBUTION,
        )
        assert "CC BY 4.0" in THE_WELL_ATTRIBUTION

    def test_attribution_mentions_polymathic(self):
        for key in list(sys.modules.keys()):
            if "digitalmodel.nde.well_acoustic" in key:
                del sys.modules[key]
        from digitalmodel.nde.well_acoustic.acoustic_scattering_loader import (
            THE_WELL_ATTRIBUTION,
        )
        assert "Polymathic" in THE_WELL_ATTRIBUTION


# ---------------------------------------------------------------------------
# Guard import: WELL_AVAILABLE flag
# ---------------------------------------------------------------------------

class TestWellAvailableFlag:
    """WELL_AVAILABLE is True when the_well mock is present, False otherwise."""

    def test_well_available_true_when_mock_present(self, mock_the_well):
        for key in list(sys.modules.keys()):
            if "digitalmodel.nde.well_acoustic" in key:
                del sys.modules[key]
        from digitalmodel.nde.well_acoustic.acoustic_scattering_loader import (
            WELL_AVAILABLE,
        )
        assert WELL_AVAILABLE is True

    def test_well_available_false_when_not_installed(self):
        # Remove the_well from sys.modules to simulate it not being installed
        saved = {}
        for key in list(sys.modules.keys()):
            if "the_well" in key or "digitalmodel.nde.well_acoustic" in key:
                saved[key] = sys.modules.pop(key)

        import builtins
        real_import = builtins.__import__

        def _block_the_well(name, *args, **kwargs):
            if name.startswith("the_well"):
                raise ImportError("the_well not installed (test)")
            return real_import(name, *args, **kwargs)

        builtins.__import__ = _block_the_well
        try:
            from digitalmodel.nde.well_acoustic.acoustic_scattering_loader import (
                WELL_AVAILABLE,
            )
            assert WELL_AVAILABLE is False
        finally:
            builtins.__import__ = real_import
            # Restore saved modules
            sys.modules.update(saved)

    def test_stream_sample_raises_when_well_unavailable(self):
        """stream_sample raises RuntimeError when the_well is not installed."""
        for key in list(sys.modules.keys()):
            if "the_well" in key or "digitalmodel.nde.well_acoustic" in key:
                sys.modules.pop(key)

        import builtins
        real_import = builtins.__import__

        def _block_the_well(name, *args, **kwargs):
            if name.startswith("the_well"):
                raise ImportError("the_well not installed (test)")
            return real_import(name, *args, **kwargs)

        builtins.__import__ = _block_the_well
        try:
            from digitalmodel.nde.well_acoustic.acoustic_scattering_loader import (
                AcousticScatteringLoader,
            )
            loader = AcousticScatteringLoader(variant="basic")
            with pytest.raises(RuntimeError, match="the_well"):
                loader.stream_sample(n_steps=5)
        finally:
            builtins.__import__ = real_import
