"""Tests for solver/orcawave_converter.py.

OrcFxAPI is NOT available in CI — every test mocks it at module-level.
We use function-level imports to bypass the solver/__init__.py chain
that does ``import OrcFxAPI`` at the top.
"""
from __future__ import annotations

import sys
import types
from pathlib import Path
from unittest.mock import MagicMock, patch

import numpy as np
import pytest


# ---------------------------------------------------------------------------
# Fixtures: build a fake OrcFxAPI module that the converter expects
# ---------------------------------------------------------------------------

N_FREQ = 3
N_HEAD = 2
FREQUENCIES = np.array([0.3, 0.6, 1.0])
HEADINGS = np.array([0.0, 90.0])


def _make_mock_orcfxapi() -> types.ModuleType:
    """Create a stub ``OrcFxAPI`` module suitable for the converter."""
    mod = types.ModuleType("OrcFxAPI")
    mod.Model = MagicMock
    mod.Diffraction = MagicMock
    mod.otVessel = "Vessel"
    # Ensure attribute access works for any attribute
    mod.__getattr__ = lambda name: MagicMock()
    return mod


@pytest.fixture(autouse=True)
def _install_mock_orcfxapi(monkeypatch):
    """Ensure ``import OrcFxAPI`` succeeds before we import solver code."""
    fake = _make_mock_orcfxapi()
    monkeypatch.setitem(sys.modules, "OrcFxAPI", fake)


def _fake_rao_data():
    """Return synthetic RAO extraction dict matching extract_all_rao_data output."""
    data = {
        "frequencies": FREQUENCIES.copy(),
        "headings": HEADINGS.copy(),
    }
    for dof in ("surge", "sway", "heave", "roll", "pitch", "yaw"):
        data[dof] = {
            "magnitude": np.random.default_rng(42).uniform(0, 1, (N_FREQ, N_HEAD)),
            "phase": np.random.default_rng(42).uniform(-180, 180, (N_FREQ, N_HEAD)),
        }
    return data


def _fake_matrix_data():
    """Return synthetic matrix extraction dict matching extract_all_added_mass output."""
    matrices = [np.eye(6) * (i + 1) * 100 for i in range(N_FREQ)]
    return {
        "frequencies": FREQUENCIES.copy(),
        "matrices": matrices,
    }


# ---------------------------------------------------------------------------
# Tests: OrcaWaveConverter initialisation
# ---------------------------------------------------------------------------


class TestOrcaWaveConverterInit:

    def test_raises_file_not_found(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        with pytest.raises(FileNotFoundError, match="Model file not found"):
            OrcaWaveConverter(tmp_path / "nonexistent.sim")

    def test_accepts_existing_file(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        model_file = tmp_path / "test_model.sim"
        model_file.touch()
        converter = OrcaWaveConverter(model_file)
        assert converter.model_file == model_file
        assert converter.vessel_name is None

    def test_stores_vessel_name(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        model_file = tmp_path / "test_model.sim"
        model_file.touch()
        converter = OrcaWaveConverter(model_file, vessel_name="MyVessel")
        assert converter.vessel_name == "MyVessel"


# ---------------------------------------------------------------------------
# Tests: _load_model
# ---------------------------------------------------------------------------


class TestLoadModel:

    def test_load_model_stores_model(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        model_file = tmp_path / "test.sim"
        model_file.touch()
        converter = OrcaWaveConverter(model_file)

        converter._load_model()
        # Model should be set (it's whatever OrcFxAPI.Model() returns)
        assert converter.model is not None

    def test_load_model_raises_on_failure(self, tmp_path, monkeypatch):
        from digitalmodel.hydrodynamics.diffraction.solver import orcawave_converter as _mod
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        model_file = tmp_path / "bad.sim"
        model_file.touch()
        converter = OrcaWaveConverter(model_file)

        # Patch OrcFxAPI at the module level where the converter accesses it
        bad_orcfx = MagicMock()
        bad_orcfx.Model = MagicMock(side_effect=RuntimeError("DLL fail"))
        monkeypatch.setattr(_mod, "OrcFxAPI", bad_orcfx)

        with pytest.raises(RuntimeError, match="Failed to load"):
            converter._load_model()


# ---------------------------------------------------------------------------
# Tests: _find_vessel
# ---------------------------------------------------------------------------


class TestFindVessel:

    def test_find_vessel_by_name(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        model_file = tmp_path / "m.sim"
        model_file.touch()
        converter = OrcaWaveConverter(model_file, vessel_name="V1")

        mock_vessel = MagicMock()
        mock_vessel.Name = "V1"
        mock_model = MagicMock()
        mock_model.__getitem__ = MagicMock(return_value=mock_vessel)
        converter.model = mock_model

        converter._find_vessel()
        assert converter.vessel is mock_vessel

    def test_find_vessel_auto_detect(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        model_file = tmp_path / "m.sim"
        model_file.touch()
        converter = OrcaWaveConverter(model_file)

        vessel_obj = MagicMock()
        vessel_obj.type = "Vessel"  # otVessel
        vessel_obj.Name = "AutoVessel"
        mock_model = MagicMock()
        mock_model.objects = [vessel_obj]

        converter.model = mock_model
        # Patch the otVessel constant
        with patch.dict(sys.modules["OrcFxAPI"].__dict__, {"otVessel": "Vessel"}):
            converter._find_vessel()
        assert converter.vessel_name == "AutoVessel"

    def test_find_vessel_raises_if_none(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        model_file = tmp_path / "m.sim"
        model_file.touch()
        converter = OrcaWaveConverter(model_file)

        mock_model = MagicMock()
        mock_model.objects = []  # no vessels
        converter.model = mock_model

        with patch.dict(sys.modules["OrcFxAPI"].__dict__, {"otVessel": "Vessel"}):
            with pytest.raises(ValueError, match="No vessel objects found"):
                converter._find_vessel()


# ---------------------------------------------------------------------------
# Tests: _build_rao_set
# ---------------------------------------------------------------------------


class TestBuildRAOSet:

    def test_build_rao_set_creates_raos(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        model_file = tmp_path / "m.sim"
        model_file.touch()
        converter = OrcaWaveConverter(model_file, vessel_name="TestV")

        rao_data = _fake_rao_data()
        rao_set = converter._build_rao_set(rao_data, water_depth=100.0)

        assert rao_set.vessel_name == "TestV"
        assert rao_set.analysis_tool == "OrcaWave"
        assert rao_set.water_depth == 100.0
        assert rao_set.surge is not None
        assert rao_set.heave is not None
        assert rao_set.surge.magnitude.shape == (N_FREQ, N_HEAD)

    def test_build_rao_set_frequency_data(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        model_file = tmp_path / "m.sim"
        model_file.touch()
        converter = OrcaWaveConverter(model_file, vessel_name="V")

        rao_data = _fake_rao_data()
        rao_set = converter._build_rao_set(rao_data, water_depth=50.0)

        # FrequencyData.__post_init__ recomputes count/min/max
        assert rao_set.surge.frequencies.count == N_FREQ
        assert rao_set.surge.frequencies.min_freq == pytest.approx(0.3)


# ---------------------------------------------------------------------------
# Tests: _build_added_mass_set / _build_damping_set
# ---------------------------------------------------------------------------


class TestBuildMatrixSets:

    def test_build_added_mass_set(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        model_file = tmp_path / "m.sim"
        model_file.touch()
        converter = OrcaWaveConverter(model_file, vessel_name="V")

        am_data = _fake_matrix_data()
        am_set = converter._build_added_mass_set(am_data, 100.0)

        assert len(am_set.matrices) == N_FREQ
        assert am_set.matrices[0].matrix_type == "added_mass"
        assert am_set.matrices[0].matrix.shape == (6, 6)

    def test_build_damping_set(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        model_file = tmp_path / "m.sim"
        model_file.touch()
        converter = OrcaWaveConverter(model_file, vessel_name="V")

        damp_data = _fake_matrix_data()
        damp_set = converter._build_damping_set(damp_data, 100.0)

        assert len(damp_set.matrices) == N_FREQ
        assert damp_set.matrices[0].matrix_type == "damping"

    def test_added_mass_units(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        units = OrcaWaveConverter._get_added_mass_units()
        assert "linear-linear" in units
        assert units["linear-linear"] == "kg"

    def test_damping_units(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        units = OrcaWaveConverter._get_damping_units()
        assert "linear-linear" in units
        assert units["linear-linear"] == "N.s/m"


# ---------------------------------------------------------------------------
# Tests: convert_to_unified_schema (integration-level, model load skipped)
# ---------------------------------------------------------------------------


class TestConvertToUnifiedSchema:

    def test_convert_with_load_false(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        model_file = tmp_path / "m.sim"
        model_file.touch()
        converter = OrcaWaveConverter(model_file, vessel_name="TestV")

        # Pre-set vessel to avoid auto-detect
        vessel_mock = MagicMock()
        vessel_mock.Name = "TestV"
        converter.vessel = vessel_mock

        # Patch extraction methods
        rao_data = _fake_rao_data()
        am_data = _fake_matrix_data()
        damp_data = _fake_matrix_data()

        with patch.object(converter, "_extract_rao_data", return_value=rao_data), \
             patch.object(converter, "_extract_added_mass_data", return_value=am_data), \
             patch.object(converter, "_extract_damping_data", return_value=damp_data):
            results = converter.convert_to_unified_schema(
                water_depth=200.0, load_model=False
            )

        assert results.vessel_name == "TestV"
        assert results.analysis_tool == "OrcaWave"
        assert results.water_depth == 200.0
        assert results.raos is not None
        assert results.added_mass is not None
        assert results.damping is not None
        assert len(results.source_files) == 1


# ---------------------------------------------------------------------------
# Tests: _get_analysis_date
# ---------------------------------------------------------------------------


class TestAnalysisDate:

    def test_returns_none_by_default(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_converter import (
            OrcaWaveConverter,
        )
        model_file = tmp_path / "m.sim"
        model_file.touch()
        converter = OrcaWaveConverter(model_file)
        assert converter._get_analysis_date() is None
