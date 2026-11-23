"""Tests for solver/orcawave_data_extraction.py.

All OrcFxAPI interactions are mocked via a fake module installed into
sys.modules before importing the SUT.
"""
from __future__ import annotations

import sys
import types
import warnings
from unittest.mock import MagicMock

import numpy as np
import pytest


# ---------------------------------------------------------------------------
# Mock OrcFxAPI at the module level
# ---------------------------------------------------------------------------


def _make_mock_orcfxapi() -> types.ModuleType:
    mod = types.ModuleType("OrcFxAPI")
    mod.Model = MagicMock
    mod.otVessel = "Vessel"
    mod.Diffraction = MagicMock
    return mod


@pytest.fixture(autouse=True)
def _install_mock_orcfxapi(monkeypatch):
    fake = _make_mock_orcfxapi()
    monkeypatch.setitem(sys.modules, "OrcFxAPI", fake)


# ---------------------------------------------------------------------------
# Helpers: build mock vessel objects
# ---------------------------------------------------------------------------


def _make_rao_entry(period: float, direction: float, surge: complex = 1 + 0j):
    """Create a single mock LoadRAO entry."""
    entry = MagicMock()
    entry.Period = period
    entry.Direction = direction
    entry.LoadRAOSurge = surge
    entry.LoadRAOSway = surge * 0.5
    entry.LoadRAOHeave = surge * 0.8
    entry.LoadRAORoll = surge * 0.1
    entry.LoadRAOPitch = surge * 0.2
    entry.LoadRAOYaw = surge * 0.05
    return entry


def _make_am_entry(period: float, diag_value: float = 100.0):
    """Create a mock added-mass/damping entry with all fields."""
    entry = MagicMock()
    entry.Period = period

    # Diagonal added mass
    for attr in ("AddedMassX", "AddedMassY", "AddedMassZ",
                 "AddedMassRx", "AddedMassRy", "AddedMassRz"):
        setattr(entry, attr, diag_value)
    # Off-diagonal added mass: zero
    for attr in ("AddedMassXY", "AddedMassXZ", "AddedMassXRx", "AddedMassXRy", "AddedMassXRz",
                 "AddedMassYZ", "AddedMassYRx", "AddedMassYRy", "AddedMassYRz",
                 "AddedMassZRx", "AddedMassZRy", "AddedMassZRz",
                 "AddedMassRxRy", "AddedMassRxRz", "AddedMassRyRz"):
        setattr(entry, attr, 0.0)

    # Diagonal damping
    for attr in ("DampingX", "DampingY", "DampingZ",
                 "DampingRx", "DampingRy", "DampingRz"):
        setattr(entry, attr, diag_value / 10)
    # Off-diagonal damping: zero
    for attr in ("DampingXY", "DampingXZ", "DampingXRx", "DampingXRy", "DampingXRz",
                 "DampingYZ", "DampingYRx", "DampingYRy", "DampingYRz",
                 "DampingZRx", "DampingZRy", "DampingZRz",
                 "DampingRxRy", "DampingRxRz", "DampingRyRz"):
        setattr(entry, attr, 0.0)

    return entry


def _build_vessel(periods, headings, am_periods=None):
    """Build a mock vessel with LoadRAOs and AddedMassAndDamping tables."""
    vessel = MagicMock()

    # LoadRAOs
    entries = []
    for p in periods:
        for h in headings:
            entries.append(_make_rao_entry(p, h))
    load_raos = MagicMock()
    load_raos.Size = len(entries)
    load_raos.__getitem__ = lambda self, idx: entries[idx]
    vessel.VesselType.LoadRAOs = load_raos

    # AddedMassAndDamping
    am_entries = []
    for p in (am_periods or periods):
        am_entries.append(_make_am_entry(p))
    am_damp = MagicMock()
    am_damp.Size = len(am_entries)
    am_damp.__getitem__ = lambda self, idx: am_entries[idx]
    vessel.VesselType.AddedMassAndDamping = am_damp

    return vessel


# ---------------------------------------------------------------------------
# Tests: OrcaWaveDataExtractor
# ---------------------------------------------------------------------------


class TestExtractRaoFrequencies:

    def test_extracts_unique_frequencies(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        periods = [10.0, 5.0]  # 2 unique periods
        headings = [0.0, 90.0]
        vessel = _build_vessel(periods, headings)

        extractor = OrcaWaveDataExtractor(vessel)
        freqs = extractor.extract_rao_frequencies()

        # freq = 2π / period
        expected = sorted([2 * np.pi / p for p in periods])
        np.testing.assert_allclose(freqs, expected, rtol=1e-6)

    def test_raises_if_no_rao_data(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        vessel = MagicMock()
        load_raos = MagicMock()
        load_raos.Size = 0
        vessel.VesselType.LoadRAOs = load_raos

        extractor = OrcaWaveDataExtractor(vessel)
        with pytest.raises(ValueError, match="No RAO data found"):
            extractor.extract_rao_frequencies()


class TestExtractRaoHeadings:

    def test_extracts_unique_headings(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        periods = [10.0]
        headings = [0.0, 45.0, 90.0]
        vessel = _build_vessel(periods, headings)

        extractor = OrcaWaveDataExtractor(vessel)
        result = extractor.extract_rao_headings()

        np.testing.assert_allclose(result, [0.0, 45.0, 90.0])

    def test_raises_if_no_headings(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        vessel = MagicMock()
        load_raos = MagicMock()
        load_raos.Size = 0
        vessel.VesselType.LoadRAOs = load_raos

        extractor = OrcaWaveDataExtractor(vessel)
        with pytest.raises(ValueError, match="No RAO data found"):
            extractor.extract_rao_headings()


class TestExtractRaoForDof:

    def test_extracts_magnitude_and_phase(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        periods = [10.0, 5.0]
        headings = [0.0]
        vessel = _build_vessel(periods, headings)

        extractor = OrcaWaveDataExtractor(vessel)
        freqs = extractor.extract_rao_frequencies()
        heads = extractor.extract_rao_headings()

        mag, phase = extractor.extract_rao_for_dof("Surge", freqs, heads)

        assert mag.shape == (len(freqs), len(heads))
        assert phase.shape == (len(freqs), len(heads))
        # Surge RAO was set to 1+0j so magnitude should be 1.0
        assert mag[0, 0] == pytest.approx(1.0, abs=0.01) or mag[1, 0] == pytest.approx(1.0, abs=0.01)

    def test_invalid_dof_raises(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        vessel = _build_vessel([10.0], [0.0])
        extractor = OrcaWaveDataExtractor(vessel)

        with pytest.raises(ValueError, match="Invalid DOF"):
            extractor.extract_rao_for_dof(
                "BadDOF", np.array([0.6]), np.array([0.0])
            )


class TestExtractAddedMassFrequencies:

    def test_extracts_from_am_damp_table(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        vessel = _build_vessel([10.0], [0.0], am_periods=[8.0, 4.0])
        extractor = OrcaWaveDataExtractor(vessel)

        freqs = extractor.extract_added_mass_frequencies()
        expected = sorted([2 * np.pi / 8.0, 2 * np.pi / 4.0])
        np.testing.assert_allclose(freqs, expected, rtol=1e-6)

    def test_raises_if_no_am_data(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        vessel = MagicMock()
        am_damp = MagicMock()
        am_damp.Size = 0
        vessel.VesselType.AddedMassAndDamping = am_damp

        extractor = OrcaWaveDataExtractor(vessel)
        with pytest.raises(ValueError, match="No added mass"):
            extractor.extract_added_mass_frequencies()


class TestExtractAddedMassAtFrequency:

    def test_returns_6x6_diagonal(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        vessel = _build_vessel([10.0], [0.0])
        extractor = OrcaWaveDataExtractor(vessel)

        freq = 2 * np.pi / 10.0
        matrix = extractor.extract_added_mass_at_frequency(freq)

        assert matrix.shape == (6, 6)
        # Diagonal should be 100 (from _make_am_entry default)
        for i in range(6):
            assert matrix[i, i] == pytest.approx(100.0)
        # Off-diagonal should be 0
        assert matrix[0, 1] == pytest.approx(0.0)

    def test_returns_zero_matrix_with_warning_if_not_found(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        vessel = _build_vessel([10.0], [0.0])
        extractor = OrcaWaveDataExtractor(vessel)

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            matrix = extractor.extract_added_mass_at_frequency(999.0)
            assert len(w) == 1
            assert "No added mass data" in str(w[0].message)

        np.testing.assert_array_equal(matrix, np.zeros((6, 6)))


class TestExtractDampingAtFrequency:

    def test_returns_6x6_diagonal(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        vessel = _build_vessel([10.0], [0.0])
        extractor = OrcaWaveDataExtractor(vessel)

        freq = 2 * np.pi / 10.0
        matrix = extractor.extract_damping_at_frequency(freq)

        assert matrix.shape == (6, 6)
        # Diagonal should be 10 (100/10 from _make_am_entry)
        for i in range(6):
            assert matrix[i, i] == pytest.approx(10.0)

    def test_returns_zero_matrix_with_warning_if_not_found(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        vessel = _build_vessel([10.0], [0.0])
        extractor = OrcaWaveDataExtractor(vessel)

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            matrix = extractor.extract_damping_at_frequency(999.0)
            assert len(w) == 1

        np.testing.assert_array_equal(matrix, np.zeros((6, 6)))


# ---------------------------------------------------------------------------
# Tests: validate_data_consistency
# ---------------------------------------------------------------------------


class TestValidateDataConsistency:

    def test_consistent_data(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        periods = [10.0, 5.0]
        headings = [0.0, 90.0]
        vessel = _build_vessel(periods, headings, am_periods=periods)
        extractor = OrcaWaveDataExtractor(vessel)

        result = extractor.validate_data_consistency()
        assert result["has_rao_data"] is True
        assert result["has_added_mass"] is True
        assert result["rao_freq_count"] == 2
        assert result["rao_heading_count"] == 2

    def test_mismatched_frequencies_logged(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            OrcaWaveDataExtractor,
        )
        # RAO periods differ from AM periods
        vessel = _build_vessel([10.0, 5.0], [0.0], am_periods=[8.0])
        extractor = OrcaWaveDataExtractor(vessel)

        result = extractor.validate_data_consistency()
        # The source uses a key for AM freq count that may appear as '***'.
        # Find the AM freq count key dynamically (not rao_freq_count, not the known keys)
        known_non_am = {"has_rao_data", "has_added_mass", "has_damping",
                        "rao_freq_count", "rao_heading_count", "issues"}
        am_keys = set(result.keys()) - known_non_am
        assert len(am_keys) == 1, f"Expected one AM key, got: {am_keys}"
        am_key = am_keys.pop()
        assert result["rao_freq_count"] != result[am_key]
        assert len(result["issues"]) > 0


# ---------------------------------------------------------------------------
# Tests: convenience functions
# ---------------------------------------------------------------------------


class TestConvenienceFunctions:

    def test_extract_all_rao_data(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            extract_all_rao_data,
        )
        vessel = _build_vessel([10.0, 5.0], [0.0, 90.0])
        data = extract_all_rao_data(vessel)

        assert "frequencies" in data
        assert "headings" in data
        assert "surge" in data
        assert "yaw" in data
        assert data["surge"]["magnitude"].shape == (2, 2)

    def test_extract_all_added_mass(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            extract_all_added_mass,
        )
        vessel = _build_vessel([10.0], [0.0])
        data = extract_all_added_mass(vessel)

        assert "frequencies" in data
        assert "matrices" in data
        assert len(data["matrices"]) == 1
        assert data["matrices"][0].shape == (6, 6)

    def test_extract_all_damping(self):
        from digitalmodel.hydrodynamics.diffraction.solver.orcawave_data_extraction import (
            extract_all_damping,
        )
        vessel = _build_vessel([10.0], [0.0])
        data = extract_all_damping(vessel)

        assert "frequencies" in data
        assert "matrices" in data
        assert len(data["matrices"]) == 1
        assert data["matrices"][0].shape == (6, 6)
