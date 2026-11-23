"""Tests for solver/report_extractors.py.

OrcFxAPI is mocked. The two public functions are:
  - extract_report_data_from_owr(owr_path)
  - build_report_data_from_solver_results(solver_results, ...)

extract_report_data_from_owr relies heavily on OrcFxAPI.Diffraction which
we mock with realistic ndarray shapes.

build_report_data_from_solver_results has a "minimal path" that only needs
DiffractionResults objects (available from conftest fixtures), so we can
test that branch without any mocking.
"""
from __future__ import annotations

import sys
import types
from pathlib import Path
from unittest.mock import MagicMock, patch

import numpy as np
import pytest


# ---------------------------------------------------------------------------
# Constants matching the fake diffraction data we will set up
# ---------------------------------------------------------------------------

N_FREQ = 4
N_HEAD = 3
N_DOF = 6


def _make_mock_orcfxapi() -> types.ModuleType:
    mod = types.ModuleType("OrcFxAPI")
    mod.Model = MagicMock
    mod.Diffraction = MagicMock
    mod.otVessel = "Vessel"
    return mod


@pytest.fixture(autouse=True)
def _install_mock_orcfxapi(monkeypatch):
    fake = _make_mock_orcfxapi()
    monkeypatch.setitem(sys.modules, "OrcFxAPI", fake)


# ---------------------------------------------------------------------------
# Helpers: build a mock Diffraction object
# ---------------------------------------------------------------------------


def _mock_diffraction_object():
    """Return a mock OrcFxAPI.Diffraction() whose data arrays mimic real shapes."""
    d = MagicMock()

    # frequencies: Hz descending (as OrcFxAPI returns)
    freq_hz = np.array([0.20, 0.15, 0.10, 0.05])  # 4 freqs
    d.frequencies = freq_hz

    headings = np.array([0.0, 90.0, 180.0])
    d.headings = headings

    # Hydrostatic results
    hs = {
        "volume": 5000.0,
        "mass": 500000.0,
        "centreOfBuoyancy": np.array([0.0, 0.0, -2.0]),
        "centreOfMass": np.array([0.0, 0.0, -1.0]),
        "Awp": 200.0,
        "Lxx": 1e4,
        "Lyy": 1e5,
        "Lxy": 0.0,
        "centreOfFloatation": np.array([0.0, 0.0]),
        "restoringMatrix": np.eye(6) * 1e6,
        "inertiaMatrix": np.eye(6) * 1e8,
    }
    d.hydrostaticResults = [hs]

    # Roll damping percent critical (nfreq, nheading) – use (nfreq, 1)
    d.rollDampingPercentCritical = np.ones((N_FREQ, 1)) * 5.0

    # Added mass (nfreq, 6, 6) and damping (nfreq, 6, 6)
    d.addedMass = np.ones((N_FREQ, 6, 6)) * 1000.0
    d.damping = np.ones((N_FREQ, 6, 6)) * 50.0

    # Displacement RAOs: (nheading, nfreq, 6) of complex values
    # Real data: shape = (nheading, nfreq, ndof)
    rng = np.random.default_rng(42)
    disp_raos = rng.uniform(0, 1, (N_HEAD, N_FREQ, N_DOF)) + 0j
    d.displacementRAOs = disp_raos

    # Load RAOs diffraction: (nheading, nfreq, 6) of complex values
    load_raos = rng.uniform(0, 1, (N_HEAD, N_FREQ, N_DOF)) + 0j
    d.loadRAOsDiffraction = load_raos

    # Panel geometry
    d.panelGeometry = [
        {"area": 2.0},
        {"area": 3.0},
        {"area": 2.5},
    ]

    # Infinite frequency added mass
    d.infiniteFrequencyAddedMass = np.ones((6, 6)) * 900.0

    return d


# ---------------------------------------------------------------------------
# Tests: extract_report_data_from_owr
# ---------------------------------------------------------------------------


class TestExtractReportDataFromOwr:

    def test_returns_diffraction_report_data(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            extract_report_data_from_owr,
        )
        from digitalmodel.hydrodynamics.diffraction.solver import report_extractors as _mod

        mock_d = _mock_diffraction_object()
        owr_path = tmp_path / "test_results.owr"
        owr_path.touch()

        with patch.object(_mod, "OrcFxAPI") as fake_api:
            fake_api.Diffraction.return_value = mock_d
            data = extract_report_data_from_owr(owr_path)

        from digitalmodel.hydrodynamics.diffraction.report_data_models import (
            DiffractionReportData,
        )
        assert isinstance(data, DiffractionReportData)

    def test_frequencies_sorted_ascending(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            extract_report_data_from_owr,
        )
        from digitalmodel.hydrodynamics.diffraction.solver import report_extractors as _mod

        mock_d = _mock_diffraction_object()
        owr_path = tmp_path / "test.owr"
        owr_path.touch()

        with patch.object(_mod, "OrcFxAPI") as fake_api:
            fake_api.Diffraction.return_value = mock_d
            data = extract_report_data_from_owr(owr_path)

        freqs = data.frequencies_rad_s
        assert freqs == sorted(freqs), "Frequencies should be sorted ascending"

    def test_hydrostatic_data_populated(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            extract_report_data_from_owr,
        )
        from digitalmodel.hydrodynamics.diffraction.solver import report_extractors as _mod

        mock_d = _mock_diffraction_object()
        owr_path = tmp_path / "test.owr"
        owr_path.touch()

        with patch.object(_mod, "OrcFxAPI") as fake_api:
            fake_api.Diffraction.return_value = mock_d
            data = extract_report_data_from_owr(owr_path)

        assert data.hydrostatics is not None
        assert data.hydrostatics.volume == pytest.approx(5000.0)
        assert data.hydrostatics.mass == pytest.approx(500000.0)
        assert data.hydrostatics.waterplane_area == pytest.approx(200.0)

    def test_roll_damping_populated(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            extract_report_data_from_owr,
        )
        from digitalmodel.hydrodynamics.diffraction.solver import report_extractors as _mod

        mock_d = _mock_diffraction_object()
        owr_path = tmp_path / "test.owr"
        owr_path.touch()

        with patch.object(_mod, "OrcFxAPI") as fake_api:
            fake_api.Diffraction.return_value = mock_d
            data = extract_report_data_from_owr(owr_path)

        assert data.roll_damping is not None
        assert len(data.roll_damping.frequencies_rad_s) == N_FREQ
        assert data.roll_damping.C_44 == pytest.approx(1e6)

    def test_load_raos_populated(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            extract_report_data_from_owr,
        )
        from digitalmodel.hydrodynamics.diffraction.solver import report_extractors as _mod

        mock_d = _mock_diffraction_object()
        owr_path = tmp_path / "test.owr"
        owr_path.touch()

        with patch.object(_mod, "OrcFxAPI") as fake_api:
            fake_api.Diffraction.return_value = mock_d
            data = extract_report_data_from_owr(owr_path)

        assert data.load_raos is not None
        assert data.load_raos.method == "diffraction"
        assert "surge" in data.load_raos.amplitude

    def test_mesh_quality_populated(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            extract_report_data_from_owr,
        )
        from digitalmodel.hydrodynamics.diffraction.solver import report_extractors as _mod

        mock_d = _mock_diffraction_object()
        owr_path = tmp_path / "test.owr"
        owr_path.touch()

        with patch.object(_mod, "OrcFxAPI") as fake_api:
            fake_api.Diffraction.return_value = mock_d
            data = extract_report_data_from_owr(owr_path)

        assert data.mesh_quality is not None
        assert data.mesh_quality.panel_count == 3
        assert data.mesh_quality.mean_area == pytest.approx(2.5)

    def test_vessel_name_from_filename(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            extract_report_data_from_owr,
        )
        from digitalmodel.hydrodynamics.diffraction.solver import report_extractors as _mod

        mock_d = _mock_diffraction_object()
        owr_path = tmp_path / "my_ship_ground_truth.owr"
        owr_path.touch()

        with patch.object(_mod, "OrcFxAPI") as fake_api:
            fake_api.Diffraction.return_value = mock_d
            data = extract_report_data_from_owr(owr_path)

        # "_ground_truth" should be stripped and underscores become spaces
        assert "ground_truth" not in data.vessel_name
        assert "my ship" in data.vessel_name.lower()

    def test_executive_warnings_generated(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            extract_report_data_from_owr,
        )
        from digitalmodel.hydrodynamics.diffraction.solver import report_extractors as _mod

        mock_d = _mock_diffraction_object()
        owr_path = tmp_path / "test.owr"
        owr_path.touch()

        with patch.object(_mod, "OrcFxAPI") as fake_api:
            fake_api.Diffraction.return_value = mock_d
            data = extract_report_data_from_owr(owr_path)

        # executive_warnings should be a list (possibly empty)
        assert isinstance(data.executive_warnings, list)


# ---------------------------------------------------------------------------
# Tests: build_report_data_from_solver_results — minimal path (no owr)
# ---------------------------------------------------------------------------


class TestBuildReportDataMinimalPath:
    """Test the minimal-path branch that builds from DiffractionResults only."""

    def test_returns_report_data(self, mock_diffraction_results):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            build_report_data_from_solver_results,
        )
        solver_results = {"OrcaWave": mock_diffraction_results}

        data = build_report_data_from_solver_results(solver_results)

        from digitalmodel.hydrodynamics.diffraction.report_data_models import (
            DiffractionReportData,
        )
        assert isinstance(data, DiffractionReportData)

    def test_uses_first_solver_vessel_name(self, mock_diffraction_results):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            build_report_data_from_solver_results,
        )
        solver_results = {"OrcaWave": mock_diffraction_results}
        data = build_report_data_from_solver_results(solver_results)
        assert data.vessel_name == "TestVessel"

    def test_vessel_name_override(self, mock_diffraction_results):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            build_report_data_from_solver_results,
        )
        solver_results = {"OrcaWave": mock_diffraction_results}
        data = build_report_data_from_solver_results(
            solver_results, vessel_name="CustomName"
        )
        assert data.vessel_name == "CustomName"

    def test_solver_names_populated(self, mock_diffraction_results):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            build_report_data_from_solver_results,
        )
        solver_results = {
            "AQWA": mock_diffraction_results,
            "OrcaWave": mock_diffraction_results,
        }
        data = build_report_data_from_solver_results(solver_results)
        assert "AQWA" in data.solver_names
        assert "OrcaWave" in data.solver_names

    def test_frequencies_extracted(self, mock_diffraction_results):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            build_report_data_from_solver_results,
        )
        solver_results = {"S": mock_diffraction_results}
        data = build_report_data_from_solver_results(solver_results)
        assert len(data.frequencies_rad_s) > 0
        assert len(data.periods_s) == len(data.frequencies_rad_s)

    def test_added_mass_diagonal_extracted(self, mock_diffraction_results):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            build_report_data_from_solver_results,
        )
        solver_results = {"S": mock_diffraction_results}
        data = build_report_data_from_solver_results(solver_results)
        assert "surge" in data.added_mass_diagonal
        assert "yaw" in data.added_mass_diagonal
        # Length should equal number of frequencies
        nfreq = len(data.frequencies_rad_s)
        assert len(data.added_mass_diagonal["surge"]) == nfreq

    def test_damping_diagonal_extracted(self, mock_diffraction_results):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            build_report_data_from_solver_results,
        )
        solver_results = {"S": mock_diffraction_results}
        data = build_report_data_from_solver_results(solver_results)
        assert "surge" in data.damping_diagonal
        nfreq = len(data.frequencies_rad_s)
        assert len(data.damping_diagonal["surge"]) == nfreq


# ---------------------------------------------------------------------------
# Tests: build_report_data_from_solver_results — owr path delegation
# ---------------------------------------------------------------------------


class TestBuildReportDataOwrPath:
    """Test the owr_path delegation branch."""

    def test_delegates_to_owr_extractor(self, tmp_path, mock_diffraction_results):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            build_report_data_from_solver_results,
        )
        from digitalmodel.hydrodynamics.diffraction.solver import report_extractors as _mod

        mock_d = _mock_diffraction_object()
        owr_path = tmp_path / "test.owr"
        owr_path.touch()

        solver_results = {
            "AQWA": mock_diffraction_results,
            "OrcaWave": mock_diffraction_results,
        }

        with patch.object(_mod, "OrcFxAPI") as fake_api:
            fake_api.Diffraction.return_value = mock_d
            data = build_report_data_from_solver_results(
                solver_results, owr_path=owr_path
            )

        # Should have both solver names
        assert "AQWA" in data.solver_names
        assert "OrcaWave" in data.solver_names

    def test_vessel_name_override_with_owr(self, tmp_path, mock_diffraction_results):
        from digitalmodel.hydrodynamics.diffraction.solver.report_extractors import (
            build_report_data_from_solver_results,
        )
        from digitalmodel.hydrodynamics.diffraction.solver import report_extractors as _mod

        mock_d = _mock_diffraction_object()
        owr_path = tmp_path / "test.owr"
        owr_path.touch()

        solver_results = {"S": mock_diffraction_results}

        with patch.object(_mod, "OrcFxAPI") as fake_api:
            fake_api.Diffraction.return_value = mock_d
            data = build_report_data_from_solver_results(
                solver_results, owr_path=owr_path, vessel_name="Override"
            )

        assert data.vessel_name == "Override"
