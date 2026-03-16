"""
ABOUTME: Tests for design table batch studies and manifold validation --
parametric hull variations, batch hydrostatics, and geometry checks.
"""

from __future__ import annotations

import copy
from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)
from digitalmodel.visualization.design_tools.design_table import DesignTable
from digitalmodel.visualization.design_tools.manifold_check import (
    ManifoldChecker,
)


# -------------------------------------------------------------------
# Fixtures
# -------------------------------------------------------------------


def _make_simple_profile(
    length: float = 100.0,
    beam: float = 20.0,
    draft: float = 8.0,
    depth: float = 12.0,
    n_stations: int = 5,
) -> HullProfile:
    """Build a simple barge-like hull with rectangular sections."""
    half_beam = beam / 2.0
    stations = []
    for i in range(n_stations):
        x = length * i / (n_stations - 1)
        # Taper at bow and stern
        taper = 1.0 - 0.3 * abs(2.0 * i / (n_stations - 1) - 1.0)
        hb = half_beam * taper
        offsets = [
            (0.0, 0.0),
            (draft * 0.25, hb * 0.6),
            (draft * 0.5, hb * 0.85),
            (draft * 0.75, hb * 0.95),
            (draft, hb),
            (depth, hb),
        ]
        stations.append(
            HullStation(x_position=x, waterline_offsets=offsets)
        )
    return HullProfile(
        name="test-hull",
        hull_type=HullType.BARGE,
        stations=stations,
        length_bp=length,
        beam=beam,
        draft=draft,
        depth=depth,
        source="unit-test",
    )


@pytest.fixture
def base_profile() -> HullProfile:
    return _make_simple_profile()


@pytest.fixture
def surface_grid(base_profile: HullProfile) -> np.ndarray:
    """Build a structured surface grid from the base profile."""
    stations = sorted(base_profile.stations, key=lambda s: s.x_position)
    n_st = len(stations)
    n_wl = 6
    z_uniform = np.linspace(0, base_profile.draft, n_wl)
    grid = np.zeros((n_st, n_wl, 3))
    for i, station in enumerate(stations):
        offsets = sorted(station.waterline_offsets, key=lambda o: o[0])
        z_arr = np.array([o[0] for o in offsets if o[0] <= base_profile.draft])
        y_arr = np.array([o[1] for o in offsets if o[0] <= base_profile.draft])
        y_interp = np.interp(z_uniform, z_arr, y_arr)
        grid[i, :, 0] = station.x_position
        grid[i, :, 1] = y_interp
        grid[i, :, 2] = z_uniform
    return grid


# -------------------------------------------------------------------
# TestDesignTable
# -------------------------------------------------------------------


class TestDesignTable:
    """Tests for DesignTable parametric study engine."""

    def test_design_table_from_yaml_config(
        self, base_profile: HullProfile, tmp_path: Path
    ):
        """Load YAML parameter variations, verify correct number of
        combinations."""
        config = {
            "parameters": {
                "length_bp": [90.0, 100.0, 110.0],
                "beam": [18.0, 20.0],
            }
        }
        config_path = tmp_path / "study.yaml"
        with open(config_path, "w") as f:
            yaml.dump(config, f)

        dt = DesignTable.from_yaml(config_path, base_profile)
        variations = dt.generate_variations()
        assert len(variations) == 6  # 3 x 2

    def test_design_table_generates_hull_variations(
        self, base_profile: HullProfile
    ):
        """3 variations produce 3 distinct hull profiles."""
        dt = DesignTable(base_profile)
        dt.add_parameter("length_bp", [90.0, 100.0, 110.0])
        variations = dt.generate_variations()
        assert len(variations) == 3
        assert all(isinstance(v, HullProfile) for v in variations)
        lengths = [v.length_bp for v in variations]
        assert lengths == [90.0, 100.0, 110.0]

    def test_design_table_batch_hydrostatics(
        self, base_profile: HullProfile
    ):
        """Run hydrostatics for all variations, verify results list."""
        dt = DesignTable(base_profile)
        dt.add_parameter("length_bp", [90.0, 100.0, 110.0])
        results = dt.run_batch_hydrostatics(parallel=False)
        assert len(results) == 3
        for r in results:
            assert "displaced_volume" in r
            assert "displacement" in r
            assert "waterplane_area" in r
            assert r["displaced_volume"] > 0

    def test_design_table_results_comparison_yaml(
        self, base_profile: HullProfile, tmp_path: Path
    ):
        """Output comparison as YAML, verify structure."""
        dt = DesignTable(base_profile)
        dt.add_parameter("length_bp", [90.0, 100.0])
        dt.run_batch_hydrostatics(parallel=False)
        out_path = tmp_path / "results.yaml"
        dt.export_results_yaml(out_path)
        assert out_path.exists()
        with open(out_path) as f:
            data = yaml.safe_load(f)
        assert "variations" in data
        assert len(data["variations"]) == 2
        for v in data["variations"]:
            assert "parameters" in v
            assert "hydrostatics" in v

    def test_design_table_parameter_validation(
        self, base_profile: HullProfile
    ):
        """Reject invalid parameter ranges (negative dimensions)."""
        dt = DesignTable(base_profile)
        with pytest.raises(ValueError, match="positive"):
            dt.add_parameter("length_bp", [-10.0, 100.0])

    def test_design_table_cartesian_product(
        self, base_profile: HullProfile
    ):
        """2 lengths x 2 beams = 4 combinations."""
        dt = DesignTable(base_profile)
        dt.add_parameter("length_bp", [90.0, 100.0])
        dt.add_parameter("beam", [18.0, 20.0])
        variations = dt.generate_variations()
        assert len(variations) == 4
        combos = [(v.length_bp, v.beam) for v in variations]
        assert (90.0, 18.0) in combos
        assert (90.0, 20.0) in combos
        assert (100.0, 18.0) in combos
        assert (100.0, 20.0) in combos

    def test_design_table_process_isolation(
        self, base_profile: HullProfile
    ):
        """Each variation is independent (no shared state)."""
        dt = DesignTable(base_profile)
        dt.add_parameter("length_bp", [90.0, 110.0])
        variations = dt.generate_variations()
        # Mutate one — should not affect the other
        variations[0].stations[0].waterline_offsets[0] = (999.0, 999.0)
        assert variations[1].stations[0].waterline_offsets[0] != (
            999.0,
            999.0,
        )


# -------------------------------------------------------------------
# TestManifoldCheck
# -------------------------------------------------------------------


class TestManifoldCheck:
    """Tests for ManifoldChecker geometry validation."""

    def test_manifold_check_valid_hull_passes(
        self, surface_grid: np.ndarray
    ):
        """Valid hull surface passes manifold check."""
        checker = ManifoldChecker(surface_grid)
        report = checker.run_all_checks()
        assert report["pass"]

    def test_manifold_check_detects_open_surface(self):
        """Open surface (NaN gap) fails watertight check."""
        # Create a grid with a gap (NaN values)
        grid = np.zeros((3, 4, 3))
        for i in range(3):
            for j in range(4):
                grid[i, j] = [i * 10.0, float(j), float(j)]
        # Introduce NaN to simulate open boundary
        grid[1, 2, :] = np.nan
        checker = ManifoldChecker(grid)
        assert not checker.check_watertight()

    def test_manifold_check_detects_self_intersection(self):
        """Self-intersecting surface (folded grid) fails."""
        # Create a valid grid then fold one row back on itself
        grid = np.zeros((4, 4, 3))
        for i in range(4):
            for j in range(4):
                grid[i, j] = [i * 10.0, float(j) * 2.0, float(j)]
        # Fold: reverse the y-coordinates of one interior station
        grid[2, :, 1] = grid[2, ::-1, 1]
        checker = ManifoldChecker(grid)
        assert not checker.check_self_intersection()

    def test_manifold_check_report_structure(
        self, surface_grid: np.ndarray
    ):
        """Verify report dict keys."""
        checker = ManifoldChecker(surface_grid)
        report = checker.run_all_checks()
        expected_keys = {
            "watertight",
            "no_self_intersection",
            "normals_consistent",
            "pass",
        }
        assert set(report.keys()) == expected_keys


# -------------------------------------------------------------------
# TestRoundTrip
# -------------------------------------------------------------------


class TestRoundTrip:
    """Tests for hull round-trip validation."""

    def test_round_trip_hull_to_hydrostatics(
        self, base_profile: HullProfile
    ):
        """Hull generation -> hydrostatics -> results consistent."""
        from digitalmodel.visualization.design_tools.hull_hydrostatics import (
            HullHydrostatics,
        )

        # Run hydrostatics directly
        hydro = HullHydrostatics(base_profile)
        direct = hydro.compute_all()

        # Run via design table (single variation = same profile)
        dt = DesignTable(base_profile)
        dt.add_parameter("length_bp", [base_profile.length_bp])
        results = dt.run_batch_hydrostatics(parallel=False)

        assert len(results) == 1
        for key in ("displaced_volume", "displacement", "waterplane_area"):
            assert abs(results[0][key] - direct[key]) < 1e-6

    def test_round_trip_results_serializable(
        self, base_profile: HullProfile, tmp_path: Path
    ):
        """All results can be serialized to YAML."""
        dt = DesignTable(base_profile)
        dt.add_parameter("beam", [18.0, 20.0])
        dt.run_batch_hydrostatics(parallel=False)
        out_path = tmp_path / "roundtrip.yaml"
        result_path = dt.export_results_yaml(out_path)
        assert result_path == out_path

        with open(out_path) as f:
            data = yaml.safe_load(f)
        # Verify it round-trips through YAML
        yaml_str = yaml.dump(data)
        reloaded = yaml.safe_load(yaml_str)
        assert reloaded == data
