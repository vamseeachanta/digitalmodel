"""
Tests for parametric_hull_analysis.passing_ship_sweep module.

Covers hull_profile_to_vessel_config conversion, run_passing_ship_sweep,
passing_ship_to_dataframe, peak_force_envelope, pianc_operability_check,
_parse_variation_id, plus physics sanity and edge cases.
"""

from __future__ import annotations

from dataclasses import dataclass
from types import SimpleNamespace
from unittest.mock import MagicMock, patch

import numpy as np
import pandas as pd
import pytest

from digitalmodel.hydrodynamics.parametric_hull_analysis.models import (
    DepthClassification,
    PassingShipSweepConfig,
    PassingShipSweepEntry,
    classify_depth,
)
from digitalmodel.hydrodynamics.parametric_hull_analysis.passing_ship_sweep import (
    _parse_variation_id,
    hull_profile_to_vessel_config,
    passing_ship_to_dataframe,
    peak_force_envelope,
    pianc_operability_check,
    run_passing_ship_sweep,
)


# ---------------------------------------------------------------------------
# Helpers – lightweight stand-ins for HullProfile and VesselConfig
# ---------------------------------------------------------------------------

def _make_hull_profile(
    length_bp: float = 200.0,
    beam: float = 32.0,
    draft: float = 12.0,
    block_coefficient: float = 0.82,
    name: str = "test_hull",
) -> SimpleNamespace:
    """Create a minimal object that quacks like HullProfile."""
    return SimpleNamespace(
        length_bp=length_bp,
        beam=beam,
        draft=draft,
        block_coefficient=block_coefficient,
        name=name,
    )


def _make_sweep_entries(
    n_sep: int = 3,
    n_speed: int = 3,
    variation_id: str = "base__beam=32.0",
    water_depth: float = 15.0,
    draft: float = 12.0,
) -> list[PassingShipSweepEntry]:
    """Generate synthetic PassingShipSweepEntry list."""
    seps = np.linspace(30, 100, n_sep)
    speeds = np.linspace(1, 5, n_speed)
    entries = []
    for sep in seps:
        for speed in speeds:
            # Force ~ speed^2 / sep  (simple physics-inspired)
            surge = 1000 * speed**2 / sep
            sway = 2000 * speed**2 / sep
            yaw = 5000 * speed**2 / sep
            entries.append(
                PassingShipSweepEntry(
                    variation_id=variation_id,
                    hull_params={"beam": 32.0},
                    separation_m=float(sep),
                    speed_ms=float(speed),
                    water_depth_m=water_depth,
                    peak_surge_N=surge,
                    peak_sway_N=sway,
                    peak_yaw_Nm=yaw,
                    depth_class=classify_depth(water_depth, draft),
                )
            )
    return entries


# ===================================================================
# Tests for _parse_variation_id
# ===================================================================


class TestParseVariationId:
    """Tests for the private _parse_variation_id helper."""

    def test_simple_variation_id(self):
        params = _parse_variation_id("base__beam=32.0_draft=12.0")
        assert params == {"beam": 32.0, "draft": 12.0}

    def test_no_double_underscore(self):
        """No '__' separator means no parseable params."""
        params = _parse_variation_id("base_hull_v1")
        assert params == {}

    def test_single_param(self):
        params = _parse_variation_id("hull__length=200")
        assert params == {"length": 200.0}

    def test_non_numeric_param_ignored(self):
        params = _parse_variation_id("hull__cb=high_beam=40")
        # 'cb=high' should be skipped (non-numeric), beam=40 kept
        assert "cb" not in params
        assert params["beam"] == 40.0

    def test_empty_string(self):
        params = _parse_variation_id("")
        assert params == {}


# ===================================================================
# Tests for hull_profile_to_vessel_config
# ===================================================================


class TestHullProfileToVesselConfig:
    """Tests for hull_profile_to_vessel_config conversion."""

    def test_basic_conversion(self):
        profile = _make_hull_profile()
        vc = hull_profile_to_vessel_config(profile)
        assert vc.length == 200.0
        assert vc.beam == 32.0
        assert vc.draft == 12.0
        assert vc.block_coefficient == 0.82
        assert vc.name == "test_hull"

    def test_none_block_coefficient_defaults(self):
        """When block_coefficient is None, should default to 0.80."""
        profile = _make_hull_profile(block_coefficient=None)
        vc = hull_profile_to_vessel_config(profile)
        assert vc.block_coefficient == pytest.approx(0.80)

    def test_returned_type(self):
        from digitalmodel.hydrodynamics.passing_ship.configuration import VesselConfig

        profile = _make_hull_profile()
        vc = hull_profile_to_vessel_config(profile)
        assert isinstance(vc, VesselConfig)


# ===================================================================
# Tests for passing_ship_to_dataframe
# ===================================================================


class TestPassingShipToDataframe:
    """Tests for passing_ship_to_dataframe."""

    def test_returns_dataframe(self):
        entries = _make_sweep_entries(n_sep=2, n_speed=2)
        df = passing_ship_to_dataframe(entries)
        assert isinstance(df, pd.DataFrame)

    def test_column_names(self):
        entries = _make_sweep_entries(n_sep=2, n_speed=2)
        df = passing_ship_to_dataframe(entries)
        expected = {
            "variation_id", "separation_m", "speed_ms",
            "water_depth_m", "peak_surge_N", "peak_sway_N",
            "peak_yaw_Nm", "depth_class",
        }
        assert expected.issubset(set(df.columns))

    def test_row_count(self):
        entries = _make_sweep_entries(n_sep=3, n_speed=4)
        df = passing_ship_to_dataframe(entries)
        assert len(df) == 12

    def test_hull_params_as_columns(self):
        """Hull params should appear as param_<key> columns."""
        entries = _make_sweep_entries()
        df = passing_ship_to_dataframe(entries)
        assert "param_beam" in df.columns

    def test_empty_input(self):
        df = passing_ship_to_dataframe([])
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 0


# ===================================================================
# Tests for peak_force_envelope
# ===================================================================


class TestPeakForceEnvelope:
    """Tests for peak_force_envelope."""

    def test_returns_three_components(self):
        entries = _make_sweep_entries(n_sep=3, n_speed=3)
        env = peak_force_envelope(entries, "base__beam=32.0")
        assert set(env.keys()) == {"surge", "sway", "yaw"}

    def test_pivot_shape(self):
        entries = _make_sweep_entries(n_sep=3, n_speed=4)
        env = peak_force_envelope(entries, "base__beam=32.0")
        # Each pivot should be (n_sep x n_speed)
        assert env["sway"].shape == (3, 4)

    def test_empty_for_nonexistent_variant(self):
        entries = _make_sweep_entries()
        env = peak_force_envelope(entries, "nonexistent__x=0")
        for component in ("surge", "sway", "yaw"):
            assert env[component].empty

    def test_values_are_positive(self):
        """Our synthetic forces are positive; envelope maxes should be too."""
        entries = _make_sweep_entries(n_sep=3, n_speed=3)
        env = peak_force_envelope(entries, "base__beam=32.0")
        assert (env["sway"].values > 0).all()


# ===================================================================
# Tests for pianc_operability_check
# ===================================================================


class TestPiancOperabilityCheck:
    """Tests for pianc_operability_check."""

    def test_all_acceptable(self):
        """With very generous thresholds, all should pass."""
        entries = _make_sweep_entries(n_sep=2, n_speed=2)
        df = pianc_operability_check(entries, max_sway_N=1e10, max_yaw_Nm=1e10)
        assert df["acceptable"].all()

    def test_none_acceptable(self):
        """With impossibly tight thresholds, none should pass."""
        entries = _make_sweep_entries(n_sep=2, n_speed=2)
        df = pianc_operability_check(entries, max_sway_N=0.001, max_yaw_Nm=0.001)
        assert not df["acceptable"].any()

    def test_partial_acceptability(self):
        """Some pass, some fail with intermediate thresholds."""
        entries = _make_sweep_entries(n_sep=3, n_speed=3)
        # Pick thresholds that allow slow-speed/far-separation combos
        df = pianc_operability_check(entries, max_sway_N=50, max_yaw_Nm=200)
        assert not df["acceptable"].all()
        assert df["acceptable"].any()

    def test_has_acceptable_column(self):
        entries = _make_sweep_entries()
        df = pianc_operability_check(entries, max_sway_N=1e6, max_yaw_Nm=1e6)
        assert "acceptable" in df.columns
        assert df["acceptable"].dtype == bool


# ===================================================================
# Tests for classify_depth (models utility, used in sweep)
# ===================================================================


class TestClassifyDepth:
    """Tests for depth classification used by the sweep module."""

    def test_deep_water(self):
        assert classify_depth(50.0, 10.0) == DepthClassification.DEEP

    def test_medium_water(self):
        assert classify_depth(25.0, 10.0) == DepthClassification.MEDIUM

    def test_shallow_water(self):
        assert classify_depth(17.0, 10.0) == DepthClassification.SHALLOW

    def test_very_shallow_water(self):
        assert classify_depth(14.0, 10.0) == DepthClassification.VERY_SHALLOW

    def test_infinite_depth(self):
        assert classify_depth(float("inf"), 10.0) == DepthClassification.DEEP


# ===================================================================
# Tests for run_passing_ship_sweep (with mocks)
# ===================================================================


class TestRunPassingShipSweep:
    """Tests for run_passing_ship_sweep using mocked calculator.

    The function under test uses lazy (in-function) imports, so we patch
    at the *source* module path, not the parametric_hull_analysis module.
    """

    @patch("digitalmodel.hydrodynamics.passing_ship.force_time_history.generate_time_history")
    @patch("digitalmodel.hydrodynamics.passing_ship.calculator.PassingShipCalculator")
    @patch("digitalmodel.hydrodynamics.passing_ship.configuration.EnvironmentalConfig")
    def test_basic_sweep_returns_results(
        self, mock_env, mock_calc_cls, mock_gen_th
    ):
        """Integration-level: sweep returns correct number of entries."""
        fth = SimpleNamespace(peak_surge=100.0, peak_sway=200.0, peak_yaw=300.0)
        mock_gen_th.return_value = fth

        hull_variants = [
            ("base__beam=32.0", _make_hull_profile(beam=32.0)),
        ]
        passing_vessel = MagicMock()
        config = PassingShipSweepConfig(
            separations_m=[50.0, 100.0],
            speeds_ms=[2.0, 4.0],
            water_depths_m=[15.0],
        )
        results = run_passing_ship_sweep(hull_variants, passing_vessel, config)
        assert len(results) == 4  # 2 seps x 2 speeds x 1 depth
        assert all(isinstance(r, PassingShipSweepEntry) for r in results)

    @patch("digitalmodel.hydrodynamics.passing_ship.force_time_history.generate_time_history")
    @patch("digitalmodel.hydrodynamics.passing_ship.calculator.PassingShipCalculator")
    @patch("digitalmodel.hydrodynamics.passing_ship.configuration.EnvironmentalConfig")
    def test_sweep_stores_peak_forces(
        self, mock_env, mock_calc_cls, mock_gen_th
    ):
        fth = SimpleNamespace(peak_surge=111.0, peak_sway=222.0, peak_yaw=333.0)
        mock_gen_th.return_value = fth

        hull_variants = [
            ("base__beam=32.0", _make_hull_profile()),
        ]
        config = PassingShipSweepConfig(
            separations_m=[50.0],
            speeds_ms=[3.0],
            water_depths_m=[15.0],
        )
        results = run_passing_ship_sweep(hull_variants, MagicMock(), config)
        assert results[0].peak_surge_N == pytest.approx(111.0)
        assert results[0].peak_sway_N == pytest.approx(222.0)
        assert results[0].peak_yaw_Nm == pytest.approx(333.0)

    @patch("digitalmodel.hydrodynamics.passing_ship.force_time_history.generate_time_history")
    @patch("digitalmodel.hydrodynamics.passing_ship.calculator.PassingShipCalculator")
    @patch("digitalmodel.hydrodynamics.passing_ship.configuration.EnvironmentalConfig")
    def test_sweep_handles_calculation_failure(
        self, mock_env, mock_calc_cls, mock_gen_th
    ):
        """When generate_time_history throws, result should have NaN forces."""
        mock_gen_th.side_effect = RuntimeError("solver diverged")

        hull_variants = [
            ("base__beam=32.0", _make_hull_profile()),
        ]
        config = PassingShipSweepConfig(
            separations_m=[50.0],
            speeds_ms=[3.0],
            water_depths_m=[15.0],
        )
        results = run_passing_ship_sweep(hull_variants, MagicMock(), config)
        assert len(results) == 1
        assert np.isnan(results[0].peak_surge_N)
        assert np.isnan(results[0].peak_sway_N)
        assert np.isnan(results[0].peak_yaw_Nm)


# ===================================================================
# Physics sanity tests
# ===================================================================


class TestPhysicsSanity:
    """Physics-based sanity checks on the synthetic sweep data."""

    def test_force_increases_with_speed(self):
        """At constant separation, peak sway should increase with speed."""
        entries = _make_sweep_entries(n_sep=1, n_speed=5)
        forces = [(e.speed_ms, e.peak_sway_N) for e in entries]
        forces.sort(key=lambda x: x[0])
        for i in range(len(forces) - 1):
            assert forces[i][1] < forces[i + 1][1], (
                "Sway force should increase with speed"
            )

    def test_force_decreases_with_separation(self):
        """At constant speed, peak sway should decrease with separation."""
        entries = _make_sweep_entries(n_sep=5, n_speed=1)
        forces = [(e.separation_m, e.peak_sway_N) for e in entries]
        forces.sort(key=lambda x: x[0])
        for i in range(len(forces) - 1):
            assert forces[i][1] > forces[i + 1][1], (
                "Sway force should decrease with separation"
            )

    def test_zero_speed_zero_force(self):
        """At zero passing speed, all forces should be zero."""
        entry = PassingShipSweepEntry(
            variation_id="base__beam=32.0",
            hull_params={"beam": 32.0},
            separation_m=50.0,
            speed_ms=0.0,
            water_depth_m=15.0,
            peak_surge_N=0.0,
            peak_sway_N=0.0,
            peak_yaw_Nm=0.0,
            depth_class=DepthClassification.SHALLOW,
        )
        assert entry.peak_surge_N == 0.0
        assert entry.peak_sway_N == 0.0
        assert entry.peak_yaw_Nm == 0.0

    def test_large_separation_small_force(self):
        """At very large separation, forces should be negligible."""
        entries = _make_sweep_entries(n_sep=1, n_speed=1)
        # Manually create entry with huge separation
        big_sep = PassingShipSweepEntry(
            variation_id="base__beam=32.0",
            hull_params={"beam": 32.0},
            separation_m=1e6,
            speed_ms=5.0,
            water_depth_m=15.0,
            peak_surge_N=1000 * 25 / 1e6,  # ~0.025 N
            peak_sway_N=2000 * 25 / 1e6,
            peak_yaw_Nm=5000 * 25 / 1e6,
            depth_class=DepthClassification.SHALLOW,
        )
        assert abs(big_sep.peak_sway_N) < 1.0
        assert abs(big_sep.peak_yaw_Nm) < 1.0

    def test_yaw_scales_with_speed_squared(self):
        """In our synthetic model, yaw ~ speed^2 / sep."""
        entries = _make_sweep_entries(n_sep=1, n_speed=3)
        sep = entries[0].separation_m
        for e in entries:
            expected = 5000 * e.speed_ms**2 / sep
            assert e.peak_yaw_Nm == pytest.approx(expected, rel=1e-6)
