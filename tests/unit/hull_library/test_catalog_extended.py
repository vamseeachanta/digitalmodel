"""
ABOUTME: Extended unit tests for hull_library/catalog.py — HullCatalog,
HullCatalogEntry, SeaStateDefinition, MotionResponse, HullVariation,
and the full compute_motions / compute_accelerations pipeline.

TDD approach: pure Python calculations, no OrcaFlex or AQWA license needed.

WRK-149 — catalog.py coverage extension (target: 65%+).
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.hull_library.catalog import (
    HullCatalog,
    HullCatalogEntry,
    HullVariation,
    MotionResponse,
    SeaStateDefinition,
    _trapz,
)
from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
    MeshGeneratorConfig,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)
from digitalmodel.hydrodynamics.models import RAOData


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _barge_profile(name: str = "test_barge") -> HullProfile:
    """Minimal 100 m rectangular barge hull profile."""
    half = 10.0
    stations = [
        HullStation(x_position=0.0, waterline_offsets=[(0.0, half), (5.0, half)]),
        HullStation(x_position=100.0, waterline_offsets=[(0.0, half), (5.0, half)]),
    ]
    return HullProfile(
        name=name,
        hull_type=HullType.BARGE,
        stations=stations,
        length_bp=100.0,
        beam=20.0,
        draft=5.0,
        depth=8.0,
        source="unit-test",
    )


def _flat_rao_data(n_freq: int = 30, n_dir: int = 2) -> RAOData:
    """RAO data with uniform amplitude 0.5 across all DOF/directions."""
    frequencies = np.linspace(0.05, 2.0, n_freq)
    directions = np.array([0.0, 180.0]) if n_dir == 2 else np.array([0.0])
    amplitudes = np.ones((n_freq, n_dir, 6)) * 0.5
    phases = np.zeros((n_freq, n_dir, 6))
    return RAOData(
        frequencies=frequencies,
        directions=directions,
        amplitudes=amplitudes,
        phases=phases,
        vessel_name="TestBarge",
    )


def _simple_sea_state(**kwargs) -> SeaStateDefinition:
    """Default JONSWAP sea state: Hs=3 m, Tp=10 s."""
    defaults = dict(significant_height=3.0, peak_period=10.0)
    defaults.update(kwargs)
    return SeaStateDefinition(**defaults)


# ---------------------------------------------------------------------------
# _trapz helper
# ---------------------------------------------------------------------------


class TestTrapzHelper:
    """Tests for the internal _trapz compatibility wrapper."""

    def test_constant_integrand(self):
        x = np.linspace(0.0, 1.0, 100)
        y = np.ones_like(x)
        result = _trapz(y, x)
        assert result == pytest.approx(1.0, rel=1e-3)

    def test_linear_integrand(self):
        x = np.linspace(0.0, 2.0, 200)
        y = x  # integral = x^2/2 from 0 to 2 = 2
        result = _trapz(y, x)
        assert result == pytest.approx(2.0, rel=1e-3)

    def test_zero_integrand(self):
        x = np.linspace(0.0, 5.0, 50)
        y = np.zeros_like(x)
        result = _trapz(y, x)
        assert result == pytest.approx(0.0, abs=1e-12)

    def test_returns_float(self):
        x = np.array([0.0, 1.0])
        y = np.array([1.0, 1.0])
        assert isinstance(_trapz(y, x), float)


# ---------------------------------------------------------------------------
# SeaStateDefinition tests
# ---------------------------------------------------------------------------


class TestSeaStateDefinition:
    def test_create_default(self):
        ss = SeaStateDefinition(significant_height=4.0, peak_period=12.0)
        assert ss.significant_height == pytest.approx(4.0)
        assert ss.peak_period == pytest.approx(12.0)
        assert ss.spectrum_type == "jonswap"
        assert ss.gamma == pytest.approx(3.3)
        assert ss.heading == pytest.approx(0.0)

    def test_custom_spectrum_type(self):
        ss = SeaStateDefinition(
            significant_height=2.0,
            peak_period=8.0,
            spectrum_type="pierson_moskowitz",
        )
        assert ss.spectrum_type == "pierson_moskowitz"

    def test_custom_gamma(self):
        ss = SeaStateDefinition(
            significant_height=2.0, peak_period=8.0, gamma=5.0
        )
        assert ss.gamma == pytest.approx(5.0)

    def test_custom_heading(self):
        ss = SeaStateDefinition(
            significant_height=2.0, peak_period=8.0, heading=90.0
        )
        assert ss.heading == pytest.approx(90.0)

    def test_zero_hs_raises(self):
        with pytest.raises(Exception):
            SeaStateDefinition(significant_height=0.0, peak_period=10.0)

    def test_negative_hs_raises(self):
        with pytest.raises(Exception):
            SeaStateDefinition(significant_height=-1.0, peak_period=10.0)

    def test_zero_tp_raises(self):
        with pytest.raises(Exception):
            SeaStateDefinition(significant_height=3.0, peak_period=0.0)

    def test_negative_gamma_raises(self):
        with pytest.raises(Exception):
            SeaStateDefinition(
                significant_height=3.0, peak_period=10.0, gamma=-0.5
            )


# ---------------------------------------------------------------------------
# HullVariation tests
# ---------------------------------------------------------------------------


class TestHullVariation:
    def test_create_default_variation(self):
        v = HullVariation(variation_id="baseline")
        assert v.variation_id == "baseline"
        assert v.scale_factors["length"] == pytest.approx(1.0)
        assert v.scale_factors["beam"] == pytest.approx(1.0)
        assert v.scale_factors["draft"] == pytest.approx(1.0)

    def test_custom_scale_factors(self):
        v = HullVariation(
            variation_id="stretched",
            scale_factors={"length": 1.2, "beam": 1.0, "draft": 0.9},
        )
        assert v.scale_factors["length"] == pytest.approx(1.2)
        assert v.scale_factors["draft"] == pytest.approx(0.9)

    def test_variation_id_stored(self):
        v = HullVariation(variation_id="deep_draft")
        assert v.variation_id == "deep_draft"

    def test_mesh_config_is_default(self):
        v = HullVariation(variation_id="x")
        assert isinstance(v.mesh_config, MeshGeneratorConfig)


# ---------------------------------------------------------------------------
# MotionResponse tests
# ---------------------------------------------------------------------------


class TestMotionResponse:
    def test_create_empty_motion_response(self):
        ss = _simple_sea_state()
        mr = MotionResponse(sea_state=ss)
        assert mr.significant_values == {}
        assert mr.accelerations == {}

    def test_significant_values_stored(self):
        ss = _simple_sea_state()
        vals = {"heave_sig": 0.5, "surge_sig": 0.3}
        mr = MotionResponse(sea_state=ss, significant_values=vals)
        assert mr.significant_values["heave_sig"] == pytest.approx(0.5)

    def test_accelerations_stored(self):
        ss = _simple_sea_state()
        accel = {"bow": {"vertical": 1.2, "lateral": 0.3, "longitudinal": 0.5}}
        mr = MotionResponse(sea_state=ss, accelerations=accel)
        assert mr.accelerations["bow"]["vertical"] == pytest.approx(1.2)


# ---------------------------------------------------------------------------
# HullCatalogEntry tests
# ---------------------------------------------------------------------------


class TestHullCatalogEntry:
    def test_create_entry(self):
        profile = _barge_profile()
        entry = HullCatalogEntry(hull_id="barge_001", profile=profile)
        assert entry.hull_id == "barge_001"
        assert entry.profile.name == "test_barge"
        assert entry.variations == []

    def test_variations_list(self):
        profile = _barge_profile()
        v = HullVariation(variation_id="deep")
        entry = HullCatalogEntry(
            hull_id="barge_002", profile=profile, variations=[v]
        )
        assert len(entry.variations) == 1
        assert entry.variations[0].variation_id == "deep"


# ---------------------------------------------------------------------------
# HullCatalog basic registry tests
# ---------------------------------------------------------------------------


class TestHullCatalogRegistry:
    def test_empty_catalog_on_init(self):
        catalog = HullCatalog()
        assert catalog.list_hulls() == []

    def test_register_hull_returns_entry(self):
        catalog = HullCatalog()
        profile = _barge_profile("barge_a")
        entry = catalog.register_hull(profile)
        assert isinstance(entry, HullCatalogEntry)
        assert entry.hull_id == "barge_a"

    def test_list_hulls_after_register(self):
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("hull_1"))
        catalog.register_hull(_barge_profile("hull_2"))
        hulls = catalog.list_hulls()
        assert "hull_1" in hulls
        assert "hull_2" in hulls

    def test_list_hulls_sorted(self):
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("zebra"))
        catalog.register_hull(_barge_profile("alpha"))
        catalog.register_hull(_barge_profile("mango"))
        hulls = catalog.list_hulls()
        assert hulls == sorted(hulls)

    def test_get_hull_existing(self):
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("hull_x"))
        entry = catalog.get_hull("hull_x")
        assert entry.hull_id == "hull_x"

    def test_get_hull_missing_raises_key_error(self):
        catalog = HullCatalog()
        with pytest.raises(KeyError):
            catalog.get_hull("nonexistent_hull")

    def test_key_error_message_contains_hull_id(self):
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("existing_hull"))
        with pytest.raises(KeyError, match="missing_hull"):
            catalog.get_hull("missing_hull")

    def test_register_overwrites_same_id(self):
        catalog = HullCatalog()
        p1 = _barge_profile("hull_a")
        p2 = _barge_profile("hull_a")
        catalog.register_hull(p1)
        catalog.register_hull(p2)
        assert len(catalog.list_hulls()) == 1

    def test_register_hull_id_from_profile_name(self):
        catalog = HullCatalog()
        profile = _barge_profile("named_hull")
        entry = catalog.register_hull(profile)
        assert entry.hull_id == "named_hull"
        assert catalog.get_hull("named_hull") is entry


# ---------------------------------------------------------------------------
# HullCatalog loading from directory tests
# ---------------------------------------------------------------------------


class TestHullCatalogLoadProfiles:
    def test_load_profiles_from_dir(self, tmp_path):
        profile = _barge_profile("loaded_barge")
        yaml_file = tmp_path / "loaded_barge.yaml"
        profile.save_yaml(yaml_file)

        catalog = HullCatalog(profiles_dir=tmp_path)
        assert "loaded_barge" in catalog.list_hulls()

    def test_load_multiple_profiles(self, tmp_path):
        for name in ["barge_alpha", "barge_beta", "barge_gamma"]:
            _barge_profile(name).save_yaml(tmp_path / f"{name}.yaml")

        catalog = HullCatalog(profiles_dir=tmp_path)
        hulls = catalog.list_hulls()
        assert len(hulls) == 3

    def test_missing_profiles_dir_ignored(self, tmp_path):
        missing = tmp_path / "does_not_exist"
        catalog = HullCatalog(profiles_dir=missing)
        assert catalog.list_hulls() == []

    def test_none_profiles_dir_gives_empty(self):
        catalog = HullCatalog(profiles_dir=None)
        assert catalog.list_hulls() == []

    def test_malformed_yaml_skipped(self, tmp_path):
        bad_yaml = tmp_path / "bad.yaml"
        bad_yaml.write_text("this: is: not: valid: hull: yaml\n")
        catalog = HullCatalog(profiles_dir=tmp_path)
        # Bad file should not raise; it should be skipped
        assert "bad" not in catalog.list_hulls()

    def test_load_ignores_non_yaml_files(self, tmp_path):
        profile = _barge_profile("real_hull")
        profile.save_yaml(tmp_path / "real_hull.yaml")
        (tmp_path / "readme.txt").write_text("ignore me")
        (tmp_path / "notes.md").write_text("# notes")

        catalog = HullCatalog(profiles_dir=tmp_path)
        assert catalog.list_hulls() == ["real_hull"]


# ---------------------------------------------------------------------------
# HullCatalog compute_motions tests
# ---------------------------------------------------------------------------


class TestHullCatalogComputeMotions:
    def _setup(self):
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("barge_motion"))
        rao_data = _flat_rao_data()
        sea_state = _simple_sea_state()
        return catalog, rao_data, sea_state

    def test_compute_motions_returns_motion_response(self):
        catalog, rao_data, sea_state = self._setup()
        result = catalog.compute_motions("barge_motion", sea_state, rao_data)
        assert isinstance(result, MotionResponse)

    def test_compute_motions_has_all_six_dof(self):
        catalog, rao_data, sea_state = self._setup()
        result = catalog.compute_motions("barge_motion", sea_state, rao_data)
        dof_names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]
        for dof in dof_names:
            assert f"{dof}_sig" in result.significant_values

    def test_compute_motions_significant_values_non_negative(self):
        catalog, rao_data, sea_state = self._setup()
        result = catalog.compute_motions("barge_motion", sea_state, rao_data)
        for val in result.significant_values.values():
            assert val >= 0.0

    def test_compute_motions_significant_values_finite(self):
        catalog, rao_data, sea_state = self._setup()
        result = catalog.compute_motions("barge_motion", sea_state, rao_data)
        for val in result.significant_values.values():
            assert math.isfinite(val)

    def test_compute_motions_sea_state_preserved(self):
        catalog, rao_data, sea_state = self._setup()
        result = catalog.compute_motions("barge_motion", sea_state, rao_data)
        assert result.sea_state.significant_height == pytest.approx(
            sea_state.significant_height
        )

    def test_compute_motions_missing_hull_raises(self):
        catalog = HullCatalog()
        rao_data = _flat_rao_data()
        sea_state = _simple_sea_state()
        with pytest.raises(KeyError):
            catalog.compute_motions("ghost_hull", sea_state, rao_data)

    def test_compute_motions_higher_hs_more_response(self):
        """Higher Hs should produce larger significant motion values."""
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("barge_hs"))
        rao_data = _flat_rao_data()

        result_low = catalog.compute_motions(
            "barge_hs", _simple_sea_state(significant_height=1.0), rao_data
        )
        result_high = catalog.compute_motions(
            "barge_hs", _simple_sea_state(significant_height=4.0), rao_data
        )
        # heave_sig should scale with Hs (roughly Hs^1 for given RAO)
        assert (
            result_high.significant_values["heave_sig"]
            > result_low.significant_values["heave_sig"]
        )

    def test_compute_motions_bretschneider_spectrum(self):
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("barge_bs"))
        rao_data = _flat_rao_data()
        sea_state = SeaStateDefinition(
            significant_height=3.0,
            peak_period=10.0,
            spectrum_type="bretschneider",
        )
        result = catalog.compute_motions("barge_bs", sea_state, rao_data)
        assert isinstance(result, MotionResponse)
        for val in result.significant_values.values():
            assert val >= 0.0

    def test_compute_motions_pm_spectrum(self):
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("barge_pm"))
        rao_data = _flat_rao_data()
        sea_state = SeaStateDefinition(
            significant_height=3.0,
            peak_period=10.0,
            spectrum_type="pierson_moskowitz",
        )
        result = catalog.compute_motions("barge_pm", sea_state, rao_data)
        assert isinstance(result, MotionResponse)

    def test_compute_motions_heading_selection(self):
        """180-degree heading should select second direction in RAO data."""
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("barge_hdg"))

        # Create RAO with different amplitudes per direction
        n_freq = 30
        frequencies = np.linspace(0.05, 2.0, n_freq)
        directions = np.array([0.0, 180.0])
        amplitudes = np.zeros((n_freq, 2, 6))
        amplitudes[:, 0, :] = 0.3   # head seas
        amplitudes[:, 1, :] = 0.8   # following seas
        phases = np.zeros((n_freq, 2, 6))
        rao_data = RAOData(
            frequencies=frequencies,
            directions=directions,
            amplitudes=amplitudes,
            phases=phases,
        )

        result_head = catalog.compute_motions(
            "barge_hdg",
            SeaStateDefinition(
                significant_height=3.0, peak_period=10.0, heading=0.0
            ),
            rao_data,
        )
        result_following = catalog.compute_motions(
            "barge_hdg",
            SeaStateDefinition(
                significant_height=3.0, peak_period=10.0, heading=180.0
            ),
            rao_data,
        )
        # Following seas have higher RAO amplitudes → bigger motion
        assert (
            result_following.significant_values["heave_sig"]
            > result_head.significant_values["heave_sig"]
        )


# ---------------------------------------------------------------------------
# HullCatalog compute_accelerations tests
# ---------------------------------------------------------------------------


class TestHullCatalogComputeAccelerations:
    def _setup(self):
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("barge_acc"))
        rao_data = _flat_rao_data()
        sea_state = _simple_sea_state()
        return catalog, rao_data, sea_state

    def test_compute_accelerations_returns_dict(self):
        catalog, rao_data, sea_state = self._setup()
        result = catalog.compute_accelerations(
            "barge_acc", sea_state, rao_data, point=(50.0, 0.0, 10.0)
        )
        assert isinstance(result, dict)

    def test_compute_accelerations_has_three_components(self):
        catalog, rao_data, sea_state = self._setup()
        result = catalog.compute_accelerations(
            "barge_acc", sea_state, rao_data, point=(50.0, 0.0, 10.0)
        )
        assert "vertical" in result
        assert "lateral" in result
        assert "longitudinal" in result

    def test_compute_accelerations_non_negative(self):
        catalog, rao_data, sea_state = self._setup()
        result = catalog.compute_accelerations(
            "barge_acc", sea_state, rao_data, point=(50.0, 0.0, 10.0)
        )
        for val in result.values():
            assert val >= 0.0

    def test_compute_accelerations_finite(self):
        catalog, rao_data, sea_state = self._setup()
        result = catalog.compute_accelerations(
            "barge_acc", sea_state, rao_data, point=(50.0, 0.0, 10.0)
        )
        for val in result.values():
            assert math.isfinite(val)

    def test_compute_accelerations_missing_hull_raises(self):
        catalog = HullCatalog()
        rao_data = _flat_rao_data()
        sea_state = _simple_sea_state()
        with pytest.raises(KeyError):
            catalog.compute_accelerations(
                "ghost_hull", sea_state, rao_data, point=(0.0, 0.0, 0.0)
            )

    def test_compute_accelerations_cog_at_point_no_lever(self):
        """When point == COG, lever arms are zero; result remains non-negative."""
        catalog, rao_data, sea_state = self._setup()
        cog = (50.0, 0.0, 5.0)
        result = catalog.compute_accelerations(
            "barge_acc",
            sea_state,
            rao_data,
            point=cog,
            cog=cog,
        )
        for val in result.values():
            assert val >= 0.0

    def test_compute_accelerations_large_lever_arm(self):
        """Large lever arm (e.g. bow of long vessel) should give larger or equal
        accelerations compared to COG (rotational contribution adds energy)."""
        catalog, rao_data, sea_state = self._setup()
        result_cog = catalog.compute_accelerations(
            "barge_acc",
            sea_state,
            rao_data,
            point=(50.0, 0.0, 5.0),
            cog=(50.0, 0.0, 5.0),
        )
        result_bow = catalog.compute_accelerations(
            "barge_acc",
            sea_state,
            rao_data,
            point=(100.0, 0.0, 10.0),
            cog=(50.0, 0.0, 5.0),
        )
        # Vertical acceleration at bow >= COG (rotational terms add contribution)
        assert result_bow["vertical"] >= result_cog["vertical"] - 1e-9

    def test_compute_accelerations_higher_hs_more_acceleration(self):
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("barge_hs_acc"))
        rao_data = _flat_rao_data()

        result_low = catalog.compute_accelerations(
            "barge_hs_acc",
            _simple_sea_state(significant_height=1.0),
            rao_data,
            point=(50.0, 0.0, 5.0),
        )
        result_high = catalog.compute_accelerations(
            "barge_hs_acc",
            _simple_sea_state(significant_height=4.0),
            rao_data,
            point=(50.0, 0.0, 5.0),
        )
        assert result_high["vertical"] > result_low["vertical"]


# ---------------------------------------------------------------------------
# HullCatalog._interpolate_rao static method tests
# ---------------------------------------------------------------------------


class TestInterpolateRao:
    def test_exact_match_frequencies(self):
        freq = np.array([0.1, 0.5, 1.0, 1.5])
        amp = np.array([1.0, 2.0, 1.5, 0.5])
        result = HullCatalog._interpolate_rao(freq, amp, freq)
        np.testing.assert_allclose(result, amp, atol=1e-10)

    def test_interpolated_midpoint(self):
        freq = np.array([0.0, 1.0])
        amp = np.array([0.0, 1.0])
        result = HullCatalog._interpolate_rao(freq, amp, np.array([0.5]))
        assert result[0] == pytest.approx(0.5)

    def test_outside_range_fills_zero(self):
        freq = np.array([0.5, 1.0])
        amp = np.array([1.0, 1.0])
        # Request far outside range
        result = HullCatalog._interpolate_rao(freq, amp, np.array([5.0]))
        assert result[0] == pytest.approx(0.0, abs=1e-10)

    def test_below_range_fills_zero(self):
        freq = np.array([0.5, 1.0])
        amp = np.array([1.0, 1.0])
        result = HullCatalog._interpolate_rao(freq, amp, np.array([0.01]))
        assert result[0] == pytest.approx(0.0, abs=1e-10)

    def test_output_length_matches_target(self):
        freq = np.linspace(0.1, 2.0, 20)
        amp = np.ones(20)
        target = np.linspace(0.05, 2.5, 50)
        result = HullCatalog._interpolate_rao(freq, amp, target)
        assert len(result) == 50


# ---------------------------------------------------------------------------
# HullCatalog._generate_spectrum private helper tests (via compute_motions)
# ---------------------------------------------------------------------------


class TestGenerateSpectrumHelper:
    """Exercise _generate_spectrum indirectly via compute_motions."""

    def test_jonswap_spectrum_generated(self):
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("spec_hull"))
        rao = _flat_rao_data()
        ss = SeaStateDefinition(
            significant_height=3.0,
            peak_period=10.0,
            spectrum_type="jonswap",
            gamma=3.3,
        )
        result = catalog.compute_motions("spec_hull", ss, rao)
        assert result.significant_values["heave_sig"] > 0.0

    def test_issc_spectrum_generated(self):
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("issc_hull"))
        rao = _flat_rao_data()
        ss = SeaStateDefinition(
            significant_height=2.5,
            peak_period=9.0,
            spectrum_type="issc",
        )
        result = catalog.compute_motions("issc_hull", ss, rao)
        for val in result.significant_values.values():
            assert val >= 0.0

    def test_unknown_spectrum_type_raises(self):
        catalog = HullCatalog()
        catalog.register_hull(_barge_profile("bad_spec_hull"))
        rao = _flat_rao_data()
        ss = SeaStateDefinition(
            significant_height=2.0,
            peak_period=8.0,
            spectrum_type="unknown_spectrum",
        )
        with pytest.raises(Exception):
            catalog.compute_motions("bad_spec_hull", ss, rao)
