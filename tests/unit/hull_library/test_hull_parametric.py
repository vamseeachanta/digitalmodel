"""
ABOUTME: Unit tests for hull_library profile_schema and lookup modules.

TDD approach: tests cover HullType enum, HullStation validation,
HullProfile construction/serialisation, and HullLookup nearest-neighbour
matching — all without any license-dependent tools.

WRK-149 — hull_library parametric hull form tests.
"""

from __future__ import annotations

import math
import tempfile
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)
from digitalmodel.hydrodynamics.hull_library.lookup import (
    HullLookup,
    HullLookupTarget,
    HullMatch,
    _distance_to_score,
    _normalised_distance,
    _scaling_factors,
    _validate_target,
    get_hull_form,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _simple_station(x: float, beam: float = 20.0, draft: float = 10.0) -> HullStation:
    """Build a rectangular cross-section station."""
    half = beam / 2.0
    return HullStation(
        x_position=x,
        waterline_offsets=[(0.0, half), (draft, half)],
    )


def _barge_profile(
    name: str = "test_barge",
    length: float = 100.0,
    beam: float = 20.0,
    draft: float = 5.0,
    depth: float = 8.0,
) -> HullProfile:
    """Minimal rectangular barge hull profile for testing."""
    half = beam / 2.0
    stations = [
        HullStation(
            x_position=0.0,
            waterline_offsets=[(0.0, half), (draft, half)],
        ),
        HullStation(
            x_position=length,
            waterline_offsets=[(0.0, half), (draft, half)],
        ),
    ]
    return HullProfile(
        name=name,
        hull_type=HullType.BARGE,
        stations=stations,
        length_bp=length,
        beam=beam,
        draft=draft,
        depth=depth,
        source="unit-test",
    )


# ---------------------------------------------------------------------------
# HullType tests
# ---------------------------------------------------------------------------


class TestHullType:
    """Verify HullType enum membership and string values."""

    def test_tanker_value(self):
        assert HullType.TANKER.value == "tanker"

    def test_fpso_value(self):
        assert HullType.FPSO.value == "fpso"

    def test_barge_value(self):
        assert HullType.BARGE.value == "barge"

    def test_spar_value(self):
        assert HullType.SPAR.value == "spar"

    def test_semi_pontoon_value(self):
        assert HullType.SEMI_PONTOON.value == "semi_pontoon"

    def test_cylinder_value(self):
        assert HullType.CYLINDER.value == "cylinder"

    def test_custom_value(self):
        assert HullType.CUSTOM.value == "custom"

    def test_lngc_value(self):
        assert HullType.LNGC.value == "lngc"

    def test_hull_type_from_string(self):
        ht = HullType("tanker")
        assert ht == HullType.TANKER

    def test_hull_type_is_str(self):
        # HullType inherits from str
        assert isinstance(HullType.BARGE, str)


# ---------------------------------------------------------------------------
# HullStation tests
# ---------------------------------------------------------------------------


class TestHullStation:
    """Validate HullStation construction and input validation."""

    def test_create_valid_station(self):
        st = HullStation(
            x_position=50.0,
            waterline_offsets=[(0.0, 10.0), (5.0, 10.0)],
        )
        assert st.x_position == pytest.approx(50.0)
        assert len(st.waterline_offsets) == 2

    def test_empty_waterline_offsets_raises(self):
        with pytest.raises(ValueError, match="waterline_offsets"):
            HullStation(x_position=0.0, waterline_offsets=[])

    def test_negative_z_raises(self):
        with pytest.raises(ValueError, match="non-negative"):
            HullStation(
                x_position=0.0,
                waterline_offsets=[(-1.0, 5.0)],
            )

    def test_negative_y_raises(self):
        with pytest.raises(ValueError, match="non-negative"):
            HullStation(
                x_position=0.0,
                waterline_offsets=[(0.0, -1.0)],
            )

    def test_zero_values_allowed(self):
        st = HullStation(
            x_position=0.0,
            waterline_offsets=[(0.0, 0.0)],
        )
        assert st.waterline_offsets[0] == (0.0, 0.0)

    def test_single_offset_pair_allowed(self):
        st = HullStation(
            x_position=10.0,
            waterline_offsets=[(2.0, 5.0)],
        )
        assert len(st.waterline_offsets) == 1

    def test_many_offset_pairs(self):
        offsets = [(float(i), float(i)) for i in range(10)]
        st = HullStation(x_position=0.0, waterline_offsets=offsets)
        assert len(st.waterline_offsets) == 10

    def test_negative_x_position_allowed(self):
        """x_position can be negative (before aft perpendicular)."""
        st = HullStation(
            x_position=-5.0,
            waterline_offsets=[(0.0, 3.0)],
        )
        assert st.x_position == pytest.approx(-5.0)


# ---------------------------------------------------------------------------
# HullProfile construction tests
# ---------------------------------------------------------------------------


class TestHullProfileConstruction:
    """Test valid and invalid HullProfile construction."""

    def test_minimal_valid_barge(self):
        profile = _barge_profile()
        assert profile.name == "test_barge"
        assert profile.hull_type == HullType.BARGE
        assert profile.length_bp == pytest.approx(100.0)
        assert profile.beam == pytest.approx(20.0)
        assert profile.draft == pytest.approx(5.0)
        assert profile.depth == pytest.approx(8.0)

    def test_requires_at_least_two_stations(self):
        station = _simple_station(0.0)
        with pytest.raises(ValueError, match="[Aa]t least two"):
            HullProfile(
                name="single_station",
                hull_type=HullType.BARGE,
                stations=[station],
                length_bp=100.0,
                beam=20.0,
                draft=5.0,
                depth=8.0,
                source="test",
            )

    def test_zero_length_raises(self):
        with pytest.raises(ValueError):
            _barge_profile(length=0.0)

    def test_negative_beam_raises(self):
        with pytest.raises(ValueError):
            _barge_profile(beam=-5.0)

    def test_negative_draft_raises(self):
        with pytest.raises(ValueError):
            _barge_profile(draft=-1.0)

    def test_negative_depth_raises(self):
        with pytest.raises(ValueError):
            _barge_profile(depth=-1.0)

    def test_block_coefficient_zero_raises(self):
        half = 10.0
        stations = [
            HullStation(x_position=0.0, waterline_offsets=[(0.0, half)]),
            HullStation(x_position=100.0, waterline_offsets=[(0.0, half)]),
        ]
        with pytest.raises(ValueError, match="block_coefficient"):
            HullProfile(
                name="bad_cb",
                hull_type=HullType.BARGE,
                stations=stations,
                length_bp=100.0,
                beam=20.0,
                draft=5.0,
                depth=8.0,
                source="test",
                block_coefficient=0.0,
            )

    def test_block_coefficient_above_one_raises(self):
        half = 10.0
        stations = [
            HullStation(x_position=0.0, waterline_offsets=[(0.0, half)]),
            HullStation(x_position=100.0, waterline_offsets=[(0.0, half)]),
        ]
        with pytest.raises(ValueError, match="block_coefficient"):
            HullProfile(
                name="bad_cb",
                hull_type=HullType.BARGE,
                stations=stations,
                length_bp=100.0,
                beam=20.0,
                draft=5.0,
                depth=8.0,
                source="test",
                block_coefficient=1.01,
            )

    def test_valid_block_coefficient(self):
        profile = _barge_profile()
        # Create profile with explicit Cb
        half = 10.0
        stations = [
            HullStation(x_position=0.0, waterline_offsets=[(0.0, half)]),
            HullStation(x_position=100.0, waterline_offsets=[(0.0, half)]),
        ]
        p = HullProfile(
            name="with_cb",
            hull_type=HullType.BARGE,
            stations=stations,
            length_bp=100.0,
            beam=20.0,
            draft=5.0,
            depth=8.0,
            source="test",
            block_coefficient=0.75,
        )
        assert p.block_coefficient == pytest.approx(0.75)

    def test_optional_displacement_stored(self):
        half = 10.0
        stations = [
            HullStation(x_position=0.0, waterline_offsets=[(0.0, half)]),
            HullStation(x_position=100.0, waterline_offsets=[(0.0, half)]),
        ]
        p = HullProfile(
            name="with_disp",
            hull_type=HullType.BARGE,
            stations=stations,
            length_bp=100.0,
            beam=20.0,
            draft=5.0,
            depth=8.0,
            source="test",
            displacement=5000.0,
        )
        assert p.displacement == pytest.approx(5000.0)

    def test_station_x_exceeds_length_with_tolerance_raises(self):
        """Station x_position more than 1% beyond length_bp should raise."""
        half = 10.0
        stations = [
            HullStation(x_position=0.0, waterline_offsets=[(0.0, half)]),
            HullStation(x_position=110.0, waterline_offsets=[(0.0, half)]),  # >1% over
        ]
        with pytest.raises(ValueError):
            HullProfile(
                name="overshoot",
                hull_type=HullType.BARGE,
                stations=stations,
                length_bp=100.0,
                beam=20.0,
                draft=5.0,
                depth=8.0,
                source="test",
            )

    def test_station_half_breadth_exceeds_beam_tolerance_raises(self):
        """half-breadth > beam/2 + 5% should raise."""
        stations = [
            HullStation(x_position=0.0, waterline_offsets=[(0.0, 11.5)]),  # 11.5 > 10*1.05=10.5
            HullStation(x_position=100.0, waterline_offsets=[(0.0, 10.0)]),
        ]
        with pytest.raises(ValueError):
            HullProfile(
                name="wide",
                hull_type=HullType.BARGE,
                stations=stations,
                length_bp=100.0,
                beam=20.0,
                draft=5.0,
                depth=8.0,
                source="test",
            )


# ---------------------------------------------------------------------------
# HullProfile YAML round-trip tests
# ---------------------------------------------------------------------------


class TestHullProfileYamlRoundTrip:
    """Verify to_yaml_dict / from_yaml_dict round-trip."""

    def test_basic_round_trip(self):
        original = _barge_profile()
        data = original.to_yaml_dict()
        recovered = HullProfile.from_yaml_dict(data)
        assert recovered.name == original.name
        assert recovered.hull_type == original.hull_type
        assert recovered.length_bp == pytest.approx(original.length_bp)
        assert recovered.beam == pytest.approx(original.beam)
        assert recovered.draft == pytest.approx(original.draft)

    def test_hull_type_serialised_as_string(self):
        data = _barge_profile().to_yaml_dict()
        assert data["hull_type"] == "barge"

    def test_station_offsets_serialised_as_lists(self):
        data = _barge_profile().to_yaml_dict()
        for station_dict in data["stations"]:
            for pair in station_dict["waterline_offsets"]:
                assert isinstance(pair, list)

    def test_save_and_load_yaml_file(self, tmp_path):
        original = _barge_profile()
        yaml_path = tmp_path / "test_barge.yaml"
        original.save_yaml(yaml_path)
        loaded = HullProfile.load_yaml(yaml_path)
        assert loaded.name == original.name
        assert loaded.length_bp == pytest.approx(original.length_bp)
        assert len(loaded.stations) == len(original.stations)

    def test_load_yaml_file_not_found(self, tmp_path):
        with pytest.raises(FileNotFoundError):
            HullProfile.load_yaml(tmp_path / "nonexistent.yaml")

    def test_optional_fields_preserved(self, tmp_path):
        half = 10.0
        stations = [
            HullStation(x_position=0.0, waterline_offsets=[(0.0, half)]),
            HullStation(x_position=100.0, waterline_offsets=[(0.0, half)]),
        ]
        original = HullProfile(
            name="full",
            hull_type=HullType.TANKER,
            stations=stations,
            length_bp=100.0,
            beam=20.0,
            draft=5.0,
            depth=8.0,
            source="test",
            block_coefficient=0.80,
            displacement=12000.0,
        )
        data = original.to_yaml_dict()
        recovered = HullProfile.from_yaml_dict(data)
        assert recovered.block_coefficient == pytest.approx(0.80)
        assert recovered.displacement == pytest.approx(12000.0)

    def test_deck_profile_serialised(self):
        half = 10.0
        stations = [
            HullStation(x_position=0.0, waterline_offsets=[(0.0, half)]),
            HullStation(x_position=100.0, waterline_offsets=[(0.0, half)]),
        ]
        original = HullProfile(
            name="with_deck",
            hull_type=HullType.BARGE,
            stations=stations,
            length_bp=100.0,
            beam=20.0,
            draft=5.0,
            depth=8.0,
            source="test",
            deck_profile=[(0.0, 10.0), (100.0, 10.0)],
        )
        data = original.to_yaml_dict()
        assert "deck_profile" in data
        recovered = HullProfile.from_yaml_dict(data)
        assert len(recovered.deck_profile) == 2


# ---------------------------------------------------------------------------
# HullLookup helper function tests
# ---------------------------------------------------------------------------


class TestHullLookupHelpers:
    """Unit tests for the private helper functions in lookup module."""

    def test_normalised_distance_exact_match(self):
        target = HullLookupTarget(loa_m=200.0, beam_m=40.0, draft_m=12.0)
        dist = _normalised_distance(target, 200.0, 40.0, 12.0)
        assert dist == pytest.approx(0.0, abs=1e-10)

    def test_normalised_distance_positive_for_mismatch(self):
        target = HullLookupTarget(loa_m=200.0, beam_m=40.0, draft_m=12.0)
        dist = _normalised_distance(target, 100.0, 20.0, 6.0)
        assert dist > 0.0

    def test_normalised_distance_is_euclidean(self):
        target = HullLookupTarget(loa_m=200.0, beam_m=40.0, draft_m=12.0)
        # single-dimension shift: loa differs by 200/200=1.0 normalised unit
        dist = _normalised_distance(target, 0.0, 40.0, 12.0)
        assert dist == pytest.approx(1.0, rel=1e-6)

    def test_distance_to_score_exact_match(self):
        score = _distance_to_score(0.0)
        assert score == pytest.approx(1.0)

    def test_distance_to_score_decreasing(self):
        scores = [_distance_to_score(d) for d in [0.0, 0.5, 1.0, 2.0, 5.0]]
        for i in range(len(scores) - 1):
            assert scores[i] > scores[i + 1]

    def test_distance_to_score_positive(self):
        for d in [0.0, 0.1, 1.0, 10.0]:
            assert _distance_to_score(d) > 0.0

    def test_distance_to_score_at_most_one(self):
        for d in [0.0, 0.001, 1.0]:
            assert _distance_to_score(d) <= 1.0

    def test_scaling_factors_equal_dimensions(self):
        target = HullLookupTarget(loa_m=200.0, beam_m=40.0, draft_m=12.0)
        factors = _scaling_factors(target, 200.0, 40.0, 12.0)
        assert factors["loa"] == pytest.approx(1.0)
        assert factors["beam"] == pytest.approx(1.0)
        assert factors["draft"] == pytest.approx(1.0)

    def test_scaling_factors_double_loa(self):
        target = HullLookupTarget(loa_m=200.0, beam_m=40.0, draft_m=12.0)
        factors = _scaling_factors(target, 100.0, 40.0, 12.0)
        assert factors["loa"] == pytest.approx(2.0)

    def test_scaling_factors_zero_candidate_returns_one(self):
        target = HullLookupTarget(loa_m=200.0, beam_m=40.0, draft_m=12.0)
        factors = _scaling_factors(target, 0.0, 40.0, 12.0)
        assert factors["loa"] == pytest.approx(1.0)  # fallback

    def test_validate_target_raises_for_zero_loa(self):
        target = HullLookupTarget(loa_m=0.0, beam_m=40.0, draft_m=12.0)
        with pytest.raises(ValueError, match="loa_m"):
            _validate_target(target)

    def test_validate_target_raises_for_negative_beam(self):
        target = HullLookupTarget(loa_m=200.0, beam_m=-5.0, draft_m=12.0)
        with pytest.raises(ValueError, match="beam_m"):
            _validate_target(target)

    def test_validate_target_raises_for_zero_draft(self):
        target = HullLookupTarget(loa_m=200.0, beam_m=40.0, draft_m=0.0)
        with pytest.raises(ValueError, match="draft_m"):
            _validate_target(target)

    def test_validate_target_passes_for_valid_dimensions(self):
        target = HullLookupTarget(loa_m=200.0, beam_m=40.0, draft_m=12.0)
        # Should not raise
        _validate_target(target)


# ---------------------------------------------------------------------------
# HullLookup class tests
# ---------------------------------------------------------------------------


class TestHullLookup:
    """Test HullLookup nearest-neighbour matching with built-in fleet."""

    def test_lookup_returns_list(self):
        lookup = HullLookup()
        target = HullLookupTarget(loa_m=150.0, beam_m=25.0, draft_m=7.0)
        results = lookup.find_closest(target, n=3)
        assert isinstance(results, list)

    def test_lookup_returns_requested_count(self):
        lookup = HullLookup()
        target = HullLookupTarget(loa_m=150.0, beam_m=25.0, draft_m=7.0)
        results = lookup.find_closest(target, n=3)
        assert len(results) == 3

    def test_lookup_results_are_hull_match_instances(self):
        lookup = HullLookup()
        target = HullLookupTarget(loa_m=150.0, beam_m=25.0, draft_m=7.0)
        results = lookup.find_closest(target, n=1)
        assert isinstance(results[0], HullMatch)

    def test_best_match_has_highest_score(self):
        lookup = HullLookup()
        target = HullLookupTarget(loa_m=150.0, beam_m=25.0, draft_m=7.0)
        results = lookup.find_closest(target, n=3)
        scores = [r.similarity_score for r in results]
        assert scores[0] >= scores[1] >= scores[2]

    def test_exact_match_scores_close_to_one(self):
        """Exact match on FST-150 (150m, 25m, 7m) should score very high."""
        lookup = HullLookup()
        target = HullLookupTarget(loa_m=150.0, beam_m=25.0, draft_m=7.0)
        best = lookup.get_hull_form(target)
        assert best.similarity_score > 0.95
        assert best.hull_id == "FST-150"

    def test_exact_match_lngc_250(self):
        lookup = HullLookup()
        target = HullLookupTarget(loa_m=250.0, beam_m=43.0, draft_m=11.5)
        best = lookup.get_hull_form(target)
        assert best.hull_id == "LNGC-250"

    def test_exact_match_fpso_260(self):
        lookup = HullLookup()
        target = HullLookupTarget(loa_m=260.0, beam_m=46.0, draft_m=14.0)
        best = lookup.get_hull_form(target)
        assert best.hull_id == "FPSO-260"

    def test_source_is_builtin_without_catalog(self):
        lookup = HullLookup()
        target = HullLookupTarget(loa_m=150.0, beam_m=25.0, draft_m=7.0)
        best = lookup.get_hull_form(target)
        assert best.source == "builtin"

    def test_scaling_factors_close_to_one_for_exact_match(self):
        lookup = HullLookup()
        target = HullLookupTarget(loa_m=150.0, beam_m=25.0, draft_m=7.0)
        best = lookup.get_hull_form(target)
        assert best.scaling_factors["loa"] == pytest.approx(1.0, rel=1e-3)
        assert best.scaling_factors["beam"] == pytest.approx(1.0, rel=1e-3)
        assert best.scaling_factors["draft"] == pytest.approx(1.0, rel=1e-3)

    def test_invalid_target_raises_value_error(self):
        lookup = HullLookup()
        target = HullLookupTarget(loa_m=0.0, beam_m=25.0, draft_m=7.0)
        with pytest.raises(ValueError):
            lookup.find_closest(target)

    def test_n_greater_than_fleet_capped(self):
        """Asking for more results than candidates returns all candidates."""
        lookup = HullLookup()
        target = HullLookupTarget(loa_m=150.0, beam_m=25.0, draft_m=7.0)
        results = lookup.find_closest(target, n=100)
        # Built-in fleet has 8 entries; should return all 8
        assert len(results) == 8


# ---------------------------------------------------------------------------
# get_hull_form convenience function tests
# ---------------------------------------------------------------------------


class TestGetHullFormConvenience:
    """Tests for the module-level get_hull_form() convenience function."""

    def test_returns_hull_match_instance(self):
        match = get_hull_form({"loa_m": 150.0, "beam_m": 25.0, "draft_m": 7.0})
        assert isinstance(match, HullMatch)

    def test_missing_loa_raises(self):
        with pytest.raises(ValueError, match="loa_m"):
            get_hull_form({"beam_m": 25.0, "draft_m": 7.0})

    def test_missing_beam_raises(self):
        with pytest.raises(ValueError, match="beam_m"):
            get_hull_form({"loa_m": 150.0, "draft_m": 7.0})

    def test_missing_draft_raises(self):
        with pytest.raises(ValueError, match="draft_m"):
            get_hull_form({"loa_m": 150.0, "beam_m": 25.0})

    def test_optional_displacement_accepted(self):
        match = get_hull_form({
            "loa_m": 150.0,
            "beam_m": 25.0,
            "draft_m": 7.0,
            "displacement_t": 8000.0,
        })
        assert isinstance(match, HullMatch)

    def test_lngc_nearest_neighbour(self):
        match = get_hull_form({"loa_m": 250.0, "beam_m": 43.0, "draft_m": 11.5})
        assert match.hull_id == "LNGC-250"

    def test_hull_match_has_required_fields(self):
        match = get_hull_form({"loa_m": 250.0, "beam_m": 43.0, "draft_m": 11.5})
        assert hasattr(match, "hull_id")
        assert hasattr(match, "similarity_score")
        assert hasattr(match, "scaling_factors")
        assert hasattr(match, "matched_entry")
        assert hasattr(match, "source")
