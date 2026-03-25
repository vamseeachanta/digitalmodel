"""
ABOUTME: Tests for parametric hull form definition — Phase 1 (WRK-043).

Tests are written FIRST before implementation (TDD).
Covers ParametricRange, HullParametricSpace combinations and generate_profiles.
"""

from __future__ import annotations

import pytest
import numpy as np


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_box_profile(name: str = "unit_box", length: float = 100.0,
                      beam: float = 20.0, draft: float = 10.0,
                      depth: float = 12.0):
    """Create a simple box hull profile for testing."""
    from digitalmodel.hydrodynamics.hull_library.profile_schema import (
        HullProfile, HullStation, HullType,
    )

    half_beam = beam / 2.0
    stations = [
        HullStation(
            x_position=0.0,
            waterline_offsets=[(0.0, half_beam), (draft, half_beam)],
        ),
        HullStation(
            x_position=length / 2,
            waterline_offsets=[(0.0, half_beam), (draft, half_beam)],
        ),
        HullStation(
            x_position=length,
            waterline_offsets=[(0.0, half_beam), (draft, half_beam)],
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
        source="test",
    )


# ---------------------------------------------------------------------------
# ParametricRange tests
# ---------------------------------------------------------------------------


class TestParametricRange:
    def test_linspace_values_three_steps(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            ParametricRange,
        )

        pr = ParametricRange(min=10.0, max=20.0, steps=3)
        values = list(pr.values())
        assert len(values) == 3
        assert values[0] == pytest.approx(10.0)
        assert values[1] == pytest.approx(15.0)
        assert values[2] == pytest.approx(20.0)

    def test_linspace_values_single_step(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            ParametricRange,
        )

        pr = ParametricRange(min=5.0, max=5.0, steps=1)
        values = list(pr.values())
        assert len(values) == 1
        assert values[0] == pytest.approx(5.0)

    def test_linspace_values_five_steps(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            ParametricRange,
        )

        pr = ParametricRange(min=0.0, max=1.0, steps=5)
        values = list(pr.values())
        assert len(values) == 5
        assert np.allclose(values, [0.0, 0.25, 0.5, 0.75, 1.0])

    def test_steps_must_be_positive(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            ParametricRange,
        )

        with pytest.raises((ValueError, Exception)):
            ParametricRange(min=0.0, max=1.0, steps=0)

    def test_min_must_not_exceed_max(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            ParametricRange,
        )

        with pytest.raises((ValueError, Exception)):
            ParametricRange(min=10.0, max=5.0, steps=3)

    def test_repr_shows_range(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            ParametricRange,
        )

        pr = ParametricRange(min=1.0, max=3.0, steps=3)
        assert "1.0" in repr(pr) or "1" in repr(pr)


# ---------------------------------------------------------------------------
# HullParametricSpace combinations tests
# ---------------------------------------------------------------------------


class TestHullParametricSpaceCombinations:
    def test_single_dimension_combinations(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )

        space = HullParametricSpace(
            base_hull_id="unit_box",
            ranges={"length_scale": ParametricRange(min=1.0, max=2.0, steps=3)},
        )
        combos = list(space.combinations())
        assert len(combos) == 3
        assert all("length_scale" in c for c in combos)

    def test_two_dimension_cartesian_product(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )

        space = HullParametricSpace(
            base_hull_id="unit_box",
            ranges={
                "length_scale": ParametricRange(min=1.0, max=2.0, steps=2),
                "beam_scale": ParametricRange(min=0.8, max=1.2, steps=2),
            },
        )
        combos = list(space.combinations())
        # 2 x 2 = 4 combinations
        assert len(combos) == 4
        # All combinations have both keys
        assert all("length_scale" in c and "beam_scale" in c for c in combos)

    def test_three_dimension_cartesian_product(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )

        space = HullParametricSpace(
            base_hull_id="unit_box",
            ranges={
                "a": ParametricRange(min=1.0, max=2.0, steps=2),
                "b": ParametricRange(min=1.0, max=3.0, steps=3),
                "c": ParametricRange(min=0.5, max=1.5, steps=2),
            },
        )
        combos = list(space.combinations())
        assert len(combos) == 2 * 3 * 2  # 12

    def test_fixed_params_override_ranges(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )

        space = HullParametricSpace(
            base_hull_id="unit_box",
            ranges={"length_scale": ParametricRange(min=1.0, max=2.0, steps=3)},
            fixed_params={"draft_scale": 0.9},
        )
        combos = list(space.combinations())
        assert len(combos) == 3
        # Each combo should have the fixed param
        assert all(c.get("draft_scale") == pytest.approx(0.9) for c in combos)

    def test_empty_ranges_single_combo_with_fixed(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace,
        )

        space = HullParametricSpace(
            base_hull_id="unit_box",
            ranges={},
            fixed_params={"length_scale": 1.5},
        )
        combos = list(space.combinations())
        assert len(combos) == 1
        assert combos[0]["length_scale"] == pytest.approx(1.5)

    def test_combinations_returns_dicts(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )

        space = HullParametricSpace(
            base_hull_id="unit_box",
            ranges={"x": ParametricRange(min=1.0, max=2.0, steps=2)},
        )
        combos = list(space.combinations())
        assert all(isinstance(c, dict) for c in combos)


# ---------------------------------------------------------------------------
# HullParametricSpace generate_profiles tests
# ---------------------------------------------------------------------------


class TestHullParametricSpaceGenerateProfiles:
    def test_generate_profiles_count(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

        catalog = HullCatalog()
        profile = _make_box_profile("unit_box", length=100.0, beam=20.0,
                                    draft=10.0)
        catalog.register_hull(profile)

        space = HullParametricSpace(
            base_hull_id="unit_box",
            ranges={
                "length_scale": ParametricRange(min=1.0, max=2.0, steps=3),
            },
        )
        profiles = list(space.generate_profiles(catalog))
        assert len(profiles) == 3

    def test_generate_profiles_returns_id_and_profile_tuples(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
        )

        catalog = HullCatalog()
        profile = _make_box_profile("unit_box")
        catalog.register_hull(profile)

        space = HullParametricSpace(
            base_hull_id="unit_box",
            ranges={"length_scale": ParametricRange(min=1.0, max=2.0, steps=2)},
        )
        profiles = list(space.generate_profiles(catalog))
        assert len(profiles) == 2
        for variation_id, hull_profile in profiles:
            assert isinstance(variation_id, str)
            assert isinstance(hull_profile, HullProfile)

    def test_length_scale_scales_proportionally(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

        catalog = HullCatalog()
        base = _make_box_profile("box100", length=100.0, beam=20.0, draft=10.0)
        catalog.register_hull(base)

        space = HullParametricSpace(
            base_hull_id="box100",
            ranges={
                "length_scale": ParametricRange(min=2.0, max=2.0, steps=1),
            },
        )
        profiles = list(space.generate_profiles(catalog))
        assert len(profiles) == 1
        _, scaled = profiles[0]
        # length_bp should be doubled
        assert scaled.length_bp == pytest.approx(200.0)
        # beam and draft unchanged when only length scaled
        assert scaled.beam == pytest.approx(20.0)
        assert scaled.draft == pytest.approx(10.0)

    def test_beam_scale_scales_proportionally(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

        catalog = HullCatalog()
        base = _make_box_profile("box100", length=100.0, beam=20.0, draft=10.0)
        catalog.register_hull(base)

        space = HullParametricSpace(
            base_hull_id="box100",
            ranges={
                "beam_scale": ParametricRange(min=1.5, max=1.5, steps=1),
            },
        )
        profiles = list(space.generate_profiles(catalog))
        _, scaled = profiles[0]
        assert scaled.beam == pytest.approx(30.0)
        assert scaled.length_bp == pytest.approx(100.0)

    def test_draft_scale_scales_proportionally(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

        catalog = HullCatalog()
        base = _make_box_profile("box100", length=100.0, beam=20.0,
                                 draft=10.0, depth=12.0)
        catalog.register_hull(base)

        space = HullParametricSpace(
            base_hull_id="box100",
            ranges={
                "draft_scale": ParametricRange(min=0.5, max=0.5, steps=1),
            },
        )
        profiles = list(space.generate_profiles(catalog))
        _, scaled = profiles[0]
        assert scaled.draft == pytest.approx(5.0)

    def test_variation_ids_are_unique(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

        catalog = HullCatalog()
        base = _make_box_profile("unit_box")
        catalog.register_hull(base)

        space = HullParametricSpace(
            base_hull_id="unit_box",
            ranges={"length_scale": ParametricRange(min=1.0, max=3.0, steps=5)},
        )
        profiles = list(space.generate_profiles(catalog))
        ids = [vid for vid, _ in profiles]
        assert len(ids) == len(set(ids)), "Variation IDs must be unique"

    def test_fixed_params_applied_to_all_profiles(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

        catalog = HullCatalog()
        base = _make_box_profile("unit_box", length=100.0, beam=20.0,
                                 draft=10.0)
        catalog.register_hull(base)

        space = HullParametricSpace(
            base_hull_id="unit_box",
            ranges={"length_scale": ParametricRange(min=1.0, max=2.0, steps=2)},
            fixed_params={"beam_scale": 2.0},
        )
        profiles = list(space.generate_profiles(catalog))
        for _, hp in profiles:
            # beam always doubled by fixed param
            assert hp.beam == pytest.approx(40.0)

    def test_missing_base_hull_raises(self):
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

        catalog = HullCatalog()  # empty catalog

        space = HullParametricSpace(
            base_hull_id="nonexistent",
            ranges={"length_scale": ParametricRange(min=1.0, max=2.0, steps=2)},
        )
        with pytest.raises((KeyError, ValueError)):
            list(space.generate_profiles(catalog))

    def test_generate_profiles_pass_validation(self):
        """All generated profiles must satisfy HullProfile Pydantic validation."""
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog
        from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile

        catalog = HullCatalog()
        base = _make_box_profile("hull_val", length=100.0, beam=20.0,
                                 draft=10.0, depth=12.0)
        catalog.register_hull(base)

        space = HullParametricSpace(
            base_hull_id="hull_val",
            ranges={
                "beam_scale": ParametricRange(min=0.8, max=1.4, steps=3),
                "draft_scale": ParametricRange(min=0.7, max=1.3, steps=3),
            },
        )
        # Should not raise — all profiles must pass HullProfile validation
        profiles = list(space.generate_profiles(catalog))
        # 3 x 3 = 9 combinations
        assert len(profiles) == 9
        for vid, hp in profiles:
            assert isinstance(hp, HullProfile)
            # Re-validate by round-tripping through the model constructor
            HullProfile(**hp.model_dump())


# ---------------------------------------------------------------------------
# Variation ID tests
# ---------------------------------------------------------------------------


class TestVariationId:
    def test_variation_id_deterministic(self):
        """Same params always produce the same variation ID."""
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

        catalog = HullCatalog()
        base = _make_box_profile("det_hull", length=100.0, beam=20.0, draft=10.0)
        catalog.register_hull(base)

        space = HullParametricSpace(
            base_hull_id="det_hull",
            ranges={"length_scale": ParametricRange(min=1.0, max=2.0, steps=2)},
        )
        ids_first = [vid for vid, _ in space.generate_profiles(catalog)]
        ids_second = [vid for vid, _ in space.generate_profiles(catalog)]
        assert ids_first == ids_second

    def test_variation_id_different_for_different_params(self):
        """Different param combos produce different variation IDs."""
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

        catalog = HullCatalog()
        base = _make_box_profile("diff_hull", length=100.0, beam=20.0, draft=10.0)
        catalog.register_hull(base)

        space = HullParametricSpace(
            base_hull_id="diff_hull",
            ranges={"beam_scale": ParametricRange(min=0.8, max=1.6, steps=5)},
        )
        ids = [vid for vid, _ in space.generate_profiles(catalog)]
        # All 5 IDs must be distinct
        assert len(ids) == len(set(ids))


# ---------------------------------------------------------------------------
# Beam-and-draft cartesian product test
# ---------------------------------------------------------------------------


class TestBeamDraftCombinations:
    def test_combinations_beam_and_draft(self):
        """beam(3 steps) x draft(3 steps) = 9 combinations."""
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace, ParametricRange,
        )

        # Use scale factors: 3 beam scales x 3 draft scales = 9 combos
        space = HullParametricSpace(
            base_hull_id="box",
            ranges={
                "beam_scale": ParametricRange(min=0.8, max=1.2, steps=3),
                "draft_scale": ParametricRange(min=0.7, max=1.3, steps=3),
            },
        )
        combos = list(space.combinations())
        assert len(combos) == 9
        assert all("beam_scale" in c and "draft_scale" in c for c in combos)
