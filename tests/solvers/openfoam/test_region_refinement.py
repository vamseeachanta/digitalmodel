#!/usr/bin/env python3
"""
ABOUTME: Deriving each appendage's snappyHexMesh refinement level from its own
bounding box instead of leaving it at the default (0 0).

WHAT THIS TEST DEFENDS
----------------------
An under-refined appendage does not fail. It meshes, it solves, and it reports
a force. On a real case a 6 m appendage inside a 158 m domain came out of the
mesher with 179 faces and returned 1.8 N of skin friction against 13,921 N of
pressure -- a ratio that is only possible when there is no boundary layer on
the body at all. Nothing in the run said so; the number simply arrived.

The derivation is a HEURISTIC and the tests treat it as one. What they pin is
not a table of blessed levels but the four properties that make the heuristic
trustworthy:

  * the level actually DELIVERS the target -- refining by it puts at least
    ``target_cells_across`` cells across the region's smallest dimension;
  * the target is a PARAMETER, so changing the judgement changes the answer and
    the judgement is visible in the call rather than buried as a constant;
  * a region that is already resolved gets level 0 rather than a gratuitous
    refinement, because every level is 8x the cells in that box;
  * degenerate and runaway inputs are refused or clamped rather than silently
    producing a mesh nobody can afford.

The dimensions below are those of two generic stern appendages, described by
size only.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.solvers.openfoam.hull_case_regions import SurfaceRegion
from digitalmodel.solvers.openfoam.region_refinement import (
    DEFAULT_MAX_LEVEL,
    DEFAULT_TARGET_CELLS_ACROSS,
    RegionRefinement,
    derive_region_levels,
    near_hull_cell_size,
    refinement_for_bbox,
    refinement_for_extent,
)

BASE_CELL_M = 21.0
STAGES = 6


# --------------------------------------------------------------------------- #
#  The cell the appendage actually meets
# --------------------------------------------------------------------------- #

def test_near_hull_cell_is_the_base_halved_once_per_stage():
    assert near_hull_cell_size(BASE_CELL_M, STAGES) == pytest.approx(0.328125)
    assert near_hull_cell_size(1.0, 0) == pytest.approx(1.0)
    assert near_hull_cell_size(8.0, 3) == pytest.approx(1.0)


def test_near_hull_cell_rejects_nonsense():
    with pytest.raises(ValueError):
        near_hull_cell_size(0.0, STAGES)
    with pytest.raises(ValueError):
        near_hull_cell_size(-1.0, STAGES)
    with pytest.raises(ValueError):
        near_hull_cell_size(BASE_CELL_M, -1)


# --------------------------------------------------------------------------- #
#  The derivation
# --------------------------------------------------------------------------- #

def test_a_thin_high_aspect_appendage_needs_two_levels():
    """A fin-like appendage: long and tall, but barely a metre thick."""
    result = refinement_for_extent(
        "appendage", (5.51, 1.18, 7.53),
        base_cell_size_m=BASE_CELL_M, stages=STAGES,
    )
    assert result.level == 2
    assert result.min_extent_m == pytest.approx(1.18)
    assert result.cells_across_before == pytest.approx(1.18 / 0.328125)
    assert result.cells_across_after >= DEFAULT_TARGET_CELLS_ACROSS


def test_a_small_compact_appendage_needs_three_levels():
    """A stub of roughly one metre in two directions needs one level more."""
    result = refinement_for_extent(
        "appendage", (1.63, 1.00, 1.00),
        base_cell_size_m=BASE_CELL_M, stages=STAGES,
    )
    assert result.level == 3
    assert result.cells_across_after >= DEFAULT_TARGET_CELLS_ACROSS


def test_the_level_is_the_smallest_one_that_reaches_the_target():
    """Not merely sufficient -- minimal. One level too many is 8x the cells."""
    for extent in ((5.51, 1.18, 7.53), (1.63, 1.00, 1.00), (12.0, 3.4, 6.0)):
        result = refinement_for_extent(
            "appendage", extent, base_cell_size_m=BASE_CELL_M, stages=STAGES
        )
        assert result.cells_across_after >= result.target_cells_across
        if result.level:
            one_less = result.cells_across_before * 2 ** (result.level - 1)
            assert one_less < result.target_cells_across


def test_the_target_is_a_named_parameter_not_a_constant():
    """The judgement is 'how many cells across is enough', and it is visible."""
    extent = (5.51, 1.18, 7.53)
    lenient = refinement_for_extent(
        "appendage", extent, base_cell_size_m=BASE_CELL_M, stages=STAGES,
        target_cells_across=4,
    )
    strict = refinement_for_extent(
        "appendage", extent, base_cell_size_m=BASE_CELL_M, stages=STAGES,
        target_cells_across=28,
    )
    assert lenient.level < strict.level
    assert lenient.target_cells_across == 4
    assert strict.target_cells_across == 28
    with pytest.raises(ValueError):
        refinement_for_extent(
            "appendage", extent, base_cell_size_m=BASE_CELL_M,
            target_cells_across=0,
        )


def test_an_already_resolved_region_is_left_alone():
    """A body many base cells across needs nothing, and asking for nothing is
    the right answer: refinement is not free anywhere it is applied."""
    result = refinement_for_extent(
        "appendage", (60.0, 40.0, 30.0),
        base_cell_size_m=BASE_CELL_M, stages=STAGES,
    )
    assert result.level == 0
    assert result.cells_across_after == pytest.approx(
        result.cells_across_before
    )
    assert result.refinement_level == (0, 0)


def test_a_runaway_region_is_clamped_and_says_so():
    """A millimetric feature would ask for a level that cannot be paid for.

    The clamp is the honest failure: the level is capped, ``clamped`` is set,
    and ``cells_across_after`` reports what the mesh will ACTUALLY deliver
    rather than the target that was asked for.
    """
    result = refinement_for_extent(
        "appendage", (0.5, 0.002, 0.5),
        base_cell_size_m=BASE_CELL_M, stages=STAGES,
    )
    assert result.level == DEFAULT_MAX_LEVEL
    assert result.clamped is True
    assert result.cells_across_after < result.target_cells_across
    assert result.level_required > DEFAULT_MAX_LEVEL


def test_degenerate_extents_are_refused():
    for extent in ((0.0, 1.0, 1.0), (1.0, -2.0, 1.0)):
        with pytest.raises(ValueError):
            refinement_for_extent(
                "appendage", extent, base_cell_size_m=BASE_CELL_M,
                stages=STAGES,
            )
    with pytest.raises(ValueError):
        refinement_for_extent(
            "appendage", (1.0, 1.0), base_cell_size_m=BASE_CELL_M
        )


def test_bbox_form_agrees_with_extent_form():
    lo, hi = (5.27, -0.50, 2.61), (6.90, 0.50, 3.61)
    from_bbox = refinement_for_bbox(
        "appendage", lo, hi, base_cell_size_m=BASE_CELL_M, stages=STAGES
    )
    from_extent = refinement_for_extent(
        "appendage", tuple(hi[i] - lo[i] for i in range(3)),
        base_cell_size_m=BASE_CELL_M, stages=STAGES,
    )
    assert from_bbox.level == from_extent.level
    assert from_bbox.min_extent_m == pytest.approx(from_extent.min_extent_m)


def test_an_inverted_bbox_is_refused():
    with pytest.raises(ValueError):
        refinement_for_bbox(
            "appendage", (5.0, 0.0, 0.0), (4.0, 1.0, 1.0),
            base_cell_size_m=BASE_CELL_M,
        )


# --------------------------------------------------------------------------- #
#  Over a manifest's regions
# --------------------------------------------------------------------------- #

def _manifest_regions():
    return [
        {
            "name": "hull", "role": "hull",
            "bbox_min_m": [-0.35, -13.50, 0.0],
            "bbox_max_m": [158.20, 13.50, 14.72],
        },
        {
            "name": "stub", "role": "appendage",
            "bbox_min_m": [5.27, -0.50, 2.61],
            "bbox_max_m": [6.90, 0.50, 3.61],
        },
        {
            "name": "fin", "role": "appendage",
            "bbox_min_m": [0.24, -0.59, 0.96],
            "bbox_max_m": [5.75, 0.59, 8.49],
        },
    ]


def test_derive_region_levels_skips_the_hull_and_sizes_the_rest():
    derived = derive_region_levels(
        _manifest_regions(), base_cell_size_m=BASE_CELL_M, stages=STAGES
    )
    assert [d.name for d in derived] == ["stub", "fin"]
    assert {d.name: d.level for d in derived} == {"stub": 3, "fin": 2}
    assert all(isinstance(d, RegionRefinement) for d in derived)


def test_derive_region_levels_ignores_regions_without_a_bbox():
    regions = _manifest_regions() + [{"name": "unknown", "role": "appendage"}]
    derived = derive_region_levels(
        regions, base_cell_size_m=BASE_CELL_M, stages=STAGES
    )
    assert [d.name for d in derived] == ["stub", "fin"]


def test_derived_levels_drop_straight_into_a_surface_region():
    """The output is only useful if snappyHexMesh can be handed it directly."""
    derived = derive_region_levels(
        _manifest_regions(), base_cell_size_m=BASE_CELL_M, stages=STAGES
    )
    region = SurfaceRegion(
        name=derived[0].name,
        stl_path="stub.stl",
        refinement_level=derived[0].refinement_level,
    )
    assert region.refinement_level == (3, 3)


def test_records_carry_the_derivation_not_just_the_answer():
    """A level with no working shown is indistinguishable from a guess, which
    is the thing this module was written to replace."""
    record = refinement_for_extent(
        "appendage", (5.51, 1.18, 7.53),
        base_cell_size_m=BASE_CELL_M, stages=STAGES,
    ).to_dict()
    for key in (
        "name", "extent_m", "min_extent_m", "base_cell_size_m", "stages",
        "near_cell_size_m", "cells_across_before", "target_cells_across",
        "level", "cells_across_after", "clamped",
    ):
        assert key in record, key
    assert record["near_cell_size_m"] == pytest.approx(0.328125)


def test_worked_example_matches_a_hand_calculation():
    """The whole derivation, done by hand, so a reader can check the code.

    near cell   = 21.0 / 2**6                = 0.328125 m
    cells now   = 1.18 / 0.328125            = 3.596
    levels      = ceil(log2(14 / 3.596))     = ceil(1.961) = 2
    cells after = 3.596 * 2**2               = 14.38  >= 14
    """
    near = 21.0 / 2**6
    now = 1.18 / near
    assert math.ceil(math.log2(14 / now)) == 2
    result = refinement_for_extent(
        "appendage", (5.51, 1.18, 7.53),
        base_cell_size_m=21.0, stages=6, target_cells_across=14,
    )
    assert result.near_cell_size_m == pytest.approx(near)
    assert result.cells_across_before == pytest.approx(now)
    assert result.level == 2
    assert result.cells_across_after == pytest.approx(now * 4)
