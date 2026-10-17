"""
ABOUTME: Domain extents as CASE configuration (#2023). ``build_hull_domain``
has always taken the five Lpp multiples as keyword arguments; ``HullCaseConfig``
did not expose them, so a caller building a case could not change the size of
the box it solved in.

WHY IT MATTERS, MEASURED. On a 158 m hull the default ``FREEBOARD_LPP = 0.65``
puts 92 m of air above the waterline for a hull carrying 4.19 m of freeboard --
22 times more than the hull has. Trimming to the ITTC-justified extents
(downstream 4.5 -> 3.5, lateral 3.0 -> 2.0, depth 2.5 -> 1.5, freeboard
0.65 -> 0.30) roughly halves the BACKGROUND cell count. It takes far less off
the total, because the six nested refinement stages dominate and do not shrink
with the far field -- which is the honest reason to trim: cheaper blockMesh and
less far-field junk, not a transformed budget.

AND THE FLOOR. ``FS_OUTER_LPP = 0.216`` is the top of the free-surface outer
block, measured from the waterline. Below ``freeboard_lpp = FS_OUTER_LPP +
draft/Lpp`` that block inverts, and the builder refuses. That refusal is
correct -- an inverted block is a negative-volume cell -- so these tests pin it
rather than route around it.
"""

from __future__ import annotations

import json
from typing import Any, Dict

import pytest

from digitalmodel.solvers.openfoam.hull_case import HullCaseConfig, derive_hull_case
from digitalmodel.solvers.openfoam.hull_case_physics import derive_cell_budget
from digitalmodel.solvers.openfoam.hull_domain import (
    DEPTH_LPP,
    DOWNSTREAM_LPP,
    FREEBOARD_LPP,
    FS_OUTER_LPP,
    KEEL_CLEARANCE_DRAFTS,
    LATERAL_LPP,
    UPSTREAM_LPP,
    HullDomainError,
    block_divisions,
    build_hull_domain,
    refinement_boxes,
)
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest

#: The ITTC-justified trim, as one named set so the tests and the report cannot
#: quote different numbers.
TRIMMED: Dict[str, float] = {
    "upstream_lpp": 2.0,
    "downstream_lpp": 3.5,
    "lateral_lpp": 2.0,
    "depth_lpp": 1.5,
    "freeboard_lpp": 0.30,
}

FULL_SCALE: Dict[str, Any] = {
    "source_file": "hull.stl",
    "source_sha256": "0" * 64,
    "units_in": "m",
    "scale_to_m": 1.0,
    "orientation": {"x": "forward", "y": "port", "z": "up"},
    "origin": "aft_perpendicular_keel",
    "lpp_m": 158.20,
    "beam_m": 27.004,
    "draft_m": 10.40,
    "wetted_surface_m2": 6216.8,
    "displacement_m3": 0.80 * 158.20 * 27.004 * 10.40,
    "watertight": True,
    "n_triangles": 500000,
    "bbox_min_m": [-1.0, -13.502, 0.0],
    "bbox_max_m": [160.2, 13.502, 14.59],
}


@pytest.fixture
def full_scale() -> HullManifest:
    return HullManifest.from_dict(json.loads(json.dumps(FULL_SCALE)))


def _config(manifest: HullManifest, stl, **kw) -> HullCaseConfig:
    return HullCaseConfig(
        manifest=manifest,
        stl_path=stl,
        velocity=7.2022,
        ranks=8,
        name="extents_case",
        **kw,
    )


# --------------------------------------------------------------------------- #
#  The five multiples reach the domain
# --------------------------------------------------------------------------- #

def test_the_defaults_are_todays_values_so_existing_callers_do_not_move(
    full_scale, stl_file
) -> None:
    """Additive: a caller that states nothing gets exactly what it got before."""
    config = _config(full_scale, stl_file)
    assert config.upstream_lpp == UPSTREAM_LPP
    assert config.downstream_lpp == DOWNSTREAM_LPP
    assert config.lateral_lpp == LATERAL_LPP
    assert config.depth_lpp == DEPTH_LPP
    assert config.freeboard_lpp == FREEBOARD_LPP
    assert config.keel_clearance_drafts == KEEL_CLEARANCE_DRAFTS

    derived = derive_hull_case(config).domain
    default = build_hull_domain(full_scale)
    assert derived.x_inlet == pytest.approx(default.x_inlet)
    assert derived.x_outlet == pytest.approx(default.x_outlet)
    assert derived.y_side == pytest.approx(default.y_side)
    assert derived.z_levels == pytest.approx(default.z_levels)


def test_each_multiple_reaches_the_emitted_box(full_scale, stl_file) -> None:
    lpp = full_scale.lpp_m
    domain = derive_hull_case(_config(full_scale, stl_file, **TRIMMED)).domain
    assert domain.x_inlet == pytest.approx(TRIMMED["upstream_lpp"] * lpp)
    assert domain.x_outlet == pytest.approx(-TRIMMED["downstream_lpp"] * lpp)
    assert domain.y_side == pytest.approx(-TRIMMED["lateral_lpp"] * lpp)
    assert domain.z_levels[0] == pytest.approx(-TRIMMED["depth_lpp"] * lpp)
    assert domain.z_levels[-1] == pytest.approx(TRIMMED["freeboard_lpp"] * lpp)


def test_the_keel_clearance_reaches_the_second_z_level(full_scale, stl_file) -> None:
    config = _config(full_scale, stl_file, keel_clearance_drafts=2.0)
    domain = derive_hull_case(config).domain
    assert domain.z_levels[1] == pytest.approx(-2.0 * full_scale.draft_m)


def test_the_default_freeboard_is_twenty_two_times_the_hulls_own(full_scale) -> None:
    """The measurement that motivates the knob, pinned so it cannot drift."""
    domain = build_hull_domain(full_scale)
    air = domain.z_levels[-1] - domain.waterline
    hull_freeboard = full_scale.bbox_max_m[2] - full_scale.draft_m
    assert air == pytest.approx(92.4, abs=0.5)
    assert air / hull_freeboard == pytest.approx(22, abs=1)


# --------------------------------------------------------------------------- #
#  What trimming buys, and what it does not
# --------------------------------------------------------------------------- #

def test_trimming_roughly_halves_the_background_but_not_the_total(
    full_scale, stl_file
) -> None:
    """The refinement stages dominate; say so in a test rather than in a plan.

    A trim that halves the background and is then reported as halving the case
    is the sort of claim a programme schedule gets built on.
    """
    def budget(**extents):
        derivation = derive_hull_case(_config(full_scale, stl_file, **extents))
        return derive_cell_budget(
            derivation.domain,
            derivation.boxes,
            derivation.divisions,
            ranks=8,
        )

    default, trimmed = budget(), budget(**TRIMMED)
    assert trimmed.background_cells < 0.60 * default.background_cells
    assert trimmed.estimated_cells > 0.85 * default.estimated_cells


def test_a_trimmed_domain_still_contains_its_refinement_boxes(
    full_scale, stl_file
) -> None:
    derivation = derive_hull_case(_config(full_scale, stl_file, **TRIMMED))
    domain = derivation.domain
    for lo, hi in refinement_boxes(full_scale, domain):
        assert domain.x_outlet <= lo[0] and hi[0] <= domain.x_inlet
        assert domain.y_side <= lo[1] and hi[1] <= 0.0
        assert domain.z_levels[0] <= lo[2] and hi[2] <= domain.z_levels[-1]


def test_the_trimmed_domain_still_emits_usable_divisions(
    full_scale, stl_file
) -> None:
    div = block_divisions(
        derive_hull_case(_config(full_scale, stl_file, **TRIMMED)).domain
    )
    for key in ("nx", "ny", "nza", "nzb", "nzc", "nzd"):
        assert div[key] >= 1, key


# --------------------------------------------------------------------------- #
#  The FS_OUTER_LPP floor -- a refusal, kept as a refusal
# --------------------------------------------------------------------------- #

def _freeboard_floor(manifest: HullManifest) -> float:
    """The smallest ``freeboard_lpp`` that leaves the outer block positive."""
    return FS_OUTER_LPP + manifest.draft_m / manifest.lpp_m


def test_a_freeboard_inside_the_free_surface_outer_block_is_refused(
    full_scale,
) -> None:
    floor = _freeboard_floor(full_scale)
    with pytest.raises(HullDomainError) as excinfo:
        build_hull_domain(full_scale, freeboard_lpp=floor - 0.005)
    assert "FS_OUTER_LPP" in str(excinfo.value), (
        "the refusal must name the binding constant, not just an index"
    )


def test_the_published_floor_of_about_0_2792_still_refuses(full_scale) -> None:
    """The number recorded when this was measured. It must keep refusing."""
    with pytest.raises(HullDomainError, match="FS_OUTER_LPP"):
        build_hull_domain(full_scale, freeboard_lpp=0.2792)


def test_the_trimmed_freeboard_of_0_30_sits_above_the_floor(full_scale) -> None:
    assert TRIMMED["freeboard_lpp"] > _freeboard_floor(full_scale)
    domain = build_hull_domain(full_scale, **TRIMMED)
    assert domain.z_levels[5] < domain.z_levels[6]


def test_the_refusal_reaches_the_case_builder(full_scale, stl_file) -> None:
    """Config is the surface a caller actually touches; the floor must hold
    there too, and it must hold BEFORE anything is written."""
    floor = _freeboard_floor(full_scale)
    with pytest.raises(HullDomainError, match="FS_OUTER_LPP"):
        derive_hull_case(_config(full_scale, stl_file, freeboard_lpp=floor / 2))


def test_the_refusal_names_the_levels_and_the_index_as_it_always_did(
    full_scale,
) -> None:
    """The new hint is additive: the numeric evidence stays."""
    with pytest.raises(HullDomainError) as excinfo:
        build_hull_domain(full_scale, freeboard_lpp=0.10)
    message = str(excinfo.value)
    assert "index 5" in message
    assert "negative-volume" in message


def test_the_floor_moves_with_the_draft_to_lpp_ratio() -> None:
    """It is not a constant: a deeper hull needs more freeboard_lpp."""
    deep = json.loads(json.dumps(FULL_SCALE))
    deep["draft_m"] = 20.0
    deep["displacement_m3"] = 0.80 * 158.20 * 27.004 * 20.0
    deep["bbox_max_m"] = [160.2, 13.502, 24.19]
    deep_manifest = HullManifest.from_dict(deep)

    assert _freeboard_floor(deep_manifest) > _freeboard_floor(
        HullManifest.from_dict(json.loads(json.dumps(FULL_SCALE)))
    )
    build_hull_domain(deep_manifest, freeboard_lpp=_freeboard_floor(deep_manifest) + 0.02)
    with pytest.raises(HullDomainError, match="FS_OUTER_LPP"):
        build_hull_domain(deep_manifest, freeboard_lpp=0.30)
