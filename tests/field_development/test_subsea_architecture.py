# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Bounded planar placement tests using independent synthetic geometry.

Basis: the Euclidean definition d = sqrt((x_b-x_a)**2 + (y_b-y_a)**2).
All coordinates below are deliberately invented planar examples, not field data.
Invalid numeric values exercise the finite, real, two-coordinate contract.
"""

from itertools import permutations
from math import sqrt
from pathlib import Path
import runpy

import pytest

from digitalmodel.field_development.subsea_architecture import (
    ManifoldCandidate,
    PlanarPoint,
    optimize_manifold,
)


@pytest.fixture
def geometry():
    """Centre: 5+5+4=14 m; west: 4+sqrt(52)+5; at host: 2*sqrt(73)."""
    wells = [PlanarPoint("well-a", (0, 0), "m"), PlanarPoint("well-b", (6, 0), "m")]
    host = PlanarPoint("host", (3, 8), "m")
    candidates = [
        ManifoldCandidate("west", (0, 4), "m", "synthetic west option"),
        ManifoldCandidate("centre", (3, 4), "m", "synthetic centre option"),
        ManifoldCandidate("at-host", (3, 8), "m", "synthetic host option"),
    ]
    return wells, host, candidates


def test_candidate_minimum_by_hand(geometry):
    """Centre wins: 14 < 9+sqrt(52) and 14 < 2*sqrt(73), in metres."""
    result = optimize_manifold(*geometry)
    assert result.chosen_candidate.identifier == "centre"
    assert result.chosen_candidate.coordinates == (3, 4)
    assert result.objective == pytest.approx(14)
    assert result.units == "m"
    assert {item.candidate.identifier: item.objective for item in result.evaluations} == (
        pytest.approx({"centre": 14, "west": 9 + sqrt(52), "at-host": 2 * sqrt(73)})
    )


def test_leg_breakdown(geometry):
    """Well legs sqrt(3**2+4**2)=5 each; host leg sqrt(0**2+4**2)=4 m."""
    result = optimize_manifold(*geometry)
    assert [(leg.source_id, leg.target_id, leg.length) for leg in result.well_legs] == [
        ("well-a", "centre", 5),
        ("well-b", "centre", 5),
    ]
    assert (result.host_leg.source_id, result.host_leg.target_id) == ("centre", "host")
    assert result.host_leg.length == pytest.approx(4)
    assert result.objective == sum(leg.length for leg in result.well_legs) + result.host_leg.length
    assert result.chosen_candidate.provenance == "synthetic centre option"
    assert tuple(item.candidate for item in result.evaluations) == tuple(
        sorted(geometry[2], key=lambda candidate: candidate.identifier)
    )


def test_input_order_invariance(geometry):
    """Every ordering must return the same 5+5+4=14 m breakdown and audit."""
    wells, host, candidates = geometry
    expected = optimize_manifold(wells, host, candidates)
    for well_order in permutations(wells):
        for candidate_order in permutations(candidates):
            assert optimize_manifold(well_order, host, candidate_order) == expected


def test_tie_break():
    """Both options cost sqrt(5)+sqrt(5)+1 m; smallest identifier wins."""
    wells = [PlanarPoint("left", (-2, 0), "m"), PlanarPoint("right", (2, 0), "m")]
    host = PlanarPoint("host", (0, 0), "m")
    candidates = [
        ManifoldCandidate("z", (0, -1), "m", "synthetic lower option"),
        ManifoldCandidate("a", (0, 1), "m", "synthetic upper option"),
    ]
    for ordering in permutations(candidates):
        result = optimize_manifold(wells, host, ordering)
        assert result.chosen_candidate.identifier == "a"
        assert result.objective == pytest.approx(2 * sqrt(5) + 1)


def test_host_leg_changes_selection():
    """Midpoint costs 1+1+9=11 m; well option costs 2+0+8=10 m."""
    wells = [PlanarPoint("left", (0, 0), "m"), PlanarPoint("right", (2, 0), "m")]
    host = PlanarPoint("host", (10, 0), "m")
    candidates = [
        ManifoldCandidate("a-midpoint", (1, 0), "m", "synthetic midpoint"),
        ManifoldCandidate("b-well", (2, 0), "m", "synthetic well option"),
    ]
    result = optimize_manifold(wells, host, candidates)
    assert result.chosen_candidate.identifier == "b-well"
    assert result.objective == pytest.approx(10)


def test_kilometres_are_preserved(geometry):
    """The same numeric geometry in km costs 5+5+4=14 km, without conversion."""
    wells, host, candidates = geometry
    result = optimize_manifold(
        [PlanarPoint(w.identifier, w.coordinates, "km") for w in wells],
        PlanarPoint(host.identifier, host.coordinates, "km"),
        [ManifoldCandidate(c.identifier, c.coordinates, "km", c.provenance) for c in candidates],
    )
    assert result.objective == pytest.approx(14)
    assert result.units == "km"


def test_coincident_points():
    """One well, host and candidate at the origin give 0+0=0 m."""
    result = optimize_manifold(
        [PlanarPoint("well", (0, 0), "m")],
        PlanarPoint("host", (0, 0), "m"),
        [ManifoldCandidate("candidate", (0, 0), "m", "synthetic origin")],
    )
    assert result.objective == 0
    assert result.well_legs[0].length == result.host_leg.length == 0


@pytest.mark.parametrize(
    "coordinates",
    [
        (float("nan"), 0), (0, float("inf")), (float("-inf"), 0),
        (), (0,), (0, 0, 0), "00", {"x": 0, "y": 0}, {0, 1},
        (True, 0), (0, False), ("0", 0), (0j, 0), None,
    ],
)
def test_reject_nonfinite_or_ambiguous_coordinates(coordinates):
    """Reject non-real, nonfinite, unordered, labelled or non-planar input."""
    with pytest.raises(ValueError, match="coordinates"):
        PlanarPoint("point", coordinates, "m")


@pytest.mark.parametrize("identifier", ["", " ", " padded", "padded ", None, 1])
def test_reject_ambiguous_identifiers(identifier):
    """Identifiers must be nonempty strings without surrounding whitespace."""
    with pytest.raises(ValueError, match="identifier"):
        PlanarPoint(identifier, (0, 0), "m")


@pytest.mark.parametrize("units", ["degrees", "ft", "M", "", None])
def test_reject_unsupported_units(units):
    """Only explicit planar m and km are supported; no unit guessing occurs."""
    with pytest.raises(ValueError, match="units"):
        PlanarPoint("point", (0, 0), units)


@pytest.mark.parametrize("provenance", ["", " ", None, 1])
def test_reject_missing_provenance(provenance):
    """A candidate needs caller-supplied source text, even for synthetic data."""
    with pytest.raises(ValueError, match="provenance"):
        ManifoldCandidate("candidate", (0, 0), "m", provenance)


@pytest.mark.parametrize("role", ["well", "candidate", "host"])
def test_reject_incompatible_units(geometry, role):
    """Metres and kilometres are compatible dimensions but require explicit conversion."""
    wells, host, candidates = geometry
    if role == "well":
        wells[0] = PlanarPoint(wells[0].identifier, wells[0].coordinates, "km")
    elif role == "candidate":
        c = candidates[0]
        candidates[0] = ManifoldCandidate(c.identifier, c.coordinates, "km", c.provenance)
    else:
        host = PlanarPoint(host.identifier, host.coordinates, "km")
    with pytest.raises(ValueError, match="same units"):
        optimize_manifold(wells, host, candidates)


@pytest.mark.parametrize("duplicate", ["wells", "candidates", "host-well", "host-candidate", "well-candidate"])
def test_reject_duplicate_identifiers(geometry, duplicate):
    """Identifiers are globally unique across wells, host and candidates."""
    wells, host, candidates = geometry
    if duplicate == "wells":
        wells.append(wells[0])
    elif duplicate == "candidates":
        candidates.append(candidates[0])
    elif duplicate == "host-well":
        host = PlanarPoint(wells[0].identifier, host.coordinates, "m")
    else:
        identifier = host.identifier if duplicate == "host-candidate" else wells[0].identifier
        candidates[0] = ManifoldCandidate(identifier, (0, 0), "m", "synthetic duplicate")
    with pytest.raises(ValueError, match="duplicate identifier"):
        optimize_manifold(wells, host, candidates)


@pytest.mark.parametrize("role", ["wells", "candidates"])
def test_reject_empty_sets(geometry, role):
    """A finite search needs at least one well and at least one candidate."""
    wells, host, candidates = geometry
    with pytest.raises(ValueError, match="nonempty"):
        optimize_manifold([] if role == "wells" else wells, host, [] if role == "candidates" else candidates)


@pytest.mark.parametrize("role", ["wells", "host", "candidates", "well-record", "candidate-record"])
def test_reject_ambiguous_records(geometry, role):
    """Mappings, missing records and points without candidate provenance are rejected."""
    wells, host, candidates = geometry
    if role == "wells":
        wells = {w.identifier: w for w in wells}
    elif role == "host":
        host = None
    elif role == "candidates":
        candidates = iter(candidates)
    elif role == "well-record":
        wells = [(0, 0)]
    else:
        candidates = [host]
    with pytest.raises(ValueError):
        optimize_manifold(wells, host, candidates)


def test_coordinate_list_is_copied():
    """The origin remains fixed when the caller mutates its input coordinate list."""
    coordinates = [0, 0]
    point = PlanarPoint("point", coordinates, "m")
    coordinates[0] = 1
    assert point.coordinates == (0, 0)


@pytest.mark.parametrize("candidate_x", [-1e308, 0])
def test_reject_length_overflow(candidate_x):
    """Finite extreme inputs overflow either a difference or the three-leg sum."""
    with pytest.raises(ValueError, match="length"):
        optimize_manifold(
            [PlanarPoint("a", (1e308, 0), "m"), PlanarPoint("b", (1e308, 0), "m")],
            PlanarPoint("host", (1e308, 0), "m"),
            [ManifoldCandidate("candidate", (candidate_x, 0), "m", "synthetic overflow")],
        )


def test_runnable_example(capsys):
    """The example reports the independently derived centre winner, 5+5+4=14 m."""
    example = Path(__file__).resolve().parents[2] / "examples/field_development/subsea_architecture.py"
    runpy.run_path(str(example), run_name="__main__")
    output = capsys.readouterr().out
    assert "centre" in output
    assert "14.0 m" in output
    assert "planar" in output
