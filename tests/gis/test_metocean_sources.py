# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline source-selection tests with independent distance expectations.

All catalogue entries below are synthetic, not observations. Coordinate units
and geographic bounds follow RFC 7946 (2016), sections 3.1.1, 4 and 5.3.
"""

import math
from dataclasses import FrozenInstanceError

import pytest

from digitalmodel.gis.core.geometry import GeoPoint
from digitalmodel.gis.metocean_sources import MetoceanSource, nearest_sources


def source(source_id, longitude, latitude, variables=("hs", "tp")):
    """Build synthetic metadata; the reference is deliberately not a live feed."""
    return MetoceanSource(
        source_id=source_id,
        longitude=longitude,
        latitude=latitude,
        variables=variables,
        reference="Synthetic catalogue for independent calculations",
    )


@pytest.mark.parametrize(
    "latitude1,latitude2,longitude2,expected_m",
    [
        (-30.12345, -30.12344, 0.00005, 4.944208),
        (-30.0, 29.9, 179.8, 19989832.827610),
    ],
)
def test_published_inverse_examples(latitude1, latitude2, longitude2, expected_m):
    """Algorithms for geodesics (2013), open preprint, Tables 3 and 4–6.

    https://arxiv.org/pdf/1109.4448 (2012 revision), WGS 84 examples:
    Table 3 gives s12=4.944208 m; Table 6 gives s12=19989832.827610 m.
    Both set longitude1=0 because only the longitude difference is specified.
    The absolute comparison budget is one micrometre, the tables' last printed
    distance place. It is a numerical test budget, not a site acceptance limit.
    """
    match = nearest_sources(
        GeoPoint(0.0, latitude1),
        [source("example", longitude2, latitude2)],
        required_variables={"hs"},
    )[0]
    assert match.distance_m == pytest.approx(expected_m, rel=0, abs=1e-6)


@pytest.mark.parametrize("target_lon,source_lon", [(0.0, 2.0), (179.0, -179.0)])
def test_equatorial_arc_and_antimeridian(target_lon, source_lon):
    """Independent hand calculation: the shorter equatorial arc is two degrees.

    s = a * delta_lambda = 6378137 * (2*pi/180) metres, using the WGS84
    equatorial radius in PROJ's Ellipsoids / Built-in ellipsoids table:
    https://proj.org/en/stable/usage/ellipsoids.html . Degrees-to-radians is
    the definition of angular units. The micrometre numerical budget follows
    the published examples above and exceeds PROJ's stated 15 nm round-off.
    """
    match = nearest_sources(
        GeoPoint(target_lon, 0.0),
        [source("equatorial", source_lon, 0.0)],
        required_variables={"hs"},
    )[0]
    expected_m = 6378137.0 * (2 * math.pi / 180)
    assert match.distance_m == pytest.approx(expected_m, rel=0, abs=1e-6)


def test_ranking_filtering_and_limit_preserve_metadata():
    """On the equator, arc lengths are proportional to longitude separation.

    Hand ordering: 0 < 1 < 2 degrees from target (0,0). The colocated entry
    lacks tp, so the qualifying order is the one-degree, then two-degree entry.
    These are synthetic inputs; s=a*delta_lambda has the basis cited above.
    """
    far = source("far", 2.0, 0.0)
    near = source("near", 1.0, 0.0)
    missing = source("missing", 0.0, 0.0, variables=("hs",))
    matches = nearest_sources(
        GeoPoint(0.0, 0.0),
        iter([far, missing, near]),
        required_variables={"hs", "tp"},
        limit=10,
    )
    assert [match.source for match in matches] == [near, far]
    assert matches[0].source is near
    assert matches[0].extrapolation_status == "NOT_EVALUATED"
    assert matches[0].source.reference == near.reference
    default_matches = nearest_sources(
        GeoPoint(0.0, 0.0), [far, near], required_variables={"tp"}
    )
    assert len(default_matches) == 1
    assert default_matches[0].source is near


def test_exact_ties_use_source_id_and_colocated_distance_is_zero():
    """Identical coordinates have zero path length; identifiers break exact ties."""
    entries = [source("b", 0.0, 0.0), source("a", 0.0, 0.0)]
    for ordered in (entries, list(reversed(entries))):
        matches = nearest_sources(
            GeoPoint(0.0, 0.0), ordered, required_variables={"hs"}, limit=2
        )
        assert [match.source.source_id for match in matches] == ["a", "b"]
        assert [match.distance_m for match in matches] == [0.0, 0.0]


@pytest.mark.parametrize("latitude", [-90.0, 90.0])
def test_pole_longitudes_identify_same_location(latitude):
    """RFC 7946 (2016), section 5.3: all meridians meet at each pole."""
    matches = nearest_sources(
        GeoPoint(180.0, latitude),
        [source("pole", -180.0, latitude)],
        required_variables={"hs"},
    )
    assert matches[0].distance_m == 0.0


@pytest.mark.parametrize("entries", [[], [source("only-current", 0, 0, ("speed",))]])
def test_no_eligible_source_returns_empty_list(entries):
    """An empty candidate set has no nearest member; no fallback is invented."""
    assert nearest_sources(GeoPoint(0.0, 0.0), entries, required_variables={"hs"}) == []


@pytest.mark.parametrize(
    "longitude,latitude",
    [(181, 0), (-181, 0), (0, 91), (0, -91), (math.nan, 0), (0, math.inf)],
)
def test_invalid_coordinates_rejected_for_source_and_target(longitude, latitude):
    """Outside RFC 7946 sections 4/5.3 geographic bounds, or non-finite."""
    with pytest.raises(ValueError, match="coordinate"):
        source("bad", longitude, latitude)
    with pytest.raises(ValueError, match="coordinate"):
        nearest_sources(GeoPoint(longitude, latitude), [], required_variables={"hs"})


def test_duplicate_identifiers_rejected_even_when_filtered_out():
    """A catalogue identifier must identify one source, independent of filters."""
    with pytest.raises(ValueError, match="Duplicate"):
        nearest_sources(
            GeoPoint(0.0, 0.0),
            [source("same", 0, 0), source("same", 1, 0, ("speed",))],
            required_variables={"hs"},
        )


@pytest.mark.parametrize("limit", [0, -1, 1.5, True])
def test_invalid_limit_rejected(limit):
    """A result count must be a positive integer; this is an API constraint."""
    with pytest.raises(ValueError, match="limit"):
        nearest_sources(GeoPoint(0.0, 0.0), [], required_variables={"hs"}, limit=limit)


@pytest.mark.parametrize("variables", [(), ("",), (" ",), "hs", (None,)])
def test_invalid_variables_rejected(variables):
    """Variable names must be an explicit nonempty collection of nonblank strings."""
    with pytest.raises(ValueError, match="variables"):
        source("bad", 0, 0, variables)
    with pytest.raises(ValueError, match="variables"):
        nearest_sources(GeoPoint(0, 0), [], required_variables=variables)


@pytest.mark.parametrize("field", ["source_id", "reference"])
def test_blank_metadata_rejected(field):
    """Selection must retain a usable identifier and provenance reference."""
    metadata = dict(
        source_id="test",
        longitude=0,
        latitude=0,
        variables=frozenset({"hs"}),
        reference="synthetic",
    )
    metadata[field] = " "
    with pytest.raises(ValueError, match=field):
        MetoceanSource(**metadata)


def test_metadata_is_immutable_and_snapshots_variable_collection():
    """Changing caller metadata later cannot alter an already ranked source."""
    variables = {"hs"}
    entry = MetoceanSource("test", 0, 0, variables, "synthetic")
    variables.add("tp")
    assert entry.variables == frozenset({"hs"})
    with pytest.raises(FrozenInstanceError):
        entry.longitude = 1.0
