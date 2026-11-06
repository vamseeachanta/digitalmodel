# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Select nearby metocean metadata without transferring wave/current values.

Coordinates are WGS 84 longitude/latitude in decimal degrees. Distances are
shortest surface geodesics in metres, not propagation paths or evidence that
a source represents the target. No source retrieval or licensed solver is used.

Basis: Algorithms for geodesics (2013), sections 2 and 4, open preprint at
https://arxiv.org/pdf/1109.4448 ; PROJ implementation documentation at
https://proj.org/en/stable/geodesic.html .
"""

from __future__ import annotations

import math
from collections.abc import Iterable
from dataclasses import dataclass, field
from typing import Literal

from pyproj import Geod

from .core.geometry import GeoPoint

# WGS84 parameters are supplied by PROJ, not duplicated or approximated here.
# PROJ, Ellipsoids / Built-in ellipsoids, WGS84 entry (online edition):
# https://proj.org/en/stable/usage/ellipsoids.html
_GEODESIC = Geod(ellps="WGS84")


def _validate_coordinates(longitude: float, latitude: float) -> None:
    # Geographic degree bounds: RFC 7946 (2016), sections 4 and 5.3.
    try:
        valid = (
            not isinstance(longitude, bool)
            and not isinstance(latitude, bool)
            and math.isfinite(longitude)
            and math.isfinite(latitude)
            and -180 <= longitude <= 180
            and -90 <= latitude <= 90
        )
    except TypeError:
        valid = False
    if not valid:
        raise ValueError(
            "Invalid coordinate: finite WGS 84 longitude/latitude required"
        )


def _variable_names(variables: Iterable[str]) -> frozenset[str]:
    if isinstance(variables, str):
        raise ValueError("variables must be a collection, not a single string")
    names = frozenset(variables)
    if not names or any(
        not isinstance(name, str) or not name.strip() for name in names
    ):
        raise ValueError("variables must contain nonblank string names")
    return names


@dataclass(frozen=True)
class MetoceanSource:
    """Caller-supplied source metadata with a provenance reference.

    Variable names are caller-defined exact identifiers, such as ``hs`` and
    ``tp``. They declare availability only, not units, time coverage, quality,
    licensing, or compatibility. ``reference`` identifies the metadata basis;
    this module does not fetch or verify it.
    """

    source_id: str
    longitude: float
    latitude: float
    variables: frozenset[str]
    reference: str

    def __post_init__(self) -> None:
        """Validate metadata and snapshot the supplied variable collection."""
        _validate_coordinates(self.longitude, self.latitude)
        for name in ("source_id", "reference"):
            value = getattr(self, name)
            if not isinstance(value, str) or not value.strip():
                raise ValueError(f"{name} must be a nonblank string")
        object.__setattr__(self, "variables", _variable_names(self.variables))


@dataclass(frozen=True)
class SourceMatch:
    """A geographic candidate, with physical extrapolation unevaluated."""

    source: MetoceanSource
    distance_m: float
    extrapolation_status: Literal["NOT_EVALUATED"] = field(
        default="NOT_EVALUATED", init=False
    )


def nearest_sources(
    target: GeoPoint,
    sources: Iterable[MetoceanSource],
    *,
    required_variables: Iterable[str],
    limit: int = 1,
) -> list[SourceMatch]:
    """Rank sources containing all requested variables by geodesic distance.

    ``target.x`` is longitude and ``target.y`` latitude; elevation is ignored.
    ``limit`` is a positive result count, with one result by default. Exact
    distance ties use source identifier order. Duplicate identifiers are an
    error even in entries excluded by the variable filter. Empty catalogues
    and no qualifying entries return an empty list; no fallback is inferred.

    The inverse geodesic gives s12 for each coordinate pair (Algorithms for
    geodesics, 2013, section 4); ordering by (s12, source_id) is an API policy.
    No distance cutoff or site-acceptance threshold is assumed. Bathymetry,
    sheltering, fetch, tides, and wave/current transformations are NOT_EVALUATED.
    """
    _validate_coordinates(target.longitude, target.latitude)
    if isinstance(limit, bool) or not isinstance(limit, int) or limit <= 0:
        raise ValueError("limit must be a positive integer")
    required = _variable_names(required_variables)
    seen: set[str] = set()
    matches: list[SourceMatch] = []
    for source in sources:
        if source.source_id in seen:
            raise ValueError("Duplicate source_id in catalogue")
        seen.add(source.source_id)
        if not required.issubset(source.variables):
            continue
        _, _, distance_m = _GEODESIC.inv(
            target.longitude, target.latitude, source.longitude, source.latitude
        )
        matches.append(SourceMatch(source, distance_m))
    matches.sort(key=lambda match: (match.distance_m, match.source.source_id))
    return matches[:limit]
