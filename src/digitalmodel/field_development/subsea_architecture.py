# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Select a manifold from a finite, caller-supplied set of planar locations.

For candidate c, wells w and explicit host h, the objective is
sum(hypot(w.x - c.x, w.y - c.y)) + hypot(c.x - h.x, c.y - h.y).
This is the Euclidean distance definition applied to a star layout: each well
has one manifold leg and the manifold has one host leg. No extra allowances,
weights, terrain, pressure or thermal constraints are inferred.

Coordinates must already share a Cartesian reference frame and one explicit
unit (m or km). Geographic longitude/latitude must be projected by the caller.
Unit tags do not verify the reference frame or the physical feasibility of a
candidate. Results are planar length objectives, not qualified installation
routes or continuous global optima. See examples/field_development/subsea_architecture.py
for an independently calculated synthetic example.
"""

from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass
from math import fsum, hypot, isfinite
from numbers import Real
from typing import TypeVar


def _require_text(value: str, label: str) -> None:
    if not isinstance(value, str) or not value.strip() or value != value.strip():
        raise ValueError(f"{label} must be nonempty text without surrounding whitespace")


@dataclass(frozen=True)
class PlanarPoint:
    """An identified Cartesian location in explicit m or km.

    Coordinates are exactly an ordered pair of finite real numbers; booleans,
    numeric strings, labelled mappings and extra dimensions are rejected.
    Coordinate lists are accepted and copied to an immutable tuple.
    """

    identifier: str
    coordinates: tuple[float, float]
    units: str

    def __post_init__(self) -> None:
        """Validate and freeze the caller's coordinate pair."""
        _require_text(self.identifier, "identifier")
        if not isinstance(self.units, str) or self.units not in ("m", "km"):
            raise ValueError("units must be explicit planar 'm' or 'km'")
        # A planar Cartesian location has exactly two ordered components.
        if not isinstance(self.coordinates, (tuple, list)) or len(self.coordinates) != 2:
            raise ValueError("coordinates must be an ordered pair of finite real numbers")
        if any(isinstance(v, bool) or not isinstance(v, Real) for v in self.coordinates):
            raise ValueError("coordinates must be finite real numbers, excluding booleans")
        try:
            x, y = (float(v) for v in self.coordinates)
        except (OverflowError, ValueError) as exc:
            raise ValueError("coordinates must be finite real numbers") from exc
        if not (isfinite(x) and isfinite(y)):
            raise ValueError("coordinates must be finite real numbers")
        object.__setattr__(self, "coordinates", (x, y))


@dataclass(frozen=True)
class ManifoldCandidate(PlanarPoint):
    """A candidate location with caller-supplied provenance text.

    Provenance identifies the supplied option's source or construction basis;
    it is preserved verbatim, without treating it as evidence of feasibility.
    """

    provenance: str

    def __post_init__(self) -> None:
        """Require both valid coordinates and a stated candidate source."""
        super().__post_init__()
        _require_text(self.provenance, "provenance")


@dataclass(frozen=True)
class FlowlineLeg:
    """A straight planar leg whose length uses the result's units."""

    source_id: str
    target_id: str
    length: float


@dataclass(frozen=True)
class CandidateEvaluation:
    """A supplied candidate, including its provenance, and its total length."""

    candidate: ManifoldCandidate
    objective: float


@dataclass(frozen=True)
class ArchitectureResult:
    """Chosen location, all chosen legs, units and the finite-search audit.

    Well legs and evaluations are sorted by identifier. The objective includes
    all well legs and exactly one host leg, even if endpoints coincide.
    """

    chosen_candidate: ManifoldCandidate
    well_legs: tuple[FlowlineLeg, ...]
    host_leg: FlowlineLeg
    objective: float
    units: str
    evaluations: tuple[CandidateEvaluation, ...]


_Point = TypeVar("_Point", bound=PlanarPoint)


def _ordered_points(
    points: Sequence[_Point], record_type: type[_Point], label: str
) -> tuple[_Point, ...]:
    if isinstance(points, (str, bytes)) or not isinstance(points, Sequence):
        raise ValueError(f"{label} must be a finite nonempty sequence")
    if not points:
        raise ValueError(f"{label} must be a finite nonempty sequence")
    if any(not isinstance(point, record_type) for point in points):
        raise ValueError(f"{label} must contain {record_type.__name__} records")
    return tuple(sorted(points, key=lambda point: point.identifier))


def _validate_network(points: Sequence[PlanarPoint]) -> None:
    identifiers = [point.identifier for point in points]
    if len(set(identifiers)) != len(identifiers):
        raise ValueError("duplicate identifier across wells, host or candidates")
    if len({point.units for point in points}) != 1:
        raise ValueError("wells, host and candidates must use the same units")


def _leg(source: PlanarPoint, target: PlanarPoint) -> FlowlineLeg:
    sx, sy = source.coordinates
    tx, ty = target.coordinates
    length = hypot(sx - tx, sy - ty)
    if not isfinite(length):
        raise ValueError("leg length exceeds finite numeric range")
    return FlowlineLeg(source.identifier, target.identifier, length)


def _total_length(well_legs: tuple[FlowlineLeg, ...], host_leg: FlowlineLeg) -> float:
    try:
        total = fsum([leg.length for leg in well_legs] + [host_leg.length])
    except OverflowError as exc:
        raise ValueError("total length exceeds finite numeric range") from exc
    if not isfinite(total):
        raise ValueError("total length exceeds finite numeric range")
    return total


def optimize_manifold(
    wells: Sequence[PlanarPoint],
    host: PlanarPoint,
    candidates: Sequence[ManifoldCandidate],
) -> ArchitectureResult:
    """Minimize total straight-line length over the supplied candidates.

    The candidate with the smallest floating-point objective wins. Exactly
    equal totals select the lexicographically smallest identifier (case
    sensitive, Unicode code-point order), without a tolerance. Sorting wells
    and candidates by identifier and using fsum makes the full result invariant
    to input ordering. Every candidate is evaluated; no locations are generated.

    Args:
        wells: Nonempty finite sequence of identified planar well locations.
        host: Explicit planar host location in the wells' coordinate frame.
        candidates: Nonempty finite sequence with coordinates and provenance.

    Returns:
        The chosen candidate, ordered well legs, one host leg, total length,
        explicit units and ordered candidate evaluations including provenance.

    Raises:
        ValueError: Records are ambiguous, identifiers repeat, unit tags differ,
            a set is empty, or a leg or total cannot be represented finitely.
    """
    ordered_wells = _ordered_points(wells, PlanarPoint, "wells")
    ordered_candidates = _ordered_points(candidates, ManifoldCandidate, "candidates")
    if not isinstance(host, PlanarPoint):
        raise ValueError("host must be an explicit PlanarPoint record")
    _validate_network((*ordered_wells, host, *ordered_candidates))

    evaluations = []
    for candidate in ordered_candidates:
        well_legs = tuple(_leg(well, candidate) for well in ordered_wells)
        host_leg = _leg(candidate, host)
        total = _total_length(well_legs, host_leg)
        evaluations.append(CandidateEvaluation(candidate, total))
    best = min(
        evaluations,
        key=lambda item: (item.objective, item.candidate.identifier),
    )
    chosen = best.candidate
    return ArchitectureResult(
        chosen,
        tuple(_leg(well, chosen) for well in ordered_wells),
        _leg(chosen, host),
        best.objective,
        host.units,
        tuple(evaluations),
    )
