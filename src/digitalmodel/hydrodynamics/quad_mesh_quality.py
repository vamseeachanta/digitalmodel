# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Geometric quality of ordered four-node surface panels, without a solver.

[S1] Coreform Cubit 2026.8, Quadrilateral Metrics, definitions of minimum/
maximum angle and Jacobian (corners and centre):
https://coreform.com/cubit_help/mesh_generation/mesh_quality_assessment/quadrilateral_metrics.htm
[S2] Introduction to Solid Mechanics, Quadrilateral Elements, shape functions
(undated online text, accessed 2026-10-19):
https://engcourses-uofa.ca/books/introduction-to-solid-mechanics/finite-element-analysis/two-dimensional-solid-elements/quadrilateral-elements/

The unit-square map and its corner-minimum proof are derived from [S2] in
``docs/domains/hull_library/quad-mesh-quality.md``. All numerical acceptance
limits are caller inputs. Strict positive Jacobian is a declared geometric
criterion, not a solver limit. Warped panels retain only a projected diagnostic.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from fractions import Fraction
import math
from typing import Literal, TypeVar

import numpy as np
from numpy.typing import ArrayLike, NDArray

Status = Literal["PASS", "FAIL", "NOT_EVALUATED"]
Array = NDArray[np.float64]


@dataclass(frozen=True)
class _Interval:
    """Enclose exact arithmetic on binary64 inputs with outward rounding."""

    lower: float
    upper: float

    @classmethod
    def _rounded(cls, values: list[float]) -> _Interval:
        if not all(math.isfinite(value) for value in values):
            return cls(-math.inf, math.inf)
        return cls(math.nextafter(min(values), -math.inf),
                   math.nextafter(max(values), math.inf))

    def __add__(self, other: _Interval) -> _Interval:
        return self._rounded([self.lower + other.lower, self.upper + other.upper])

    def __sub__(self, other: _Interval) -> _Interval:
        return self._rounded([self.lower - other.upper, self.upper - other.lower])

    def __mul__(self, other: _Interval) -> _Interval:
        return self._rounded([a * b for a in (self.lower, self.upper)
                              for b in (other.lower, other.upper)])


_Scalar = TypeVar("_Scalar", _Interval, Fraction)


def _cross(a: list[_Scalar], b: list[_Scalar]) -> list[_Scalar]:
    return [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0]]


def _corner_determinants(points: list[list[_Scalar]]) -> list[_Scalar]:
    """Corner cross products dotted with the unnormalized centre normal."""
    edges = [[points[(i + 1) % 4][j] - points[i][j] for j in range(3)]
             for i in range(4)]
    normal = _cross([points[2][j] - points[0][j] for j in range(3)],
                    [points[3][j] - points[1][j] for j in range(3)])
    corners = [_cross(edges[i - 1], edges[i]) for i in range(4)]
    return [c[0] * normal[0] + c[1] * normal[1] + c[2] * normal[2] for c in corners]


def _reliable_turns(points: Array, turns: Array, scale: float,
                    normal_length: float) -> Array:
    """Certify signs before normalization; resolve cancellation exactly."""
    bounds = _corner_determinants([
        [_Interval(float(x), float(x)) for x in point] for point in points
    ])
    exact = None
    for i, bound in enumerate(bounds):
        if ((bound.lower > 0 and turns[i] > 0)
                or (bound.upper < 0 and turns[i] < 0)):
            continue
        if exact is None:
            exact = _corner_determinants([
                [Fraction(float(x)) for x in point] for point in points
            ])
        # Each determinant contains four coordinate differences. Divide out
        # scale^4 exactly, then the normalized centre-normal length, to recover
        # the normalized projected turn without a cancellation residual.
        turns[i] = float(exact[i] / Fraction(scale) ** 4) / normal_length
        if exact[i] != 0 and turns[i] == 0:
            raise ValueError("panel Jacobian exceeds floating-point range")
    return turns


@dataclass(frozen=True)
class QualityCheck:
    """A result and the geometric basis or missing qualification."""

    status: Status
    reason: str


def _combined(statuses: list[Status]) -> Status:
    if "FAIL" in statuses:
        return "FAIL"
    if not statuses or "NOT_EVALUATED" in statuses:
        return "NOT_EVALUATED"
    return "PASS"


@dataclass(frozen=True)
class QuadQualityCriteria:
    """Optional inclusive geometric bounds, with a mandatory declared basis.

    Jacobian units are coordinate units squared on the reference domain [0,1]^2.
    Bounds do not establish solver eligibility, even if their basis names a solver.
    """

    basis: str
    max_edge_ratio: float | None = None
    min_angle_deg: float | None = None
    max_angle_deg: float | None = None
    min_jacobian: float | None = None

    def __post_init__(self) -> None:
        if not isinstance(self.basis, str) or not self.basis.strip():
            raise ValueError("criteria basis must be nonempty")
        for name in ("max_edge_ratio", "min_angle_deg", "max_angle_deg", "min_jacobian"):
            value = getattr(self, name)
            if value is not None and (not math.isfinite(value) or value < 0):
                raise ValueError(f"{name} must be finite and nonnegative")
        # Longest/shortest >= 1 by definition; [S1], Maximum Angle table row,
        # gives a full-range endpoint of 360 degrees. These are domains,
        # not adopted acceptance limits.
        if self.max_edge_ratio is not None and self.max_edge_ratio < 1:
            raise ValueError("max_edge_ratio must be at least one")
        for value in (self.min_angle_deg, self.max_angle_deg):
            if value is not None and value > 360:
                raise ValueError("angle bounds must be at most a full turn")
        if (self.min_angle_deg is not None and self.max_angle_deg is not None
                and self.min_angle_deg > self.max_angle_deg):
            raise ValueError("minimum angle exceeds maximum angle")


@dataclass(frozen=True)
class QuadPanelQuality:
    """Panel diagnostics; undefined measurements use None, never infinity/NaN."""

    panel_index: int
    edge_ratio: float | None
    corner_angles_deg: tuple[float, ...] | None
    convex: bool | None
    min_projected_jacobian: float | None
    max_plane_distance: float | None
    checks: dict[str, QualityCheck]

    @property
    def status(self) -> Status:
        return _combined([check.status for check in self.checks.values()])


@dataclass(frozen=True)
class QuadMeshQualityReport:
    """Shared geometry report, independent of hydrostatics and outward winding."""

    panels: tuple[QuadPanelQuality, ...]
    planarity_tolerance: float
    criteria: QuadQualityCriteria | None
    solver_qualification: QualityCheck = field(default_factory=lambda: QualityCheck(
        "NOT_EVALUATED", "No solver-specific qualification is implemented.",
    ))

    @property
    def status(self) -> Status:
        """Aggregate geometric checks only; missing bounds prevent a full PASS."""
        return _combined([panel.status for panel in self.panels])


def _panel_quality(
    points: Array, index: int, tolerance: float, criteria: QuadQualityCriteria | None,
) -> QuadPanelQuality:
    checks = {
        key: QualityCheck("NOT_EVALUATED", "Geometry does not define this measurement.")
        for key in ("edge_ratio", "corner_angles", "convexity", "mapping_jacobian",
                    "planarity")
    }
    for key in ("edge_ratio", "corner_angles"):
        checks[key] = QualityCheck("NOT_EVALUATED", "Required geometric bound was not supplied.")
    edges = np.roll(points, -1, axis=0) - points
    scale = float(np.max(np.hypot.reduce(edges, axis=1)))
    if not math.isfinite(scale):
        raise ValueError("panel edge lengths exceed floating-point range")
    if scale == 0:
        for key in ("edge_ratio", "convexity", "mapping_jacobian"):
            checks[key] = QualityCheck("FAIL", "All panel vertices coincide.")
        return QuadPanelQuality(index, None, None, False, None, None, checks)

    # Normalization preserves geometry in exact arithmetic. Certify corner signs
    # from the original inputs below; restore J with scale^2 as derived in [S2].
    local = (points - points[0]) / scale
    edges = np.roll(local, -1, axis=0) - local
    lengths = np.hypot.reduce(edges, axis=1)
    ratio = None
    if float(np.min(lengths)) == 0:
        checks["edge_ratio"] = QualityCheck("FAIL", "A panel edge has zero length.")
    else:
        ratio = float(np.max(lengths) / np.min(lengths))
        if not math.isfinite(ratio):
            raise ValueError("panel edge ratio exceeds floating-point range")
        if criteria is not None and criteria.max_edge_ratio is not None:
            checks["edge_ratio"] = QualityCheck(
                "PASS" if ratio <= criteria.max_edge_ratio else "FAIL", criteria.basis,
            )

    # [S2] on [0,1]^2: the diagonal cross product is twice the centre
    # derivative cross product. Its direction follows the panel winding.
    normal = np.cross(local[2] - local[0], local[3] - local[1])
    normal_length = float(np.hypot.reduce(normal))
    if normal_length == 0:
        for key in ("convexity", "mapping_jacobian"):
            checks[key] = QualityCheck("FAIL", "Bilinear map is singular at its centre.")
        return QuadPanelQuality(index, ratio, None, False, None, None, checks)
    normal /= normal_length
    distance = float(np.max(np.abs(local @ normal)) * scale)
    # Corner derivatives on [0,1]^2 equal the adjacent edges up to signs.
    # [S2] gives an affine projected determinant, so its minimum is a corner.
    turns = np.cross(np.roll(edges, 1, axis=0), edges) @ normal
    turns = _reliable_turns(points, turns, scale, normal_length)
    normalized_minimum = float(np.min(turns))
    minimum = normalized_minimum * scale * scale
    if not math.isfinite(minimum) or (normalized_minimum != 0 and minimum == 0):
        raise ValueError("panel Jacobian exceeds floating-point range")
    planar = distance <= tolerance
    checks["planarity"] = QualityCheck(
        "PASS" if planar else "FAIL", "Distance to the centre-normal plane through p0 "
        "compared with the caller's absolute planarity tolerance.",
    )
    if not planar:
        for key in ("convexity", "mapping_jacobian", "corner_angles"):
            checks[key] = QualityCheck(
                "NOT_EVALUATED", "Warped panel: projection cannot qualify the surface map.",
            )
        return QuadPanelQuality(index, ratio, None, None, minimum, distance, checks)

    convex = bool(np.all(turns > 0))
    checks["convexity"] = QualityCheck(
        "PASS" if convex else "FAIL", "Declared geometry: all projected corner turns "
        "must be strictly positive relative to the winding-defined centre normal.",
    )
    jacobian_ok = minimum > 0
    if criteria is not None and criteria.min_jacobian is not None:
        jacobian_ok = jacobian_ok and minimum >= criteria.min_jacobian
    checks["mapping_jacobian"] = QualityCheck(
        "PASS" if jacobian_ok else "FAIL", "Declared geometry: positive minimum on "
        "[0,1]^2; any caller-supplied minimum is an additional inclusive bound.",
    )

    angles = None
    projected_edges = edges - np.outer(edges @ normal, normal)
    if np.all(np.hypot.reduce(projected_edges, axis=1) > 0):
        previous = -np.roll(projected_edges, 1, axis=0)
        # atan2(|a cross b|, a dot b) is the unsigned included angle in [S1].
        # A negative turn selects its reflex complement, one full turn minus it.
        unsigned = np.degrees(np.arctan2(
            np.hypot.reduce(np.cross(previous, projected_edges), axis=1),
            np.sum(previous * projected_edges, axis=1),
        ))
        angles = tuple(float(a) for a in np.where(turns < 0, 360 - unsigned, unsigned))
        if criteria is not None and (criteria.min_angle_deg is not None
                                     or criteria.max_angle_deg is not None):
            angle_ok = ((criteria.min_angle_deg is None
                         or min(angles) >= criteria.min_angle_deg)
                        and (criteria.max_angle_deg is None
                             or max(angles) <= criteria.max_angle_deg))
            checks["corner_angles"] = QualityCheck(
                "PASS" if angle_ok else "FAIL", criteria.basis,
            )
    else:
        checks["corner_angles"] = QualityCheck("FAIL", "A projected edge has zero length.")
    return QuadPanelQuality(index, ratio, angles, convex, minimum, distance, checks)


def assess_quad_mesh(
    nodes: ArrayLike, panels: ArrayLike, *, planarity_tolerance: float,
    criteria: QuadQualityCriteria | None = None,
) -> QuadMeshQualityReport:
    """Assess true quads with cyclic perimeter connectivity and finite coordinates.

    ``nodes`` has shape (n,3), ``panels`` has integer shape (m,4). Repeated-index
    triangle encodings are rejected explicitly; this API only assesses quads.
    Planarity tolerance is a required nonnegative input in coordinate units.
    Angles and convexity use projection onto the centre-normal plane through p0
    only when its maximum vertex distance is within that tolerance. The minimum
    projected Jacobian has coordinate units squared; it is not a scaled Jacobian.
    """
    if not math.isfinite(planarity_tolerance) or planarity_tolerance < 0:
        raise ValueError("planarity_tolerance must be finite and nonnegative")
    coordinates = np.asarray(nodes, dtype=float)
    connectivity = np.asarray(panels)
    if coordinates.ndim != 2 or coordinates.shape[1] != 3:
        raise ValueError("nodes must have shape (n, 3)")
    if not np.isfinite(coordinates).all():
        raise ValueError("nodes must be finite")
    if connectivity.ndim != 2 or connectivity.shape[1] != 4:
        raise ValueError("panels must have shape (m, 4)")
    if not np.issubdtype(connectivity.dtype, np.integer):
        raise ValueError("panel indices must be integers")
    if np.any(connectivity < 0) or np.any(connectivity >= len(coordinates)):
        raise ValueError("panel indices must reference existing nodes")
    if any(len(set(row)) != 4 for row in connectivity):
        raise ValueError("quad panels require four distinct node indices")
    try:
        with np.errstate(over="raise", invalid="raise", divide="raise"):
            results = tuple(
                _panel_quality(coordinates[row], index, planarity_tolerance, criteria)
                for index, row in enumerate(connectivity)
            )
    except FloatingPointError as error:
        raise ValueError("panel arithmetic exceeds floating-point range") from error
    return QuadMeshQualityReport(results, float(planarity_tolerance), criteria)
