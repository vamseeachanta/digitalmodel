# ABOUTME: 2D river-bottom effective-area — projects an axial x circumferential
# ABOUTME: metal-loss grid to a 1D RSTRENG profile (max / area-weighted rules).
"""2D river-bottom effective-area assessment for corroded pipe.

The validated 1D RSTRENG effective-area engine
(:func:`digitalmodel.asset_integrity.corroded_pipe.rstreng_effective_area`)
consumes a single river-bottom *axial* metal-loss profile ``d(x)``. Inline
inspection and laser-scan data, however, are a 2D **grid** of metal-loss depth
over both the axial (``x``) and circumferential (``theta``) directions:
``d[a][c]`` = depth at axial station ``a`` and circumferential column ``c``.

This module is a thin **projection layer** that reduces the 2D grid to a 1D
effective axial profile ``d_eff(x_a)`` and then calls the 1D engine *verbatim*
(the engine is reused unchanged). Two projection rules are provided:

* **MAX** (default, conservative — how RSTRENG forms the river-bottom path):
  ``d_eff[a] = max_c d(x_a, theta_c)`` — the deepest pit at each axial station.
  This is the standard, defensible river-bottom projection: it never
  under-predicts metal loss because it takes the worst pit at every station,
  regardless of whether the deepest pits actually line up axially.

* **AREA-WEIGHTED** (alternative, less conservative):
  ``d_eff[a] = (1/w) * sum_c d(x_a, theta_c) * dtheta_c`` averaged over the
  affected circumferential width ``w = sum_c dtheta_c``. Because the 1D engine
  has no notion of circumferential width, this rule **requires an explicit
  affected-width input** (``circ_weights``, the arc width represented by each
  column; their sum is the affected width ``w``). When ``circ_weights`` is
  omitted, equal angular spacing across the affected arc is assumed, so the
  rule reduces to the plain circumferential mean.

  .. warning::
     The area-weighted rule **under-predicts** the failure pressure hazard
     (i.e. predicts a higher, less conservative burst pressure) when the
     deepest pits do not co-locate at the same axial station, because averaging
     dilutes an isolated deep pit. Prefer MAX for fitness-for-service verdicts;
     AREA-WEIGHTED is offered for sensitivity / less-conservative screening.

SIMPLIFICATION: this is a *simple projection* (collapse each axial station to a
scalar, then run the validated 1D method). It is **not** a full 2D interacting-
flaw coalescence assessment (e.g. circumferential interaction spacing rules,
combined axial+circumferential stress). See the validation doc
``docs/domains/rstreng-2d-validation-2026-06-29.md``.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional, Sequence

import numpy as np

from digitalmodel.asset_integrity.corroded_pipe import (
    CorrodedPipeResult,
    rstreng_effective_area,
)

# Default safety factor — mirrors corroded_pipe / ASME B31G practice (1.39).
DEFAULT_SAFETY_FACTOR = 1.39

PROJECTION_MAX = "max"
PROJECTION_AREA_WEIGHTED = "area_weighted"
_PROJECTIONS = (PROJECTION_MAX, PROJECTION_AREA_WEIGHTED)


@dataclass
class RiverBottom2DResult:
    """Result of a 2D river-bottom effective-area assessment.

    Surfaces the projection rule used and the governing axial segment, and
    carries the full 1D :class:`CorrodedPipeResult` (``result``) so every
    failure-pressure field is available unchanged.
    """

    projection_rule: str  # "max" | "area_weighted"
    axial_positions_in: list  # the axial stations x_a (in)
    d_eff_in: list  # projected 1D effective-depth profile
    governing_segment: tuple  # (i, j) axial indices of worst segment
    result: CorrodedPipeResult  # full 1D RSTRENG result (verbatim)
    within_applicability: bool  # max(d)/t <= 0.80 (B31G validity)
    details: dict = field(default_factory=dict)

    # --- convenience pass-throughs to the embedded 1D result ---------------
    @property
    def failure_pressure_psi(self) -> float:
        return self.result.failure_pressure_psi

    @property
    def intact_pressure_psi(self) -> float:
        return self.result.intact_pressure_psi

    @property
    def safe_pressure_psi(self) -> float:
        return self.result.safe_pressure_psi

    @property
    def rsf(self) -> float:
        return self.result.rsf


def project_grid(
    depth_grid: Sequence[Sequence[float]],
    t: float,
    *,
    projection: str = PROJECTION_MAX,
    circ_weights: Optional[Sequence[float]] = None,
) -> np.ndarray:
    """Reduce a 2D metal-loss grid ``d[a][c]`` to a 1D axial profile ``d_eff``.

    Args:
        depth_grid: rectangular grid of metal-loss DEPTH (in); row = axial
            station, column = circumferential measurement. 0 = full wall.
        t: nominal wall thickness (in), for validation only.
        projection: ``"max"`` (deepest pit per station) or ``"area_weighted"``
            (arc-width-weighted circumferential mean over the affected width).
        circ_weights: arc width ``dtheta_c`` represented by each circumferential
            column (any consistent unit); their sum is the affected width ``w``.
            Used only by the area-weighted rule; ``None`` => equal weights
            (plain circumferential mean).

    Returns:
        1D numpy array ``d_eff`` of length = number of axial stations.
    """
    grid = np.asarray(depth_grid, dtype=float)
    if grid.ndim != 2 or grid.shape[0] < 2 or grid.shape[1] < 1:
        raise ValueError(
            "depth_grid must be 2D with >=2 axial rows and >=1 circ column."
        )
    if np.any(grid < 0.0) or np.any(grid > t):
        raise ValueError(f"all grid depths must be within [0, t={t}].")

    if projection == PROJECTION_MAX:
        return np.max(grid, axis=1)

    if projection == PROJECTION_AREA_WEIGHTED:
        n_circ = grid.shape[1]
        if circ_weights is None:
            weights = np.ones(n_circ, dtype=float)
        else:
            weights = np.asarray(circ_weights, dtype=float)
            if weights.shape != (n_circ,):
                raise ValueError(
                    "circ_weights length must equal the number of circ columns."
                )
            if np.any(weights <= 0.0):
                raise ValueError("circ_weights (arc widths) must be > 0.")
        w = float(np.sum(weights))
        return (grid * weights).sum(axis=1) / w

    raise ValueError(f"projection must be one of {_PROJECTIONS}, got {projection!r}.")


def rstreng_2d_river_bottom(
    D: float,
    t: float,
    axial_positions_in: Sequence[float],
    depth_grid: Sequence[Sequence[float]],
    smys_psi: float,
    *,
    projection: str = PROJECTION_MAX,
    circ_weights: Optional[Sequence[float]] = None,
    maop_psi: Optional[float] = None,
    safety_factor: float = DEFAULT_SAFETY_FACTOR,
) -> RiverBottom2DResult:
    """Assess a 2D axial x circumferential metal-loss grid via RSTRENG.

    Projects the grid to a 1D effective axial profile using ``projection`` and
    then calls the validated 1D engine
    :func:`~digitalmodel.asset_integrity.corroded_pipe.rstreng_effective_area`
    **unchanged**.

    Args:
        D: pipe outside diameter (in). t: nominal wall thickness (in).
        axial_positions_in: axial positions ``x_a`` of the grid rows (in),
            strictly increasing; length must equal the number of grid rows.
        depth_grid: ``d[a][c]`` metal-loss depth grid (in); see
            :func:`project_grid`.
        smys_psi: specified minimum yield strength (psi).
        projection: ``"max"`` (default, conservative) or ``"area_weighted"``.
        circ_weights: per-column arc widths for the area-weighted rule.
        maop_psi: maximum allowable operating pressure for an accept/reject flag.
        safety_factor: applied to failure pressure for the safe pressure.

    Returns:
        :class:`RiverBottom2DResult` exposing the projection rule, governing
        axial segment, applicability flag and the full 1D RSTRENG result.
    """
    if projection not in _PROJECTIONS:
        raise ValueError(
            f"projection must be one of {_PROJECTIONS}, got {projection!r}."
        )
    x = np.asarray(axial_positions_in, dtype=float)
    grid = np.asarray(depth_grid, dtype=float)
    if grid.ndim != 2 or x.shape != (grid.shape[0],):
        raise ValueError(
            "axial_positions_in length must equal the number of grid rows."
        )

    d_eff = project_grid(grid, t, projection=projection, circ_weights=circ_weights)

    result = rstreng_effective_area(
        D,
        t,
        x,
        d_eff,
        smys_psi,
        maop_psi=maop_psi,
        safety_factor=safety_factor,
    )

    i, j = result.details["critical_segment"]
    governing_segment = (int(i), int(j))
    within = bool(result.details.get("within_applicability", True))

    affected_width = None
    if projection == PROJECTION_AREA_WEIGHTED:
        n_circ = grid.shape[1]
        weights = (
            np.ones(n_circ, dtype=float)
            if circ_weights is None
            else np.asarray(circ_weights, dtype=float)
        )
        affected_width = float(np.sum(weights))

    return RiverBottom2DResult(
        projection_rule=projection,
        axial_positions_in=[float(v) for v in x],
        d_eff_in=[float(v) for v in d_eff],
        governing_segment=governing_segment,
        result=result,
        within_applicability=within,
        details={
            "n_axial": int(grid.shape[0]),
            "n_circ": int(grid.shape[1]),
            "affected_width": affected_width,
            "critical_length_in": float(result.details["critical_length_in"]),
            "max_d_over_t": float(result.details["d_over_t"]),
        },
    )
