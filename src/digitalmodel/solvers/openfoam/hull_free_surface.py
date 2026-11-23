#!/usr/bin/env python3
"""
ABOUTME: The free-surface HORIZONTAL resolution requirement (#2023). The grid
ladder for the arbitrary-hull resistance case is sized from the hull, and the
vertical free-surface band is sized from the background cell -- but the wave the
case exists to compute has a length set by the SPEED, and nothing in the mesh
derivation looked at the speed.

lambda = 2 pi V^2 / g, so the requirement moves with the SQUARE of the speed
while the mesh does not move at all. A ladder that clears 80 cells per
wavelength at 14 kn delivers 52 at 11 kn on the same grid. That failure is
silent in every direction it can be looked at: an under-resolved wave train is
damped by numerical diffusion, the residuals converge normally, and the
wave-making component of resistance simply comes out low.

WHY THE IN-PLANE CELL IS THE BACKGROUND CELL OVER 2**stages. The six
topoSet/refineMesh stages run with ``directions (tan1 tan2)``, tan1 = (1 0 0)
and tan2 = (0 1 0): each stage halves x and y and leaves z untouched. So the
free-surface plane resolution is a property of nx and ny, and the vertical band
cell -- ``block_divisions``' ``FREE_SURFACE_CELL_FRACTION`` -- has no bearing on
it. This module is that argument's IN-PLANE analogue and has the same shape:
state the cell size the physics requires, then take the ceiling of the count
that delivers it.

WHY A STATED base_cell_size IS REFUSED RATHER THAN REFINED. A grid-convergence
triplet is read from the RATIO between its levels. Silently refining a level the
caller named would meet the criterion and destroy that ratio, so the levels
would no longer differ by the factor the study assumes -- and the study would
still produce a number. The builder refines only the grid it chose itself, and
refuses the one it was handed.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Dict, Mapping, Optional

from .hull_domain import REFINEMENT_STAGES

if TYPE_CHECKING:  # pragma: no cover - annotations only
    from .hull_domain import HullDomain

__all__ = [
    "DEFAULT_FREE_SURFACE_CELLS_PER_WAVELENGTH",
    "GRAVITY",
    "FreeSurfaceResolution",
    "FreeSurfaceResolutionError",
    "check_free_surface_resolution",
    "deep_water_wavelength",
    "free_surface_divisions",
    "free_surface_resolution",
    "free_surface_target_cell_size",
]

#: Standard gravity. The same value ``hull_case`` reports the Froude number
#: with, stated once so the wavelength and the Froude number cannot disagree.
GRAVITY = 9.80665

#: Cells per wavelength in the free-surface plane. The acceptance criterion in
#: the analysis plan; ITTC 7.5-03-02-03 asks for the wave system to be resolved
#: and practice puts that at 80-100 cells per fundamental wavelength.
DEFAULT_FREE_SURFACE_CELLS_PER_WAVELENGTH = 80.0


class FreeSurfaceResolutionError(ValueError):
    """A mesh that would not resolve the wave the case exists to compute."""


def deep_water_wavelength(velocity: float, *, gravity: float = GRAVITY) -> float:
    """The linear deep-water wavelength of the ship-generated wave system.

    ``lambda = 2 pi V^2 / g``. Deep water is the right relation here because
    the domain is sized with the keel clearance measured in drafts and the
    depth in Lpp; a case run in a stated finite depth would need the dispersion
    relation solved instead, and would need to say so.
    """
    if velocity <= 0:
        raise FreeSurfaceResolutionError(
            f"velocity must be positive to have a wavelength, got {velocity}"
        )
    if gravity <= 0:
        raise FreeSurfaceResolutionError(f"gravity must be positive, got {gravity}")
    return 2.0 * math.pi * velocity * velocity / gravity


@dataclass(frozen=True)
class FreeSurfaceResolution:
    """What the emitted mesh actually delivers in the free-surface plane."""

    velocity: float
    wavelength: float
    cell_size: float
    cells_per_wavelength: float
    required_cells_per_wavelength: Optional[float]
    refinement_stages: int

    @property
    def enforced(self) -> bool:
        return self.required_cells_per_wavelength is not None

    @property
    def meets_requirement(self) -> bool:
        if self.required_cells_per_wavelength is None:
            return True
        return self.cells_per_wavelength >= self.required_cells_per_wavelength

    def to_provenance(self) -> Dict[str, Any]:
        return {
            "velocity_m_s": self.velocity,
            "wavelength_m": self.wavelength,
            "in_plane_cell_size_m": self.cell_size,
            "cells_per_wavelength": self.cells_per_wavelength,
            "required_cells_per_wavelength": self.required_cells_per_wavelength,
            "enforced": self.enforced,
            "refinement_stages": self.refinement_stages,
            "note": (
                "linear deep-water wavelength 2 pi V^2 / g, resolved in the "
                "free-surface PLANE. The topoSet/refineMesh stages refine x "
                "and y only, so the in-plane cell is the coarser of the two "
                "background cells halved once per stage. This is the achieved "
                "in-plane size; mesh.finest_cell_size_m is the background cell "
                "under the same halving and ignores any in-plane refinement "
                "added to meet this criterion."
            ),
        }


def in_plane_cell_size(
    domain: "HullDomain",
    divisions: Mapping[str, int],
    *,
    refinement_stages: int = REFINEMENT_STAGES,
) -> float:
    """The COARSER of the two in-plane cells, after the refinement stages.

    Scoring dx alone passes a mesh that is coarse across y, and a Kelvin
    pattern is not one-dimensional: the divergent system carries the same
    wavelength into the transverse direction.
    """
    if refinement_stages < 0:
        raise FreeSurfaceResolutionError(
            f"refinement_stages must be >= 0, got {refinement_stages}"
        )
    dx = domain.length / divisions["nx"]
    dy = domain.width / divisions["ny"]
    return max(dx, dy) / 2**refinement_stages


def free_surface_target_cell_size(
    velocity: float,
    cells_per_wavelength: float = DEFAULT_FREE_SURFACE_CELLS_PER_WAVELENGTH,
    *,
    refinement_stages: int = REFINEMENT_STAGES,
    gravity: float = GRAVITY,
) -> float:
    """The BACKGROUND cell size that delivers ``cells_per_wavelength``.

    The stages do the work, so the background cell is allowed to be
    ``2**stages`` times the cell the criterion asks for in the plane.
    """
    if cells_per_wavelength <= 0:
        raise FreeSurfaceResolutionError(
            f"cells_per_wavelength must be positive, got {cells_per_wavelength}"
        )
    wavelength = deep_water_wavelength(velocity, gravity=gravity)
    return wavelength / cells_per_wavelength * 2**refinement_stages


def free_surface_resolution(
    domain: "HullDomain",
    divisions: Mapping[str, int],
    velocity: float,
    *,
    required_cells_per_wavelength: Optional[
        float
    ] = DEFAULT_FREE_SURFACE_CELLS_PER_WAVELENGTH,
    refinement_stages: int = REFINEMENT_STAGES,
    gravity: float = GRAVITY,
) -> FreeSurfaceResolution:
    """Score an emitted mesh against the criterion. Never raises on shortfall."""
    wavelength = deep_water_wavelength(velocity, gravity=gravity)
    cell = in_plane_cell_size(
        domain, divisions, refinement_stages=refinement_stages
    )
    return FreeSurfaceResolution(
        velocity=velocity,
        wavelength=wavelength,
        cell_size=cell,
        cells_per_wavelength=wavelength / cell,
        required_cells_per_wavelength=required_cells_per_wavelength,
        refinement_stages=refinement_stages,
    )


def free_surface_divisions(
    domain: "HullDomain",
    divisions: Mapping[str, int],
    velocity: float,
    *,
    cells_per_wavelength: float = DEFAULT_FREE_SURFACE_CELLS_PER_WAVELENGTH,
    refinement_stages: int = REFINEMENT_STAGES,
    gravity: float = GRAVITY,
) -> Dict[str, int]:
    """``divisions`` with nx and ny raised until the criterion is met.

    The in-plane analogue of the vertical band's ``nzb``: a target cell size,
    and the ceiling of the count that delivers it. Only nx and ny move --
    refining z would be a cost the stages cannot convert into wavelength
    resolution, because they do not refine z.

    The cost is the SQUARE of the linear factor, applied to every cell in the
    case, not just the free-surface plane. blockMesh has one nx and one ny for
    all six stacked blocks, so an in-plane requirement anywhere is an in-plane
    requirement everywhere; the alternative -- an extra refinement stage over
    the free-surface band only -- buys a factor of two when a factor of 1.1 is
    what the physics asked for.
    """
    target = free_surface_target_cell_size(
        velocity,
        cells_per_wavelength,
        refinement_stages=refinement_stages,
        gravity=gravity,
    )
    out = dict(divisions)
    out["nx"] = max(int(out["nx"]), math.ceil(domain.length / target))
    out["ny"] = max(int(out["ny"]), math.ceil(domain.width / target))
    return out


def check_free_surface_resolution(
    resolution: FreeSurfaceResolution, *, remedy: str = ""
) -> None:
    """Refuse an under-resolved mesh BEFORE anything is written.

    The same posture as ``CellBudgetError``: the alternative is a case that
    meshes, solves, converges and reports a resistance whose wave-making
    component has been damped away by the grid.
    """
    if resolution.meets_requirement:
        return
    raise FreeSurfaceResolutionError(
        f"the free-surface plane resolves "
        f"{resolution.cells_per_wavelength:.4g} cells per wavelength, and the "
        f"criterion is {resolution.required_cells_per_wavelength:.4g}. At "
        f"{resolution.velocity:.4g} m/s the linear deep-water wavelength is "
        f"{resolution.wavelength:.4g} m and the in-plane cell after "
        f"{resolution.refinement_stages} refinement stages is "
        f"{resolution.cell_size:.4g} m. An under-resolved wave train is damped "
        f"by numerical diffusion: the solve converges and reports a low "
        f"wave-making resistance rather than failing. {remedy}".strip()
    )
