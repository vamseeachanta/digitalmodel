#!/usr/bin/env python3
"""
ABOUTME: The hull-scale-dependent quantities that are not geometry (#2023):
inlet turbulence, the snappyHexMesh cell budget, the hierarchical decomposition
vector, and the forceCoeffs reference values. Each is a bare literal in the
frozen KCS/DTC templates and each is wrong for any other hull.

Three of the four fail QUIETLY when wrong:

* ``maxGlobalCells`` stops refinement and reports success, handing back a mesh
  that is missing levels nobody chose;
* ``0.orig/{k,omega,nut}`` at another hull's condition just shifts the answer;
* ``Aref`` from the wrong hull is a clean multiplicative error in every
  reported coefficient.

The fourth, ``numberOfSubdomains``, is fatal -- but only AFTER the mesh is
built, which on a production grid is hours.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Mapping, Optional, Sequence, Tuple

from .hull_domain import Box, HullDomain
from .hull_turbulence import (
    C_MU,
    DEFAULT_LENGTH_SCALE_FACTOR,
    DEFAULT_TURBULENCE_INTENSITY,
    DEFAULT_VISCOSITY_RATIO,
    InletTurbulence,
    TurbulenceMethod,
    derive_inlet_turbulence,
    wall_normal_first_cell_height,
)
from .hull_manifest import HullManifest

__all__ = [
    "C_MU",
    "DEFAULT_LENGTH_SCALE_FACTOR",
    "DEFAULT_TURBULENCE_INTENSITY",
    "DEFAULT_VISCOSITY_RATIO",
    "KCS_CHAIN_DECOMPOSITION",
    "CellBudget",
    "CellBudgetError",
    "DecompositionError",
    "ForceReference",
    "InletTurbulence",
    "TurbulenceMethod",
    "decomposition_vector",
    "derive_cell_budget",
    "derive_force_reference",
    "derive_inlet_turbulence",
    "kcs_chain_config_path",
    "vof_vertical_blocks",
    "wall_normal_first_cell_height",
]

Vec3 = Tuple[float, float, float]

#: One stacked blockMesh block: ``(z_lo, z_hi, layers)``. The estimate has to
#: walk the stack rather than average it, because the blocks' cell heights
#: differ by two orders of magnitude.
Block = Tuple[float, float, int]



class CellBudgetError(ValueError):
    """A cell cap that would truncate refinement rather than error."""


class DecompositionError(ValueError):
    """A decomposition that decomposePar would reject, or silently mis-split."""


# --------------------------------------------------------------------------- #
#  Cell budget -- snappyHexMeshDict maxGlobalCells
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class CellBudget:
    """What the mesh will cost, and the cap the mesher is given."""

    background_cells: int
    estimated_cells: int
    max_global_cells: int
    max_local_cells: int
    finest_cell_size: float
    refinement_stages: int
    ranks: int
    safety_factor: float

    def to_provenance(self) -> Dict[str, object]:
        return {
            "background_cells": self.background_cells,
            "estimated_cells": self.estimated_cells,
            "max_global_cells": self.max_global_cells,
            "max_local_cells": self.max_local_cells,
            "finest_cell_size_m": self.finest_cell_size,
            "refinement_stages": self.refinement_stages,
            "safety_factor": self.safety_factor,
        }


#: Headroom over the estimate. The estimate models refinement by volume
#: fraction and cannot see buffer layers (nCellsBetweenLevels 3) or snappy's
#: own feature refinement, both of which add cells.
DEFAULT_CELL_SAFETY_FACTOR = 1.5

#: Per-rank headroom for maxLocalCells. Above this snappy switches to balancing
#: before refinement rather than after, which is a performance choice.
LOCAL_CELL_HEADROOM = 2.0


def vof_vertical_blocks(
    domain: HullDomain, divisions: Mapping[str, int]
) -> Tuple[Block, ...]:
    """The six stacked blocks of the free-surface template's blockMeshDict.

    Named rather than inlined because it is a property of ONE template tree,
    not of the estimate. The double-body tree stacks two blocks between the
    floor and the waterline and passes its own.
    """
    z, nz = domain.z_levels, divisions
    return (
        (z[0], z[1], nz["nza"]),
        (z[1], z[2], nz["nza"]),
        (z[2], z[3], nz["nzb"]),
        (z[3], z[4], nz["nzb"]),
        (z[4], z[5], nz["nzc"]),
        (z[5], z[6], nz["nzd"]),
    )


def derive_cell_budget(
    domain: HullDomain,
    boxes: Sequence[Box],
    divisions: Mapping[str, int],
    ranks: int,
    *,
    safety_factor: float = DEFAULT_CELL_SAFETY_FACTOR,
    cap: Optional[int] = None,
    blocks: Optional[Sequence[Block]] = None,
) -> CellBudget:
    """Size ``maxGlobalCells`` from the domain and the target cell size.

    The background count is EXACT -- it is the blockMesh product. The
    refinement growth is estimated: each topoSet/refineMesh stage splits the
    cells inside its box in x and y only (``directions (tan1 tan2)``), so each
    stage is a factor of four on the volume it covers, and the stages nest.

    ``cap`` is a hard ceiling from the caller. If it sits below the estimate
    this raises, because the alternative is what the frozen template does:
    snappyHexMesh stops refining at the cap, reports success, and hands back a
    mesh missing levels nobody chose. The solve then runs for days on it.
    """
    if ranks < 1:
        raise DecompositionError(f"ranks must be >= 1, got {ranks}")
    if safety_factor < 1.0:
        raise CellBudgetError(f"safety_factor must be >= 1, got {safety_factor}")

    stack = tuple(
        vof_vertical_blocks(domain, divisions) if blocks is None else blocks
    )
    background = _background_cells(divisions, stack)
    estimated = _estimate_refined_cells(domain, boxes, divisions, stack)

    if cap is not None:
        if cap < estimated:
            raise CellBudgetError(
                f"maxGlobalCells cap of {cap} would TRUNCATE refinement: this "
                f"domain needs an estimated {estimated} cells "
                f"({background} background plus {len(boxes)} refinement "
                f"stages). snappyHexMesh does not fail on this -- it stops "
                f"refining, reports success, and returns a mesh missing "
                f"levels. Raise the cap or coarsen the background."
            )
        max_global = int(cap)
    else:
        max_global = math.ceil(estimated * safety_factor)

    return CellBudget(
        background_cells=background,
        estimated_cells=estimated,
        max_global_cells=max_global,
        max_local_cells=max(1, math.ceil(max_global * LOCAL_CELL_HEADROOM / ranks)),
        finest_cell_size=domain.base_cell_size / 2 ** len(boxes),
        refinement_stages=len(boxes),
        ranks=ranks,
        safety_factor=safety_factor,
    )


def _background_cells(
    divisions: Mapping[str, int], blocks: Sequence[Block]
) -> int:
    """Exact blockMesh cell count: stacked blocks sharing nx and ny."""
    layers = sum(int(n) for _, _, n in blocks)
    return divisions["nx"] * divisions["ny"] * layers


def _estimate_refined_cells(
    domain: HullDomain,
    boxes: Sequence[Box],
    divisions: Mapping[str, int],
    blocks: Sequence[Block],
) -> int:
    """Cell count after the nested topoSet/refineMesh stages.

    Walks the blockMesh blocks because their vertical cell heights differ by
    two orders of magnitude, so a single domain-average cell volume would be
    meaningless. Within a block the cells are uniform in x and y and treated as
    uniform in z (the grading redistributes them, it does not change how many
    there are), which makes the volume fraction the cell fraction.
    """
    dx = domain.length / divisions["nx"]
    dy = domain.width / divisions["ny"]

    total = 0.0
    for z_lo, z_hi, n_layers in blocks:
        cell_volume = dx * dy * ((z_hi - z_lo) / n_layers)
        block_volume = domain.length * domain.width * (z_hi - z_lo)

        # Volume covered at each stage, forced non-increasing so a box that
        # escaped its parent cannot make a shell negative.
        covered = [block_volume]
        for box in boxes:
            covered.append(
                min(covered[-1], _overlap_volume(box, domain, z_lo, z_hi))
            )
        covered.append(0.0)

        for stage in range(len(boxes) + 1):
            shell = covered[stage] - covered[stage + 1]
            total += shell / cell_volume * 4**stage

    return int(math.ceil(total))


def _overlap_volume(box: Box, domain: HullDomain, z_lo: float, z_hi: float) -> float:
    (bx0, by0, bz0), (bx1, by1, bz1) = box
    ox = max(0.0, min(bx1, domain.x_inlet) - max(bx0, domain.x_outlet))
    oy = max(0.0, min(by1, domain.y_centreplane) - max(by0, domain.y_side))
    oz = max(0.0, min(bz1, z_hi) - max(bz0, z_lo))
    return ox * oy * oz


# --------------------------------------------------------------------------- #
#  Decomposition -- decomposeParDict numberOfSubdomains and coeffs n
# --------------------------------------------------------------------------- #

#: The table committed at config/cfd/kcs_chain.yml, mirrored here so the
#: package works when installed without the repo's config tree. The test suite
#: pins the two against each other.
KCS_CHAIN_DECOMPOSITION: Dict[int, Tuple[int, int, int]] = {
    8: (2, 2, 2),
    16: (4, 2, 2),
    32: (8, 2, 2),
}


def kcs_chain_config_path() -> Path:
    """Path of the committed chain config (repo checkout only)."""
    here = Path(__file__).resolve()
    for parent in here.parents:
        cand = parent / "config" / "cfd" / "kcs_chain.yml"
        if cand.is_file():
            return cand
    raise FileNotFoundError(
        "config/cfd/kcs_chain.yml not found - requires a repo checkout"
    )


def decomposition_vector(
    ranks: int, table: Optional[Mapping[int, Sequence[int]]] = None
) -> Tuple[int, int, int]:
    """The hierarchical ``n`` vector, guaranteed to multiply to ``ranks``.

    ``hierarchical`` requires ``prod(n) == numberOfSubdomains`` exactly, and
    decomposePar exits fatally when it does not -- AFTER the mesh is built,
    which on a production grid is hours of meshing thrown away.

    The rule is the one recorded in ``config/cfd/kcs_chain.yml``: two splits
    each in y and z, everything else on x, because x is the long axis of a
    towing domain and compact subdomains keep the halo small. That reproduces
    the committed table for 8, 16 and 32 ranks.
    """
    if ranks < 1:
        raise DecompositionError(f"ranks must be >= 1, got {ranks}")

    lookup = KCS_CHAIN_DECOMPOSITION if table is None else table
    if ranks in lookup:
        vec = tuple(int(v) for v in lookup[ranks])
        if len(vec) != 3:
            raise DecompositionError(f"decomposition vector must be 3 long: {vec}")
        _check_product(vec, ranks)
        return vec  # type: ignore[return-value]

    if ranks % 4 == 0 and ranks >= 8:
        vec = (ranks // 4, 2, 2)
    elif ranks % 2 == 0 and ranks >= 4:
        vec = (ranks // 2, 2, 1)
    else:
        vec = (ranks, 1, 1)

    _check_product(vec, ranks)
    return vec


def _check_product(vec: Tuple[int, ...], ranks: int) -> None:
    if any(v < 1 for v in vec):
        raise DecompositionError(f"decomposition vector {vec} has a non-positive term")
    product = vec[0] * vec[1] * vec[2]
    if product != ranks:
        raise DecompositionError(
            f"decomposition vector {vec} multiplies to {product}, not {ranks}. "
            f"decomposePar exits fatally on this, and it does so AFTER the "
            f"mesh is built."
        )


# --------------------------------------------------------------------------- #
#  forceCoeffs reference values
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class ForceReference:
    """Everything ``forceCoeffs`` normalises by, taken from THIS hull."""

    mag_u_inf: float
    l_ref: float
    a_ref: float
    rho_inf: float
    c_of_r: Vec3
    drag_direction: Vec3
    lift_direction: Vec3
    pitch_axis: Vec3
    half_domain: bool

    def to_provenance(self) -> Dict[str, object]:
        return {
            "mag_u_inf": self.mag_u_inf,
            "l_ref": self.l_ref,
            "a_ref": self.a_ref,
            "rho_inf": self.rho_inf,
            "c_of_r": list(self.c_of_r),
            "half_domain": self.half_domain,
            "a_ref_note": (
                "half the wetted surface: the solve is cut at the centreplane "
                "and reports half of every force, and forceCoeffs cannot "
                "double the numerator"
            ),
        }


def derive_force_reference(
    manifest: HullManifest,
    velocity: float,
    density: float,
    domain: HullDomain,
    *,
    half_domain: bool = True,
) -> ForceReference:
    """Reference values for the coefficient denominators.

    ``Aref`` is HALF the wetted surface on a half domain. The centreplane cut
    means the function object integrates half the hull and reports half of
    every force; forceCoeffs divides by ``0.5 rho U^2 Aref`` and has no way to
    double the numerator, so the halving has to happen here. Using the full
    wetted surface is a clean factor of two in every reported coefficient.

    ``CofR`` is midship at the free surface. The hull is translated so midship
    sits on x = 0, and the keel is the manifest's origin, so the free surface
    is at z = draft.
    """
    if velocity <= 0:
        raise ValueError(f"velocity must be positive, got {velocity}")
    if density <= 0:
        raise ValueError(f"density must be positive, got {density}")

    a_ref = manifest.wetted_surface_m2 / (2.0 if half_domain else 1.0)
    return ForceReference(
        mag_u_inf=velocity,
        l_ref=manifest.lpp_m,
        a_ref=a_ref,
        rho_inf=density,
        c_of_r=(0.0, 0.0, domain.waterline),
        drag_direction=domain.flow_direction,
        lift_direction=(0.0, 0.0, 1.0),
        pitch_axis=(0.0, 1.0, 0.0),
        half_domain=half_domain,
    )
