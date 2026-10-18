#!/usr/bin/env python3
"""
ABOUTME: The DOUBLE-BODY (no free surface) variant of the arbitrary-hull
resistance case (#2023). The free surface is replaced by a symmetry plane at
the waterline, the domain holds one phase, and gravity is absent. What comes
back is viscous resistance with no wave-making component, which is the only
thing a form factor can be extracted from:

    (1 + k) = C_v,double-body / C_f,ITTC-57

WHY A SIBLING TEMPLATE TREE RATHER THAN A TRANSFORM OF THE VOF ONE.
``templates/hull_resistance`` was itself created as an additive fork of
``templates/ship_resistance`` rather than a parameterisation, because the
frozen tree carried a gate worth keeping. The same argument applies one step
further out, and harder: the difference here is not four tokens, it is the
solver (simpleFoam, not interFoam), the pressure variable (``p``, not
``p_rgh``), the transported phase count (one, not two), the number of stacked
blockMesh blocks (two, not six), the boundary list (a ``waterline`` symmetry
plane where the ``atmosphere`` patch was), the time derivative (steadyState,
not localEuler) and the density source in the force integral (``rho rhoInf``,
not ``rho rho``). A transform that edited all of that would BE the second
tree, expressed as a diff nobody can read, and the files it produced would not
exist on disk for the template sweeps in ``test_forces_density_source`` to
inspect. The two trees are held together instead by a test that asserts the
dictionaries which SHOULD be identical are byte-identical.

WHAT IS REUSED, AND IT IS ALMOST EVERYTHING. The ITTC extents, the mirror into
the template's flow direction, the refinement staging, the cell-budget model,
the inlet turbulence, the decomposition vector and the forceCoeffs reference
values are the free-surface case's own, called and not copied.

The three things that genuinely differ live next door, so that "what is
different about a double body" is one file and not a diff:

  hull_double_body_domain  the truncated domain, the clipped staging and the
                           two-block vertical stack
  hull_double_body_dicts   the tokens, and the provenance statements that the
                           case is single-phase, that gravity is off and that
                           the wave criterion was deliberately not enforced

and the reduction that consumes the solved run is
``validation.double_body_form_factor``.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

from .hull_case import HullCaseConfig
from .hull_case_dicts import render_case_tree
from .hull_field_patches import add_appendage_patchfields
from .hull_case_regions import check_region_surfaces, copy_region_surfaces
from .hull_case_physics import (
    Block,
    CellBudget,
    ForceReference,
    InletTurbulence,
    decomposition_vector,
    derive_cell_budget,
    derive_force_reference,
    derive_inlet_turbulence,
    wall_normal_first_cell_height,
)
from .hull_domain import FREEBOARD_LPP, Box, HullDomain
from .hull_double_body_dicts import (
    double_body_case_provenance,
    double_body_tokens,
)
from .hull_double_body_domain import (
    build_double_body_domain,
    double_body_divisions,
    double_body_refinement_boxes,
    double_body_vertical_blocks,
)
from .hull_manifest import load_hull_manifest

__all__ = [
    "DOUBLE_BODY_END_TIME",
    "DOUBLE_BODY_WRITE_INTERVAL",
    "DoubleBodyCaseConfig",
    "DoubleBodyCaseError",
    "DoubleBodyDerivation",
    "build_double_body_case",
    "build_double_body_case_from_files",
    "derive_double_body_case",
    "double_body_templates_dir",
]

#: SIMPLE iterations and write cadence. Steady, so ``endTime`` counts
#: iterations and not seconds; a stated default, not a derivation. The forces
#: function object writes every iteration regardless, so the averaging window
#: a reduction sees is not limited by this number.
DOUBLE_BODY_END_TIME = 3000
DOUBLE_BODY_WRITE_INTERVAL = 500


class DoubleBodyCaseError(ValueError):
    """A double-body case whose settings contradict what a double body is."""


@dataclass(frozen=True)
class DoubleBodyCaseConfig(HullCaseConfig):
    """The free-surface case's configuration, minus the free surface.

    Inherited rather than restated so a knob cannot mean one thing on one case
    type and another on the other. The two settings that a double body cannot
    honour are REFUSED rather than silently ignored:

    * ``free_surface_cells_per_wavelength`` scores the mesh against a wave
      that does not exist here;
    * ``freeboard_lpp`` sizes the AIR column above the waterline, and this
      domain stops at the waterline.

    A knob that is accepted and then discarded is how a reviewer comes to
    believe a case was built to a specification it never saw.
    """

    name: str = "hull_double_body"
    end_time: int = DOUBLE_BODY_END_TIME
    write_interval: int = DOUBLE_BODY_WRITE_INTERVAL
    free_surface_cells_per_wavelength: Optional[float] = None

    def __post_init__(self) -> None:
        super().__post_init__()
        if self.free_surface_cells_per_wavelength is not None:
            raise DoubleBodyCaseError(
                "free_surface_cells_per_wavelength was set to "
                f"{self.free_surface_cells_per_wavelength!r} on a double-body "
                "case. The criterion scores cells per WAVELENGTH, and a "
                "double-body run replaces the free surface with a symmetry "
                "plane: there is no wave, so there is no wavelength to "
                "resolve. Leave it None; the provenance records that it was "
                "deliberately not enforced."
            )
        if self.freeboard_lpp != FREEBOARD_LPP:
            raise DoubleBodyCaseError(
                f"freeboard_lpp was set to {self.freeboard_lpp!r} on a "
                "double-body case. Freeboard sizes the air column above the "
                "waterline; this domain is truncated AT the waterline and has "
                "no air, so the setting would be accepted and discarded."
            )


@dataclass(frozen=True)
class DoubleBodyDerivation:
    """Every derived quantity, computed once so the tokens and the provenance
    cannot disagree."""

    config: DoubleBodyCaseConfig
    domain: HullDomain
    #: The untruncated free-surface domain the extents and the vertical ladder
    #: were taken from. Carried so a reader can see that this case is the same
    #: box with the air removed, and not a differently-sized one.
    reference_domain: HullDomain
    boxes: List[Box]
    divisions: Dict[str, int]
    blocks: Tuple[Block, ...]
    budget: CellBudget
    turbulence: InletTurbulence
    force_reference: ForceReference
    decomposition: Sequence[int]
    first_cell_height: float


# --------------------------------------------------------------------------- #
#  Derivation and emission
# --------------------------------------------------------------------------- #

def _derive_geometry(
    config: DoubleBodyCaseConfig,
) -> Tuple[HullDomain, HullDomain, List[Box], Dict[str, int], Tuple[Block, ...]]:
    """Domain, reference, clipped boxes, divisions and the vertical stack.

    The order matters. The extents fix the box, the box fixes the truncation,
    and only then can the staging be clipped to it -- the boxes are generated
    against the UNTRUNCATED domain because the innermost one is the hull's own
    bounding box and reaches above the waterline.
    """
    manifest = config.manifest
    domain, reference = build_double_body_domain(
        manifest,
        base_cell_size=config.base_cell_size,
        upstream_lpp=config.upstream_lpp,
        downstream_lpp=config.downstream_lpp,
        lateral_lpp=config.lateral_lpp,
        depth_lpp=config.depth_lpp,
        keel_clearance_drafts=config.keel_clearance_drafts,
    )
    boxes = double_body_refinement_boxes(manifest, reference, domain)
    divisions = double_body_divisions(reference, domain)
    return domain, reference, boxes, divisions, double_body_vertical_blocks(
        domain, divisions
    )


def derive_double_body_case(
    config: DoubleBodyCaseConfig,
) -> DoubleBodyDerivation:
    """Run every derivation. Raises before anything is written."""
    manifest = config.manifest
    domain, reference, boxes, divisions, blocks = _derive_geometry(config)

    return DoubleBodyDerivation(
        config=config,
        domain=domain,
        reference_domain=reference,
        boxes=boxes,
        divisions=divisions,
        blocks=blocks,
        budget=derive_cell_budget(
            domain,
            boxes,
            divisions,
            ranks=config.ranks,
            safety_factor=config.cell_safety_factor,
            cap=config.max_global_cells,
            blocks=blocks,
        ),
        turbulence=derive_inlet_turbulence(
            config.velocity,
            manifest.lpp_m,
            config.nu,
            intensity=config.turbulence_intensity,
            method=config.turbulence_method,
            viscosity_ratio=config.eddy_viscosity_ratio,
            length_scale_factor=config.length_scale_factor,
        ),
        force_reference=derive_force_reference(
            manifest,
            config.velocity,
            config.density,
            domain,
            half_domain=config.half_domain,
        ),
        decomposition=decomposition_vector(config.ranks),
        first_cell_height=wall_normal_first_cell_height(
            config.velocity, manifest.lpp_m, config.nu, y_plus=config.y_plus_target
        ),
    )


def double_body_templates_dir() -> Path:
    """The double-body template tree (installed package data)."""
    cand = Path(__file__).resolve().parent / "templates" / "hull_double_body"
    if not (cand / "system" / "controlDict").is_file():
        raise FileNotFoundError(f"hull_double_body templates not found at {cand}")
    return cand


def build_double_body_case(
    config: DoubleBodyCaseConfig, parent_dir: Path | str = "."
) -> Path:
    """Emit a complete, solvable double-body case directory."""
    derivation = derive_double_body_case(config)
    regions = config.surface_regions
    check_region_surfaces(regions)

    case = render_case_tree(
        double_body_templates_dir(),
        double_body_tokens(derivation),
        Path(parent_dir) / config.name,
    )
    (case / "0").mkdir(exist_ok=True)
    copy_region_surfaces(regions, case / "constant" / "triSurface")
    add_appendage_patchfields(case, regions)  # else decomposePar aborts post-mesh

    (case / "case_provenance.json").write_text(
        json.dumps(double_body_case_provenance(derivation), indent=2, sort_keys=True)
        + "\n"
    )
    return case


def build_double_body_case_from_files(
    manifest_path: Path | str,
    stl_path: Path | str,
    velocity: float,
    ranks: int,
    parent_dir: Path | str = ".",
    **options: Any,
) -> Path:
    """``(hull_manifest, STL, speed, target_ranks) -> case directory``."""
    config = DoubleBodyCaseConfig(
        manifest=load_hull_manifest(manifest_path),
        stl_path=Path(stl_path),
        velocity=velocity,
        ranks=ranks,
        **options,
    )
    return build_double_body_case(config, parent_dir)
