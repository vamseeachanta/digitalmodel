#!/usr/bin/env python3
"""
ABOUTME: Template tokens and case provenance for the DOUBLE-BODY hull case
(#2023). Both are built from one :class:`DoubleBodyDerivation`, so the
dictionaries the solver reads and the record of how they were sized cannot
disagree with each other.

The provenance carries three statements the free-surface case has no need of,
and each is written down because its ABSENCE is what a defect looks like:
the case is single-phase, gravity is off, and the cells-per-wavelength
criterion was deliberately not enforced rather than skipped by accident.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Dict

# The number formatting must not diverge between the two template trees: a
# coordinate written to different precision in the two cases is a difference
# in the emitted mesh that no test comparing the two would attribute to
# formatting.
from .hull_case_dicts import _fmt, _vec, surfaces_provenance
from .hull_case_regions import region_tokens

if TYPE_CHECKING:  # pragma: no cover - annotations only
    from .hull_domain import HullDomain
    from .hull_double_body import DoubleBodyCaseConfig, DoubleBodyDerivation

__all__ = ["double_body_case_provenance", "double_body_tokens"]

#: What replaced the free surface, stated once and used by both the tokens'
#: consumers and the provenance.
WATERLINE_PATCH = "waterline"
WATERLINE_PATCH_TYPE = "symmetryPlane"

#: The reduction this case exists to feed.
FORM_FACTOR_DEFINITION = "(1 + k) = C_v,double-body / C_f,ITTC-57"


def double_body_tokens(derivation: "DoubleBodyDerivation") -> Dict[str, str]:
    """Every ``@TOKEN@`` value. Nothing here is a constant."""
    tokens: Dict[str, str] = {}
    tokens.update(_condition_tokens(derivation))
    tokens.update(_geometry_tokens(derivation))
    tokens.update(_mesh_tokens(derivation))
    force = _force_tokens(derivation)
    tokens.update(force)
    # LAST, for the reason spelled out in ``hull_case_dicts``: the region
    # blocks are the only token values built from another token's VALUE.
    tokens.update(
        region_tokens(
            derivation.config.surface_regions, c_of_r=force["COFR"]
        )
    )
    return tokens


def _condition_tokens(derivation: "DoubleBodyDerivation") -> Dict[str, str]:
    cfg = derivation.config
    turb = derivation.turbulence
    return {
        "STL": cfg.stl_name,
        "EMESH": cfg.emesh_name,
        "UMEAN": _fmt(cfg.velocity),
        "NU": f"{cfg.nu:.10e}",
        "ENDTIME": str(cfg.end_time),
        "WRITEINTERVAL": str(cfg.write_interval),
        "KINLET": f"{turb.k:.10e}",
        "OMEGAINLET": f"{turb.omega:.10e}",
        "NUTINLET": f"{turb.nut:.10e}",
    }


def _geometry_tokens(derivation: "DoubleBodyDerivation") -> Dict[str, str]:
    """Domain extents and the refinement boxes.

    There is no setFields box: with one phase there is no phase fraction to
    initialise, and the dictionary that would carry it is absent from the tree
    rather than emitted empty.
    """
    domain = derivation.domain
    tokens = {
        "WATERLINE": _fmt(domain.waterline),
        "X0": _fmt(domain.x_outlet),
        "X1": _fmt(domain.x_inlet),
        "Y0": _fmt(domain.y_side),
        "LOCATIONINMESH": _vec(domain.location_in_mesh),
    }
    for i, z in enumerate(domain.z_levels):
        tokens[f"Z{i}"] = _fmt(z)
    for i, (lo, hi) in enumerate(derivation.boxes, start=1):
        tokens[f"B{i}LO"] = _vec(lo)
        tokens[f"B{i}HI"] = _vec(hi)
    return tokens


def _mesh_tokens(derivation: "DoubleBodyDerivation") -> Dict[str, str]:
    div = derivation.divisions
    budget = derivation.budget
    return {
        "NX": str(div["nx"]),
        "NY": str(div["ny"]),
        "NZDEEP": str(div["nzdeep"]),
        "NZNEAR": str(div["nznear"]),
        "MAXGLOBALCELLS": str(budget.max_global_cells),
        "MAXLOCALCELLS": str(budget.max_local_cells),
        "NRANKS": str(derivation.config.ranks),
        "DECOMPN": " ".join(str(int(v)) for v in derivation.decomposition),
    }


def _force_tokens(derivation: "DoubleBodyDerivation") -> Dict[str, str]:
    """The forceCoeffs reference values.

    ``RHOINF`` does double duty here and it is not a shortcut. In the VOF case
    it is the coefficient denominator ONLY, because the force integral names
    the density field. simpleFoam has no density field, so the same constant
    is also what converts the kinematic integral into Newtons.
    """
    ref = derivation.force_reference
    return {
        "RHOINF": _fmt(ref.rho_inf),
        "MAGUINF": _fmt(ref.mag_u_inf),
        "LREF": _fmt(ref.l_ref),
        "AREF": _fmt(ref.a_ref),
        "COFR": _vec(ref.c_of_r),
        "DRAGDIR": _vec(ref.drag_direction),
        "LIFTDIR": _vec(ref.lift_direction),
        "PITCHAXIS": _vec(ref.pitch_axis),
    }


# --------------------------------------------------------------------------- #
#  Provenance
# --------------------------------------------------------------------------- #

def double_body_case_provenance(
    derivation: "DoubleBodyDerivation",
) -> Dict[str, Any]:
    """Everything a reader needs to know WHAT was solved and HOW it was sized."""
    cfg = derivation.config
    return {
        "case_name": cfg.name,
        "case_type": "double_body",
        "issue": "#2023",
        "hull": cfg.manifest.to_provenance(),
        "surface": {"stl": cfg.stl_name, "emesh": cfg.emesh_name},
        "surfaces": surfaces_provenance(cfg),
        "condition": _condition_provenance(cfg),
        "free_surface": _free_surface_provenance(),
        "domain": _domain_provenance(derivation),
        "mesh": _mesh_provenance(derivation),
        "decomposition": {
            "ranks": cfg.ranks,
            "method": "hierarchical",
            "n": list(derivation.decomposition),
        },
        "turbulence": derivation.turbulence.to_provenance(),
        "force_reference": derivation.force_reference.to_provenance(),
        "iterations": {
            "end_time": cfg.end_time,
            "write_interval": cfg.write_interval,
            "note": (
                "simpleFoam is steady: end_time counts SIMPLE iterations, not "
                "seconds. A time in this file is an iteration index."
            ),
        },
        "form_factor": {
            "definition": FORM_FACTOR_DEFINITION,
            "reduced_by": (
                "digitalmodel.solvers.openfoam.validation."
                "double_body_form_factor"
            ),
            "note": (
                "C_v,double-body is the TOTAL coefficient of this run, "
                "pressure plus friction. The pressure part is the viscous "
                "form drag, and it is precisely what k measures; reducing "
                "only the friction integral returns (1 + k) near unity by "
                "construction and would hide the quantity being sought."
            ),
        },
    }


def _condition_provenance(cfg: "DoubleBodyCaseConfig") -> Dict[str, Any]:
    return {
        "velocity_m_s": cfg.velocity,
        "froude": cfg.froude,
        "froude_note": (
            "reported to identify the condition only. This run has no free "
            "surface, so it carries no wave-making resistance and no Froude "
            "scaling; the Reynolds number is the condition that matters here."
        ),
        "reynolds": cfg.reynolds_number,
        "kinematic_viscosity_m2_s": cfg.nu,
        "density_kg_m3": cfg.density,
        "density_note": (
            "not written to constant/transportProperties: simpleFoam is "
            "incompressible and solves the kinematic equations. This value "
            "reaches the case once, as rhoInf in system/controlDict."
        ),
        "viscosity_source": (
            "matched to the requested Reynolds number"
            if cfg.reynolds is not None
            else "stated fluid property"
        ),
    }


def _free_surface_provenance() -> Dict[str, Any]:
    """The three statements whose absence is what a defect looks like."""
    return {
        "present": False,
        "phases": 1,
        "gravity_enabled": False,
        "replaced_by": {
            "patch": WATERLINE_PATCH,
            "type": WATERLINE_PATCH_TYPE,
            "at_z": "waterline (z = draft)",
        },
        "gravity_note": (
            "constant/g is ABSENT from this case, and simpleFoam never reads "
            "it. With one phase of uniform density the buoyant term is a "
            "constant gradient that the kinematic pressure absorbs, so there "
            "is nothing for gravity to do and no p_rgh split to make."
        ),
        "cells_per_wavelength": {
            "criterion": "cells_per_wavelength",
            "enforced": False,
            "reason": (
                "DELIBERATELY NOT ENFORCED, not skipped. The criterion scores "
                "the in-plane cell against the linear deep-water wavelength "
                "2 pi V^2 / g of the ship-generated wave system. A symmetry "
                "plane at the waterline is the zero-elevation limit of a free "
                "surface: no wave is generated, so there is no wavelength to "
                "resolve and no wave-making resistance to damp. Enforcing it "
                "would raise nx and ny by the square of a linear factor for a "
                "quantity this case does not compute."
            ),
        },
    }


def _domain_provenance(derivation: "DoubleBodyDerivation") -> Dict[str, Any]:
    domain, reference = derivation.domain, derivation.reference_domain
    return {
        "x_outlet": domain.x_outlet,
        "x_inlet": domain.x_inlet,
        "y_side": domain.y_side,
        "z_levels": list(domain.z_levels),
        "z_top": domain.z_levels[-1],
        "waterline_m": domain.waterline,
        "truncated_at_waterline": domain.z_levels[-1] == domain.waterline,
        "base_cell_size_m": domain.base_cell_size,
        "volume_m3": domain.volume,
        "half_domain": domain.half_domain,
        "location_in_mesh": list(domain.location_in_mesh),
        "free_surface_reference": _reference_provenance(domain, reference),
    }


def _reference_provenance(
    domain: "HullDomain", reference: "HullDomain"
) -> Dict[str, Any]:
    """The untruncated box this one was cut from, so the cut is auditable."""
    return {
        "z_levels": list(reference.z_levels),
        "volume_m3": reference.volume,
        "volume_ratio": domain.volume / reference.volume,
        "note": (
            "the free-surface domain at the same level, from the same ITTC "
            "extents. x, y, the floor, the background cell and the point in "
            "the free stream are identical; only the column above the "
            "waterline and the free-surface band are removed."
        ),
    }


def _mesh_provenance(derivation: "DoubleBodyDerivation") -> Dict[str, Any]:
    return {
        **derivation.budget.to_provenance(),
        "block_divisions": dict(derivation.divisions),
        "vertical_blocks": [
            {"z_lo": lo, "z_hi": hi, "layers": n}
            for lo, hi, n in derivation.blocks
        ],
        "vertical_ladder_note": (
            "the near-field vertical cell is the free-surface ladder's "
            "near-field cell at this level, taken from that ladder rather "
            "than re-derived. A form factor is a ratio applied to a "
            "free-surface result; a boundary layer resolved differently by "
            "the two meshes lands in k and is indistinguishable from hull "
            "form."
        ),
        "refinement_boxes": [
            {"lo": list(lo), "hi": list(hi)} for lo, hi in derivation.boxes
        ],
        "refinement_box_note": (
            "the free-surface staging, clipped at the waterline: the part of "
            "each box above it enclosed air that this domain does not have."
        ),
        "wall_normal_first_cell_height_m": derivation.first_cell_height,
        "y_plus_target": derivation.config.y_plus_target,
        "wall_resolution_note": (
            "this height is what the near-wall mesh has to deliver at this "
            "condition; it is recorded, not enforced. It carries MORE weight "
            "here than on the free-surface case: with no wave-making "
            "component, every part of the reported resistance is viscous, so "
            "a wall-function regime error lands entirely in the form factor."
        ),
    }
