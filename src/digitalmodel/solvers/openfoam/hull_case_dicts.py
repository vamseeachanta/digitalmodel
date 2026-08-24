#!/usr/bin/env python3
"""
ABOUTME: Template tokens and case provenance for the arbitrary-hull resistance
case (#2023). Both are built from one :class:`HullCaseDerivation`, so the
dictionaries the solver reads and the record of how they were sized cannot
disagree with each other.
"""

from __future__ import annotations

import re
from pathlib import Path
from typing import TYPE_CHECKING, Any, Dict, Mapping, Sequence

from .region_refinement import finest_in_plane_cell
from .hull_case_regions import region_provenance, region_tokens

if TYPE_CHECKING:  # pragma: no cover - annotations only
    from .hull_case import HullCaseConfig, HullCaseDerivation
    from .hull_domain import HullDomain

__all__ = [
    "case_provenance",
    "hull_case_tokens",
    "render_case_tree",
    "surfaces_provenance",
]


def render_case_tree(
    templates: Path, tokens: Mapping[str, str], case: Path
) -> Path:
    """Copy a template tree into ``case``, substituting every ``@TOKEN@``.

    ONE implementation for both case types. A leftover token is an error and
    not a warning: OpenFOAM parses ``@NX@`` as a word, so an unsubstituted
    count reaches blockMesh as a syntax error at best and, in a dictionary
    entry that accepts a word, as a silently different case at worst.
    """
    for src in sorted(templates.rglob("*")):
        if not src.is_file():
            continue
        rel = src.relative_to(templates)
        text = src.read_text()
        for key, value in tokens.items():
            text = text.replace(f"@{key}@", value)
        leftover = re.findall(r"@[A-Z0-9_]+@", text)
        if leftover:
            raise ValueError(
                f"unsubstituted token(s) {sorted(set(leftover))} in {rel}"
            )
        dst = case / rel
        dst.parent.mkdir(parents=True, exist_ok=True)
        dst.write_text(text)
    return case


def _fmt(value: float) -> str:
    return f"{value:.10g}"


def _vec(values: Sequence[float]) -> str:
    return " ".join(_fmt(v) for v in values)


def hull_case_tokens(derivation: "HullCaseDerivation") -> Dict[str, str]:
    """Every ``@TOKEN@`` value. Nothing here is a constant."""
    tokens: Dict[str, str] = {}
    tokens.update(_condition_tokens(derivation))
    tokens.update(_geometry_tokens(derivation))
    tokens.update(_mesh_tokens(derivation))
    force = _force_tokens(derivation)
    tokens.update(force)
    # LAST, and it must stay last. The region blocks are the only token values
    # that quote another token's VALUE (the centre of rotation, in the
    # per-patch forces objects). They take it already formatted, so nothing
    # here depends on substitution order -- but a region block written to
    # contain "@COFR@" would, and would only fail once someone reordered this.
    tokens.update(
        region_tokens(
            derivation.config.surface_regions, c_of_r=force["COFR"]
        )
    )
    return tokens


def _condition_tokens(derivation: "HullCaseDerivation") -> Dict[str, str]:
    cfg = derivation.config
    turb = derivation.turbulence
    return {
        "STL": cfg.stl_name,
        "EMESH": cfg.emesh_name,
        "UMEAN": _fmt(cfg.velocity),
        "NU": f"{cfg.nu:.10e}",
        "RHO": _fmt(cfg.density),
        "ENDTIME": str(cfg.end_time),
        "WRITEINTERVAL": str(cfg.write_interval),
        "KINLET": f"{turb.k:.10e}",
        "OMEGAINLET": f"{turb.omega:.10e}",
        "NUTINLET": f"{turb.nut:.10e}",
    }


def _geometry_tokens(derivation: "HullCaseDerivation") -> Dict[str, str]:
    """Domain extents, the setFields box, and the refinement boxes.

    The setFields box is the DOMAIN, padded outward so no boundary cell is
    missed by a rounding difference, and cut at the free surface on top. The
    tutorial's (-999 ... 999) literal clips any domain wider than 999 m, and
    the clipped region silently stays air.
    """
    domain = derivation.domain
    pad = 0.01 * max(domain.length, domain.width, domain.height)
    tokens = {
        "WATERLINE": _fmt(domain.waterline),
        "X0": _fmt(domain.x_outlet),
        "X1": _fmt(domain.x_inlet),
        "Y0": _fmt(domain.y_side),
        "LOCATIONINMESH": _vec(domain.location_in_mesh),
        "SETFIELDSLO": _vec(
            (domain.x_outlet - pad, domain.y_side - pad, domain.z_levels[0] - pad)
        ),
        "SETFIELDSHI": _vec((domain.x_inlet + pad, pad, domain.waterline)),
    }
    for i, z in enumerate(domain.z_levels):
        tokens[f"Z{i}"] = _fmt(z)
    for i, (lo, hi) in enumerate(derivation.boxes, start=1):
        tokens[f"B{i}LO"] = _vec(lo)
        tokens[f"B{i}HI"] = _vec(hi)
    return tokens


def _mesh_tokens(derivation: "HullCaseDerivation") -> Dict[str, str]:
    div = derivation.divisions
    budget = derivation.budget
    return {
        "NX": str(div["nx"]),
        "NY": str(div["ny"]),
        "NZA": str(div["nza"]),
        "NZB": str(div["nzb"]),
        "NZC": str(div["nzc"]),
        "NZD": str(div["nzd"]),
        "MAXGLOBALCELLS": str(budget.max_global_cells),
        "MAXLOCALCELLS": str(budget.max_local_cells),
        "NRANKS": str(derivation.config.ranks),
        "DECOMPN": " ".join(str(int(v)) for v in derivation.decomposition),
    }


def _force_tokens(derivation: "HullCaseDerivation") -> Dict[str, str]:
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


def case_provenance(derivation: "HullCaseDerivation") -> Dict[str, Any]:
    """Everything a reader needs to know WHAT was solved and HOW it was sized."""
    cfg = derivation.config
    return {
        "case_name": cfg.name,
        "hull": cfg.manifest.to_provenance(),
        "surface": {"stl": cfg.stl_name, "emesh": cfg.emesh_name},
        "surfaces": surfaces_provenance(cfg),
        "condition": _condition_provenance(cfg),
        "domain": _domain_provenance(derivation.domain),
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
        },
    }


def surfaces_provenance(cfg: "HullCaseConfig") -> Dict[str, Any]:
    """What was meshed, and WHICH wetted area ``Aref`` was built from.

    Shared with the double-body tree: the two cases are compared against each
    other to extract a form factor, so a reader has to be able to see that
    both normalised by the same area. Two copies of this could disagree.
    """
    return {
        **region_provenance(cfg.surface_regions),
        "aref_source": (
            "regions.union.wetted_surface_external_m2"
            if cfg.manifest.regions
            else "wetted_surface_m2"
        ),
        "aref_wetted_surface_m2": cfg.manifest.reference_wetted_surface_m2,
        "wetted_surface_upper_bound_m2": (
            cfg.manifest.wetted_surface_upper_bound_m2
        ),
    }


def _condition_provenance(cfg: "HullCaseConfig") -> Dict[str, Any]:
    return {
        "velocity_m_s": cfg.velocity,
        "froude": cfg.froude,
        "reynolds": cfg.reynolds_number,
        "kinematic_viscosity_m2_s": cfg.nu,
        "density_kg_m3": cfg.density,
        "viscosity_source": (
            "matched to the requested Reynolds number"
            if cfg.reynolds is not None
            else "stated fluid property"
        ),
    }


def _domain_provenance(domain: "HullDomain") -> Dict[str, Any]:
    return {
        "x_outlet": domain.x_outlet,
        "x_inlet": domain.x_inlet,
        "y_side": domain.y_side,
        "z_levels": list(domain.z_levels),
        "waterline_m": domain.waterline,
        "base_cell_size_m": domain.base_cell_size,
        "volume_m3": domain.volume,
        "half_domain": domain.half_domain,
        "location_in_mesh": list(domain.location_in_mesh),
    }


def _mesh_provenance(derivation: "HullCaseDerivation") -> Dict[str, Any]:
    """The cell budget, the free-surface resolution it was sized to meet, and
    the near-wall requirement it does NOT enforce."""
    return {
        **derivation.budget.to_provenance(),
        "free_surface": derivation.free_surface.to_provenance(),
        "block_divisions": dict(derivation.divisions),
        "refinement_boxes": [
            {"lo": list(lo), "hi": list(hi)} for lo, hi in derivation.boxes
        ],
        "finest_in_plane_cell_m": finest_in_plane_cell(
            derivation.domain, derivation.divisions, len(derivation.boxes)
        ),
        "finest_in_plane_cell_note": (
            "the in-plane cell inside the innermost refinement box, on the "
            "mesh as emitted: blockMesh takes an integer count, so this is "
            "the delivered cell and not the requested base_cell_size / 2**stages. "
            "The post-mesh face-area gate (#2033) scores the written hull "
            "patch against it."
        ),
        "wall_normal_first_cell_height_m": derivation.first_cell_height,
        "y_plus_target": derivation.config.y_plus_target,
        "wall_resolution_note": (
            "snappyHexMesh is scale-invariant here (relativeSizes true, hull "
            "level (0 0), refinement from length-scaled topoSet boxes) but the "
            "requirement it must satisfy scales with Reynolds number, not with "
            "geometry. This height is what the near-wall mesh has to deliver "
            "at this condition; it is recorded, not enforced."
        ),
    }
