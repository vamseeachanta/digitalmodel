# ABOUTME: Triangle-mesh hull adapter: geometry contract checks, waterline clip with trim, hydrostatics
# ABOUTME: Refuses (never repairs) bad meshes; outputs tagged computed/declared with an input hash (#2239 W0)
"""
Mesh hydrostatics adapter (issue #2239, work package W0).

Geometry contract (violations are refused with :class:`MeshContractError`, never repaired):

* Canonical frame: right-handed, x positive forward, y positive to port, z positive up.
  The caller declares how the mesh's own axes map onto it (``axes``), e.g.
  ``("forward", "port", "up")`` or ``("aft", "starboard", "up")``; a left-handed or
  non-permutation declaration is refused.
* Units are declared, ``"m"`` or ``"mm"``; an undeclared mesh is refused. All outputs are SI.
  Condition inputs (draft, trim, stations) are always metres in the canonical frame.
* The draft datum is the baseline (keel) at canonical z = 0: the lowest vertex must lie on it.
* Topology and orientation policy:

  - every edge is used by exactly two faces (watertight, no non-manifold edges);
  - each directed edge is used once (consistent orientation);
  - every vertex has a single fan of faces around it (no non-manifold vertices, e.g. two
    shells touching at one shared vertex);
  - the mesh may have several connected components (e.g. twin hulls), but **every component
    must enclose positive signed volume** (outward normals; inward shells such as cavities
    or reversed bodies are refused) and **component axis-aligned bounding boxes must be
    pairwise disjoint** (overlap > 1e-9 of the mesh diagonal is refused; this also refuses
    nested shells). Triangle-triangle intersection *within* a component is not tested.
  - no degenerate (zero-area / repeated-index) or non-finite faces.

  A mesh reversed as a whole is refused unless ``flip_normals=True`` is passed explicitly.
* The validated vertex and face arrays are private read-only copies; the digest is computed
  from the validated canonical geometry.
* Trim is T_fwd - T_aft (negative by the stern) over ``l_pp`` and is applied about the midship
  waterline point (x_midship, T_mean), so the mean draft is preserved. ``x_midship`` must lie
  within the waterline extent.
* The waterline cut is capped; **cap faces are tagged artificial**: they count toward volume
  and centroid integrals, never toward wetted area, and never toward any geometric extent
  (L_wl, B_wl, half-breadths come from physical faces only). The cap is a signed fan, exact
  for integrals over concave or multi-loop waterplanes. Signed distances within 1e-12 of the
  mesh diagonal are snapped and the vertex is projected onto the cutting plane; after the
  cut the body is checked closed and its two independent volume integrals must agree to
  1e-9 relative.
* LCB is positive forward of midship, measured along the waterplane from the midship
  waterline point and reported as a percentage of L_wl.
* Sections are transverse planes normal to hull-frame x. A station may lie anywhere in the
  closed hull x-range **including its end points**: the section is taken as the boundary of
  the part of the body on the side of the station that contains midship, so at an end
  station it is the (physical) end face itself, e.g. an immersed transom.
  A_BT and A_T are submerged section areas at caller-declared bulb and transom stations;
  without a declared station they are not computed and carry the reason.
* The half-breadth grid y(x_i, z_j) (max |y| of the physical submerged section, z from
  baseline; symmetric hull assumed) is given only on caller-declared stations and waterlines.
* Denominators are checked: V, L_wl and B_wl at or below 1e-12 diag^3 / 1e-8 diag are refused
  as degenerate; a dry midship section (A_M ~ 0, e.g. between tandem bodies) makes C_M and
  C_P ``not_computed`` with the reason.

Two independent volume integrations are exposed for verification: :func:`volume_divergence`
(divergence theorem with F = (0, 0, z)) and :func:`volume_tetra` (signed tetrahedra to an
origin). Analytic fixtures (box, Wigley, Wigley wetted-area quadrature) live in
:mod:`digitalmodel.naval_architecture.hull_fixtures` and are re-exported here.
"""


from __future__ import annotations

import hashlib
import json
import math
from copy import deepcopy
from dataclasses import dataclass, field
from typing import Iterator, Mapping, Optional, Sequence

import numpy as np

from .hull_fixtures import (box_mesh, wigley_half_breadth, wigley_mesh, wigley_wetted_area_reference)
from .mesh_geometry import (MeshContractError, SCHEMA_VERSION, UNIT_SCALE, CANONICAL_AXES,
    _SNAP_REL, _VOLUME_DEGENERATE_REL, _LENGTH_DEGENERATE_REL, _DEGENERATE_AREA_REL,
    _face_area_vectors, _tetra_moments, volume_tetra, volume_divergence)
from .mesh_validation import TriMesh
from .mesh_clipping import ClippedHull, clip_at_waterline, _condition_geometry
from .mesh_sections import SectionIndex, _half_breadth
from .mesh_results import Quantity, HydrostaticsResult, _CONVENTIONS, _check_station

__all__ = [
    "MeshContractError", "TriMesh", "ClippedHull", "Quantity", "HydrostaticsResult",
    "SCHEMA_VERSION", "UNIT_SCALE", "CANONICAL_AXES", "box_mesh", "wigley_mesh",
    "wigley_half_breadth", "wigley_wetted_area_reference", "volume_tetra",
    "volume_divergence", "clip_at_waterline", "compute_hydrostatics",
    # Retain the baseline wildcard namespace, including incidental imports.
    "Iterator", "Mapping", "Optional", "Sequence", "annotations", "dataclass",
    "deepcopy", "field", "hashlib", "json", "math", "np",
]


def compute_hydrostatics(
    mesh: TriMesh,
    draft: float,
    trim: float = 0.0,
    *,
    x_midship: Optional[float] = None,
    l_pp: Optional[float] = None,
    bulb_station: Optional[float] = None,
    transom_station: Optional[float] = None,
    grid_stations: Optional[Sequence[float]] = None,
    grid_waterlines: Optional[Sequence[float]] = None,
) -> HydrostaticsResult:
    """Hydrostatics of the mesh at mean draft ``draft`` (m) and trim (m) in the canonical frame.

    ``x_midship`` defaults to the mid-point of the mesh x-extent and ``l_pp`` to that extent
    (both then tagged ``computed``). Stations are hull-frame x in metres, end points included.
    """
    draft, trim, xm, xm_decl, lpp, lpp_decl, normal, ex, point = _condition_geometry(
        mesh, draft, trim, x_midship, l_pp)
    lo, hi = mesh.bounds
    diag, eps = mesh.scale_diag, _SNAP_REL * mesh.scale_diag
    bulb = None if bulb_station is None else _check_station("bulb", bulb_station, lo, hi, eps)
    transom = None if transom_station is None else _check_station("transom", transom_station, lo, hi, eps)
    grid_decl, gx, gz = _grid_inputs(grid_stations, grid_waterlines, lo, hi, eps)
    h = _input_hash(mesh, draft, trim, x_midship, l_pp, bulb, transom, grid_decl, gx, gz)
    clipped = clip_at_waterline(mesh, draft, trim, x_midship=x_midship, l_pp=l_pp)
    metrics = _hydro_integrals(clipped, xm, diag, eps, normal, ex, point)
    index = SectionIndex(clipped, xm, eps)
    a_m, _ = index.section(xm)
    q = _base_quantities(draft, trim, xm, xm_decl, lpp, lpp_decl, *metrics, a_m, diag, h)
    _feature_quantities(q, index, bulb, transom, grid_decl, gx, gz, 1e-9 * diag, h)
    return HydrostaticsResult(q, h, dict(_CONVENTIONS))


def _grid_inputs(grid_stations, grid_waterlines, lo, hi, eps):
    grid_decl = grid_stations is not None and grid_waterlines is not None
    if grid_decl:
        gx = np.asarray(grid_stations, dtype=float).ravel()
        gz = np.asarray(grid_waterlines, dtype=float).ravel()
        if gx.size == 0 or gz.size == 0 or not (np.all(np.isfinite(gx)) and np.all(np.isfinite(gz))):
            raise MeshContractError("grid stations and waterlines must be finite, non-empty")
        gx = np.array([_check_station("half-breadth grid", x, lo, hi, eps) for x in gx])
        if np.any(gz < 0):
            raise MeshContractError("grid waterlines are heights above baseline and must be >= 0")

    return grid_decl, (gx if grid_decl else None), (gz if grid_decl else None)


def _input_hash(mesh, draft, trim, x_midship, l_pp, bulb, transom, grid_decl, gx, gz):
    payload = json.dumps(
        {
            "mesh": mesh.source_digest, "draft": repr(draft), "trim": repr(trim),
            "x_midship": None if x_midship is None else repr(float(x_midship)),
            "l_pp": None if l_pp is None else repr(float(l_pp)),
            "bulb": None if bulb is None else repr(bulb),
            "transom": None if transom is None else repr(transom),
            "grid_x": None if not grid_decl else [repr(float(v)) for v in gx],
            "grid_z": None if not grid_decl else [repr(float(v)) for v in gz],
        },
        sort_keys=True,
    )
    return hashlib.sha256(payload.encode()).hexdigest()



def _hydro_integrals(clipped, xm, diag, eps, normal, ex, point):
    wl = clipped.waterline_points
    if not (wl[:, 0].min() - eps <= xm <= wl[:, 0].max() + eps):
        raise MeshContractError(
            f"x_midship = {xm!r} m lies outside the waterline extent "
            f"[{wl[:, 0].min()!r}, {wl[:, 0].max()!r}] m"
        )

    vol, centroid = _tetra_moments(clipped.vertices, clipped.faces, None)
    if vol <= _VOLUME_DEGENERATE_REL * diag**3:
        raise MeshContractError(f"degenerate submerged volume V = {vol!r} m^3")
    areas = clipped.face_areas()
    s_wet = float(areas[~clipped.artificial].sum())
    cap_av = _face_area_vectors(clipped.vertices, clipped.faces[clipped.artificial]).sum(axis=0)
    a_wp = float(cap_av @ normal)

    xw = (wl - point) @ ex
    l_wl = float(xw.max() - xw.min())
    b_wl = float(wl[:, 1].max() - wl[:, 1].min())
    if l_wl <= _LENGTH_DEGENERATE_REL * diag or b_wl <= _LENGTH_DEGENERATE_REL * diag:
        raise MeshContractError(
            f"degenerate waterplane: L_wl = {l_wl!r} m, B_wl = {b_wl!r} m"
        )
    lcb_m = float((centroid - point) @ ex)

    return vol, s_wet, a_wp, l_wl, b_wl, lcb_m


def _base_quantities(draft, trim, xm, xm_decl, lpp, lpp_decl,
                     vol, s_wet, a_wp, l_wl, b_wl, lcb_m, a_m, diag, h):
    c_b = vol / (l_wl * b_wl * draft)
    c_wp = a_wp / (l_wl * b_wl)

    def comp(v, u):
        return Quantity(float(v), u, "computed", h)

    def decl(v, u):
        return Quantity(float(v), u, "declared", h)

    q: dict = {
        "draft_mean": decl(draft, "m"),
        "trim": decl(trim, "m"),
        "draft_fwd": decl(draft + 0.5 * trim, "m"),
        "draft_aft": decl(draft - 0.5 * trim, "m"),
        "x_midship": (decl if xm_decl else comp)(xm, "m"),
        "l_pp": (decl if lpp_decl else comp)(lpp, "m"),
        "V": comp(vol, "m^3"),
        "S": comp(s_wet, "m^2"),
        "L_wl": comp(l_wl, "m"),
        "B_wl": comp(b_wl, "m"),
        "A_WP": comp(a_wp, "m^2"),
        "A_M": comp(a_m, "m^2"),
        "C_B": comp(c_b, "-"),
        "C_WP": comp(c_wp, "-"),
        "LCB": comp(100.0 * lcb_m / l_wl, "% L_wl"),
    }
    if a_m <= _DEGENERATE_AREA_REL * diag**2:
        reason = (f"midship section at x = {xm!r} m is dry (A_M = {a_m!r} m^2); "
                  "C_M and C_P are undefined")
        q["A_M"] = comp(0.0, "m^2")
        q["C_M"] = Quantity(None, "-", "not_computed", h, reason)
        q["C_P"] = Quantity(None, "-", "not_computed", h, reason)
    else:
        q["C_M"] = comp(a_m / (b_wl * draft), "-")
        q["C_P"] = comp(vol / (a_m * l_wl), "-")
    return q


def _feature_quantities(q, index, bulb, transom, grid_decl, gx, gz, tol, h):
    for key, station in (("A_BT", bulb), ("A_T", transom)):
        if station is None:
            q[key] = Quantity(None, "m^2", "not_computed", h,
                              f"no {('bulb' if key == 'A_BT' else 'transom')} station declared; "
                              "the adapter does not infer feature stations")
        else:
            q[key] = Quantity(float(index.section(station)[0]), "m^2", "computed", h)
    if grid_decl:
        grid = np.zeros((gx.size, gz.size))
        for i, x in enumerate(gx):
            _, segs = index.section(x)
            for j, z in enumerate(gz):
                grid[i, j] = _half_breadth(segs, float(z), tol)
        q["half_breadth"] = Quantity(grid.tolist(), "m", "computed", h,
                                     f"stations x={gx.tolist()} m; heights z={gz.tolist()} m")
    else:
        q["half_breadth"] = Quantity(None, "m", "not_computed", h,
                                     "no grid stations/waterlines declared")
