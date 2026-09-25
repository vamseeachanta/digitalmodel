"""Weldolet attachment-weld root-flaw crack model for MAPDL CINT (#2157 P0b).

Generates a complete MAPDL ``.inp`` for a 6 x 1/2 STD weldolet on an NPS 6
Sch 40S run pipe under internal pressure, with a full-circumference weld-root
flaw on the run-pipe fusion face, solved with the contour-integral (CINT)
extraction of K_I, K_II, K_III and J along the closed crack front. The same
generator writes the uncracked global model (σ_ref linearisation paths, the
far-field hoop plausibility check) and, through ``DeckHooks`` used by
``weldolet_limit``, the elastic-perfectly-plastic limit-load model. Every
input is an assumed design basis recorded in
``examples/workflows/crack-fe-weldolet/design-data-register.json``.

Geometry (units mm, N, MPa)
    Run-pipe axis = global X, branch axis = global Z, crown at z = R_o. A point
    of the fitting region is located by (rho, theta, v): rho = distance from the
    branch axis, theta = angle about it (theta = 0 on +X, the crotch), and
    v = height above the run-pipe OD. The map is x = rho cos(theta),
    y = rho sin(theta), z = sqrt((R_o + v)^2 - y^2) for v <= v_B, so v = 0 is
    the run-pipe OD cylinder exactly and constant rho is a vertical cylinder
    (the hole wall, the weldolet bore). Above the groove top v_B the surfaces of
    constant v blend smoothly into flat planes, reaching z = R_o + A at the
    weldolet outlet; the branch stub is flat (z = R_o + v).

    Section in (rho, v), the same for every theta:
    * run-pipe wall v in [-t, 0] for rho >= C/2 (the hole is cut to the
      weldolet opening C);
    * weld + weldolet + branch above v = 0 as one monolithic solid, bounded
      inside by the wetted root face (rho = C/2, v <= root gap), the internal
      taper to the bore and the bore; outside by the 3 x 3 mm cover fillet
      (toe at rho = B/2 + 3 on the run pipe), the base cylinder B/2, the outer
      cone to the branch OD at v = A and the branch OD;
    * the crack lies on v = 0 from the root (rho = C/2) to the front at
      rho_f = C/2 + a, both faces free (no symmetry): the front is a closed
      curve on the run-pipe OD.

Meshing approach (pure APDL, no geometry kernel), reusing the P0a crack block
    A structured 20-node hexahedral mesh (SOLID186) built in Python and written
    as N/E commands. In the (rho, v) section a square "tube" of half-size d
    around the front carries a full 360 deg spider-web of rings, the innermost
    ring collapsed onto the tip with quarter-point mid-side nodes (the P0a
    construction extended from 180 to 360 deg, with separate upper and lower
    crack-face nodes). The rest of the section is block structured (tensor
    blocks in the pipe wall, a column-fraction block following the weldolet
    contour). The section is swept through 360 deg about the branch axis;
    beyond rho = rho_P the run pipe is an O-block to a square in the unrolled
    (x, s = R_o psi) surface plus a tensor mesh of the rest of the pipe.

    Only +, -, *, /, sqrt and series evaluations of sin/cos/atan with a fixed
    number of terms are used, so the deck text is byte-identical on every
    platform. ``mesh_level`` halves the crack-front element size per level
    (first ring and spacing along the front).

Loads and supports
    Internal pressure on every wetted surface (run-pipe bore, hole wall, root
    face, weldolet taper and bore, branch bore), optionally on both crack faces;
    closed-end thrust as an equivalent tensile end pressure on the free run-pipe
    end (x = +L) and on the branch end; UX = 0 on the fixed end (x = -L) with
    three point supports removing the remaining rigid-body modes.

Outputs written by the deck (relative names, in the solver working directory)
    ``weldolet_cint_L<n>.txt`` (cracked): the CINT table in the P0a format;
    ``weldolet_reac_L<n>.txt``: solved reaction sums and the MAPDL revision;
    ``weldolet_path_L<n>.txt`` and ``weldolet_hoop_L<n>.txt`` (uncracked):
    stresses along the fusion-face linearisation paths and through the wall at
    the hoop check location; ``weldolet_sifs_start_L<n>.txt`` (cracked): both
    SIFS definitions at the first front nodes (audit of the start-node
    correction). The limit-load deck (``weldolet_limit``) adds its
    load-deflection record.
"""

from __future__ import annotations

import hashlib
import json
import math
from collections import defaultdict
from dataclasses import dataclass
from itertools import pairwise
from pathlib import Path

from digitalmodel.ansys.crack_verification import (
    CrackMesh,
    _doubled,
    _fmt,
    _geometric,
)

N_CONTOURS = 6
# Declared crack-depth states (register F-04), fixed before any solve.
DEPTH_STATES_MM = (2.35, 2.8, 3.2, 3.6, 4.0, 4.5, 5.0, 5.5, 6.0)
A0_MM = 2.35
UNCRACKED_LAYOUT_DEPTH_MM = A0_MM  # block layout of the uncracked model

_BASE_FRONT_DIVISIONS = 64
_BASE_FIRST_RING_MM = 0.01 * A0_MM
_RING_RATIO = 1.35
_TUBE_SIDE_DIVISIONS = 3
_GROWTH = 1.3
_PATCH_RADIUS_MM = 36.0
_SQUARE_HALF_MM = 56.0
_HOOP_CHECK_X_MM = 100.0
_BAND_DIVISIONS = 28
_O_BLOCK_DIVISIONS = 4
PATH_THETAS_DEG = (0.0, 45.0, 90.0)
PATH_POINTS = 17
PATH_DIVISIONS = 8

GENERATOR_FILES = (
    "src/digitalmodel/ansys/weldolet_crack.py",
    "src/digitalmodel/ansys/crack_verification.py",
    "src/digitalmodel/ansys/cint_parser.py",
)


# --------------------------------------------------------------------------- #
# Deterministic elementary functions (IEEE basic operations only)
# --------------------------------------------------------------------------- #
_PI = 3.141592653589793
_PIO2_HI = 1.5707963267341256
_PIO2_LO = 6.077100506506192e-11


def _sincos(x: float) -> tuple[float, float]:
    """(sin x, cos x) by range reduction and fixed-length Taylor series."""
    k = round(x / (0.5 * _PI))
    r = (x - k * _PIO2_HI) - k * _PIO2_LO
    r2 = r * r
    s, term = r, r
    for n in range(1, 12):
        term = -term * r2 / ((2 * n) * (2 * n + 1))
        s += term
    c, term = 1.0, 1.0
    for n in range(1, 12):
        term = -term * r2 / ((2 * n - 1) * (2 * n))
        c += term
    q = k % 4
    if q == 0:
        return s, c
    if q == 1:
        return c, -s
    if q == 2:
        return -s, -c
    return -c, s


def _atan(t: float) -> float:
    """atan(t) for |t| <= 1: two argument halvings and a fixed series."""
    for _ in range(2):
        t = t / (1.0 + math.sqrt(1.0 + t * t))
    t2 = t * t
    acc, term = t, t
    for n in range(1, 20):
        term = -term * t2
        acc += term / (2 * n + 1)
    return 4.0 * acc


# --------------------------------------------------------------------------- #
# Specification
# --------------------------------------------------------------------------- #
@dataclass(frozen=True)
class WeldoletSpec:
    """Assumed design basis (register ids in the comments)."""

    run_od_mm: float = 168.3  # G-01
    run_wall_mm: float = 7.11  # G-02
    branch_od_mm: float = 21.34  # G-03
    branch_wall_mm: float = 2.77  # G-04
    weldolet_a_mm: float = 19.05  # G-06
    weldolet_b_mm: float = 34.93  # G-07
    weldolet_c_mm: float = 23.81  # G-08
    root_gap_mm: float = 1.6  # G-10
    bevel_deg: float = 45.0  # G-11
    fillet_leg_mm: float = 3.0  # G-12
    run_half_length_mm: float = 200.0  # N-01
    branch_length_mm: float = 40.0  # N-02
    pressure_mpa: float = 4.7  # L-01
    youngs_modulus_mpa: float = 182_500.0  # M-01
    poisson: float = 0.3  # M-02
    yield_mpa: float = 127.0  # M-03
    crack_depth_mm: float | None = A0_MM  # F-01; None = uncracked model
    crack_face_pressure: bool = True  # L-03
    tube_radius_mm: float = 0.8  # N-03
    mesh_level: int = 0

    def validate(self) -> list[str]:
        issues: list[str] = []
        geo = derived_geometry(self)
        if self.mesh_level not in (0, 1, 2):
            issues.append("mesh_level must be 0, 1 or 2")
        a = self.crack_depth_mm
        d = self.tube_radius_mm
        if a is not None:
            rho_f = geo["hole_radius_mm"] + a
            rout_d = geo["toe_radius_mm"] - (geo["toe_radius_mm"] - geo["base_radius_mm"]) * (
                d / self.fillet_leg_mm
            )
            if a - d < 0.5 or rho_f + d > rout_d - 0.5:
                issues.append(
                    "crack depth outside the crack-block tube limits "
                    "(need a - d >= 0.5 and rho_f + d <= fillet at v = d minus 0.5)"
                )
        if not 0.0 < d < self.root_gap_mm:
            issues.append("tube radius too large for the root gap")
        if self.run_half_length_mm - geo["toe_radius_mm"] < geo["shell_decay_length_mm"]:
            issues.append("run-pipe half-length shorter than toe + 2.5 sqrt(R_m t)")
        if self.run_half_length_mm <= _HOOP_CHECK_X_MM + 20.0:
            issues.append("run-pipe half-length must exceed the hoop check location")
        if self.pressure_mpa <= 0 or self.youngs_modulus_mpa <= 0:
            issues.append("pressure and modulus must be positive")
        return issues


def derived_geometry(spec: WeldoletSpec) -> dict:
    """Derived section dimensions (register G-05, G-09, G-13, G-14, F-03)."""
    ro = spec.run_od_mm / 2.0
    ri = ro - spec.run_wall_mm
    rm = ro - 0.5 * spec.run_wall_mm
    rw = spec.weldolet_c_mm / 2.0
    rb = spec.weldolet_b_mm / 2.0
    tan_bevel = math.tan(math.radians(spec.bevel_deg))
    groove_top = spec.root_gap_mm + (rb - rw) * tan_bevel
    toe = rb + spec.fillet_leg_mm
    return {
        "run_outer_radius_mm": ro,
        "run_inner_radius_mm": ri,
        "run_mean_radius_mm": rm,
        "hole_radius_mm": rw,
        "base_radius_mm": rb,
        "toe_radius_mm": toe,
        "footprint_mm": toe - rw,
        "groove_top_mm": groove_top,
        "inner_taper_end_mm": spec.weldolet_a_mm / 2.0,
        "branch_inner_radius_mm": spec.branch_od_mm / 2.0 - spec.branch_wall_mm,
        "branch_outer_radius_mm": spec.branch_od_mm / 2.0,
        "shell_decay_length_mm": 2.5 * math.sqrt(rm * spec.run_wall_mm),
    }


def remaining_ligament_mm(spec: WeldoletSpec) -> float:
    """Remaining ligament on the fusion face: footprint - a (register F-03)."""
    if spec.crack_depth_mm is None:
        raise ValueError("uncracked model has no ligament")
    return derived_geometry(spec)["footprint_mm"] - spec.crack_depth_mm


def closed_forms(spec: WeldoletSpec) -> dict:
    """Closed-form comparators for the uncracked model (pre-stated)."""
    geo = derived_geometry(spec)
    ro, ri, rm = (geo[k] for k in ("run_outer_radius_mm", "run_inner_radius_mm",
                                   "run_mean_radius_mm"))
    p, t = spec.pressure_mpa, spec.run_wall_mm
    lame_a = p * ri * ri / (ro * ro - ri * ri)
    rbo, rbi = geo["branch_outer_radius_mm"], geo["branch_inner_radius_mm"]
    return {
        "barlow_id_hoop_mpa": p * (2.0 * ri) / (2.0 * t),
        "barlow_mean_hoop_mpa": p * rm / t,
        "barlow_od_hoop_mpa": p * (2.0 * ro) / (2.0 * t),
        "lame_hoop_mean_radius_mpa": lame_a * (1.0 + ro * ro / (rm * rm)),
        "axial_closed_end_mpa": lame_a,
        "end_thrust_n": p * math.pi * ri * ri,
        "run_end_pressure_mpa": lame_a,
        "branch_end_pressure_mpa": p * rbi * rbi / (rbo * rbo - rbi * rbi),
        "hoop_comparator": "barlow_id_hoop_mpa",
        "hoop_tolerance": 0.02,
        "hoop_check_location": {
            "x_mm": _HOOP_CHECK_X_MM,
            "psi_deg": 180.0,
            "radius": "mean radius R_m",
        },
    }


def lame_hoop(spec: WeldoletSpec, r: float) -> float:
    geo = derived_geometry(spec)
    ro, ri = geo["run_outer_radius_mm"], geo["run_inner_radius_mm"]
    return spec.pressure_mpa * ri * ri / (ro * ro - ri * ri) * (1.0 + ro * ro / (r * r))


def linearise(s: list[float], sigma: list[float]) -> tuple[float, float]:
    """Membrane and bending (at s = 0) of sigma(s) over [s0, s_end].

    sigma_m = (1/L) int sigma ds; sigma_b = (6/L^2) int sigma (L/2 - x) ds,
    x = s - s0; trapezoidal rule on the sampled points (exact for linear data).
    """
    s0, length = s[0], s[-1] - s[0]
    m_int = 0.0
    b_int = 0.0
    for (sa, fa), (sb, fb) in pairwise(zip(s, sigma, strict=True)):
        h = sb - sa
        xa, xb = sa - s0, sb - s0
        m_int += 0.5 * h * (fa + fb)
        # exact integral of the linear interpolant times (L/2 - x)
        b_int += h * (fa * (0.5 * length - (2.0 * xa + xb) / 3.0)
                      + fb * (0.5 * length - (xa + 2.0 * xb) / 3.0)) / 2.0
    return m_int / length, 6.0 * b_int / (length * length)


def mesh_parameters(spec: WeldoletSpec, level: int | None = None) -> dict:
    lv = spec.mesh_level if level is None else level
    scale = 2**lv
    return {
        "level": lv,
        "front_divisions": _BASE_FRONT_DIVISIONS * scale,
        "first_ring_mm": _BASE_FIRST_RING_MM / scale,
        "ring_ratio_target": _RING_RATIO,
        "tube_side_divisions": _TUBE_SIDE_DIVISIONS,
        "tube_radius_mm": spec.tube_radius_mm,
        "patch_radius_mm": _PATCH_RADIUS_MM,
        "square_half_mm": _SQUARE_HALF_MM,
        "band_divisions": _BAND_DIVISIONS,
        "o_block_divisions": _O_BLOCK_DIVISIONS,
    }


# --------------------------------------------------------------------------- #
# Section mesh in (rho, v)
# --------------------------------------------------------------------------- #
class _Section:
    def __init__(self, rho_f: float, cracked: bool) -> None:
        self.rho_f = rho_f
        self.cracked = cracked
        self.pts: list[tuple[float, float]] = []
        self.tags: list[str] = []
        self.index: dict[tuple[str, str, str], int] = {}
        self.quads: list[tuple[int, ...]] = []
        self.sets: dict[str, set[int]] = defaultdict(set)
        self.tip = -1

    def node(self, rho: float, v: float, side: str) -> int:
        fr, fv = _fmt(rho), _fmt(v)
        tag = ""
        if self.cracked and fv == "0.00000000" and rho < self.rho_f - 1e-9:
            tag = side
        key = (fr, fv, tag)
        nid = self.index.get(key)
        if nid is None:
            self.pts.append((rho + 0.0, v + 0.0))
            self.tags.append(tag)
            nid = len(self.pts) - 1
            self.index[key] = nid
        return nid

    def block(self, grid: list[list[tuple[float, float]]], side) -> list[list[int]]:
        """Structured block on a doubled grid; ``side`` is 'L'/'U' or a
        function (i2, j2) -> 'L'/'U'. Returns the node-id grid."""
        ni, nj = len(grid), len(grid[0])
        ids = [
            [
                self.node(*grid[i][j], side if isinstance(side, str) else side(i, j))
                for j in range(nj)
            ]
            for i in range(ni)
        ]
        for ei in range((ni - 1) // 2):
            for ej in range((nj - 1) // 2):
                i0, j0 = 2 * ei, 2 * ej
                c = [ids[i0][j0], ids[i0 + 2][j0], ids[i0 + 2][j0 + 2], ids[i0][j0 + 2]]
                m = [ids[i0 + 1][j0], ids[i0 + 2][j0 + 1], ids[i0 + 1][j0 + 2],
                     ids[i0][j0 + 1]]
                if self._area(c) < 0.0:
                    c = [c[0], c[3], c[2], c[1]]
                    m = [m[3], m[2], m[1], m[0]]
                # collapsed (crack-tip) quad: rotate so that the repeated corner
                # is c2 = c3, i.e. SOLID186's degenerate prism form K = L, O = P
                for _ in range(4):
                    if len(set(c)) == 4 or c[2] == c[3]:
                        break
                    c = c[1:] + c[:1]
                    m = m[1:] + m[:1]
                self.quads.append(tuple(c + m))
        return ids

    def _area(self, c: list[int]) -> float:
        p = [self.pts[n] for n in c]
        acc = 0.0
        for k in range(4):
            (x0, y0), (x1, y1) = p[k], p[(k + 1) % 4]
            acc += x0 * y1 - x1 * y0
        return 0.5 * acc


def _levels(v0: float, breaks: list[float], size) -> list[float]:
    """Corner levels from v0 through each break (nodes at every break)."""
    out = [v0]
    for lo, hi in pairwise([v0, *breaks]):
        n = max(2, math.ceil((hi - lo) / size(0.5 * (lo + hi))))
        for k in range(1, n):
            out.append(lo + (hi - lo) * k / n)
        out.append(hi)
    return out


def _build_section(spec: WeldoletSpec, geo: dict, par: dict) -> tuple[_Section, dict]:
    cracked = spec.crack_depth_mm is not None
    a = spec.crack_depth_mm if cracked else UNCRACKED_LAYOUT_DEPTH_MM
    t = spec.run_wall_mm
    rw, rb, toe = geo["hole_radius_mm"], geo["base_radius_mm"], geo["toe_radius_mm"]
    rbi, rbo = geo["branch_inner_radius_mm"], geo["branch_outer_radius_mm"]
    g, leg, vb = spec.root_gap_mm, spec.fillet_leg_mm, geo["groove_top_mm"]
    vc, va = geo["inner_taper_end_mm"], spec.weldolet_a_mm
    top = va + spec.branch_length_mm
    d = spec.tube_radius_mm
    ns = par["tube_side_divisions"]
    ht = d / ns
    rho_f = rw + a
    sec = _Section(rho_f, cracked)

    tube_side = [d * k / (2 * ns) for k in range(2 * ns + 1)]
    tube_side[-1] = d
    tube_across = [-d + 2.0 * d * k / (4 * ns) for k in range(4 * ns + 1)]
    tube_across[0], tube_across[-1] = -d, d
    b1 = rho_f + tube_across[0]
    b2 = rho_f + tube_across[-1]

    def rho_in(v: float) -> float:
        if v <= g:
            return rw
        if v >= vc:
            return rbi
        return rw + (rbi - rw) * (v - g) / (vc - g)

    def rho_out(v: float) -> float:
        if v <= leg:
            return toe - (toe - rb) * (v / leg)
        if v <= vb:
            return rb
        if v >= va:
            return rbo
        return rb + (rbo - rb) * (v - vb) / (va - vb)

    # --- 1-D doubled lists -------------------------------------------------
    gc1 = _geometric(ht, b1 - rw, _GROWTH)
    c1 = _doubled([rw] + [b1 - p for p in reversed(gc1[1:-1])] + [b1])
    c2 = [rho_f + s for s in tube_across]
    c3 = _doubled([b2 + p for p in _geometric(ht, toe - b2, _GROWTH)[:-1]] + [toe])
    c4 = _doubled(
        [toe + p for p in _geometric(0.5, _PATCH_RADIUS_MM - toe, _GROWTH)[:-1]]
        + [_PATCH_RADIUS_MM]
    )
    r1 = _doubled([-t, -0.75 * t, -0.5 * t, -0.5 * (0.5 * t + d), -tube_side[-1]])
    r2a = [-tube_side[2 * ns - k] for k in range(2 * ns + 1)]
    r2b = list(tube_side)

    def grid(rl: list[float], vl: list[float]):
        return [[(r, v) for v in vl] for r in rl]

    # --- run-pipe wall ------------------------------------------------------
    for rl in (c1, c2, c3, c4):
        sec.block(grid(rl, r1), "L")
    for rl in (c1, c3, c4):
        sec.block(grid(rl, r2a), "L")

    # --- crack-front tube (360 deg spider-web) ------------------------------
    perim = [(-d, -s) for s in tube_side]  # (-d,0) -> (-d,-d)
    perim += [(s, -d) for s in tube_across[1:]]  # bottom
    perim += [(d, s) for s in [-x for x in reversed(tube_side)][1:] + tube_side[1:]]
    perim += [(s, d) for s in list(reversed(tube_across))[1:]]  # top, right->left
    perim += [(-d, s) for s in list(reversed(tube_side))[1:]]  # (-d,d) -> (-d,0)
    rings = _geometric(par["first_ring_mm"], d, _RING_RATIO)
    n_ring = len(rings) - 1
    ring2 = _doubled(rings)
    ring2[1] = (0.25 if cracked else 0.5) * rings[1]
    n_perim = len(perim) - 1

    def tube_pt(m2: int, k2: int) -> tuple[float, float]:
        if k2 == 0:
            return (rho_f, 0.0)
        rs, rz = perim[m2]
        if k2 == 2 * n_ring:
            return (rho_f + rs, 0.0 + rz)
        length = math.sqrt(rs * rs + rz * rz)
        rho = ring2[k2]
        beta = (rho / d) * (rho / d)
        scale = rho * ((1.0 - beta) + beta * length / d) / length
        return (rho_f + rs * scale, 0.0 + rz * scale)

    tube_grid = [[tube_pt(m2, k2) for k2 in range(2 * n_ring + 1)]
                 for m2 in range(n_perim + 1)]
    sec.block(tube_grid, lambda i, j: "L" if i < n_perim // 2 else "U")
    sec.tip = sec.index[(_fmt(rho_f), _fmt(0.0), "")]

    # --- above v = 0: C1 x R2b, C3 x R2b (fillet), upper column block -------
    sec.block(grid(c1, r2b), "U")
    rout_d = rho_out(d)
    c3top = [b2 + (x - b2) * (rout_d - b2) / (toe - b2) for x in c3]
    c3top[0], c3top[-1] = b2, rout_d
    c3grid = []
    for x, xt in zip(c3, c3top, strict=True):
        frac = (x - b2) / (toe - b2)
        col = []
        for k, v in enumerate(r2b):
            if k == 0:
                col.append((x, 0.0))
            elif k == len(r2b) - 1:
                col.append((xt, v))
            else:
                col.append((b2 + frac * (rho_out(v) - b2), v))
        c3grid.append(col)
    sec.block(c3grid, "U")

    row_d = c1 + c2[1:] + c3top[1:]
    span_d = rout_d - rw
    fracs = [(r - rw) / span_d for r in row_d]

    def size(v: float) -> float:
        return 0.3 + 0.12 * v

    breaks = sorted({g, leg, vb, vc, va})
    v_up_c = _levels(d, breaks, size)
    h_last = v_up_c[-1] - v_up_c[-2]
    v_up_c += [va + p for p in _geometric(h_last, top - va, _GROWTH)[1:-1]] + [top]
    v_up = _doubled(v_up_c)
    v_up[0] = tube_side[-1]
    upper = []
    for j, fj in enumerate(fracs):
        col = []
        for k, v in enumerate(v_up):
            if k == 0:
                col.append((row_d[j], v))
            else:
                ri_, ro_ = rho_in(v), rho_out(v)
                if j == 0:
                    col.append((ri_, v))
                elif j == len(fracs) - 1:
                    col.append((ro_, v))
                else:
                    col.append((ri_ + fj * (ro_ - ri_), v))
        upper.append(col)
    up_ids = sec.block(upper, "U")

    # --- named section node sets ------------------------------------------
    f_rw = _fmt(rw)
    for nid, (r, v) in enumerate(sec.pts):
        fv = _fmt(v)
        if fv == _fmt(-t):
            sec.sets["pipe_id"].add(nid)
        if _fmt(r) == f_rw and v <= g + 1e-9:
            sec.sets["hole"].add(nid)
        if fv == "0.00000000" and r >= toe - 1e-9:
            sec.sets["pipe_od"].add(nid)
        if _fmt(r) == _fmt(_PATCH_RADIUS_MM):
            sec.sets["patch_edge"].add(nid)
    for k, v in enumerate(v_up):
        if v >= g - 1e-9:
            sec.sets["weldolet_bore"].add(up_ids[0][k])
        sec.sets["weldolet_outer"].add(up_ids[-1][k])
    for k in range(len(r2b)):
        nid = sec.index.get((_fmt(c3grid[-1][k][0]), _fmt(c3grid[-1][k][1]), ""))
        if nid is not None:
            sec.sets["weldolet_outer"].add(nid)
    for j in range(len(fracs)):
        sec.sets["branch_end"].add(up_ids[j][-1])
    if cracked:
        for nid, tag in enumerate(sec.tags):
            if tag == "L":
                sec.sets["crack_lower"].add(nid)
            elif tag == "U":
                sec.sets["crack_upper"].add(nid)
        sec.sets["crack_lower"].add(sec.tip)
        sec.sets["crack_upper"].add(sec.tip)
    info = {
        "rho_f": rho_f,
        "pipe_v": r1[:-1] + r2a,
        "n_ring": n_ring,
        "top": top,
        "vb": vb,
        "va": va,
        "c3_right": [c3grid[-1][k] for k in range(len(r2b))],
    }
    return sec, info


# --------------------------------------------------------------------------- #
# 3-D mesh
# --------------------------------------------------------------------------- #
class WeldoletMesh(CrackMesh):
    """Nodes (1-based ids), 20-node elements and named boundary node sets."""

    def __init__(self) -> None:
        super().__init__()
        self.boundary_nodes: dict[str, set[int]] = {}
        self.front_vectors: list[tuple[float, float, float]] = []
        self.path_nodes: dict[float, tuple[int, int]] = {}
        self.fixed_end: list[int] = []
        self.support_nodes: dict[str, int] = {}
        self.hoop_line: list[int] = []
        self.wet_runs: list[tuple[int, int]] = []
        self.crack_runs: list[tuple[int, int]] = []

    def add_node(self, x: float, y: float, z: float, tag: str = "") -> int:
        key = (_fmt(x), _fmt(y), _fmt(z), tag)
        nid = self._index.get(key)
        if nid is None:
            self.nodes.append((x + 0.0, y + 0.0, z + 0.0))
            self.keys.append(key)
            nid = len(self.nodes)
            self._index[key] = nid
        return nid


def _zmap(v: float, y: float, ro: float, vb: float, va: float) -> float:
    r = ro + v
    if v <= vb:
        return math.sqrt(r * r - y * y)
    if v >= va:
        return r
    u = (v - vb) / (va - vb)
    beta = u * u * (3.0 - 2.0 * u)
    return (1.0 - beta) * math.sqrt(r * r - y * y) + beta * r


def build_mesh(spec: WeldoletSpec) -> WeldoletMesh:
    """Build the structured 3-D mesh for ``spec``."""
    issues = spec.validate()
    if issues:
        raise ValueError("invalid weldolet spec: " + "; ".join(issues))
    geo = derived_geometry(spec)
    par = mesh_parameters(spec)
    sec, info = _build_section(spec, geo, par)
    ro = geo["run_outer_radius_mm"]
    vb, va = info["vb"], info["va"]
    n_t = par["front_divisions"]
    n_l = 2 * n_t  # doubled layers around the branch axis
    trig = [_sincos(_PI * lay / n_t) for lay in range(n_l)]

    raw = WeldoletMesh()
    patch_id: dict[tuple[int, int], int] = {}

    def pnode(sid: int, lay: int) -> int:
        key = (sid, lay)
        nid = patch_id.get(key)
        if nid is None:
            rho, v = sec.pts[sid]
            s, c = trig[lay]
            y = rho * s
            nid = raw.add_node(rho * c, y, _zmap(v, y, ro, vb, va), sec.tags[sid])
            patch_id[key] = nid
        return nid

    for q in sec.quads:
        cq, mq = q[:4], q[4:]
        for k in range(0, n_l, 2):
            k1, k2 = (k + 1) % n_l, (k + 2) % n_l
            conn = [pnode(n, k) for n in cq] + [pnode(n, k2) for n in cq]
            conn += [pnode(n, k) for n in mq] + [pnode(n, k2) for n in mq]
            conn += [pnode(n, k1) for n in cq]
            raw.add_element(conn)

    # --- run pipe outside the patch ----------------------------------------
    pipe_v = info["pipe_v"]
    q_half = _SQUARE_HALF_MM
    sq = [-q_half + 2.0 * q_half * k / (n_t // 2) for k in range(n_t // 2 + 1)]
    sq[0], sq[-1] = -q_half, q_half
    h = n_t // 2  # doubled points per square side

    def pipe_pt(x: float, s: float, v: float) -> tuple[float, float, float]:
        sn, cs = _sincos(s / ro)
        r = ro + v
        return (x, r * sn, r * cs)

    def square_pt(lay: int) -> tuple[float, float]:
        lp = (lay + n_t // 4) % n_l
        side, p = divmod(lp, h)
        if side == 0:
            return sq[h], sq[p]
        if side == 1:
            return sq[h - p], sq[h]
        if side == 2:
            return sq[0], sq[h - p]
        return sq[p], sq[0]

    ring = _doubled([k / _O_BLOCK_DIVISIONS for k in range(_O_BLOCK_DIVISIONS + 1)])
    ring[-1] = 1.0

    def o_point(p: int, q: int, w: int):
        lay = p % n_l
        v = pipe_v[w]
        s_, c_ = trig[lay]
        x_in, y_in = _PATCH_RADIUS_MM * c_, _PATCH_RADIUS_MM * s_
        z_in = _zmap(v, y_in, ro, vb, va)
        if q == 0:
            return (x_in, y_in, z_in)
        xs, ss = square_pt(lay)
        if q == len(ring) - 1:
            return pipe_pt(xs, ss, v)
        s_in = ro * _atan(y_in / z_in)
        f = ring[q]
        return pipe_pt(x_in + f * (xs - x_in), s_in + f * (ss - s_in), v)

    n_w = (len(pipe_v) - 1) // 2
    raw.add_block(o_point, n_t, _O_BLOCK_DIVISIONS, n_w)

    length = spec.run_half_length_mm
    xr_c = [q_half] + [q_half + p for p in
                       _geometric(2.0 * q_half / (n_t // 4), _HOOP_CHECK_X_MM - q_half,
                                  _GROWTH)[1:-1]] + [_HOOP_CHECK_X_MM]
    step = xr_c[-1] - xr_c[-2]
    xr_c += [_HOOP_CHECK_X_MM + p for p in
             _geometric(step, length - _HOOP_CHECK_X_MM, _GROWTH)[1:-1]] + [length]
    x_right = _doubled(xr_c)
    x_left = [-x for x in reversed(x_right)]
    x_all = x_left[:-1] + sq + x_right[1:]
    s_span = 2.0 * _PI * ro - 2.0 * q_half
    band = [q_half + s_span * k / (2 * _BAND_DIVISIONS) for k in range(2 * _BAND_DIVISIONS + 1)]

    def tensor(xl, sl, wrap: bool):
        def pt(p: int, q: int, w: int):
            s = sl[q]
            if wrap and q == len(sl) - 1:
                s = sq[0]
            if q == 0 and wrap:
                s = sq[-1]
            return pipe_pt(xl[p], s, pipe_v[w])

        raw.add_block(pt, (len(xl) - 1) // 2, (len(sl) - 1) // 2, n_w)

    tensor(x_left, sq, False)
    tensor(x_right, sq, False)
    tensor(x_all, band, True)

    # --- renumber: front corners, front mid-sides, patch nodes by section
    #     node then layer, then the rest --------------------------------------
    order: list[int] = []
    seen: set[int] = set()
    tip = sec.tip
    for lay in list(range(0, n_l, 2)) + list(range(1, n_l, 2)):
        nid = patch_id[(tip, lay)]
        order.append(nid)
        seen.add(nid)
    for sid in range(len(sec.pts)):
        for lay in range(n_l):
            nid = patch_id.get((sid, lay))
            if nid is not None and nid not in seen:
                order.append(nid)
                seen.add(nid)
    for nid in range(1, len(raw.nodes) + 1):
        if nid not in seen:
            order.append(nid)
            seen.add(nid)
    new_id = {old: k + 1 for k, old in enumerate(order)}
    mesh = WeldoletMesh()
    mesh.nodes = [raw.nodes[o - 1] for o in order]
    mesh.keys = [raw.keys[o - 1] for o in order]
    mesh.elements = [tuple(new_id[n] for n in el) for el in raw.elements]
    mesh.n_front = n_t
    mesh.n_front_all = n_l
    mesh.n_ligament = 0

    # --- boundary sets ------------------------------------------------------
    def lift(names: tuple[str, ...]) -> set[int]:
        out = set()
        for name in names:
            for sid in sec.sets.get(name, ()):
                for lay in range(n_l):
                    nid = patch_id.get((sid, lay))
                    if nid is not None:
                        out.add(new_id[nid])
        return out

    bnd = {name: lift((name,)) for name in ("hole", "weldolet_bore", "weldolet_outer",
                                             "branch_end", "pipe_od", "pipe_id")}
    ri = geo["run_inner_radius_mm"]
    fx_l, fx_r = _fmt(-length), _fmt(length)
    bnd["end_fixed"] = set()
    bnd["end_free"] = set()
    for nid, (x, y, z) in enumerate(mesh.nodes, start=1):
        rr = math.sqrt(y * y + z * z)
        key = mesh.keys[nid - 1]
        if abs(rr - ri) < 1e-6:
            bnd["pipe_id"].add(nid)
        toe = geo["toe_radius_mm"]
        if abs(rr - ro) < 1e-6 and (z < 0.0 or x * x + y * y > (toe - 1e-6) ** 2):
            bnd["pipe_od"].add(nid)
        if key[0] == fx_l:
            bnd["end_fixed"].add(nid)
        elif key[0] == fx_r:
            bnd["end_free"].add(nid)
    if spec.crack_depth_mm is not None:
        bnd["crack_lower"] = lift(("crack_lower",))
        bnd["crack_upper"] = lift(("crack_upper",))
    mesh.boundary_nodes = bnd

    def runs(ids: set[int]) -> list[tuple[int, int]]:
        out: list[tuple[int, int]] = []
        for n in sorted(ids):
            if out and out[-1][1] == n - 1:
                out[-1] = (out[-1][0], n)
            else:
                out.append((n, n))
        return out

    mesh.wet_runs = runs(lift(("hole", "weldolet_bore")))
    if spec.crack_depth_mm is not None:
        mesh.crack_runs = runs(bnd["crack_lower"] | bnd["crack_upper"])

    # --- supports and check locations --------------------------------------
    def find(pt: tuple[float, float, float]) -> int:
        key = (_fmt(pt[0]), _fmt(pt[1]), _fmt(pt[2]), "")
        return new_id[raw._index[key]]

    mesh.fixed_end = sorted(bnd["end_fixed"])
    s_bottom = band[_BAND_DIVISIONS]
    mesh.support_nodes = {
        "uy_top": find(pipe_pt(-length, sq[h // 2], 0.0)),
        "uy_bottom": find(pipe_pt(-length, s_bottom, 0.0)),
        "uz_side": find(pipe_pt(-length, band[2 * round(
            _BAND_DIVISIONS * (0.5 * _PI * ro - q_half) / s_span)], 0.0)),
    }
    mesh.hoop_line = [find(pipe_pt(_HOOP_CHECK_X_MM, s_bottom, v)) for v in pipe_v]
    if spec.crack_depth_mm is None:
        root = sec.index[(_fmt(geo["hole_radius_mm"]), _fmt(0.0), "")]
        toe_n = sec.index[(_fmt(geo["toe_radius_mm"]), _fmt(0.0), "")]
        for theta in PATH_THETAS_DEG:
            lay = round(theta / 180.0 * n_t)
            mesh.path_nodes[theta] = (new_id[patch_id[(root, lay)]],
                                      new_id[patch_id[(toe_n, lay)]])

    # --- crack-extension directions at every front node (corners 1..n_t at
    #     even layers, mid-sides n_t+1..2 n_t at odd layers) ---------------------
    rho_f = info["rho_f"]
    for k in range(n_l):
        lay = 2 * k if k < n_t else 2 * (k - n_t) + 1
        s_, c_ = trig[lay]
        x, y, z = mesh.nodes[k]
        tx, ty, tz = -rho_f * s_, rho_f * c_, -(rho_f * s_) * (rho_f * c_) / z
        nx, ny, nz = 0.0, y / ro, z / ro
        ex, ey, ez = ty * nz - tz * ny, tz * nx - tx * nz, tx * ny - ty * nx
        if ex * c_ + ey * s_ < 0.0:
            ex, ey, ez = -ex, -ey, -ez
        norm = math.sqrt(ex * ex + ey * ey + ez * ez)
        mesh.front_vectors.append((ex / norm, ey / norm, ez / norm))
    return mesh


# --------------------------------------------------------------------------- #
# Deck
# --------------------------------------------------------------------------- #
def cint_table_name(level: int) -> str:
    return f"weldolet_cint_L{level}"


def reaction_file_name(level: int) -> str:
    return f"weldolet_reac_L{level}"


def path_file_name(level: int) -> str:
    return f"weldolet_path_L{level}"


def hoop_file_name(level: int) -> str:
    return f"weldolet_hoop_L{level}"


def sifs_start_file_name(level: int) -> str:
    return f"weldolet_sifs_start_L{level}"


def start_node_audit(text: str) -> dict:
    """Audit of the start-node correction from the side table.

    At the reference node (a start node of neither definition) the two SIFS
    definitions must agree after the sign factors (``ref_max_rel_diff``); at
    node 1 the size of the first-node perturbation of definition A is recorded.
    """
    sg = (1.0, 1.0)
    rows = []
    for raw in text.replace("\r\n", "\n").splitlines():
        line = raw.strip()
        if line.startswith("# sign_factors"):
            parts = line.split()
            sg = (float(parts[2]), float(parts[3]))
        elif line and not line.startswith("#"):
            rows.append([float(t) for t in line.split()])
    signs = (1.0, sg[0], sg[1])

    def rel(a: float, b: float, scale: float) -> float:
        return abs(a - b) / scale if scale else 0.0

    ref = [r for r in rows if round(r[0]) == START_REF_NODE]
    scale_ref = max(abs(v) for r in ref for v in r[2:5])
    ref_diff = max(rel(r[2 + i], signs[i] * r[5 + i], scale_ref) for r in ref for i in range(3))
    n1 = [r for r in rows if round(r[0]) == 1]
    scale_1 = max(abs(v) for r in n1 for v in r[5:8])
    last = n1[-1]
    return {
        "reference_node": START_REF_NODE,
        "sign_factors_k2_k3": list(sg),
        "ref_max_rel_diff": ref_diff,
        "node1_k1_perturbation_rel": (last[2] - last[5]) / last[5] if last[5] else None,
        "node1_max_rel_diff_a_vs_b": max(
            rel(r[2 + i], signs[i] * r[5 + i], scale_1) for r in n1 for i in range(3)
        ),
        "tolerance_ref": 1e-6,
        "within_tolerance": ref_diff <= 1e-6,
    }


def path_points(spec: WeldoletSpec, theta_deg: float) -> list[tuple[float, float, float]]:
    """Points on the fusion face from the root to the fillet toe at theta."""
    geo = derived_geometry(spec)
    rw, toe = geo["hole_radius_mm"], geo["toe_radius_mm"]
    ro = geo["run_outer_radius_mm"]
    s_, c_ = _sincos(_PI * theta_deg / 180.0)
    out = []
    for k in range(PATH_POINTS):
        rho = rw + (toe - rw) * k / (PATH_POINTS - 1)
        y = rho * s_
        out.append((rho * c_, y, math.sqrt(ro * ro - y * y)))
    return out


def front_sequence(mesh: WeldoletMesh, start: int = 1) -> list[int]:
    """Front node ids in order along the closed front (corner, mid-side, ...),
    beginning at corner ``start``."""
    out = []
    for k in range(1, mesh.n_front + 1):
        out += [k, mesh.n_front + k]
    i = 2 * (start - 1)
    return out[i:] + out[:i]


def _nsel_runs(w, runs: list[tuple[int, int]], first: str = "S") -> None:
    mode = first
    for lo, hi in runs:
        w(f"NSEL,{mode},NODE,,{lo},{hi}")
        mode = "A"


@dataclass(frozen=True)
class DeckHooks:
    """Analysis-specific additions for a deck built on the same model (used by
    ``weldolet_limit``): extra material lines, solution controls, a load-factor
    expression appended to every pressure, APDL lines defining PAPP (the applied
    pressure reported with the reactions) and a post-processing writer. A deck
    with hooks has no CINT."""

    name: str
    material: tuple[str, ...] = ()
    solution: tuple[str, ...] = ()
    load_factor: str = ""
    papp: tuple[str, ...] = ()
    post: object = None  # callable(w, spec, mesh, level)


def generate_weldolet_apdl(spec: WeldoletSpec, hooks: DeckHooks | None = None) -> str:
    """Return the complete, deterministic MAPDL ``.inp`` for ``spec``."""
    mesh = build_mesh(spec)
    geo = derived_geometry(spec)
    cf = closed_forms(spec)
    par = mesh_parameters(spec)
    lv = spec.mesh_level
    cracked = spec.crack_depth_mm is not None
    limit = hooks is not None
    analysis = hooks.name if limit else "elastic"
    ro, ri = geo["run_outer_radius_mm"], geo["run_inner_radius_mm"]
    length = spec.run_half_length_mm
    top_z = ro + spec.weldolet_a_mm + spec.branch_length_mm
    out: list[str] = []
    w = out.append
    w("! Weldolet attachment-weld root-flaw model (#2157 P0b)")
    w("! generated by digitalmodel.ansys.weldolet_crack")
    w("! Units: length = mm, force = N, stress = MPa")
    w("! raw CINT K = MPa*sqrt(mm), J = N/mm (converted once by cint_parser)")
    w("! assumed design basis: examples/workflows/crack-fe-weldolet/"
      "design-data-register.json")
    if cracked:
        w(f"! full-circumference root flaw on the fusion face, a = "
          f"{spec.crack_depth_mm!r} mm, ligament = {remaining_ligament_mm(spec)!r} mm")
    else:
        w("! uncracked model (block layout of a0 with merged crack faces)")
    w(f"! analysis = {analysis}; mesh_level = {lv}; front divisions = "
      f"{par['front_divisions']}; first ring = {par['first_ring_mm']!r} mm")
    w(f"! nodes = {len(mesh.nodes)}, elements = {len(mesh.elements)} (SOLID186)")
    if cracked:
        w(f"! crack-face pressure {'ON' if spec.crack_face_pressure else 'OFF'}")
    w("FINISH")
    w("/CLEAR,NOSTART")
    w(f"/TITLE,Weldolet root flaw a={spec.crack_depth_mm!r} level {lv} {analysis}")
    w("/UNITS,MPA")
    w("/PREP7")
    w("ET,1,SOLID186")
    w("KEYOPT,1,2,1")
    w(f"MP,EX,1,{spec.youngs_modulus_mpa!r}")
    w(f"MP,PRXY,1,{spec.poisson!r}")
    for line in hooks.material if limit else ():
        w(line)
    w("LOCAL,11,1,0.0,0.0,0.0,0.0,0.0,90.0")
    w("CSYS,0")
    w(f"NFRONT = {mesh.n_front}")
    w(f"NFRALL = {mesh.n_front_all}")
    for nid, key in enumerate(mesh.keys, start=1):
        w(f"N,{nid},{key[0]},{key[1]},{key[2]}")
    w("TYPE,1")
    w("MAT,1")
    for el in mesh.elements:
        w("E," + ",".join(str(n) for n in el[:8]))
        w("EMORE," + ",".join(str(n) for n in el[8:16]))
        w("EMORE," + ",".join(str(n) for n in el[16:]))
    if cracked and not limit:
        # one crack-extension node component per front node, mid-sides included
        for k in range(1, mesh.n_front_all + 1):
            w(f"NSEL,S,NODE,,{k}")
            w(f"CM,CF{k},NODE")
        w("ALLSEL,ALL")
    w("FINISH")
    w("/SOLU")
    w("ANTYPE,STATIC")
    for line in hooks.solution if limit else ():
        w(line)
    # supports
    w(f"NSEL,S,LOC,X,{-length - 1.0e-6!r},{-length + 1.0e-6!r}")
    w("D,ALL,UX,0.0")
    w("ALLSEL,ALL")
    w(f"D,{mesh.support_nodes['uy_top']},UY,0.0")
    w(f"D,{mesh.support_nodes['uy_bottom']},UY,0.0")
    w(f"D,{mesh.support_nodes['uz_side']},UZ,0.0")
    # the loaded end stays plane (coupled UX), matching the fixed end, so both
    # ends have the same rotation restraint and the model is symmetric in x
    w(f"NSEL,S,LOC,X,{length - 1.0e-6!r},{length + 1.0e-6!r}")
    w("CP,1,UX,ALL")
    w("ALLSEL,ALL")
    lf = hooks.load_factor if limit else ""
    w(f"PINT = {spec.pressure_mpa!r}{lf}")
    w(f"PRUN = {cf['run_end_pressure_mpa']!r}{lf}")
    w(f"PBR = {cf['branch_end_pressure_mpa']!r}{lf}")
    # wetted surfaces: run-pipe bore (by radius in CS 11) + hole, root face,
    # weldolet taper and bore, branch bore (node runs)
    w("CSYS,11")
    w(f"NSEL,S,LOC,X,{ri - 1.0e-6!r},{ri + 1.0e-6!r}")
    w("CSYS,0")
    _nsel_runs(w, mesh.wet_runs, first="A")
    if cracked and spec.crack_face_pressure:
        _nsel_runs(w, mesh.crack_runs, first="A")
    w("SF,ALL,PRES,PINT")
    w("ALLSEL,ALL")
    w(f"NSEL,S,LOC,X,{length - 1.0e-6!r},{length + 1.0e-6!r}")
    w("SF,ALL,PRES,-PRUN")
    w(f"NSEL,S,LOC,Z,{top_z - 1.0e-6!r},{top_z + 1.0e-6!r}")
    w("SF,ALL,PRES,-PBR")
    w("ALLSEL,ALL")
    if cracked and not limit:
        # CINT 1 (SIFS) and 2 (JINT) start at front node 1; CINT 3 (SIFS)
        # starts at node 2. MAPDL 2026 R1 returns a perturbed K at the first
        # CENC-defined node of a closed front (J is unaffected), so K at node 1
        # is taken from CINT 3 (see _write_cint_table).
        for cid, kind, start in ((1, "SIFS", 1), (2, "JINT", 1), (3, "SIFS", 2)):
            w(f"CINT,NEW,{cid}")
            w(f"CINT,TYPE,{kind}")
            # defined in sequence along the closed front: corner k, then the
            # mid-side between corners k and k+1 (id n_front + k)
            for k in front_sequence(mesh, start):
                ex, ey, ez = mesh.front_vectors[k - 1]
                w(f"CINT,CENC,CF{k},{k},,,{ex!r},{ey!r},{ez!r}")
            w(f"CINT,NCON,{N_CONTOURS}")
        w("OUTRES,ALL,ALL")
    w("SOLVE")
    w("FINISH")
    w("/POST1")
    w("SET,LAST")
    w("*GET,MREV,ACTIVE,0,REV")
    w(f"LVL = {lv}")
    if cracked and not limit:
        _write_cint_table(w, lv)
    _write_reactions(w, spec, mesh, lv, hooks.papp if limit else ())
    if not cracked and not limit:
        _write_paths(w, spec, lv, mesh.path_nodes)
        _write_hoop(w, mesh, lv)
    if limit and hooks.post is not None:
        hooks.post(w, spec, mesh, lv)
    w("FINISH")
    return "\n".join(out) + "\n"


START_REF_NODE = 3  # sign reference for the start-node correction


def _write_cint_table(w, lv: int) -> None:
    # sign factors that map CINT 3 onto the crack-normal convention of CINT 1,
    # from the last contour at the reference node (a start node of neither)
    w(f"*GET,RA2,CINT,1,CTIP,{START_REF_NODE},,{N_CONTOURS},DTYPE,K2")
    w(f"*GET,RB2,CINT,3,CTIP,{START_REF_NODE},,{N_CONTOURS},DTYPE,K2")
    w(f"*GET,RA3,CINT,1,CTIP,{START_REF_NODE},,{N_CONTOURS},DTYPE,K3")
    w(f"*GET,RB3,CINT,3,CTIP,{START_REF_NODE},,{N_CONTOURS},DTYPE,K3")
    w("SG2 = 1.0")
    w("*IF,RA2*RB2,LT,0.0,THEN")
    w("SG2 = -1.0")
    w("*ENDIF")
    w("SG3 = 1.0")
    w("*IF,RA3*RB3,LT,0.0,THEN")
    w("SG3 = -1.0")
    w("*ENDIF")
    # side table: both SIFS definitions at nodes 1..3 (audit of the correction)
    w(f"*CFOPEN,{sifs_start_file_name(lv)},txt")
    w("*VWRITE")
    w("('# digitalmodel weldolet_crack start-node SIFS audit')")
    w("*VWRITE")
    w("('# units: K=MPa*sqrt(mm); A = CINT 1 (starts at node 1), "
      "B = CINT 3 (starts at node 2)')")
    w("*VWRITE,SG2,SG3")
    w("('# sign_factors ',F5.1,1X,F5.1)")
    w("*VWRITE")
    w("('# columns: node contour K1A K2A K3A K1B K2B K3B')")
    w(f"*DO,NN,1,{START_REF_NODE}")
    w(f"*DO,IC,1,{N_CONTOURS}")
    w("*GET,A1,CINT,1,CTIP,NN,,IC,DTYPE,K1")
    w("*GET,A2,CINT,1,CTIP,NN,,IC,DTYPE,K2")
    w("*GET,A3,CINT,1,CTIP,NN,,IC,DTYPE,K3")
    w("*GET,B1,CINT,3,CTIP,NN,,IC,DTYPE,K1")
    w("*GET,B2,CINT,3,CTIP,NN,,IC,DTYPE,K2")
    w("*GET,B3,CINT,3,CTIP,NN,,IC,DTYPE,K3")
    w("*VWRITE,NN,IC,A1,A2,A3,B1,B2,B3")
    w("(F8.0,1X,F4.0,6(1X,E18.10))")
    w("*ENDDO")
    w("*ENDDO")
    w("*CFCLOS")
    w(f"*CFOPEN,{cint_table_name(lv)},txt")
    w("*VWRITE")
    w("('# digitalmodel weldolet_crack CINT table')")
    w("*VWRITE")
    w("('# units: length=mm force=N stress=MPa K=MPa*sqrt(mm) J=N/mm')")
    w("*VWRITE,LVL")
    w("('# mesh_level ',F6.0)")
    w("*VWRITE,NFRONT")
    w("('# front_nodes ',F8.0)")
    w(f"NCONT = {N_CONTOURS}")
    w("*VWRITE,NCONT")
    w("('# contours ',F4.0)")
    w("*VWRITE")
    w("('# columns: node contour x y z K1 K2 K3 J')")
    w("*DO,NN,1,NFRONT")
    w(f"*DO,IC,1,{N_CONTOURS}")
    w("*IF,NN,EQ,1,THEN")
    w("*GET,VK1,CINT,3,CTIP,NN,,IC,DTYPE,K1")
    w("*GET,VK2,CINT,3,CTIP,NN,,IC,DTYPE,K2")
    w("*GET,VK3,CINT,3,CTIP,NN,,IC,DTYPE,K3")
    w("VK2 = SG2*VK2")
    w("VK3 = SG3*VK3")
    w("*ELSE")
    w("*GET,VK1,CINT,1,CTIP,NN,,IC,DTYPE,K1")
    w("*GET,VK2,CINT,1,CTIP,NN,,IC,DTYPE,K2")
    w("*GET,VK3,CINT,1,CTIP,NN,,IC,DTYPE,K3")
    w("*ENDIF")
    w("*GET,VJ,CINT,2,CTIP,NN,,IC,DTYPE,JINT")
    w("VX = NX(NN)")
    w("VY = NY(NN)")
    w("VZ = NZ(NN)")
    w("*VWRITE,NN,IC,VX,VY,VZ,VK1,VK2,VK3,VJ")
    w("(F8.0,1X,F4.0,7(1X,E18.10))")
    w("*ENDDO")
    w("*ENDDO")
    w("*CFCLOS")


def _write_reactions(w, spec, mesh, lv: int, papp: tuple[str, ...] = ()) -> None:
    length = spec.run_half_length_mm
    w(f"NSEL,S,LOC,X,{-length - 1.0e-6!r},{-length + 1.0e-6!r}")
    w("*GET,NFIX,NODE,0,COUNT")
    w("FXSUM = 0.0")
    w("NN = 0")
    w("*DO,II,1,NFIX")
    w("NN = NDNEXT(NN)")
    w("*GET,RV,NODE,NN,RF,FX")
    w("FXSUM = FXSUM + RV")
    w("*ENDDO")
    w("ALLSEL,ALL")
    w(f"*GET,FYA,NODE,{mesh.support_nodes['uy_top']},RF,FY")
    w(f"*GET,FYB,NODE,{mesh.support_nodes['uy_bottom']},RF,FY")
    w(f"*GET,FZC,NODE,{mesh.support_nodes['uz_side']},RF,FZ")
    w("FYSUM = FYA + FYB")
    w(f"PAPP = {spec.pressure_mpa!r}")
    for line in papp:
        w(line)
    ri = derived_geometry(spec)["run_inner_radius_mm"]
    w(f"AREAAPP = {math.pi * ri * ri!r}")
    w(f"*CFOPEN,{reaction_file_name(lv)},txt")
    w("*VWRITE")
    w("('# digitalmodel weldolet_crack reactions')")
    w("*VWRITE")
    w("('# units: length=mm force=N stress=MPa')")
    w("*VWRITE,LVL")
    w("('mesh_level ',F6.0)")
    w("*VWRITE,MREV")
    w("('mapdl_rev ',F8.2)")
    w("*VWRITE,PAPP")
    w("('stress_mpa ',E20.12)")
    w("*VWRITE,AREAAPP")
    w("('loaded_area_mm2 ',E20.12)")
    w("*VWRITE,FXSUM")
    w("('reaction_sum ',E20.12)")
    w("*VWRITE,FYSUM")
    w("('reaction_y_sum ',E20.12)")
    w("*VWRITE,FZC")
    w("('reaction_z_sum ',E20.12)")
    w("*VWRITE,NFIX")
    w("('n_fixed_nodes ',F10.0)")
    w("*CFCLOS")


def _write_paths(w, spec: WeldoletSpec, lv: int, path_nodes: dict) -> None:
    n_samples = (PATH_POINTS - 1) * PATH_DIVISIONS + 1
    w("RSYS,11")
    w(f"*CFOPEN,{path_file_name(lv)},txt")
    w("*VWRITE")
    w("('# digitalmodel weldolet_crack fusion-face paths (uncracked)')")
    w("*VWRITE")
    w("('# units: length=mm force=N stress=MPa')")
    w("*VWRITE")
    w("('# stresses in CS 11 (cylindrical about the run-pipe axis): "
      "SRR normal to the fusion face')")
    w("*VWRITE")
    w("('# columns: path theta_deg s x y z SRR SHH SAA SRH SRA')")
    for ip, theta in enumerate(PATH_THETAS_DEG, start=1):
        pts = path_points(spec, theta)
        w(f"PATH,FP{ip},{PATH_POINTS},30,{PATH_DIVISIONS}")
        for k, (x, y, z) in enumerate(pts, start=1):
            w(f"PPATH,{k},,{_fmt(x)},{_fmt(y)},{_fmt(z)},0")
        for lab, comp in (("SRR", "X"), ("SHH", "Y"), ("SAA", "Z"), ("SRH", "XY"),
                          ("SRA", "XZ")):
            w(f"PDEF,{lab},S,{comp},AVG")
        w(f"*DEL,PT{ip},,NOPR")
        w(f"*DIM,PT{ip},ARRAY,{n_samples},9")
        w(f"PAGET,PT{ip},TABLE")
        w(f"IPATH = {ip}")
        w(f"THP = {theta!r}")
        w(f"*DO,KK,1,{n_samples}")
        w(f"VS = PT{ip}(KK,4)")
        w(f"VX = PT{ip}(KK,1)")
        w(f"VY = PT{ip}(KK,2)")
        w(f"VZ = PT{ip}(KK,3)")
        w(f"V1 = PT{ip}(KK,5)")
        w(f"V2 = PT{ip}(KK,6)")
        w(f"V3 = PT{ip}(KK,7)")
        w(f"V4 = PT{ip}(KK,8)")
        w(f"V5 = PT{ip}(KK,9)")
        w("*VWRITE,IPATH,THP,VS,VX,VY,VZ,V1,V2,V3,V4,V5")
        w("(F4.0,1X,F8.3,9(1X,E18.10))")
        w("*ENDDO")
    w("*CFCLOS")
    # MAPDL's own linearisation on the chord of each path, for the record
    # (PRSECT needs node-defined path ends: the root and toe nodes at theta)
    for ip, theta in enumerate(PATH_THETAS_DEG, start=1):
        n_root, n_toe = path_nodes[theta]
        w(f"PATH,LS{ip},2,30,48")
        w(f"PPATH,1,{n_root}")
        w(f"PPATH,2,{n_toe}")
        w("PRSECT")
    w("RSYS,0")


def _write_hoop(w, mesh: WeldoletMesh, lv: int) -> None:
    w("RSYS,11")
    w(f"*CFOPEN,{hoop_file_name(lv)},txt")
    w("*VWRITE")
    w("('# digitalmodel weldolet_crack far-field hoop check (uncracked)')")
    w("*VWRITE")
    w("('# units: length=mm force=N stress=MPa')")
    w("*VWRITE")
    w("('# columns: node r SRR SHH SAA (CS 11)')")
    for nid in mesh.hoop_line:
        w(f"NH = {nid}")
        w("VR = SQRT(NY(NH)**2 + NZ(NH)**2)")
        w("*GET,V1,NODE,NH,S,X")
        w("*GET,V2,NODE,NH,S,Y")
        w("*GET,V3,NODE,NH,S,Z")
        w("*VWRITE,NH,VR,V1,V2,V3")
        w("(F10.0,4(1X,E18.10))")
    w("*CFCLOS")
    w("RSYS,0")


def deck_sha256(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def spec_from_receipt(receipt: dict, level: int) -> WeldoletSpec:
    return WeldoletSpec(**{**receipt["spec"], "mesh_level": level})


def deck_sha256_for_receipt(receipt: dict, level: int) -> str:
    return deck_sha256(generate_weldolet_apdl(spec_from_receipt(receipt, level)))


def write_weldolet_inp(spec: WeldoletSpec, path: Path | str) -> Path:
    out = Path(path)
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_bytes(generate_weldolet_apdl(spec).encode("utf-8"))
    return out


# --------------------------------------------------------------------------- #
# Result derivation from the committed, host-free solver output
# --------------------------------------------------------------------------- #
_PATH_COLS = ("SRR", "SHH", "SAA", "SRH", "SRA")


def _data_rows(text: str) -> list[list[float]]:
    rows = []
    for raw in text.replace("\r\n", "\n").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        rows.append([float(tok) for tok in line.split()])
    return rows


def parse_path_file(text: str) -> dict[int, dict]:
    """Fusion-face path samples by path index (1-based)."""
    paths: dict[int, dict] = {}
    for row in _data_rows(text):
        ip = round(row[0])
        p = paths.setdefault(ip, {"theta_deg": row[1], "s": [], "xyz": [],
                                  **{c: [] for c in _PATH_COLS}})
        p["s"].append(row[2])
        p["xyz"].append((row[3], row[4], row[5]))
        for c, val in zip(_PATH_COLS, row[6:11], strict=True):
            p[c].append(val)
    return paths


def sigma_ref_paths(path_text: str) -> list[dict]:
    """Linearised stresses on every fusion-face path (uncracked model).

    sigma_m and sigma_b of the stress normal to the fusion face (SRR, pipe
    radial); sigma_b is the bending stress at the root end (s = 0), positive
    when the root side is more tensile. Membrane values of the two shear
    components are recorded as well.
    """
    out = []
    for ip, p in sorted(parse_path_file(path_text).items()):
        sm, sb = linearise(p["s"], p["SRR"])
        out.append({
            "path": ip,
            "theta_deg": p["theta_deg"],
            "start_xyz_mm": list(p["xyz"][0]),
            "end_xyz_mm": list(p["xyz"][-1]),
            "length_mm": p["s"][-1] - p["s"][0],
            "n_samples": len(p["s"]),
            "component": "SRR (normal to the fusion face, CS 11 radial)",
            "sigma_m_mpa": sm,
            "sigma_b_mpa": sb,
            "shear_rh_membrane_mpa": linearise(p["s"], p["SRH"])[0],
            "shear_ra_membrane_mpa": linearise(p["s"], p["SRA"])[0],
            "peak_srr_mpa": max(p["SRR"], key=abs),
        })
    return out


def hoop_check(spec: WeldoletSpec, hoop_text: str) -> dict:
    """Far-field hoop stress at the mean radius against the pre-stated
    comparator (Barlow, ID form) within the pre-stated tolerance."""
    cf = closed_forms(spec)
    geo = derived_geometry(spec)
    rows = _data_rows(hoop_text)
    rm = geo["run_mean_radius_mm"]
    at_rm = min(rows, key=lambda r: abs(r[1] - rm))
    comp = cf[cf["hoop_comparator"]]
    err = at_rm[3] / comp - 1.0
    return {
        "location": cf["hoop_check_location"],
        "radius_mm": at_rm[1],
        "radius_offset_from_rm_mm": at_rm[1] - rm,
        "fe_hoop_mpa": at_rm[3],
        "fe_axial_mpa": at_rm[4],
        "fe_radial_mpa": at_rm[2],
        "comparator": cf["hoop_comparator"],
        "comparator_mpa": comp,
        "lame_at_rm_mpa": cf["lame_hoop_mean_radius_mpa"],
        "axial_closed_end_mpa": cf["axial_closed_end_mpa"],
        "relative_error": err,
        "tolerance": cf["hoop_tolerance"],
        "within_tolerance": abs(err) <= cf["hoop_tolerance"],
        "relative_error_vs_lame_at_rm": at_rm[3] / lame_hoop(spec, at_rm[1]) - 1.0,
    }


def derive_uncracked(spec: WeldoletSpec, path_text: str, hoop_text: str) -> dict:
    return {
        "sigma_ref": {
            "basis": "FE-linearised stress normal to the fusion face, uncracked model "
                     "(owner card B02); paths from the root (rho = C/2) to the fillet "
                     "toe on v = 0",
            "paths": sigma_ref_paths(path_text),
        },
        "plausibility": {"hoop": hoop_check(spec, hoop_text)},
    }


def j_from_k_ratio(node: dict, spec: WeldoletSpec) -> float | None:
    """J(from K1,K2,K3, plane strain) / J(domain integral), K reported values."""
    e, nu = spec.youngs_modulus_mpa, spec.poisson
    k = [node.get(f"K{i}_reported") for i in (1, 2, 3)]
    j = node.get("J_reported")
    if None in k or not j:
        return None
    k1, k2, k3 = (v * math.sqrt(1000.0) for v in k)  # back to MPa*sqrt(mm)
    j_k = (k1 * k1 + k2 * k2) * (1.0 - nu * nu) / e + k3 * k3 * (1.0 + nu) / e
    return j_k / j


def depth_tag(depth_mm: float) -> str:
    return "a" + f"{depth_mm:.2f}".replace(".", "p")


def state_name(spec: WeldoletSpec) -> str:
    if spec.crack_depth_mm is None:
        return "p0b_uncracked"
    name = "p0b_fullcirc_" + depth_tag(spec.crack_depth_mm)
    return name if spec.crack_face_pressure else name + "_cfp_off"


def receipt_kind(spec: WeldoletSpec) -> str:
    return "weldolet_uncracked" if spec.crack_depth_mm is None else "weldolet_crack"


MESHING_APPROACH = (
    "Structured 20-node hexahedral mesh (SOLID186) generated in Python and written "
    "as N/E commands. In the (rho, v) section a square tube of half-size d around "
    "the crack front carries a 360 deg spider-web of rings, the innermost ring "
    "collapsed onto the tip (degenerate prism) with quarter-point mid-side nodes, "
    "with separate upper and lower crack-face nodes (the P0a crack block extended "
    "from 180 to 360 deg); tensor blocks in the run-pipe wall and a column-fraction "
    "block following the weld/weldolet contour; the section swept through 360 deg "
    "about the branch axis (closed crack front, no end nodes); an O-block and a "
    "tensor mesh for the rest of the run pipe. CINT crack-extension directions per "
    "front node (CINT,CENC) in the fusion face, normal to the front."
)


def _release_name(rev: str) -> str:
    try:
        major, minor = rev.split(".")
        return f"20{int(major)} R{int(minor)}"
    except ValueError:
        return ""


@dataclass
class StateVariant:
    """What a declared state solves and records (elastic states here, the
    limit-load sensitivity in ``weldolet_limit``)."""

    state: str
    kind: str
    spec_dict: dict
    generator_files: tuple[str, ...]
    deck: object  # callable(level) -> deck text
    outputs: object  # callable(level) -> {artifact name: file stem}
    derive: object  # callable(level, texts) -> extra mesh-entry keys
    mesh_info: object  # callable(level) -> {"n_nodes", "n_elements", "mesh_parameters"}
    top: object = None  # callable(primary entry) -> extra receipt keys
    accept_log: object = None  # callable(out text) -> bool, for a failed run
    cracked: bool = False
    meshing: dict | None = None


def run_variant(
    variant: StateVariant,
    workdir: Path | str,
    fe_states: Path | str,
    *,
    levels: tuple[int, ...] = (0, 1),
    cores: int = 4,
    timeout_seconds: int = 14400,
    repo_dir: Path | None = None,
) -> dict:
    """Generate, solve (fail-closed runner), parse and write the host-free
    receipt and artifacts of one declared state."""
    import platform

    from digitalmodel.ansys import cint_parser
    from digitalmodel.ansys.crack_receipt import (
        generator_blobs,
        generator_tree_clean,
        git,
    )
    from digitalmodel.ansys.crack_verification import save_artifact
    from digitalmodel.ansys.runner import ANSYSRunConfig, ANSYSRunner, ANSYSRunStatus

    workdir, fe_states = Path(workdir), Path(fe_states)
    repo = repo_dir or Path(__file__).resolve().parents[3]
    state, kind = variant.state, variant.kind
    geometry = {"type": "polar_z"}
    extra = ["-smp", "-np", str(cores)]  # distributed MPI hangs after a FATAL (#2196)
    commit = git(repo, "rev-parse", "HEAD")
    clean = generator_tree_clean(repo, list(variant.generator_files))
    texts: dict[int, dict[str, str]] = {}
    meshes = []
    for lv in levels:
        run_dir = workdir / state / f"L{lv}"
        run_dir.mkdir(parents=True, exist_ok=True)
        deck = run_dir / f"{state}_L{lv}.inp"
        text = variant.deck(lv)
        deck.write_bytes(text.encode("utf-8"))
        runner = ANSYSRunner(ANSYSRunConfig(output_dir=run_dir,
                                            timeout_seconds=timeout_seconds,
                                            extra_args=extra))
        exe = runner._detect_executable()
        result = runner.run(deck)
        accepted = result.status == ANSYSRunStatus.COMPLETED
        if not accepted and variant.accept_log is not None and result.log_file:
            accepted = variant.accept_log(result.log_file.read_text(errors="replace"))
        if not accepted:
            raise RuntimeError(f"{state} L{lv}: MAPDL {result.status.value}: "
                               f"{result.error_message}")
        names = variant.outputs(lv)
        texts[lv] = {k: (run_dir / f"{v}.txt").read_bytes().decode("utf-8")
                     for k, v in names.items()}
        artifacts = {k: save_artifact(texts[lv][k], fe_states,
                                      f"solved/{state}/{names[k]}.txt")
                     for k in names}
        record = cint_parser.build_mesh_record(
            level=lv, cint_text=texts[lv].get("cint") if variant.cracked else None,
            reac_text=texts[lv]["reac"], front_geometry=geometry)
        rev = cint_parser.parse_reaction_file(texts[lv]["reac"]).mapdl_rev or ""
        entry = {
            "level": lv,
            "deck_sha256": deck_sha256(text),
            "run": {
                "argv": [exe.name if exe else "mapdl", "-b", "-i", deck.name, "-o",
                         f"{deck.stem}.out", *extra],
                "mapdl_version": rev,
                "solve_seconds": round(result.duration_seconds, 1),
            },
            "artifacts": artifacts,
            **variant.mesh_info(lv),
            **{k: v for k, v in record.items() if k != "level"},
        }
        entry.update(variant.derive(lv, texts[lv]))
        meshes.append(entry)

    guards = cint_parser.evaluate_guards(
        {lv: t["cint"] for lv, t in texts.items() if variant.cracked},
        {lv: t["reac"] for lv, t in texts.items()},
        front_geometry=geometry, cracked=variant.cracked)
    if len(levels) == 1:
        guards["b_mesh_load"] = cint_parser.single_mesh_not_applicable()
    primary_level = max(levels)
    primary = next(m for m in meshes if m["level"] == primary_level)
    rev = primary["run"]["mapdl_version"]
    receipt = {
        "schema": cint_parser.RECEIPT_SCHEMA_ID,
        "state": state,
        "kind": kind,
        "issue": 2157,
        "spec": variant.spec_dict,
        "design_basis": "examples/workflows/crack-fe-weldolet/design-data-register.json",
        "units": {
            "length": "mm", "force": "N", "stress": "MPa",
            "k": cint_parser.K_UNIT, "k_raw": cint_parser.RAW_K_UNIT,
            "k_conversion_factor": cint_parser.K_RAW_TO_SI, "j": cint_parser.J_UNIT,
        },
        "meshing": variant.meshing or {},
        "run": {
            "producing_commit": commit,
            "generator_tree_clean": clean,
            "generator_files": generator_blobs(repo, commit, list(variant.generator_files)),
            "mapdl_version": rev,
            "mapdl_release": _release_name(rev),
            "cores": cores,
            "platform": platform.system().lower(),
            "solver_wrapper": "digitalmodel.ansys fail-closed MAPDL subprocess (#940)",
        },
        "meshes": meshes,
        "primary_level": primary_level,
        "guards": {name: g.to_dict() for name, g in guards.items()},
    }
    if variant.top is not None:
        receipt.update(variant.top(primary))
    out = fe_states / f"{state}.receipt.json"
    out.write_bytes((json.dumps(receipt, indent=1) + "\n").encode("utf-8"))
    if variant.cracked:
        csv_text = cint_parser.front_csv(primary)
        (fe_states / f"{state}.front.csv").write_bytes(csv_text.encode("utf-8"))
    return receipt


def crack_summary(spec: WeldoletSpec) -> dict:
    geo = derived_geometry(spec)
    return {
        "type": "full-circumference weld-root flaw on the run-pipe fusion face",
        "depth_mm": spec.crack_depth_mm,
        "front_radius_mm": geo["hole_radius_mm"] + spec.crack_depth_mm,
        "remaining_ligament_mm": remaining_ligament_mm(spec),
        "footprint_mm": geo["footprint_mm"],
        "crack_face_pressure": spec.crack_face_pressure,
    }


def elastic_variant(spec: WeldoletSpec, sigma_ref: dict | None = None) -> StateVariant:
    """The declared elastic state for ``spec`` (uncracked or cracked)."""
    kind = receipt_kind(spec)
    cracked = kind == "weldolet_crack"

    def at(level: int) -> WeldoletSpec:
        return WeldoletSpec(**{**spec.__dict__, "mesh_level": level})

    def outputs(level: int) -> dict[str, str]:
        names = {"reac": reaction_file_name(level)}
        if cracked:
            names["cint"] = cint_table_name(level)
            names["sifs"] = sifs_start_file_name(level)
        else:
            names["path"] = path_file_name(level)
            names["hoop"] = hoop_file_name(level)
        return names

    def derive(level: int, texts: dict[str, str]) -> dict:
        s = at(level)
        if not cracked:
            return {"derived": derive_uncracked(s, texts["path"], texts["hoop"])}
        from digitalmodel.ansys import cint_parser

        front = cint_parser.build_mesh_record(
            level=level, cint_text=texts["cint"], reac_text=texts["reac"],
            front_geometry={"type": "polar_z"})["front"]
        return {
            "j_from_k_ratio": [j_from_k_ratio(n, s) for n in front],
            "start_node_audit": start_node_audit(texts["sifs"]),
        }

    def mesh_info(level: int) -> dict:
        s = at(level)
        mesh = build_mesh(s)
        return {"n_nodes": len(mesh.nodes), "n_elements": len(mesh.elements),
                "mesh_parameters": mesh_parameters(s)}

    def top(primary: dict) -> dict:
        if not cracked:
            return dict(primary["derived"])
        extra = {"front_geometry": {"type": "polar_z"}, "crack": crack_summary(spec)}
        if sigma_ref is not None:
            extra["sigma_ref"] = sigma_ref
        return extra

    return StateVariant(
        state=state_name(spec),
        kind=kind,
        spec_dict={k: v for k, v in spec.__dict__.items() if k != "mesh_level"},
        generator_files=GENERATOR_FILES,
        deck=lambda level: generate_weldolet_apdl(at(level)),
        outputs=outputs,
        derive=derive,
        mesh_info=mesh_info,
        top=top,
        cracked=cracked,
        meshing={
            "approach": MESHING_APPROACH,
            "element": "SOLID186 (20-node, full integration)",
            "k_reported": "mean over contours 4-6",
            "contour_guard_quantities": ["K1", "J"],
            "modelling_route": "global structured crack-block model (no submodel)",
            "start_node_correction": (
                "K at front node 1 from a second SIFS definition starting at node 2 "
                "(MAPDL 2026 R1 perturbs K at the first CENC node of a closed front; "
                "J is unaffected); sign factors from reference node 3"
            ),
        },
    )


def run_state(spec: WeldoletSpec, workdir: Path | str, fe_states: Path | str, *,
              levels: tuple[int, ...] = (0, 1), cores: int = 4,
              sigma_ref: dict | None = None, timeout_seconds: int = 14400) -> dict:
    """Solve one declared elastic state and write its receipt and artifacts."""
    return run_variant(elastic_variant(spec, sigma_ref), workdir, fe_states,
                       levels=levels, cores=cores, timeout_seconds=timeout_seconds)
