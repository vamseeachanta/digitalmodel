"""Semi-elliptical surface-crack verification deck for MAPDL CINT (#2157 P0a).

Generates a complete MAPDL ``.inp`` for a flat plate with a semi-elliptical
surface crack under uniform remote tension, solved with the contour-integral
(CINT) extraction of K_I, K_II, K_III and J along the crack front. The CINT K at
the deepest and surface points is compared with the Newman-Raju (1981, NASA
TM-83200) closed-form fit in ``crack_fad.newman_raju_k``. The weldolet crack
model (P0b) is built only after this verification passes.

Model (units mm, N, MPa)
    Plate thickness t, crack depth a, crack half-length c (defaults a = 2,
    c = 4, t = 10), remote membrane stress sigma = 100 MPa. Coordinates: x along
    the crack length, y through the thickness from the cracked face (y = 0) to
    the back face (y = t), z normal to the crack plane (the load direction).

    Half model: the crack-plane symmetry z = 0 is used, so the crack faces are
    the unconstrained part of z = 0 inside the front and the ligament carries
    UZ = 0. The x = 0 symmetry is **not** used to cut the model; the plate spans
    x in [-b, b], so the deepest point (phi = 90 deg) is an interior crack-front
    node rather than an end node of the CINT front (CINT end-node values are
    less reliable). UX = 0 on the x = 0 plane is exact by symmetry and removes
    the rigid-body modes together with UY = 0 at one node.

    Size, edge effects: half-width b = 40 mm = 10c and half-height H = 40 mm =
    10c = 4t. The Newman-Raju finite-width factor
    [sec(pi c / (2b) sqrt(a/t))]^0.5 is 1.0012 for b = 40 mm, i.e. 0.12 % on K,
    which is small against the +/- 5 % band; the crack's stress perturbation
    decays within a few crack sizes of the front, well inside H.

Meshing approach (pure APDL, no geometry kernel)
    A structured, fully conforming 20-node hexahedral mesh (SOLID186) is built
    in Python and written as N/E/EMORE commands:

    * a crack-front "tube": in every plane normal to the front a spider-web of
      rings around the crack tip, the innermost ring collapsed onto the tip
      node (degenerate prism form of SOLID186) with its radial mid-side nodes
      at the quarter points, so the 1/sqrt(r) strain singularity is
      represented; the ring sizes grow geometrically to the tube radius;
    * the tube sits in a front-fitted band (normal offsets of the ellipse),
      which joins a three-block core under the crack face and two outer blocks
      (transfinite interpolation) that reach the plate edges;
    * the in-plane mesh is extruded in z, graded away from the crack plane.

    Every coordinate is computed with +, -, *, / and sqrt only (the ellipse is
    parametrised rationally, u = tan(phi/2)), all IEEE-754 correctly rounded,
    so the deck text is byte-identical on every platform. Nothing
    host-specific is written. ``mesh_level`` selects the density; each level
    halves the crack-front element size (first-ring radius and the spacing
    along the front).

Outputs written by the deck (relative names, in the solver working directory)
    ``crack_cint_L<level>.txt``: node, contour, x, y, z, K1, K2, K3, J for
    contours 1-6 at every crack-front corner node; ``crack_reac_L<level>.txt``:
    the solved reaction sums on the constrained faces and the MAPDL revision.
    ``cint_parser`` reads both and evaluates guards (a)-(f).

This module only writes text; the solve runs through the fail-closed
``ansys.runner`` on a licensed host.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import platform
import subprocess  # nosec B404 - fixed git argv only, for the producing commit
import time
from collections.abc import Callable
from dataclasses import asdict, dataclass
from itertools import pairwise
from pathlib import Path

# Mesh-density schedule. Level 0 is the base; every level halves the crack-front
# element size (first ring and the spacing along the front).
_BASE_FRONT_DIVISIONS_QUARTER = 12
_BASE_FIRST_RING_OVER_A = 0.01
_RING_RATIO = 1.35
_GROWTH_RATIO = 1.3
_TUBE_SIDE_DIVISIONS = 3
_CORE_RADIAL_DIVISIONS = 4
N_CONTOURS = 6

# SOLID186 node order as (p, q, w) offsets on the doubled logical grid.
# Corners I J K L M N O P, then mid-sides Q R S T U V W X Y Z A B.
_HEX20 = (
    (0, 0, 0), (2, 0, 0), (2, 2, 0), (0, 2, 0),
    (0, 0, 2), (2, 0, 2), (2, 2, 2), (0, 2, 2),
    (1, 0, 0), (2, 1, 0), (1, 2, 0), (0, 1, 0),
    (1, 0, 2), (2, 1, 2), (1, 2, 2), (0, 1, 2),
    (0, 0, 1), (2, 0, 1), (2, 2, 1), (0, 2, 1),
)
# Reordering that swaps the bottom and top faces (reverses orientation).
_FLIP = (4, 5, 6, 7, 0, 1, 2, 3, 12, 13, 14, 15, 8, 9, 10, 11, 16, 17, 18, 19)


@dataclass(frozen=True)
class CrackPlateSpec:
    """Verification plate with a semi-elliptical surface crack (mm, N, MPa)."""

    crack_depth_mm: float = 2.0  # a
    crack_half_length_mm: float = 4.0  # c
    thickness_mm: float = 10.0  # t
    half_width_mm: float = 40.0  # b (plate spans x in [-b, b])
    half_height_mm: float = 40.0  # H (crack plane to loaded end)
    stress_mpa: float = 100.0  # remote membrane tension
    youngs_modulus_mpa: float = 200_000.0
    poisson: float = 0.3
    tube_radius_mm: float = 0.6  # crack-front tube half-size
    mesh_level: int = 0

    def validate(self) -> list[str]:
        a, c, t = self.crack_depth_mm, self.crack_half_length_mm, self.thickness_mm
        d = self.tube_radius_mm
        issues: list[str] = []
        if not 0.0 < a < t:
            issues.append("require 0 < a < t")
            return issues
        if a > c:
            issues.append("require a <= c (Newman-Raju a/c <= 1 form)")
            return issues
        if self.mesh_level not in (0, 1, 2):
            issues.append("mesh_level must be 0, 1 or 2")
        if self.half_width_mm < 10.0 * c:
            issues.append("half-width must be at least 10 c (edge effects)")
        if self.half_height_mm < 10.0 * c or self.half_height_mm < 4.0 * t:
            issues.append("half-height must be at least 10 c and 4 t")
        # inner offset of the front must not fold: d below the smallest radius
        # of curvature a^2/c; the outer band must stay inside the plate.
        if not 0.0 < d <= 0.8 * a * a / c or d >= 0.5 * a or a + 2.0 * d >= t:
            issues.append("tube radius too large for the crack/plate geometry")
        if self.stress_mpa <= 0 or self.youngs_modulus_mpa <= 0:
            issues.append("stress and modulus must be positive")
        return issues


def finite_width_effect(spec: CrackPlateSpec) -> float:
    """Relative Newman-Raju finite-width correction on K for half-width b."""
    a, c, t = spec.crack_depth_mm, spec.crack_half_length_mm, spec.thickness_mm
    fw = math.sqrt(
        1.0 / math.cos(math.pi * c / (2.0 * spec.half_width_mm) * math.sqrt(a / t))
    )
    return fw - 1.0


def mesh_parameters(spec: CrackPlateSpec, level: int | None = None) -> dict:
    """Mesh-density parameters for ``level`` (defaults to ``spec.mesh_level``)."""
    lv = spec.mesh_level if level is None else level
    scale = 2**lv
    return {
        "level": lv,
        "front_divisions_quarter": _BASE_FRONT_DIVISIONS_QUARTER * scale,
        "first_ring_mm": spec.crack_depth_mm * _BASE_FIRST_RING_OVER_A / scale,
        "ring_ratio_target": _RING_RATIO,
        "tube_side_divisions": _TUBE_SIDE_DIVISIONS,
        "core_radial_divisions": _CORE_RADIAL_DIVISIONS,
        "growth_ratio_target": _GROWTH_RATIO,
    }


# --------------------------------------------------------------------------- #
# Deterministic 1-D spacing helpers (arithmetic only)
# --------------------------------------------------------------------------- #
def _geometric(h1: float, length: float, ratio: float) -> list[float]:
    """Corner positions 0..length: first step ~h1, growth solved to fit exactly."""
    n, total, term = 1, h1, h1
    while total < length:
        term *= ratio
        total += term
        n += 1
    if n == 1:
        return [0.0, length]

    def span(q: float) -> float:
        s, t = 0.0, h1
        for _ in range(n):
            s += t
            t *= q
        return s

    lo, hi = 0.05, ratio * 2.0
    for _ in range(200):
        mid = 0.5 * (lo + hi)
        if span(mid) < length:
            lo = mid
        else:
            hi = mid
    q = 0.5 * (lo + hi)
    pos, step, acc = [0.0], h1, 0.0
    for _ in range(n - 1):
        acc += step
        pos.append(acc)
        step *= q
    pos.append(length)
    return pos


def _doubled(corners: list[float]) -> list[float]:
    """Insert mid-points: corner k sits at index 2k."""
    out = [corners[0]]
    for lo, hi in pairwise(corners):
        out.append(0.5 * (lo + hi))
        out.append(hi)
    return out


def _line(p0: tuple, p1: tuple, n: int) -> list[tuple]:
    """Straight edge p0 -> p1 with n elements, doubled resolution."""
    pts = []
    for k in range(2 * n + 1):
        f = k / (2 * n)
        pts.append((p0[0] + (p1[0] - p0[0]) * f, p0[1] + (p1[1] - p0[1]) * f))
    pts[-1] = p1
    return pts


def _graded_line(p0: tuple, p1: tuple, fractions: list[float]) -> list[tuple]:
    pts = [
        (p0[0] + (p1[0] - p0[0]) * f, p0[1] + (p1[1] - p0[1]) * f) for f in fractions
    ]
    pts[0], pts[-1] = p0, p1
    return pts


def _arc_params(pts: list[tuple]) -> list[float]:
    acc = [0.0]
    for (x0, y0), (x1, y1) in pairwise(pts):
        dx, dy = x1 - x0, y1 - y0
        acc.append(acc[-1] + math.sqrt(dx * dx + dy * dy))
    return [v / acc[-1] for v in acc]


def _tfi(bottom: list, top: list, left: list, right: list) -> list[list[tuple]]:
    """Discrete transfinite (Coons) interpolation on doubled-resolution edges.

    ``grid[i][j]``: i along bottom/top, j along left/right. Boundary points are
    copied exactly from the edge arrays.
    """
    ni, nj = len(bottom) - 1, len(left) - 1
    sb, st = _arc_params(bottom), _arc_params(top)
    sl, sr = _arc_params(left), _arc_params(right)
    p00, p10, p01, p11 = bottom[0], bottom[ni], top[0], top[ni]
    grid = [[(0.0, 0.0)] * (nj + 1) for _ in range(ni + 1)]
    for i in range(ni + 1):
        for j in range(nj + 1):
            if j == 0:
                grid[i][j] = bottom[i]
            elif j == nj:
                grid[i][j] = top[i]
            elif i == 0:
                grid[i][j] = left[j]
            elif i == ni:
                grid[i][j] = right[j]
            else:
                dxi = st[i] - sb[i]
                xi = (sb[i] + sl[j] * dxi) / (1.0 - dxi * (sr[j] - sl[j]))
                eta = (1.0 - xi) * sl[j] + xi * sr[j]
                pt = []
                for k in (0, 1):
                    pt.append(
                        (1.0 - eta) * bottom[i][k]
                        + eta * top[i][k]
                        + (1.0 - xi) * left[j][k]
                        + xi * right[j][k]
                        - (1.0 - xi) * (1.0 - eta) * p00[k]
                        - xi * (1.0 - eta) * p10[k]
                        - (1.0 - xi) * eta * p01[k]
                        - xi * eta * p11[k]
                    )
                grid[i][j] = (pt[0], pt[1])
    return grid


# --------------------------------------------------------------------------- #
# Mesh
# --------------------------------------------------------------------------- #
def _fmt(v: float) -> str:
    s = f"{v + 0.0:.8f}"
    return "0.00000000" if s == "-0.00000000" else s


class CrackMesh:
    """Nodes (1-based ids) and 20-node elements of the half model."""

    def __init__(self) -> None:
        self.nodes: list[tuple[float, float, float]] = []
        self.keys: list[tuple[str, str, str]] = []
        self.elements: list[tuple[int, ...]] = []
        self.n_front = 0  # crack-front corner nodes: ids 1..n_front
        self.n_front_all = 0  # corners + front mid-sides: ids 1..n_front_all
        self.n_ligament = 0
        self.uy_node = 0
        self._index: dict[tuple[str, str, str], int] = {}
        self._tip_corner: set[int] = set()
        self._tip_mid: set[int] = set()

    def add_node(self, x: float, y: float, z: float) -> int:
        key = (_fmt(x), _fmt(y), _fmt(z))
        nid = self._index.get(key)
        if nid is None:
            self.nodes.append((x + 0.0, y + 0.0, z + 0.0))
            self.keys.append(key)
            nid = len(self.nodes)
            self._index[key] = nid
        return nid

    def add_element(self, conn: list[int]) -> None:
        conn = tuple(conn)
        if self.jacobian_at_centre(conn) < 0.0:
            conn = tuple(conn[k] for k in _FLIP)
        self.elements.append(conn)

    def jacobian_at_centre(self, conn) -> float:
        p = [self.nodes[n - 1] for n in conn[:8]]
        ci, cj, ck, cl, cm, cn, co, cp = p

        def comb(plus, minus):
            return tuple(
                (sum(v[k] for v in plus) - sum(v[k] for v in minus)) / 8.0
                for k in range(3)
            )

        dxi = comb((cj, ck, cn, co), (ci, cl, cm, cp))
        deta = comb((ck, cl, co, cp), (ci, cj, cm, cn))
        dzeta = comb((cm, cn, co, cp), (ci, cj, ck, cl))
        cross = (
            deta[1] * dzeta[2] - deta[2] * dzeta[1],
            deta[2] * dzeta[0] - deta[0] * dzeta[2],
            deta[0] * dzeta[1] - deta[1] * dzeta[0],
        )
        return dxi[0] * cross[0] + dxi[1] * cross[1] + dxi[2] * cross[2]

    def add_block(
        self,
        point: Callable[[int, int, int], tuple[float, float, float]],
        n_p: int,
        n_q: int,
        n_w: int,
        tip: Callable[[int, int, int], str | None] | None = None,
    ) -> None:
        """Structured block on a doubled logical grid (n_* elements per axis)."""
        ids: dict[tuple[int, int, int], int] = {}

        def node(i: int, j: int, k: int) -> int:
            nid = ids.get((i, j, k))
            if nid is None:
                nid = self.add_node(*point(i, j, k))
                ids[(i, j, k)] = nid
                if tip is not None:
                    kind = tip(i, j, k)
                    if kind == "corner":
                        self._tip_corner.add(nid)
                    elif kind == "mid":
                        self._tip_mid.add(nid)
            return nid

        for ep in range(n_p):
            for eq in range(n_q):
                for ew in range(n_w):
                    self.add_element(
                        [
                            node(2 * ep + dp, 2 * eq + dq, 2 * ew + dw)
                            for dp, dq, dw in _HEX20
                        ]
                    )

    def mirrored(self) -> CrackMesh:
        """Half model: this x >= 0 quarter plus its mirror image in x = 0."""
        full = CrackMesh()
        maps = []
        for sign in (1.0, -1.0):
            m = [full.add_node(sign * x, y, z) for (x, y, z) in self.nodes]
            maps.append(m)
            for n in self._tip_corner:
                full._tip_corner.add(m[n - 1])
            for n in self._tip_mid:
                full._tip_mid.add(m[n - 1])
            for conn in self.elements:
                full.add_element([m[n - 1] for n in conn])
        return full

    def renumbered(self, a: float, c: float, t: float) -> CrackMesh:
        """Renumber: front corners (by phi), front mid-sides, other ligament
        nodes (z = 0, on/outside the front), then all other nodes."""

        def ellipse(n: int) -> float:
            x, y, _ = self.nodes[n - 1]
            return (x / c) * (x / c) + (y / a) * (y / a)

        ids = range(1, len(self.nodes) + 1)
        front = sorted(self._tip_corner, key=lambda n: -self.nodes[n - 1][0])
        mids = sorted(self._tip_mid, key=lambda n: -self.nodes[n - 1][0])
        tipset = self._tip_corner | self._tip_mid
        lig = sorted(
            (
                n
                for n in ids
                if n not in tipset
                and self.nodes[n - 1][2] == 0.0
                and ellipse(n) >= 1.0 - 1e-9
            ),
            key=lambda n: (self.nodes[n - 1][1], self.nodes[n - 1][0]),
        )
        placed = set(front) | set(mids) | set(lig)
        rest = sorted(
            (n for n in ids if n not in placed),
            key=lambda n: (self.nodes[n - 1][2], self.nodes[n - 1][1], self.nodes[n - 1][0]),
        )
        order = front + mids + lig + rest
        new_id = {old: k + 1 for k, old in enumerate(order)}
        out = CrackMesh()
        out.nodes = [self.nodes[o - 1] for o in order]
        out.keys = [self.keys[o - 1] for o in order]
        out.elements = [tuple(new_id[n] for n in el) for el in self.elements]
        out.n_front = len(front)
        out.n_front_all = len(front) + len(mids)
        out.n_ligament = len(front) + len(mids) + len(lig)
        key = (_fmt(0.0), _fmt(t), _fmt(0.0))
        out.uy_node = out.keys.index(key) + 1
        return out


def build_mesh(spec: CrackPlateSpec) -> CrackMesh:
    """Build the structured half-model mesh for ``spec``."""
    issues = spec.validate()
    if issues:
        raise ValueError("invalid crack verification spec: " + "; ".join(issues))
    a, c, t = spec.crack_depth_mm, spec.crack_half_length_mm, spec.thickness_mm
    b, h, d = spec.half_width_mm, spec.half_height_mm, spec.tube_radius_mm
    par = mesh_parameters(spec)
    nq = par["front_divisions_quarter"]
    ns = par["tube_side_divisions"]
    nk = par["core_radial_divisions"]
    n_psi = 4 * ns

    # Front, rationally parametrised: u = tan(phi/2), u in [0, 1] for x >= 0.
    u2 = [k / (2 * nq) for k in range(2 * nq + 1)]
    front_pt, front_n = [], []
    for u in u2:
        den = 1.0 + u * u
        x, y = c * (1.0 - u * u) / den, a * 2.0 * u / den
        gx, gy = x / (c * c), y / (a * a)
        g = math.sqrt(gx * gx + gy * gy)
        front_pt.append((x, y))
        front_n.append((gx / g, gy / g))

    def xy(i2: int, s: float) -> tuple[float, float]:
        (x, y), (nx, ny) = front_pt[i2], front_n[i2]
        return (x + s * nx, y + s * ny)

    # Tube cross-section: rectangle s in [-d, d], z in [0, d] (doubled).
    z_low = [d * (k / (2 * ns)) for k in range(2 * ns + 1)]
    s_top = [d - 2.0 * d * (k / (4 * ns)) for k in range(4 * ns + 1)]
    rect = [(d, z) for z in z_low]
    rect += [(s, d) for s in s_top[1:]]
    rect += [(-d, z) for z in reversed(z_low[:-1])]
    rings = _geometric(par["first_ring_mm"], d, _RING_RATIO)
    n_ring = len(rings) - 1
    ring2 = _doubled(rings)
    ring2[1] = 0.25 * rings[1]  # quarter-point mid-side on the collapsed ring

    def tube_section(m2: int, k2: int) -> tuple[float, float]:
        if k2 == 2 * n_ring:
            return rect[m2]
        if k2 == 0:
            return (0.0, 0.0)
        rs, rz = rect[m2]
        length = math.sqrt(rs * rs + rz * rz)
        rho = ring2[k2]
        beta = (rho / d) * (rho / d)
        scale = rho * ((1.0 - beta) + beta * length / d) / length
        return (rs * scale, rz * scale)

    quarter = CrackMesh()

    def tube_point(p2: int, q2: int, w2: int):
        s, z = tube_section(p2, 2 * n_ring - q2)
        x, y = xy(w2, s)
        return (x, y, z)

    def tube_tip(p2: int, q2: int, w2: int):
        if 2 * n_ring - q2 != 0:
            return None
        return "corner" if w2 % 2 == 0 else "mid"

    quarter.add_block(tube_point, n_psi, n_ring, nq, tip=tube_tip)

    # z levels: tube-side spacing up to d, then graded to H.
    h1 = d / ns
    z_up = _doubled([d + p for p in _geometric(h1, h - d, _GROWTH_RATIO)])
    z_up[0] = d
    z_up[-1] = h
    z_full = z_low + z_up[1:]

    def extrude(grid: list[list[tuple]], zs: list[float]) -> None:
        n_p, n_q, n_w = (len(grid) - 1) // 2, (len(grid[0]) - 1) // 2, (len(zs) - 1) // 2

        def pt(p2: int, q2: int, w2: int):
            x, y = grid[p2][q2]
            return (x, y, zs[w2])

        quarter.add_block(pt, n_p, n_q, n_w)

    # Band above the tube: front-fitted normal offsets, z in [d, H].
    band = [[xy(i2, s) for s in s_top] for i2 in range(2 * nq + 1)]
    extrude(band, z_up)

    # Core under the crack face: three blocks meeting at G.
    ein = [xy(i2, -d) for i2 in range(2 * nq + 1)]
    n_half = nq // 2
    pa, pb, pm = ein[0], ein[-1], ein[nq]
    po = (0.0, 0.0)
    pd = (pa[0] * 0.5, 0.0)
    pe = (0.0, pb[1] * 0.5)
    pg = (pm[0] * 0.5, pm[1] * 0.5)
    l_dg, l_eg, l_gm = _line(pd, pg, n_half), _line(pe, pg, n_half), _line(pg, pm, nk)
    q1 = _tfi(_line(po, pd, n_half), l_eg, _line(po, pe, n_half), l_dg)
    q2 = _tfi(_line(pd, pa, nk), l_gm, l_dg, ein[: nq + 1])
    q3 = _tfi(l_eg, list(reversed(ein[nq:])), _line(pe, pb, nk), l_gm)
    for grid in (q1, q2, q3):
        extrude(grid, z_full)

    # Outer block O1: from the band's outer edge to the box x <= Xb, y <= t.
    eout = [xy(i2, d) for i2 in range(2 * nq + 1)]
    xb = c + t - a
    frac = _doubled([p / (xb - eout[0][0]) for p in _geometric(h1, xb - eout[0][0], _GROWTH_RATIO)])
    s2 = _graded_line(eout[0], (xb, 0.0), frac)
    s4 = _graded_line(eout[-1], (0.0, t), frac)
    corners = []
    for i2 in range(0, 2 * nq + 1, 2):
        px, py = eout[i2]
        if py * xb <= px * t:
            corners.append((xb, xb * py / px))
        else:
            corners.append((t * px / py, t))
    denom = math.sqrt(xb * xb + t * t)
    ic = min(
        range(1, nq),
        key=lambda i: abs(eout[2 * i][1] * xb - eout[2 * i][0] * t)
        / (math.sqrt(eout[2 * i][0] * eout[2 * i][0] + eout[2 * i][1] * eout[2 * i][1]) * denom),
    )
    for i in range(nq + 1):
        px, py = eout[2 * i]
        if i < ic:
            corners[i] = (xb, xb * py / px)
        elif i == ic:
            corners[i] = (xb, t)
        else:
            corners[i] = (t * px / py, t)
    corners[0], corners[-1] = (xb, 0.0), (0.0, t)
    gamma = [corners[0]]
    for p0, p1 in pairwise(corners):
        gamma.append((0.5 * (p0[0] + p1[0]), 0.5 * (p0[1] + p1[1])))
        gamma.append(p1)
    o1 = _tfi(eout, gamma, s2, s4)
    extrude(o1, z_full)

    # Outer block O2: rectangle x in [Xb, b], y on the box side of O1.
    h_last = (xb - eout[0][0]) * (frac[-1] - frac[-3])
    xs = _doubled([xb + p for p in _geometric(h_last, b - xb, _GROWTH_RATIO)])
    xs[0], xs[-1] = xb, b
    ys = [gamma[k][1] for k in range(2 * ic + 1)]
    o2 = [[(x, y) for y in ys] for x in xs]
    extrude(o2, z_full)

    return quarter.mirrored().renumbered(a, c, t)


# --------------------------------------------------------------------------- #
# Deck
# --------------------------------------------------------------------------- #
def cint_table_name(level: int) -> str:
    return f"crack_cint_L{level}"


def reaction_file_name(level: int) -> str:
    return f"crack_reac_L{level}"


def generate_crack_verification_apdl(spec: CrackPlateSpec) -> str:
    """Return the complete, deterministic MAPDL ``.inp`` for ``spec``."""
    mesh = build_mesh(spec)
    a, c, t = spec.crack_depth_mm, spec.crack_half_length_mm, spec.thickness_mm
    b, h = spec.half_width_mm, spec.half_height_mm
    par = mesh_parameters(spec)
    lv = spec.mesh_level
    area = 2.0 * b * t
    cint_name, reac_name = cint_table_name(lv), reaction_file_name(lv)
    out: list[str] = []
    w = out.append
    w("! Semi-elliptical surface crack verification deck (#2157 P0a)")
    w("! generated by digitalmodel.ansys.crack_verification")
    w("! Units: length = mm, force = N, stress = MPa")
    w("! raw CINT K = MPa*sqrt(mm), J = N/mm (converted once by cint_parser)")
    w(f"! a = {a!r} mm, c = {c!r} mm, t = {t!r} mm, sigma = {spec.stress_mpa!r} MPa")
    w(f"! half model z >= 0 (crack-plane symmetry); x in [-{b!r}, {b!r}], "
      f"y in [0, {t!r}], z in [0, {h!r}]")
    w(f"! mesh_level = {lv}; front divisions per quarter = "
      f"{par['front_divisions_quarter']}; first ring = {par['first_ring_mm']!r} mm")
    w(f"! nodes = {len(mesh.nodes)}, elements = {len(mesh.elements)} (SOLID186)")
    w("FINISH")
    w("/CLEAR,NOSTART")
    w(f"/TITLE,Surface crack verification a={a!r} c={c!r} t={t!r} level {lv}")
    w("/UNITS,MPA")
    w("/PREP7")
    w("ET,1,SOLID186")
    w("KEYOPT,1,2,1")
    w(f"MP,EX,1,{spec.youngs_modulus_mpa!r}")
    w(f"MP,PRXY,1,{spec.poisson!r}")
    w(f"NFRONT = {mesh.n_front}")
    w(f"NFRALL = {mesh.n_front_all}")
    w(f"NLIG = {mesh.n_ligament}")
    w(f"NFIXY = {mesh.uy_node}")
    for nid, (kx, ky, kz) in enumerate(mesh.keys, start=1):
        w(f"N,{nid},{kx},{ky},{kz}")
    w("TYPE,1")
    w("MAT,1")
    for el in mesh.elements:
        w("E," + ",".join(str(n) for n in el[:8]))
        w("EMORE," + ",".join(str(n) for n in el[8:16]))
        w("EMORE," + ",".join(str(n) for n in el[16:]))
    # CINT needs every crack-front node, mid-sides included, in the component.
    w("NSEL,S,NODE,,1,NFRALL")
    w("CM,CRKTIP,NODE")
    w("ALLSEL,ALL")
    w("FINISH")
    w("/SOLU")
    w("ANTYPE,STATIC")
    w("NSEL,S,NODE,,1,NLIG")
    w("D,ALL,UZ,0.0")
    w("NSEL,S,LOC,X,-1.0E-6,1.0E-6")
    w("D,ALL,UX,0.0")
    w("ALLSEL,ALL")
    w("D,NFIXY,UY,0.0")
    w(f"NSEL,S,LOC,Z,{h - 1.0e-6!r},{h + 1.0e-6!r}")
    w(f"SF,ALL,PRES,{-spec.stress_mpa!r}")
    w("ALLSEL,ALL")
    for cid, kind in ((1, "SIFS"), (2, "JINT")):
        w(f"CINT,NEW,{cid}")
        w(f"CINT,TYPE,{kind}")
        w("CINT,CTNC,CRKTIP")
        w("CINT,NORM,0,3")
        w("CINT,SYMM,ON")
        w(f"CINT,NCON,{N_CONTOURS}")
    w("OUTRES,ALL,ALL")
    w("SOLVE")
    w("FINISH")
    w("/POST1")
    w("SET,LAST")
    w("*GET,MREV,ACTIVE,0,REV")
    # crack-front table
    w(f"*CFOPEN,{cint_name},txt")
    w("*VWRITE")
    w("('# digitalmodel crack_verification CINT table')")
    w("*VWRITE")
    w("('# units: length=mm force=N stress=MPa K=MPa*sqrt(mm) J=N/mm')")
    w(f"LVL = {lv}")
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
    w("*GET,VK1,CINT,1,CTIP,NN,,IC,DTYPE,K1")
    w("*GET,VK2,CINT,1,CTIP,NN,,IC,DTYPE,K2")
    w("*GET,VK3,CINT,1,CTIP,NN,,IC,DTYPE,K3")
    w("*GET,VJ,CINT,2,CTIP,NN,,IC,DTYPE,JINT")
    w("VX = NX(NN)")
    w("VY = NY(NN)")
    w("VZ = NZ(NN)")
    w("*VWRITE,NN,IC,VX,VY,VZ,VK1,VK2,VK3,VJ")
    w("(F8.0,1X,F4.0,7(1X,E18.10))")
    w("*ENDDO")
    w("*ENDDO")
    w("*CFCLOS")
    # solved reaction sums
    w("FZSUM = 0.0")
    w("*DO,NN,1,NLIG")
    w("*GET,RV,NODE,NN,RF,FZ")
    w("FZSUM = FZSUM + RV")
    w("*ENDDO")
    w("NSEL,S,LOC,X,-1.0E-6,1.0E-6")
    w("*GET,NSX,NODE,0,COUNT")
    w("FXSUM = 0.0")
    w("NN = 0")
    w("*DO,II,1,NSX")
    w("NN = NDNEXT(NN)")
    w("*GET,RV,NODE,NN,RF,FX")
    w("FXSUM = FXSUM + RV")
    w("*ENDDO")
    w("ALLSEL,ALL")
    w("*GET,FYPT,NODE,NFIXY,RF,FY")
    w(f"SIGAPP = {spec.stress_mpa!r}")
    w(f"AREAAPP = {area!r}")
    w(f"*CFOPEN,{reac_name},txt")
    w("*VWRITE")
    w("('# digitalmodel crack_verification reactions')")
    w("*VWRITE")
    w("('# units: length=mm force=N stress=MPa')")
    w("*VWRITE,LVL")
    w("('mesh_level ',F6.0)")
    w("*VWRITE,MREV")
    w("('mapdl_rev ',F8.2)")
    w("*VWRITE,SIGAPP")
    w("('stress_mpa ',E20.12)")
    w("*VWRITE,AREAAPP")
    w("('loaded_area_mm2 ',E20.12)")
    w("*VWRITE,FZSUM")
    w("('fz_ligament ',E20.12)")
    w("*VWRITE,FXSUM")
    w("('fx_symmetry ',E20.12)")
    w("*VWRITE,FYPT")
    w("('fy_point ',E20.12)")
    w("*VWRITE,NLIG")
    w("('n_ligament ',F10.0)")
    w("*CFCLOS")
    w("FINISH")
    return "\n".join(out) + "\n"


def deck_sha256(text: str) -> str:
    """SHA-256 of the deck text (UTF-8, LF line endings as generated)."""
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def write_crack_verification_inp(spec: CrackPlateSpec, path: Path | str) -> Path:
    """Write the deck with LF line endings (byte-identical to the hash input)."""
    out = Path(path)
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_bytes(generate_crack_verification_apdl(spec).encode("utf-8"))
    return out


# --------------------------------------------------------------------------- #
# Licensed solve -> receipt
# --------------------------------------------------------------------------- #
RECEIPT_STATE = "p0a_verification"
MESHING_APPROACH = (
    "Structured 20-node hexahedral mesh (SOLID186) generated in Python and "
    "written as N/E commands: a crack-front tube with a spider-web of rings in "
    "each plane normal to the front, the innermost ring collapsed onto the tip "
    "with quarter-point mid-side nodes; a front-fitted band, a three-block core "
    "under the crack face and two transfinite outer blocks, extruded in z. "
    "Half model with crack-plane symmetry (CINT,SYMM,ON); x spans [-b, b] so "
    "the deepest point is an interior front node."
)


GENERATOR_FILES = (
    "src/digitalmodel/ansys/crack_verification.py",
    "src/digitalmodel/ansys/cint_parser.py",
)


def _producing_commit(repo_dir: Path) -> tuple[str, bool]:
    def git(*args: str) -> str:
        return subprocess.run(  # nosec B603 B607 - fixed argv, no shell
            ["git", *args],
            cwd=str(repo_dir),
            capture_output=True,
            text=True,
            check=True,
        ).stdout.strip()

    sha = git("rev-parse", "HEAD")
    dirty = git("status", "--porcelain", "--", *GENERATOR_FILES)
    return sha, dirty == ""


def save_artifact(text: str, base_dir: Path, rel_path: str) -> dict:
    """Write host-free solver output under ``base_dir`` (LF) and describe it."""
    from digitalmodel.ansys.crack_receipt import text_sha256

    norm = text.replace("\r\n", "\n")
    out = Path(base_dir) / rel_path
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_bytes(norm.encode("utf-8"))
    return {"path": rel_path, "sha256": text_sha256(norm)}


def run_verification(
    workdir: Path | str,
    receipt_path: Path | str,
    *,
    spec: CrackPlateSpec | None = None,
    levels: tuple[int, int] = (0, 1),
    cores: int = 4,
    timeout_seconds: int = 7200,
) -> dict:
    """Generate, solve (fail-closed runner) and parse both mesh densities, then
    write the host-free verification receipt. Raises if a solve fails."""
    from digitalmodel.ansys import cint_parser
    from digitalmodel.ansys.runner import ANSYSRunConfig, ANSYSRunner, ANSYSRunStatus
    from digitalmodel.asset_integrity.assessment.crack_fad import newman_raju_k

    from digitalmodel.ansys.crack_receipt import generator_blobs

    base = spec or CrackPlateSpec()
    workdir = Path(workdir)
    receipt_out = Path(receipt_path)
    geometry = {
        "type": "ellipse",
        "a": base.crack_depth_mm,
        "c": base.crack_half_length_mm,
    }
    # shared-memory: a distributed (MPI) run was seen to hang after a FATAL
    extra = ["-smp", "-np", str(cores)]
    cint_texts: dict[int, str] = {}
    reac_texts: dict[int, str] = {}
    meshes = []
    for lv in levels:
        s = CrackPlateSpec(**{**asdict(base), "mesh_level": lv})
        run_dir = workdir / f"L{lv}"
        deck = write_crack_verification_inp(s, run_dir / f"crack_verification_L{lv}.inp")
        text = deck.read_bytes().decode("utf-8")
        runner = ANSYSRunner(
            ANSYSRunConfig(
                output_dir=run_dir, timeout_seconds=timeout_seconds, extra_args=extra
            )
        )
        exe = runner._detect_executable()
        result = runner.run(deck)
        if result.status != ANSYSRunStatus.COMPLETED:
            raise RuntimeError(
                f"MAPDL solve for level {lv} did not complete: {result.status.value}; "
                f"{result.error_message}"
            )
        argv = [
            exe.name if exe else "mapdl",
            "-b",
            "-i",
            deck.name,
            "-o",
            f"{deck.stem}.out",
            *extra,
        ]
        cint_name = f"{cint_table_name(lv)}.txt"
        reac_name = f"{reaction_file_name(lv)}.txt"
        cint_texts[lv] = (run_dir / cint_name).read_bytes().decode("utf-8")
        reac_texts[lv] = (run_dir / reac_name).read_bytes().decode("utf-8")
        solved = f"solved/{RECEIPT_STATE}"
        artifacts = {
            "cint": save_artifact(cint_texts[lv], receipt_out.parent, f"{solved}/{cint_name}"),
            "reac": save_artifact(reac_texts[lv], receipt_out.parent, f"{solved}/{reac_name}"),
        }
        mesh = build_mesh(s)
        record = cint_parser.build_mesh_record(
            level=lv,
            cint_text=cint_texts[lv],
            reac_text=reac_texts[lv],
            front_geometry=geometry,
        )
        rev_lv = cint_parser.parse_reaction_file(reac_texts[lv]).mapdl_rev or ""
        record = {
            "level": lv,
            "deck_sha256": deck_sha256(text),
            "run": {
                "argv": argv,
                "mapdl_version": rev_lv,
                "solve_seconds": round(result.duration_seconds, 1),
            },
            "artifacts": artifacts,
            "n_nodes": len(mesh.nodes),
            "n_elements": len(mesh.elements),
            "mesh_parameters": mesh_parameters(s),
            **{k: v for k, v in record.items() if k != "level"},
        }
        meshes.append(record)

    guards = cint_parser.evaluate_guards(cint_texts, reac_texts, front_geometry=geometry)
    primary_level = max(levels)
    primary = next(m for m in meshes if m["level"] == primary_level)
    rev = cint_parser.parse_reaction_file(reac_texts[primary_level]).mapdl_rev or ""

    def comparator(phi: float) -> dict:
        nr = newman_raju_k(
            base.crack_depth_mm,
            base.crack_half_length_mm,
            base.thickness_mm,
            base.stress_mpa,
            phi_deg=min(phi, 180.0 - phi),
        )
        node = next(
            (n for n in primary["front"] if abs(n["phi_deg"] - phi) < 1e-6), None
        )
        fe = node["K1_reported"] if node else float("nan")
        err = 100.0 * (fe / nr - 1.0)
        return {
            "phi_deg": phi,
            "newman_raju_mpa_sqrt_m": nr,
            "fe_mpa_sqrt_m": fe,
            "error_pct": err,
            "band_pct": 5.0,
            "within_band": abs(err) <= 5.0,
        }

    repo_dir = Path(__file__).resolve().parents[3]
    commit, clean = _producing_commit(repo_dir)
    receipt = {
        "schema": cint_parser.RECEIPT_SCHEMA_ID,
        "state": RECEIPT_STATE,
        "kind": "verification",
        "issue": 2157,
        "spec": {k: v for k, v in asdict(base).items() if k != "mesh_level"},
        "front_geometry": geometry,
        "units": {
            "length": "mm",
            "force": "N",
            "stress": "MPa",
            "k": cint_parser.K_UNIT,
            "k_raw": cint_parser.RAW_K_UNIT,
            "k_conversion_factor": cint_parser.K_RAW_TO_SI,
            "j": cint_parser.J_UNIT,
        },
        "meshing": {
            "approach": MESHING_APPROACH,
            "element": "SOLID186 (20-node, full integration)",
            "k_reported": "mean K_I over contours 4-6",
            "contour_guard_quantities": ["K1", "J"],
        },
        "run": {
            "producing_commit": commit,
            "generator_tree_clean": clean,
            "generator_files": generator_blobs(repo_dir, commit, list(GENERATOR_FILES)),
            "mapdl_version": rev,
            "mapdl_release": _release_name(rev),
            "cores": cores,
            "platform": platform.system().lower(),
            "solver_wrapper": "digitalmodel.ansys fail-closed MAPDL subprocess (#940)",
        },
        "meshes": meshes,
        "primary_level": primary_level,
        "comparator": {
            "source": "Newman-Raju (1981) NASA TM-83200 via crack_fad.newman_raju_k",
            "deepest": comparator(90.0),
            "surface": comparator(0.0),
            "surface_mirror": comparator(180.0),
        },
        "guards": {name: g.to_dict() for name, g in guards.items()},
    }
    receipt_out.parent.mkdir(parents=True, exist_ok=True)
    receipt_out.write_bytes((json.dumps(receipt, indent=1) + "\n").encode("utf-8"))
    return receipt


def _release_name(rev: str) -> str:
    try:
        major, minor = rev.split(".")
        return f"20{int(major)} R{int(minor)}"
    except ValueError:
        return ""


def _main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--write-deck", type=Path, help="write the deck for --level")
    ap.add_argument("--level", type=int, default=0)
    ap.add_argument("--solve", action="store_true", help="licensed solve -> receipt")
    ap.add_argument("--workdir", type=Path, default=Path("crack_verification_run"))
    ap.add_argument("--receipt", type=Path)
    ap.add_argument("--cores", type=int, default=4)
    ns = ap.parse_args(argv)
    if ns.write_deck:
        write_crack_verification_inp(CrackPlateSpec(mesh_level=ns.level), ns.write_deck)
    if ns.solve:
        if ns.receipt is None:
            ap.error("--solve needs --receipt")
        t0 = time.monotonic()
        receipt = run_verification(ns.workdir, ns.receipt, cores=ns.cores)
        for name, g in receipt["guards"].items():
            print(f"{name}: {g['status']} ({g['value']})")
        for key in ("deepest", "surface", "surface_mirror"):
            comp = receipt["comparator"][key]
            print(f"{key}: FE {comp['fe_mpa_sqrt_m']:.4f} vs NR "
                  f"{comp['newman_raju_mpa_sqrt_m']:.4f} ({comp['error_pct']:+.2f} %)")
        print(f"elapsed {time.monotonic() - t0:.1f} s")
    return 0


if __name__ == "__main__":
    raise SystemExit(_main())
