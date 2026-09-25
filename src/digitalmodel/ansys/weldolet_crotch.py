"""Crotch-plane root flaw of the weldolet attachment for MAPDL CINT (#2157 P0b).

Owner card G15 adds a second base case to the full-circumference fusion-face
flaw of ``weldolet_crack``: a root flaw in the radial-axial plane at the crotch,
normal to the run-pipe hoop stress. Geometry, material and loads are those of
``weldolet_crack`` (the assumed design basis in the design-data register).

Flaw (register F-06 to F-09, fixed before any crotch solve)
    Plane y = 0 at the +x crotch (theta = 0), which contains the run-pipe axis
    and the branch axis and is normal to the run-pipe hoop stress there. The
    flaw is a semicircle (a/c = 1) on the bore surface (the run-pipe hole wall
    and the weld root face, rho = C/2, a straight line from the run-pipe bore
    v = -t to the root-face top v = g in the section). Its upper tip is held
    0.5 mm above the fusion line (v = 0), so the flaw contains the weld root and
    the adjacent hole wall; it grows self-similarly, downward along the bore and
    radially outward (depth a, measured along rho from the bore) into the
    attachment weld and the run-pipe wall. Limit state: the lower tip reaches
    the run-pipe bore (remaining ligament 7.61 - 2a mm; exhaustion at
    a = 3.805 mm).

Model and mesh (the P0a crack block, re-used)
    Half model: the geometry and loads are symmetric about y = 0, so only
    y >= 0 is modelled, with UY = 0 on the symmetry plane except on the crack
    faces, and CINT,SYMM,ON, CINT,CTNC with CINT,NORM,0,2 (the crack plane is
    flat). In the (rho, v) section the P0a construction is built around the
    flaw inside a box rho in [C/2, C/2 + D], v in [-t, g]: a front-fitted band
    of half-width d, a three-block core under the crack face, and a transfinite
    outer block to the box, once for each half of the semicircle (the P0a
    quarter and its mirror). The remaining section (run-pipe wall to the right
    of the box, weld/weldolet/branch above it) follows ``weldolet_crack``. The
    section is swept through theta in [0, pi] on graded levels; the P0a crack
    tube (a spider-web of rings, innermost ring collapsed onto the tip with
    quarter-point mid-side nodes) replaces the band for theta <= d/rho_c.
    The run pipe beyond rho_P is an O-block to a half square in the unrolled
    (x, s) surface and a tensor mesh. Front end nodes lie on the free bore
    surface. Only +, -, *, /, sqrt and fixed series are used (deterministic
    deck text).
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass, field
from itertools import pairwise
from pathlib import Path

from digitalmodel.ansys import weldolet_crack as wc
from digitalmodel.ansys.crack_verification import (
    _doubled,
    _fmt,
    _geometric,
    _graded_line,
    _line,
    _tfi,
)

CROTCH_TOP_TIP_MM = wc.CROTCH_TOP_TIP_MM  # register F-07
CROTCH_ASPECT_A_OVER_C = 1.0  # register F-06 (semicircle)
CROTCH_DEPTH_STATES_MM = (2.35, 2.8, 3.2)  # register F-08
_BORE_MARGIN_MM = 0.3  # clearance between the tube and the bore-line ends

GENERATOR_FILES = ("src/digitalmodel/ansys/weldolet_crotch.py", *wc.GENERATOR_FILES)

_BASE_FRONT_DIVISIONS_QUARTER = 12
_BASE_FIRST_RING_MM = 0.01 * wc.A0_MM
_RING_RATIO = 1.35
_TUBE_SIDE_DIVISIONS = 3
_CORE_RADIAL_DIVISIONS = 4
_GROWTH = 1.3
_HALF_BAND_DIVISIONS = 14


def _bore_geometry(base: wc.WeldoletSpec) -> tuple[float, float]:
    return base.run_wall_mm, base.root_gap_mm


LIGAMENT_EXHAUSTION_DEPTH_MM = (
    CROTCH_TOP_TIP_MM + wc.WeldoletSpec().run_wall_mm
) / (1.0 + 1.0 / CROTCH_ASPECT_A_OVER_C)


@dataclass(frozen=True)
class CrotchSpec:
    """Crotch-plane flaw on the assumed design basis of ``weldolet_crack``."""

    crack_depth_mm: float = wc.A0_MM
    crack_face_pressure: bool = True  # register L-03 base case
    mesh_level: int = 0
    tube_radius_mm: float = 0.6  # register N-04
    box_depth_mm: float = 6.0  # register N-04
    base: wc.WeldoletSpec = field(
        default_factory=lambda: wc.WeldoletSpec(crack_depth_mm=None)
    )

    def validate(self) -> list[str]:
        issues = [i for i in self.base.validate() if "crack depth" not in i]
        if self.mesh_level not in (0, 1, 2):
            issues.append("mesh_level must be 0, 1 or 2")
        geo = flaw_geometry(self)
        t, g = _bore_geometry(self.base)
        d, a = self.tube_radius_mm, self.crack_depth_mm
        clear = d + _BORE_MARGIN_MM
        if geo["top_tip_v_mm"] + clear > g or geo["bottom_tip_v_mm"] - clear < -t:
            issues.append("flaw plus crack-block tube does not fit on the bore line")
        if a < 1.0 or self.box_depth_mm - a < d + 0.5:
            issues.append("crack depth outside the crack-block tube and box limits")
        wg = wc.derived_geometry(self.base)
        rho_out_g = wg["toe_radius_mm"] - (wg["toe_radius_mm"] - wg["base_radius_mm"]) * (
            g / self.base.fillet_leg_mm
        )
        if wg["hole_radius_mm"] + self.box_depth_mm > rho_out_g - 0.3:
            issues.append("crack box reaches the fillet face")
        return issues


def flaw_geometry(spec: CrotchSpec) -> dict:
    a = spec.crack_depth_mm
    c = a / CROTCH_ASPECT_A_OVER_C
    v0 = CROTCH_TOP_TIP_MM - c
    return {
        "a_mm": a,
        "c_mm": c,
        "aspect_a_over_c": CROTCH_ASPECT_A_OVER_C,
        "top_tip_v_mm": v0 + c,
        "centre_v_mm": v0,
        "bottom_tip_v_mm": v0 - c,
    }


def remaining_ligament_mm(spec: CrotchSpec) -> float:
    """Lower flaw tip to the run-pipe bore along the bore surface."""
    return flaw_geometry(spec)["bottom_tip_v_mm"] + spec.base.run_wall_mm


def front_geometry(spec: CrotchSpec) -> dict:
    geo = flaw_geometry(spec)
    wg = wc.derived_geometry(spec.base)
    return {"type": "crotch", "rw": wg["hole_radius_mm"], "ro": wg["run_outer_radius_mm"],
            "v0": geo["centre_v_mm"], "a": geo["a_mm"], "c": geo["c_mm"]}


def mesh_parameters(spec: CrotchSpec, level: int | None = None) -> dict:
    lv = spec.mesh_level if level is None else level
    scale = 2**lv
    return {
        "level": lv,
        "front_divisions_quarter": _BASE_FRONT_DIVISIONS_QUARTER * scale,
        "first_ring_mm": _BASE_FIRST_RING_MM / scale,
        "ring_ratio_target": _RING_RATIO,
        "tube_side_divisions": _TUBE_SIDE_DIVISIONS,
        "core_radial_divisions": _CORE_RADIAL_DIVISIONS,
        "tube_radius_mm": spec.tube_radius_mm,
        "box_depth_mm": spec.box_depth_mm,
    }


def _fixed_count(h1: float, length: float, n: int) -> list[float]:
    """n intervals over [0, length], first about h1, geometric (or uniform)."""
    if n * h1 >= length:
        return [length * k / n for k in range(n)] + [length]

    def span(q: float) -> float:
        s, t = 0.0, h1
        for _ in range(n):
            s += t
            t *= q
        return s

    lo, hi = 1.0, 4.0
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


# --------------------------------------------------------------------------- #
# The P0a quarter construction in quarter coordinates (X along the bore from
# the flaw centre, Y = depth from the bore), for one half of the semicircle
# --------------------------------------------------------------------------- #
class _Quarter:
    def __init__(self, a: float, c: float, box_x: float, box_y: float, d: float,
                 par: dict, cracked: bool) -> None:
        nq = par["front_divisions_quarter"]
        ns = par["tube_side_divisions"]
        nk = par["core_radial_divisions"]
        self.nq, self.ns = nq, ns
        u2 = [k / (2 * nq) for k in range(2 * nq + 1)]
        self.front_pt, self.front_n = [], []
        for u in u2:
            den = 1.0 + u * u
            x, y = c * (1.0 - u * u) / den, a * 2.0 * u / den
            gx, gy = x / (c * c), y / (a * a)
            g = math.sqrt(gx * gx + gy * gy)
            self.front_pt.append((x, y))
            self.front_n.append((gx / g, gy / g))
        self.d = d
        self.z_low = [d * (k / (2 * ns)) for k in range(2 * ns + 1)]
        self.z_low[-1] = d
        self.s_top = [d - 2.0 * d * (k / (4 * ns)) for k in range(4 * ns + 1)]
        self.s_top[-1] = -d
        rect = [(d, z) for z in self.z_low]
        rect += [(s, d) for s in self.s_top[1:]]
        rect += [(-d, z) for z in reversed(self.z_low[:-1])]
        self.rect = rect
        rings = _geometric(par["first_ring_mm"], d, _RING_RATIO)
        self.n_ring = len(rings) - 1
        self.ring2 = _doubled(rings)
        self.ring2[1] = (0.25 if cracked else 0.5) * rings[1]
        self.n_psi = 4 * ns

        self.grids: list[tuple[list[list[tuple]], str]] = []
        self.band = [[self.xy(i2, s) for s in self.s_top] for i2 in range(2 * nq + 1)]
        self.grids.append((self.band, "band"))
        # core under the crack face (P0a three-block layout)
        ein = [self.xy(i2, -d) for i2 in range(2 * nq + 1)]
        n_half = nq // 2
        pa, pb, pm = ein[0], ein[-1], ein[nq]
        po, pd, pe = (0.0, 0.0), (pa[0] * 0.5, 0.0), (0.0, pb[1] * 0.5)
        pg = (pm[0] * 0.5, pm[1] * 0.5)
        l_dg, l_eg, l_gm = _line(pd, pg, n_half), _line(pe, pg, n_half), _line(pg, pm, nk)
        self.grids.append((_tfi(_line(po, pd, n_half), l_eg, _line(po, pe, n_half), l_dg),
                           "full"))
        self.grids.append((_tfi(_line(pd, pa, nk), l_gm, l_dg, ein[: nq + 1]), "full"))
        self.grids.append((_tfi(l_eg, list(reversed(ein[nq:])), _line(pe, pb, nk), l_gm),
                           "full"))
        # outer block O1: band outer edge to the box X = box_x, Y = box_y
        eout = [self.xy(i2, d) for i2 in range(2 * nq + 1)]
        h1 = d / ns
        g4 = _geometric(h1, box_y - eout[-1][1], _GROWTH)
        n_o = len(g4) - 1
        frac4 = _doubled([p / g4[-1] for p in g4])
        g2 = _fixed_count(h1, box_x - eout[0][0], n_o)
        frac2 = _doubled([p / g2[-1] for p in g2])
        s2 = _graded_line(eout[0], (box_x, 0.0), frac2)
        s4 = _graded_line(eout[-1], (0.0, box_y), frac4)
        corners = []
        denom = math.sqrt(box_x * box_x + box_y * box_y)
        ic = min(
            range(1, nq),
            key=lambda i: abs(eout[2 * i][1] * box_x - eout[2 * i][0] * box_y)
            / (math.sqrt(eout[2 * i][0] ** 2 + eout[2 * i][1] ** 2) * denom),
        )
        for i in range(nq + 1):
            px, py = eout[2 * i]
            if i < ic:
                corners.append((box_x, box_x * py / px))
            elif i == ic:
                corners.append((box_x, box_y))
            else:
                corners.append((box_y * px / py, box_y))
        corners[0], corners[-1] = (box_x, 0.0), (0.0, box_y)
        gamma = [corners[0]]
        for p0, p1 in pairwise(corners):
            gamma.append((0.5 * (p0[0] + p1[0]), 0.5 * (p0[1] + p1[1])))
            gamma.append(p1)
        self.grids.append((_tfi(eout, gamma, s2, s4), "full"))
        self.gamma, self.ic = gamma, ic

    def xy(self, i2: int, s: float) -> tuple[float, float]:
        (x, y), (nx, ny) = self.front_pt[i2], self.front_n[i2]
        return (x + s * nx, y + s * ny)

    def tube_section(self, m2: int, k2: int) -> tuple[float, float]:
        if k2 == 2 * self.n_ring:
            return self.rect[m2]
        if k2 == 0:
            return (0.0, 0.0)
        rs, rz = self.rect[m2]
        length = math.sqrt(rs * rs + rz * rz)
        rho = self.ring2[k2]
        beta = (rho / self.d) * (rho / self.d)
        scale = rho * ((1.0 - beta) + beta * length / self.d) / length
        return (rs * scale, rz * scale)

    def far_edge(self) -> list[tuple]:  # X = box_x, from Y = 0 to Y = box_y
        return self.gamma[: 2 * self.ic + 1]

    def deep_edge(self) -> list[tuple]:  # Y = box_y, from X = box_x to X = 0
        return self.gamma[2 * self.ic:]


# --------------------------------------------------------------------------- #
# Mesh
# --------------------------------------------------------------------------- #
def _theta_levels(rho_c: float, q: _Quarter) -> list[float]:
    """Doubled theta levels on [0, pi]: tube side levels, geometric to pi/4,
    uniform to 3 pi/4 and to pi (pi/4 and 3 pi/4 are corner levels)."""
    pi = wc._PI
    t_low = [z / rho_c for z in q.z_low]  # doubled already
    th_d = t_low[-1]
    h = (q.d / q.ns) / rho_c
    seg1 = [th_d + p for p in _geometric(h, 0.25 * pi - th_d, _GROWTH)]
    seg1[-1] = 0.25 * pi
    step = seg1[-1] - seg1[-2]
    n2 = max(2, math.ceil(0.5 * pi / step))
    seg2 = [0.25 * pi + 0.5 * pi * k / n2 for k in range(1, n2)] + [0.75 * pi]
    n3 = max(2, math.ceil(0.25 * pi / step))
    seg3 = [0.75 * pi + 0.25 * pi * k / n3 for k in range(1, n3)] + [pi]
    return t_low + _doubled(seg1)[1:] + _doubled([0.25 * pi, *seg2])[1:] + _doubled(
        [0.75 * pi, *seg3]
    )[1:]


def build_mesh(spec: CrotchSpec) -> wc.WeldoletMesh:
    issues = spec.validate()
    if issues:
        raise ValueError("invalid crotch spec: " + "; ".join(issues))
    base = spec.base
    wg = wc.derived_geometry(base)
    par = mesh_parameters(spec)
    geo = flaw_geometry(spec)
    a, c, v0 = geo["a_mm"], geo["c_mm"], geo["centre_v_mm"]
    t, g = _bore_geometry(base)
    rw, ro = wg["hole_radius_mm"], wg["run_outer_radius_mm"]
    toe, rb = wg["toe_radius_mm"], wg["base_radius_mm"]
    rbi, rbo = wg["branch_inner_radius_mm"], wg["branch_outer_radius_mm"]
    leg, vb, vc, va = base.fillet_leg_mm, wg["groove_top_mm"], wg["inner_taper_end_mm"], \
        base.weldolet_a_mm
    top = va + base.branch_length_mm
    d, box_d = spec.tube_radius_mm, spec.box_depth_mm
    rho_k = rw + box_d

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

    sec = wc._Section(0.0, cracked=False)
    up = _Quarter(a, c, g - v0, box_d, d, par, True)
    lo = _Quarter(a, c, v0 + t, box_d, d, par, True)

    def m_up(p):
        return (rw + p[1], v0 + p[0])

    def m_lo(p):
        return (rw + p[1], v0 - p[0])

    band_quads: list[range] = []
    for q, m in ((up, m_up), (lo, m_lo)):
        for grid, kind in q.grids:
            k0 = len(sec.quads)
            sec.block([[m(p) for p in col] for col in grid], "U")
            if kind == "band":
                band_quads.append(range(k0, len(sec.quads)))

    # --- region R1: right of the box, v in [-t, g] ---------------------------
    left = [m_lo(p) for p in lo.deep_edge()] + [m_up(p) for p in reversed(up.deep_edge())][1:]
    n_left = len(left) - 1
    k0 = min(range(0, n_left + 1, 2), key=lambda k: abs(left[k][1]))
    v_left = [p[1] for p in left]
    right = []
    for k in range(n_left + 1):
        if k <= k0:
            f = (v_left[k] - v_left[0]) / (v_left[k0] - v_left[0])
            right.append((toe, -t + t * f))
        else:
            f = (v_left[k] - v_left[k0]) / (v_left[-1] - v_left[k0])
            right.append((toe + (rho_out(g) - toe) * f, g * f))
    right[0], right[k0], right[-1] = (toe, -t), (toe, 0.0), (rho_out(g), g)
    gr = _geometric(0.5, toe - rho_k, _GROWTH)
    n_r1 = len(gr) - 1
    fr = _doubled([p / gr[-1] for p in gr])
    bottom = _graded_line((rho_k, -t), (toe, -t), fr)
    topl = _graded_line((rho_k, left[-1][1]), (rho_out(g), g), fr)
    bottom[0], topl[0] = left[0], left[-1]
    del n_r1
    r1 = _tfi(bottom, topl, left, right)
    sec.block(r1, "U")

    # --- region R2: run-pipe wall beyond the toe ----------------------------
    pipe_v = [right[k][1] for k in range(k0 + 1)]
    c4 = _doubled(
        [toe + p for p in _geometric(0.5, wc._PATCH_RADIUS_MM - toe, _GROWTH)[:-1]]
        + [wc._PATCH_RADIUS_MM]
    )
    sec.block([[(r, v) for v in pipe_v] for r in c4], "U")

    # --- upper region: weld/weldolet/branch above v = g ----------------------
    row_g = [m_up(p) for p in up.far_edge()] + [tuple(p) for p in topl[1:]]
    span_g = row_g[-1][0] - row_g[0][0]
    fracs = [(r - row_g[0][0]) / span_g for r, _ in row_g]
    v_up_c = wc._levels(g, sorted({leg, vb, vc, va}), lambda v: 0.3 + 0.12 * v)
    h_last = v_up_c[-1] - v_up_c[-2]
    v_up_c += [va + p for p in _geometric(h_last, top - va, _GROWTH)[1:-1]] + [top]
    v_up = _doubled(v_up_c)
    upper = []
    for j, fj in enumerate(fracs):
        col = []
        for k, v in enumerate(v_up):
            if k == 0:
                col.append(row_g[j])
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

    # --- named section sets --------------------------------------------------
    f_rw = _fmt(rw)
    for nid, (r, v) in enumerate(sec.pts):
        if _fmt(v) == _fmt(-t):
            sec.sets["pipe_id"].add(nid)
        if _fmt(r) == f_rw and v <= g + 1e-9:
            sec.sets["hole"].add(nid)
        if _fmt(v) == "0.00000000" and r >= toe - 1e-9:
            sec.sets["pipe_od"].add(nid)
    for k, v in enumerate(v_up):
        if v >= g - 1e-9:
            sec.sets["weldolet_bore"].add(up_ids[0][k])
        sec.sets["weldolet_outer"].add(up_ids[-1][k])
    for j in range(len(fracs)):
        sec.sets["branch_end"].add(up_ids[j][-1])
    for k in range(k0, n_left + 1):
        sec.sets["weldolet_outer"].add(sec.index[(_fmt(right[k][0]), _fmt(right[k][1]), "")])

    # --- sweep over theta in [0, pi] ------------------------------------------
    rho_c = rw + 0.5 * a
    th = _theta_levels(rho_c, up)
    trig = [wc._sincos(x) for x in th]
    raw = wc.WeldoletMesh()
    patch_id: dict[tuple[int, int], int] = {}

    def to_xyz(rho: float, v: float, k: int | None = None, theta: float | None = None):
        s_, c_ = trig[k] if k is not None else wc._sincos(theta)
        y = rho * s_
        return (rho * c_, y, wc._zmap(v, y, ro, vb, va))

    def pnode(sid: int, k: int) -> int:
        nid = patch_id.get((sid, k))
        if nid is None:
            nid = raw.add_node(*to_xyz(*sec.pts[sid], k=k))
            patch_id[(sid, k)] = nid
        return nid

    band_set = {i for r in band_quads for i in r}
    k_d = len(up.z_low) - 1  # doubled index of theta = d / rho_c
    n_th = len(th) - 1
    for qi, quad in enumerate(sec.quads):
        cq, mq = quad[:4], quad[4:]
        for k in range(k_d if qi in band_set else 0, n_th, 2):
            conn = [pnode(n, k) for n in cq] + [pnode(n, k + 2) for n in cq]
            conn += [pnode(n, k) for n in mq] + [pnode(n, k + 2) for n in mq]
            conn += [pnode(n, k + 1) for n in cq]
            raw.add_element(conn)

    tip_ids: list[tuple[int, int, str]] = []  # (raw id, w2, half) of tube tip nodes
    for q, m, half in ((up, m_up, "up"), (lo, m_lo, "lo")):
        def point(p2, q2, w2, q=q, m=m):
            s, z = q.tube_section(p2, 2 * q.n_ring - q2)
            rho, v = m(q.xy(w2, s))
            if z == 0.0:
                return to_xyz(rho, v, k=0)
            if z == q.d:
                return to_xyz(rho, v, k=k_d)
            zk = next((i for i, zz in enumerate(q.z_low) if zz == z), None)
            if zk is not None:
                return to_xyz(rho, v, k=zk)
            return to_xyz(rho, v, theta=z / rho_c)

        raw.add_block(point, q.n_psi, q.n_ring, q.nq)
        for w2 in range(2 * q.nq + 1):
            rho, v = m(q.xy(w2, 0.0))
            key = tuple(_fmt(x) for x in to_xyz(rho, v, k=0)) + ("",)
            tip_ids.append((raw._index[key], w2, half))

    # --- run pipe outside the patch (half) ------------------------------------
    length = base.run_half_length_mm
    qh = wc._SQUARE_HALF_MM
    pi = wc._PI

    def square_pt(k: int) -> tuple[float, float]:
        s_, c_ = trig[k]
        theta = th[k]
        if k == 0:
            return (qh, 0.0)
        if k == n_th:
            return (-qh, 0.0)
        if theta <= 0.25 * pi:
            return (qh, qh * s_ / c_) if theta < 0.25 * pi else (qh, qh)
        if theta < 0.75 * pi:
            return (qh * c_ / s_, qh)
        if theta == 0.75 * pi:
            return (-qh, qh)
        return (-qh, qh * s_ / (-c_))

    sq = [square_pt(k) for k in range(n_th + 1)]
    k45 = th.index(0.25 * pi)
    k135 = th.index(0.75 * pi)

    def pipe_pt(x: float, s: float, v: float):
        sn, cs = wc._sincos(s / ro)
        r = ro + v
        return (x, r * sn, r * cs)

    ring = _doubled([k / wc._O_BLOCK_DIVISIONS for k in range(wc._O_BLOCK_DIVISIONS + 1)])
    ring[-1] = 1.0

    def o_point(p: int, qq: int, w: int):
        v = pipe_v[w]
        x_in, y_in, z_in = to_xyz(wc._PATCH_RADIUS_MM, v, k=p)
        if qq == 0:
            return (x_in, y_in, z_in)
        xs, ss = sq[p]
        if qq == len(ring) - 1:
            return pipe_pt(xs, ss, v)
        s_in = ro * wc._atan(y_in / z_in)
        f = ring[qq]
        return pipe_pt(x_in + f * (xs - x_in), s_in + f * (ss - s_in), v)

    n_w = (len(pipe_v) - 1) // 2
    raw.add_block(o_point, n_th // 2, wc._O_BLOCK_DIVISIONS, n_w)

    s_right = [sq[k][1] for k in range(k45 + 1)]
    s_left = [sq[k][1] for k in range(n_th, k135 - 1, -1)]
    x_top = [sq[k][0] for k in range(k135, k45 - 1, -1)]
    step0 = x_top[2] - x_top[0]
    xr_c = [qh] + [qh + p for p in _geometric(step0, wc._HOOP_CHECK_X_MM - qh,
                                              _GROWTH)[1:-1]] + [wc._HOOP_CHECK_X_MM]
    st = xr_c[-1] - xr_c[-2]
    xr_c += [wc._HOOP_CHECK_X_MM + p for p in
             _geometric(st, length - wc._HOOP_CHECK_X_MM, _GROWTH)[1:-1]] + [length]
    x_right = _doubled(xr_c)
    x_left = [-x for x in reversed(x_right)]
    x_all = x_left[:-1] + x_top + x_right[1:]
    s_bot = pi * ro
    band = [qh + (s_bot - qh) * k / (2 * _HALF_BAND_DIVISIONS)
            for k in range(2 * _HALF_BAND_DIVISIONS + 1)]
    band[0], band[-1] = qh, s_bot

    def tensor(xl, sl):
        def pt(p, qq, w):
            return pipe_pt(xl[p], sl[qq], pipe_v[w])

        raw.add_block(pt, (len(xl) - 1) // 2, (len(sl) - 1) // 2, n_w)

    tensor(x_right, s_right)
    tensor(x_left, s_left)
    tensor(x_all, band)

    # --- renumber: front corners, front mid-sides, crack-face interior, patch
    #     nodes by section node and level, the rest --------------------------
    gf = front_geometry(spec)

    def phi_of(nid: int) -> float:
        x, y, z = raw.nodes[nid - 1]
        from digitalmodel.ansys.cint_parser import front_angle

        return front_angle(gf, x, y, z)

    corners = sorted({i for i, w2, _ in tip_ids if w2 % 2 == 0}, key=phi_of)
    mids = sorted({i for i, w2, _ in tip_ids if w2 % 2 == 1}, key=phi_of)
    front = set(corners) | set(mids)
    interior = []
    for nid, (x, y, z) in enumerate(raw.nodes, start=1):
        if nid in front or x <= 0.0 or abs(y) > 1e-9:
            continue
        rr = ((x - rw) / a) ** 2 + ((z - ro - v0) / c) ** 2
        if rr < 1.0 - 1e-9:
            interior.append(nid)
    order = corners + mids + sorted(interior)
    seen = set(order)
    for sid in range(len(sec.pts)):
        for k in range(n_th + 1):
            nid = patch_id.get((sid, k))
            if nid is not None and nid not in seen:
                order.append(nid)
                seen.add(nid)
    order += [n for n in range(1, len(raw.nodes) + 1) if n not in seen]
    new_id = {old: k + 1 for k, old in enumerate(order)}
    mesh = wc.WeldoletMesh()
    mesh.nodes = [raw.nodes[o - 1] for o in order]
    mesh.keys = [raw.keys[o - 1] for o in order]
    mesh.elements = [tuple(new_id[n] for n in el) for el in raw.elements]
    mesh.n_front = len(corners)
    mesh.n_front_all = len(corners) + len(mids)
    mesh.crack_interior = (mesh.n_front_all + 1, mesh.n_front_all + len(interior))
    mesh.area_factor = 0.5

    def lift(name: str) -> set[int]:
        out = set()
        for sid in sec.sets.get(name, ()):
            for k in range(n_th + 1):
                nid = patch_id.get((sid, k))
                if nid is not None:
                    out.add(new_id[nid])
        return out

    bnd = {n: lift(n) for n in ("hole", "weldolet_bore", "weldolet_outer", "branch_end",
                                 "pipe_od", "pipe_id")}
    ri = wg["run_inner_radius_mm"]
    bnd["end_fixed"], bnd["end_free"], bnd["symmetry"] = set(), set(), set()
    fx_l, fx_r = _fmt(-length), _fmt(length)
    for nid, (x, y, z) in enumerate(mesh.nodes, start=1):
        rr = math.sqrt(y * y + z * z)
        if abs(rr - ri) < 1e-6:
            bnd["pipe_id"].add(nid)
        if abs(rr - ro) < 1e-6 and (z < 0.0 or x * x + y * y > (toe - 1e-6) ** 2):
            bnd["pipe_od"].add(nid)
        if mesh.keys[nid - 1][0] == fx_l:
            bnd["end_fixed"].add(nid)
        elif mesh.keys[nid - 1][0] == fx_r:
            bnd["end_free"].add(nid)
        if abs(y) < 1e-9:
            bnd["symmetry"].add(nid)
        # bore surface (hole wall + root face), including the crack-tube end
        # faces at the flaw tips, which are not section nodes
        if abs(math.sqrt(x * x + y * y) - rw) < 1e-7 and z <= ro + g + 1e-6:
            bnd["hole"].add(nid)
    mesh.boundary_nodes = bnd

    def runs(ids: set[int]) -> list[tuple[int, int]]:
        out: list[tuple[int, int]] = []
        for n in sorted(ids):
            if out and out[-1][1] == n - 1:
                out[-1] = (out[-1][0], n)
            else:
                out.append((n, n))
        return out

    mesh.wet_runs = runs(bnd["hole"] | bnd["weldolet_bore"])
    mesh.crack_runs = [(1, mesh.crack_interior[1])]

    def find(pt) -> int:
        return new_id[raw._index[(_fmt(pt[0]), _fmt(pt[1]), _fmt(pt[2]), "")]]

    k_side = min(range(len(band)), key=lambda k: abs(band[k] - 0.5 * pi * ro) if k % 2 == 0
                 else math.inf)
    mesh.support_nodes = {"uz_side": find(pipe_pt(-length, band[k_side], 0.0))}
    mesh.hoop_line = [find(pipe_pt(wc._HOOP_CHECK_X_MM, s_bot, 0.0))]
    return mesh


# --------------------------------------------------------------------------- #
# Deck
# --------------------------------------------------------------------------- #
def _write_cint_table(w, lv: int) -> None:
    w(f"*CFOPEN,{wc.cint_table_name(lv)},txt")
    w("*VWRITE")
    w("('# digitalmodel weldolet_crotch CINT table')")
    w("*VWRITE")
    w("('# units: length=mm force=N stress=MPa K=MPa*sqrt(mm) J=N/mm')")
    w("*VWRITE,LVL")
    w("('# mesh_level ',F6.0)")
    w("*VWRITE,NFRONT")
    w("('# front_nodes ',F8.0)")
    w(f"NCONT = {wc.N_CONTOURS}")
    w("*VWRITE,NCONT")
    w("('# contours ',F4.0)")
    w("*VWRITE")
    w("('# columns: node contour x y z K1 K2 K3 J')")
    w("*DO,NN,1,NFRONT")
    w(f"*DO,IC,1,{wc.N_CONTOURS}")
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


def generate_crotch_apdl(spec: CrotchSpec, hooks: wc.DeckHooks | None = None) -> str:
    mesh = build_mesh(spec)
    base = spec.base
    wg = wc.derived_geometry(base)
    cf = wc.closed_forms(base)
    par = mesh_parameters(spec)
    geo = flaw_geometry(spec)
    lv = spec.mesh_level
    limit = hooks is not None
    analysis = hooks.name if limit else "elastic"
    ri = wg["run_inner_radius_mm"]
    length = base.run_half_length_mm
    top_z = wg["run_outer_radius_mm"] + base.weldolet_a_mm + base.branch_length_mm
    out: list[str] = []
    w = out.append
    w("! Weldolet crotch-plane root flaw model (#2157 P0b, owner card G15)")
    w("! generated by digitalmodel.ansys.weldolet_crotch")
    w("! Units: length = mm, force = N, stress = MPa")
    w("! raw CINT K = MPa*sqrt(mm), J = N/mm (converted once by cint_parser)")
    w("! assumed design basis: examples/workflows/crack-fe-weldolet/"
      "design-data-register.json")
    w(f"! crotch-plane root flaw (y = 0, theta = 0): semicircle a = c = "
      f"{geo['a_mm']!r} mm, centre v = {geo['centre_v_mm']!r} mm, ligament to the "
      f"run-pipe bore = {remaining_ligament_mm(spec)!r} mm")
    w(f"! half model y >= 0; analysis = {analysis}; mesh_level = {lv}; front "
      f"divisions per quarter = {par['front_divisions_quarter']}; first ring = "
      f"{par['first_ring_mm']!r} mm")
    w(f"! nodes = {len(mesh.nodes)}, elements = {len(mesh.elements)} (SOLID186)")
    w(f"! crack-face pressure {'ON' if spec.crack_face_pressure else 'OFF'}")
    w("FINISH")
    w("/CLEAR,NOSTART")
    w(f"/TITLE,Weldolet crotch flaw a={geo['a_mm']!r} level {lv} {analysis}")
    w("/UNITS,MPA")
    w("/PREP7")
    w("ET,1,SOLID186")
    w("KEYOPT,1,2,1")
    w(f"MP,EX,1,{base.youngs_modulus_mpa!r}")
    w(f"MP,PRXY,1,{base.poisson!r}")
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
    w("NSEL,S,NODE,,1,NFRALL")
    w("CM,CRKTIP,NODE")
    w("ALLSEL,ALL")
    w("FINISH")
    w("/SOLU")
    w("ANTYPE,STATIC")
    for line in hooks.solution if limit else ():
        w(line)
    lo_i, hi_i = mesh.crack_interior
    w(f"NSEL,S,LOC,X,{-length - 1.0e-6!r},{-length + 1.0e-6!r}")
    w("D,ALL,UX,0.0")
    # symmetry plane y = 0, crack faces free
    w("NSEL,S,LOC,Y,-1.0E-6,1.0E-6")
    w(f"NSEL,U,NODE,,{lo_i},{hi_i}")
    w("D,ALL,UY,0.0")
    w("ALLSEL,ALL")
    w(f"D,{mesh.support_nodes['uz_side']},UZ,0.0")
    w(f"NSEL,S,LOC,X,{length - 1.0e-6!r},{length + 1.0e-6!r}")
    w("CP,1,UX,ALL")
    w("ALLSEL,ALL")
    lf = hooks.load_factor if limit else ""
    w(f"PINT = {base.pressure_mpa!r}{lf}")
    w(f"PRUN = {cf['run_end_pressure_mpa']!r}{lf}")
    w(f"PBR = {cf['branch_end_pressure_mpa']!r}{lf}")
    w("CSYS,11")
    w(f"NSEL,S,LOC,X,{ri - 1.0e-6!r},{ri + 1.0e-6!r}")
    w("CSYS,0")
    wc._nsel_runs(w, mesh.wet_runs, first="A")
    if spec.crack_face_pressure:
        wc._nsel_runs(w, mesh.crack_runs, first="A")
    w("SF,ALL,PRES,PINT")
    w("ALLSEL,ALL")
    w(f"NSEL,S,LOC,X,{length - 1.0e-6!r},{length + 1.0e-6!r}")
    w("SF,ALL,PRES,-PRUN")
    w(f"NSEL,S,LOC,Z,{top_z - 1.0e-6!r},{top_z + 1.0e-6!r}")
    w("SF,ALL,PRES,-PBR")
    w("ALLSEL,ALL")
    if not limit:
        for cid, kind in ((1, "SIFS"), (2, "JINT")):
            w(f"CINT,NEW,{cid}")
            w(f"CINT,TYPE,{kind}")
            w("CINT,CTNC,CRKTIP")
            w("CINT,NORM,0,2")
            w("CINT,SYMM,ON")
            w(f"CINT,NCON,{wc.N_CONTOURS}")
        w("OUTRES,ALL,ALL")
    w("SOLVE")
    w("FINISH")
    w("/POST1")
    w("SET,LAST")
    w("*GET,MREV,ACTIVE,0,REV")
    w(f"LVL = {lv}")
    if not limit:
        _write_cint_table(w, lv)
    wc._write_reactions(w, base, mesh, lv, hooks.papp if limit else ())
    if limit and hooks.post is not None:
        hooks.post(w, base, mesh, lv)
    w("FINISH")
    return "\n".join(out) + "\n"


def spec_dict(spec: CrotchSpec) -> dict:
    d = asdict(spec)
    d.pop("mesh_level")
    d["base"].pop("mesh_level")
    return d


def spec_from_receipt(receipt: dict, level: int) -> CrotchSpec:
    s = dict(receipt["spec"])
    base = wc.WeldoletSpec(**s.pop("base"))
    return CrotchSpec(**s, base=base, mesh_level=level)


def deck_sha256_for_receipt(receipt: dict, level: int) -> str:
    return wc.deck_sha256(generate_crotch_apdl(spec_from_receipt(receipt, level)))


def state_name(spec: CrotchSpec) -> str:
    name = "p0b_crotch_" + wc.depth_tag(spec.crack_depth_mm)
    return name if spec.crack_face_pressure else name + "_cfp_off"


def crack_summary(spec: CrotchSpec) -> dict:
    geo = flaw_geometry(spec)
    return {
        "type": "semicircular crotch-plane root flaw on the bore surface (y = 0, +x "
                "crotch), normal to the run-pipe hoop stress",
        "depth_mm": geo["a_mm"],
        "half_length_mm": geo["c_mm"],
        "aspect_a_over_c": geo["aspect_a_over_c"],
        "centre_v_mm": geo["centre_v_mm"],
        "top_tip_v_mm": geo["top_tip_v_mm"],
        "bottom_tip_v_mm": geo["bottom_tip_v_mm"],
        "remaining_ligament_mm": remaining_ligament_mm(spec),
        "ligament_definition": "lower flaw tip to the run-pipe bore along the bore "
                               "surface; exhausted at a = 3.805 mm",
        "crack_face_pressure": spec.crack_face_pressure,
    }


MESHING_APPROACH = (
    "Half model (y >= 0, symmetry UY = 0 off the crack faces). Structured 20-node "
    "hexahedral mesh (SOLID186) written as N/E commands: the P0a crack block around "
    "the semicircular front in the (rho, v) section (front-fitted band, three-block "
    "core, transfinite outer block to a box on the bore line, one per half of the "
    "semicircle), the rest of the section as in weldolet_crack, swept through "
    "theta in [0, pi] on graded levels; the P0a crack tube (innermost ring collapsed "
    "onto the tip, quarter-point mid-side nodes) replaces the band near theta = 0; "
    "O-block and tensor mesh for the run pipe. CINT,CTNC + CINT,NORM,0,2 + "
    "CINT,SYMM,ON; front end nodes on the free bore surface."
)


def crotch_variant(spec: CrotchSpec, sigma_ref: dict | None = None) -> wc.StateVariant:
    from digitalmodel.ansys import cint_parser

    def at(level: int) -> CrotchSpec:
        return CrotchSpec(**{**spec.__dict__, "mesh_level": level})

    def outputs(level: int) -> dict[str, str]:
        return {"reac": wc.reaction_file_name(level), "cint": wc.cint_table_name(level)}

    def derive(level: int, texts: dict[str, str]) -> dict:
        s = at(level)
        front = cint_parser.build_mesh_record(
            level=level, cint_text=texts["cint"], reac_text=texts["reac"],
            front_geometry=front_geometry(s))["front"]
        return {
            "j_from_k_ratio": [wc.j_from_k_ratio(n, s.base) for n in front],
            "governing": wc.governing_summary(front, s.base),
        }

    def mesh_info(level: int) -> dict:
        s = at(level)
        mesh = build_mesh(s)
        return {"n_nodes": len(mesh.nodes), "n_elements": len(mesh.elements),
                "mesh_parameters": mesh_parameters(s)}

    def top(primary: dict) -> dict:
        extra = {"front_geometry": front_geometry(spec), "crack": crack_summary(spec),
                 "plane": "crotch", "governing": primary["governing"]}
        if sigma_ref is not None:
            extra["sigma_ref"] = sigma_ref
        return extra

    return wc.StateVariant(
        state=state_name(spec),
        kind="weldolet_crack",
        spec_dict=spec_dict(spec),
        generator_files=GENERATOR_FILES,
        deck=lambda level: generate_crotch_apdl(at(level)),
        outputs=outputs,
        derive=derive,
        mesh_info=mesh_info,
        top=top,
        cracked=True,
        meshing={
            "approach": MESHING_APPROACH,
            "element": "SOLID186 (20-node, full integration)",
            "k_reported": "mean over contours 4-6",
            "contour_guard_quantities": ["K1", "K2", "K3", "J"],
            "modelling_route": "global structured crack-block model (no submodel)",
        },
        front_geometry=front_geometry(spec),
    )


def run_crotch_state(spec: CrotchSpec, workdir: Path | str, fe_states: Path | str, *,
                     levels: tuple[int, ...] = (0, 1), cores: int = 4,
                     sigma_ref: dict | None = None, timeout_seconds: int = 14400) -> dict:
    return wc.run_variant(crotch_variant(spec, sigma_ref), workdir, fe_states,
                          levels=levels, cores=cores, timeout_seconds=timeout_seconds)
