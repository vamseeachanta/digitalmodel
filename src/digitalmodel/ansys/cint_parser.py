"""Parser and conservation guards for MAPDL crack-front (CINT) output (#2157 P0).

The crack verification deck (``crack_verification.py``) writes two small text
files per mesh density, both from **solved** results:

* ``crack_cint_L<n>.txt``: K_I, K_II, K_III (interaction integral) and J for
  every crack-front node and contour, raw in MPa*sqrt(mm) and N/mm;
* ``crack_reac_L<n>.txt``: the solved reaction sums on the constrained faces.

This module parses them into a host-free record and evaluates guards (a)-(f)
independently from that solved output (never from prescribed loads):

(a) equilibrium: the solved ligament reaction balances sigma x loaded area
    within 0.5 %;
(b) mesh independence of load: the two densities' solved reaction sums agree
    within 0.1 %;
(c) contour independence (owner card G13): at every front node, over the last
    three contours, (i) the spread of each of K_I, K_II and K_III divided by
    the maximum |K| = sqrt(K_I^2 + K_II^2 + K_III^2) along the whole front is
    <= 3 %, and (ii) the spread of J divided by that node's mean J is <= 3 %.
    The earlier per-node (max - min)/|mean| metric of K_I and J, undefined
    where K_I crosses zero on the front, is still evaluated and recorded as
    ``c_contour_legacy``; it no longer gates;
(d) completeness: every declared front node has K_I, K_II, K_III and J for
    every declared contour;
(e) sanitisation: no host or user tokens in the raw output or the record;
(f) units: mm-N-MPa declared, raw K tagged MPa*sqrt(mm), and K converted to
    MPa*sqrt(m) exactly once;
(g) mesh convergence of J (owner card G14): J at the governing node (the node
    of maximum J, i.e. of maximum K = sqrt(E' J), among the positions present
    on both meshes, taken on the finer mesh) changes by <= 1 % between the two
    mesh densities. For a front ending on a free surface (owner card G16) the
    J change is checked at every interior front node instead; the two
    free-surface end nodes are recorded as ``g_end_nodes_record`` and do not
    gate (their J carries a non-square-root singularity). The governing
    K_gov remains the maximum over all front nodes.

The limits are fixed by the approved plan and are not parameters of the
guards. The parser needs no licence.
"""

from __future__ import annotations

import csv
import getpass
import io
import json
import math
import os
import re
import socket
from collections.abc import Iterable, Mapping
from dataclasses import dataclass, field

RECEIPT_SCHEMA_ID = "digitalmodel.crack_fe.receipt/v2"
RECEIPT_KINDS = ("verification", "weldolet_uncracked", "weldolet_crack", "limit_load")
GUARD_STATUSES = ("pass", "fail", "not_applicable")

# Guard limits, frozen by the approved plan (#2157, P0 step 3).
EQUILIBRIUM_TOL = 0.005
MESH_LOAD_TOL = 0.001
CONTOUR_SPREAD_TOL = 0.03
LAST_CONTOURS = 3
J_MESH_TOL = 0.01  # guard (g), owner card G14

RAW_K_UNIT = "MPa*sqrt(mm)"
K_UNIT = "MPa*sqrt(m)"
J_UNIT = "N/mm"
K_RAW_TO_SI = 1.0 / math.sqrt(1000.0)  # MPa*sqrt(mm) -> MPa*sqrt(m)
EXPECTED_BASE_UNITS = {"length": "mm", "force": "N", "stress": "MPa"}

GUARD_NAMES = (
    "a_equilibrium",
    "b_mesh_load",
    "c_contour",
    "d_complete",
    "e_sanitised",
    "f_units",
    "g_j_mesh",
)
LEGACY_GUARD = "c_contour_legacy"  # recorded, not gating (owner card G13)

# Generic leakage patterns, checked everywhere (the producing host is unknown
# to CI): key=value host/user fields, absolute drive/home paths, UNC shares.
_HOST_PATTERNS = (
    re.compile(r"(?i)\b(host|hostname|computername|user|username|userprofile)\s*[=:]"),
    re.compile(r"(?i)(?<![A-Za-z])[A-Z]:[\\/][^\s,\"]"),
    re.compile(r"(?i)/(home|Users)/[A-Za-z]"),
    re.compile(r"\\\\[A-Za-z0-9]"),
)


# --------------------------------------------------------------------------- #
# Records
# --------------------------------------------------------------------------- #
@dataclass
class CintRow:
    node: int
    contour: int
    x: float | None
    y: float | None
    z: float | None
    k1: float | None
    k2: float | None
    k3: float | None
    j: float | None


@dataclass
class CintTable:
    units: dict[str, str] = field(default_factory=dict)
    declared_front_nodes: int | None = None
    declared_contours: int | None = None
    mesh_level: int | None = None
    rows: list[CintRow] = field(default_factory=list)
    malformed_lines: int = 0


@dataclass
class ReactionRecord:
    units: dict[str, str] = field(default_factory=dict)
    mesh_level: int | None = None
    mapdl_rev: str | None = None
    stress_mpa: float | None = None
    loaded_area_mm2: float | None = None
    fz_ligament: float | None = None
    fx_symmetry: float | None = None
    fy_point: float | None = None
    n_ligament: int | None = None
    # generic solved reaction sum balancing stress_mpa * loaded_area_mm2
    # (P0a: fz_ligament; weldolet: the axial reaction at the constrained end)
    reaction_sum: float | None = None
    extras: dict[str, float] = field(default_factory=dict)


@dataclass
class GuardResult:
    status: str  # "pass" | "fail"
    value: float | None
    limit: float | None
    detail: str = ""

    def to_dict(self) -> dict:
        return {
            "status": self.status,
            "value": self.value,
            "limit": self.limit,
            "detail": self.detail,
        }


# --------------------------------------------------------------------------- #
# Parsing
# --------------------------------------------------------------------------- #
def _num(tok: str) -> float | None:
    try:
        v = float(tok)
    except ValueError:
        return None
    return v if math.isfinite(v) else None


def _parse_units(line: str) -> dict[str, str]:
    body = line.split(":", 1)[1]
    units = {}
    for tok in body.split():
        if "=" in tok:
            k, v = tok.split("=", 1)
            units[k.strip()] = v.strip()
    return units


def parse_cint_table(text: str) -> CintTable:
    """Parse a ``crack_cint_L<n>.txt`` table written by the verification deck."""
    table = CintTable()
    for raw in text.splitlines():
        line = raw.strip()
        if not line:
            continue
        if line.startswith("#"):
            body = line[1:].strip()
            if body.lower().startswith("units:"):
                table.units = _parse_units(body)
                continue
            parts = body.split()
            if len(parts) == 2 and _num(parts[1]) is not None:
                val = round(float(parts[1]))
                if parts[0] == "front_nodes":
                    table.declared_front_nodes = val
                elif parts[0] == "contours":
                    table.declared_contours = val
                elif parts[0] == "mesh_level":
                    table.mesh_level = val
            continue
        toks = line.split()
        vals = [_num(t) for t in toks]
        if len(vals) < 2 or vals[0] is None or vals[1] is None:
            table.malformed_lines += 1
            continue
        rest = vals[2:] + [None] * max(0, 7 - len(vals[2:]))
        if len(toks) != 9:
            table.malformed_lines += 1
        table.rows.append(
            CintRow(
                round(vals[0]),
                round(vals[1]),
                *rest[:7],
            )
        )
    return table


def parse_reaction_file(text: str) -> ReactionRecord:
    """Parse a ``crack_reac_L<n>.txt`` reaction summary."""
    rec = ReactionRecord()
    for raw in text.splitlines():
        line = raw.strip()
        if not line:
            continue
        if line.startswith("#"):
            body = line[1:].strip()
            if body.lower().startswith("units:"):
                rec.units = _parse_units(body)
            continue
        parts = line.split()
        if len(parts) != 2:
            continue
        key, val = parts
        num = _num(val)
        if num is None:
            continue
        if key == "mapdl_rev":
            rec.mapdl_rev = f"{num:.2f}".rstrip("0").rstrip(".")
        elif key in ("mesh_level", "n_ligament"):
            setattr(rec, key, round(num))
        elif key in (
            "stress_mpa",
            "loaded_area_mm2",
            "fz_ligament",
            "fx_symmetry",
            "fy_point",
            "reaction_sum",
        ):
            setattr(rec, key, num)
        else:
            rec.extras[key] = num
    if rec.reaction_sum is None:
        rec.reaction_sum = rec.fz_ligament
    return rec


def _phi_deg(x: float, y: float, a: float, c: float) -> float:
    """Parametric angle of a front point (Newman-Raju phi); 90 = deepest."""
    phi = math.degrees(math.atan2(y / a, x / c))
    return round(phi, 9) + 0.0


def _polar_z_deg(x: float, y: float) -> float:
    """Position angle about the z axis in [0, 360) deg (closed weldolet front)."""
    ang = math.degrees(math.atan2(y, x))
    if ang < 0.0:
        ang += 360.0
    ang = round(ang, 9) + 0.0
    return 0.0 if ang >= 360.0 else ang


def front_angle(
    front_geometry: Mapping, x: float, y: float, z: float | None = None
) -> float:
    """Front position angle for a declared front geometry.

    ``{"type": "ellipse", "a": .., "c": ..}``: Newman-Raju parametric angle
    (90 = deepest); ``{"type": "polar_z"}``: angle about the z axis;
    ``{"type": "crotch", "rw", "ro", "v0", "a", "c"}``: parametric angle of the
    semi-elliptical crotch flaw in the plane y = 0 (rho = x, v = z - ro), 0 at
    the upper surface tip, 90 at the deepest point, 180 at the lower tip.
    """
    kind = front_geometry.get("type")
    if kind == "ellipse":
        return _phi_deg(x, y, front_geometry["a"], front_geometry["c"])
    if kind == "polar_z":
        return _polar_z_deg(x, y)
    if kind == "crotch":
        g = front_geometry
        depth = (x - g["rw"]) / g["a"]
        along = (z - g["ro"] - g["v0"]) / g["c"]
        return round(math.degrees(math.atan2(depth, along)), 9) + 0.0
    raise ValueError(f"unknown front geometry {kind!r}")


def front_records(
    table: CintTable,
    *,
    crack_depth_mm: float | None = None,
    crack_half_length_mm: float | None = None,
    front_geometry: Mapping | None = None,
) -> list[dict]:
    """Per-node records with K converted **once** to MPa*sqrt(m), ordered by
    position angle ``phi_deg``.

    ``K1_reported`` (and ``K2/K3/J_reported``) are means over the last three
    contours. The angle comes from ``front_geometry``; the P0a form
    ``crack_depth_mm``/``crack_half_length_mm`` means an elliptical front.
    """
    if front_geometry is None:
        front_geometry = {
            "type": "ellipse",
            "a": crack_depth_mm,
            "c": crack_half_length_mm,
        }
    by_node: dict[int, list[CintRow]] = {}
    for row in table.rows:
        by_node.setdefault(row.node, []).append(row)
    out = []
    for node, rows in by_node.items():
        rows = sorted(rows, key=lambda r: r.contour)
        ref = next((r for r in rows if r.x is not None and r.y is not None), None)
        if ref is None:
            continue
        contours = []
        for r in rows:
            contours.append(
                {
                    "contour": r.contour,
                    "K1": None if r.k1 is None else r.k1 * K_RAW_TO_SI,
                    "K2": None if r.k2 is None else r.k2 * K_RAW_TO_SI,
                    "K3": None if r.k3 is None else r.k3 * K_RAW_TO_SI,
                    "J": r.j,
                }
            )
        rec = {
            "node": node,
            "phi_deg": front_angle(front_geometry, ref.x, ref.y, ref.z),
            "x": ref.x,
            "y": ref.y,
            "z": ref.z,
            "contours": contours,
        }
        for key in ("K1", "K2", "K3", "J"):
            last = [c[key] for c in contours[-LAST_CONTOURS:] if c[key] is not None]
            rec[f"{key}_reported"] = sum(last) / len(last) if last else None
        out.append(rec)
    out.sort(key=lambda n: (n["phi_deg"], n["node"]))
    return out


def centre_crack_secant_k(a_mm: float, width_mm: float, sigma_mpa: float) -> float:
    """Centre-cracked plate, Feddersen secant form (MPa*sqrt(m)).

    K = sigma * sqrt(pi*a) * sqrt(sec(pi*a/W)), a = half crack length,
    W = full plate width. Textbook closed form, used as a parser sanity value.
    """
    a_m = a_mm / 1000.0
    return (
        sigma_mpa
        * math.sqrt(math.pi * a_m)
        * math.sqrt(1.0 / math.cos(math.pi * a_mm / width_mm))
    )


# --------------------------------------------------------------------------- #
# Host / user token scan
# --------------------------------------------------------------------------- #
def runtime_host_tokens() -> tuple[str, ...]:
    """Host and user names of the current process (never written anywhere)."""
    tokens: set[str] = set()
    for getter in (socket.gethostname, getpass.getuser):
        try:
            value = getter()
        except (OSError, KeyError, ImportError):  # best effort; absence is fine
            value = ""
        tokens.add(value)
    for var in ("COMPUTERNAME", "USERNAME", "USER", "HOSTNAME", "USERDOMAIN"):
        val = os.environ.get(var)
        if val:
            tokens.add(val)
    home = os.path.expanduser("~")
    if home and home != "~":
        tokens.add(home)
    return tuple(sorted(t for t in tokens if t and len(t) >= 3))


def find_host_tokens(
    text: str, extra_tokens: Iterable[str] | None = None
) -> list[str]:
    """Return the host/user leak findings in ``text`` (empty list = clean).

    ``extra_tokens`` defaults to the current host and user names; pass ``()``
    to check only the generic patterns.
    """
    tokens = runtime_host_tokens() if extra_tokens is None else tuple(extra_tokens)
    findings = []
    for pat in _HOST_PATTERNS:
        m = pat.search(text)
        if m:
            findings.append(f"pattern {pat.pattern!r} at offset {m.start()}")
    low = text.lower()
    for tok in tokens:
        if tok and tok.lower() in low:
            findings.append("runtime host/user token present")
    return findings


# --------------------------------------------------------------------------- #
# Guards
# --------------------------------------------------------------------------- #
def _pass(ok: bool) -> str:
    return "pass" if ok else "fail"


def guard_equilibrium(reactions: Mapping[int, ReactionRecord]) -> GuardResult:
    worst = None
    for lvl, r in sorted(reactions.items()):
        if None in (r.reaction_sum, r.stress_mpa, r.loaded_area_mm2):
            return GuardResult("fail", None, EQUILIBRIUM_TOL, f"L{lvl}: missing field")
        applied = r.stress_mpa * r.loaded_area_mm2
        # the solved reaction balances the applied load: sum(RF) = -sigma*A
        err = abs(r.reaction_sum + applied) / abs(applied)
        worst = err if worst is None else max(worst, err)
    ok = worst is not None and worst <= EQUILIBRIUM_TOL
    return GuardResult(_pass(ok), worst, EQUILIBRIUM_TOL, "|sum RF_z + sigma*A| / sigma*A")


def guard_mesh_load(reactions: Mapping[int, ReactionRecord]) -> GuardResult:
    if len(reactions) != 2:
        return GuardResult("fail", None, MESH_LOAD_TOL, "need exactly two mesh densities")
    (l0, r0), (l1, r1) = sorted(reactions.items())
    if r0.reaction_sum is None or r1.reaction_sum is None or r1.reaction_sum == 0:
        return GuardResult("fail", None, MESH_LOAD_TOL, "missing reaction sum")
    diff = abs(r0.reaction_sum - r1.reaction_sum) / abs(r1.reaction_sum)
    return GuardResult(
        _pass(diff <= MESH_LOAD_TOL),
        diff,
        MESH_LOAD_TOL,
        f"|RF_z(L{l0}) - RF_z(L{l1})| / |RF_z(L{l1})|",
    )


def _spread(vals: list[float]) -> float:
    mean = sum(vals) / len(vals)
    if mean == 0:
        return math.inf
    return (max(vals) - min(vals)) / abs(mean)


def contour_spread(front: list[dict], key: str) -> float | None:
    """Worst spread of ``key`` over the last three contours, across all nodes."""
    worst = None
    for node in front:
        vals = [
            c[key] for c in node["contours"][-LAST_CONTOURS:] if c[key] is not None
        ]
        if len(vals) < 2:
            continue
        s = _spread(vals)
        worst = s if worst is None else max(worst, s)
    return worst


def guard_contour_legacy(fronts: Mapping[int, list[dict]]) -> GuardResult:
    """The pre-G13 metric: per node (max - min)/|mean| of K_I and J. Recorded
    for the record only; undefined where K_I crosses zero on the front."""
    worst = None
    where = ""
    for lvl, front in sorted(fronts.items()):
        for key in ("K1", "J"):
            s = contour_spread(front, key)
            if s is not None and (worst is None or s > worst):
                worst, where = s, f"L{lvl} {key}"
    ok = worst is not None and worst <= CONTOUR_SPREAD_TOL
    return GuardResult(
        _pass(ok), worst, CONTOUR_SPREAD_TOL,
        f"legacy, not gating (owner G13): max (max-min)/|mean| per node, last 3 "
        f"contours ({where})",
    )


def _k_magnitude(node: Mapping) -> float:
    vals = [node.get(f"K{i}_reported") for i in (1, 2, 3)]
    return math.sqrt(sum(v * v for v in vals if v is not None))


def k_spread_over_front_max(front: list[dict]) -> tuple[float | None, str]:
    """G13 (i): max over nodes and modes of the last-three-contour K spread,
    divided by the maximum |K| along the whole front."""
    kmax = max((_k_magnitude(n) for n in front), default=0.0)
    worst, where = None, ""
    if kmax == 0.0:
        return (math.inf if front else None), "zero K on the front"
    for node in front:
        for key in ("K1", "K2", "K3"):
            vals = [c[key] for c in node["contours"][-LAST_CONTOURS:] if c[key] is not None]
            if len(vals) < 2:
                continue
            s = (max(vals) - min(vals)) / kmax
            if worst is None or s > worst:
                worst, where = s, f"{key} at phi {node['phi_deg']:g}"
    return worst, where


def j_spread_over_node_mean(front: list[dict]) -> tuple[float | None, str]:
    """G13 (ii): max over nodes of the last-three-contour J spread divided by
    that node's mean J."""
    worst, where = None, ""
    for node in front:
        vals = [c["J"] for c in node["contours"][-LAST_CONTOURS:] if c["J"] is not None]
        if len(vals) < 2:
            continue
        s = _spread(vals)
        if worst is None or s > worst:
            worst, where = s, f"phi {node['phi_deg']:g}"
    return worst, where


def guard_contour(fronts: Mapping[int, list[dict]]) -> GuardResult:
    """Guard (c), owner card G13: parts (i) and (ii) on every mesh density."""
    wk = wj = None
    where_k = where_j = ""
    for lvl, front in sorted(fronts.items()):
        sk, ak = k_spread_over_front_max(front)
        sj, aj = j_spread_over_node_mean(front)
        if sk is not None and (wk is None or sk > wk):
            wk, where_k = sk, f"L{lvl} {ak}"
        if sj is not None and (wj is None or sj > wj):
            wj, where_j = sj, f"L{lvl} {aj}"
    ok_k = wk is not None and wk <= CONTOUR_SPREAD_TOL
    ok_j = wj is not None and wj <= CONTOUR_SPREAD_TOL
    value = None if wk is None or wj is None else max(wk, wj)
    detail = (
        f"(i) {_pass(ok_k)}: K spread / max|K| on the front = {wk} ({where_k}); "
        f"(ii) {_pass(ok_j)}: J spread / node mean J = {wj} ({where_j})"
    )
    return GuardResult(_pass(ok_k and ok_j), value, CONTOUR_SPREAD_TOL, detail)


def k_from_j(j_n_per_mm: float, youngs_mpa: float, poisson: float) -> float:
    """Governing K = sqrt(E' J), E' = E/(1 - nu^2) (plane strain), MPa*sqrt(m)."""
    e_prime = youngs_mpa / (1.0 - poisson * poisson)
    return math.sqrt(e_prime * j_n_per_mm) * K_RAW_TO_SI


def _position_key(phi: float) -> float:
    return round(phi, 6)


def governing_node(fronts: Mapping[int, list[dict]]) -> dict | None:
    """The governing node for guard (g): maximum J (hence maximum sqrt(E' J))
    on the finer of the two meshes, among positions present on both."""
    if len(fronts) != 2:
        return None
    (lc, coarse), (lf, fine) = sorted(fronts.items())
    coarse_by_pos = {_position_key(n["phi_deg"]): n for n in coarse}
    common = [n for n in fine if _position_key(n["phi_deg"]) in coarse_by_pos
              and n.get("J_reported")]
    if not common:
        return None
    gov = max(common, key=lambda n: n["J_reported"])
    j_c = coarse_by_pos[_position_key(gov["phi_deg"])]["J_reported"]
    return {
        "level_coarse": lc,
        "level_fine": lf,
        "phi_deg": gov["phi_deg"],
        "node_fine": gov["node"],
        "j_fine_n_per_mm": gov["J_reported"],
        "j_coarse_n_per_mm": j_c,
        "j_rel_change": abs(gov["J_reported"] / j_c - 1.0) if j_c else math.inf,
    }


OPEN_FRONT_TYPES = ("ellipse", "crotch")  # fronts ending on a free surface
END_NODE_RECORD = "g_end_nodes_record"  # recorded, not gating (owner card G16)


def is_open_front(front_geometry: Mapping | None) -> bool:
    return bool(front_geometry) and front_geometry.get("type") in OPEN_FRONT_TYPES


def _j_changes(fronts: Mapping[int, list[dict]]) -> tuple[list[tuple[float, float]], int, int]:
    """(phi, |J_fine / J_coarse - 1|) at positions present on both meshes."""
    (lc, coarse), (lf, fine) = sorted(fronts.items())
    by_pos = {_position_key(n["phi_deg"]): n for n in coarse}
    out = []
    for n in sorted(fine, key=lambda n: n["phi_deg"]):
        c = by_pos.get(_position_key(n["phi_deg"]))
        if c is None or not c.get("J_reported") or n.get("J_reported") is None:
            continue
        out.append((n["phi_deg"], abs(n["J_reported"] / c["J_reported"] - 1.0)))
    return out, lc, lf


def guard_j_mesh_open(fronts: Mapping[int, list[dict]]) -> tuple[GuardResult, GuardResult]:
    """Guard (g) for a front ending on a free surface (owner card G16): the J
    change between the two meshes at every interior front node <= 1 %; the two
    free-surface end nodes are recorded (``g_end_nodes_record``), not gated."""
    if len(fronts) != 2:
        bad = GuardResult("fail", None, J_MESH_TOL, "need exactly two mesh densities")
        return bad, _not_applicable()
    changes, lc, lf = _j_changes(fronts)
    if len(changes) < 3:
        bad = GuardResult("fail", None, J_MESH_TOL, "fewer than three common front nodes")
        return bad, _not_applicable()
    interior, ends = changes[1:-1], [changes[0], changes[-1]]
    worst = max(interior, key=lambda t: t[1])
    end = max(ends, key=lambda t: t[1])
    g = GuardResult(
        _pass(worst[1] <= J_MESH_TOL), worst[1], J_MESH_TOL,
        f"max |J(L{lf}) / J(L{lc}) - 1| over interior front nodes (owner G16), "
        f"at phi {worst[0]:g}",
    )
    rec = GuardResult(
        _pass(end[1] <= J_MESH_TOL), end[1], J_MESH_TOL,
        f"record, not gating (owner G16): max |J(L{lf}) / J(L{lc}) - 1| at the "
        f"free-surface end nodes, at phi {end[0]:g}",
    )
    return g, rec


def guard_j_mesh_for(
    fronts: Mapping[int, list[dict]], front_geometry: Mapping | None
) -> tuple[GuardResult, GuardResult]:
    """Guard (g) and the end-node record for the front's topology."""
    if is_open_front(front_geometry):
        return guard_j_mesh_open(fronts)
    return guard_j_mesh(fronts), _not_applicable()


def guard_j_mesh(fronts: Mapping[int, list[dict]]) -> GuardResult:
    """Guard (g), owner card G14 (closed fronts): J at the governing node."""
    gov = governing_node(fronts)
    if gov is None:
        return GuardResult("fail", None, J_MESH_TOL, "no governing node on two meshes")
    ok = gov["j_rel_change"] <= J_MESH_TOL
    return GuardResult(
        _pass(ok), gov["j_rel_change"], J_MESH_TOL,
        f"|J(L{gov['level_fine']}) / J(L{gov['level_coarse']}) - 1| at the governing "
        f"node phi {gov['phi_deg']:g} (max J on L{gov['level_fine']})",
    )


def guard_complete(tables: Mapping[int, CintTable]) -> GuardResult:
    missing = 0
    for lvl, t in sorted(tables.items()):
        if t.declared_front_nodes is None or t.declared_contours is None:
            return GuardResult("fail", None, 0.0, f"L{lvl}: front/contour count not declared")
        seen = {}
        for r in t.rows:
            ok = None not in (r.k1, r.k2, r.k3, r.j)
            seen[(r.node, r.contour)] = ok and seen.get((r.node, r.contour), True)
        nodes = {n for n, _ in seen}
        if len(nodes) != t.declared_front_nodes:
            missing += abs(t.declared_front_nodes - len(nodes)) * t.declared_contours
        for n in nodes:
            for ic in range(1, t.declared_contours + 1):
                if not seen.get((n, ic), False):
                    missing += 1
        missing += t.malformed_lines
    return GuardResult(
        _pass(missing == 0), float(missing), 0.0, "missing (node, contour) values"
    )


def guard_sanitised(
    texts: Iterable[str], extra_tokens: Iterable[str] | None = None
) -> GuardResult:
    findings = []
    for text in texts:
        findings.extend(find_host_tokens(text, extra_tokens))
    return GuardResult(
        _pass(not findings), float(len(findings)), 0.0, "; ".join(findings[:3])
    )


def guard_units(
    tables: Mapping[int, CintTable],
    reactions: Mapping[int, ReactionRecord],
    fronts: Mapping[int, list[dict]],
) -> GuardResult:
    problems = []
    for lvl, t in sorted(tables.items()):
        for k, v in EXPECTED_BASE_UNITS.items():
            if t.units.get(k) != v:
                problems.append(f"L{lvl} CINT {k}={t.units.get(k)}")
        if t.units.get("K") != RAW_K_UNIT:
            problems.append(f"L{lvl} CINT K={t.units.get('K')}")
        if t.units.get("J") != J_UNIT:
            problems.append(f"L{lvl} CINT J={t.units.get('J')}")
    for lvl, r in sorted(reactions.items()):
        for k, v in EXPECTED_BASE_UNITS.items():
            if r.units.get(k) != v:
                problems.append(f"L{lvl} reactions {k}={r.units.get(k)}")
    # conversion applied exactly once: record K * sqrt(1000) == raw K
    for lvl, front in sorted(fronts.items()):
        raw = {(r.node, r.contour): r.k1 for r in tables[lvl].rows}
        for node in front:
            for c in node["contours"]:
                rk = raw.get((node["node"], c["contour"]))
                if rk is None or c["K1"] is None:
                    continue
                if not math.isclose(c["K1"] / K_RAW_TO_SI, rk, rel_tol=1e-12):
                    problems.append(f"L{lvl} node {node['node']}: K not converted once")
                    break
    return GuardResult(
        _pass(not problems), float(len(problems)), 0.0, "; ".join(problems[:3])
    )


NOT_APPLICABLE_DETAIL = "not applicable: the model has no crack front"


def _not_applicable() -> GuardResult:
    return GuardResult("not_applicable", None, None, NOT_APPLICABLE_DETAIL)


def single_mesh_not_applicable() -> GuardResult:
    """Guard (b) of a single-density run (the limit-load sensitivity)."""
    return GuardResult(
        "not_applicable", None, None,
        "not applicable: single mesh density (limit-load sensitivity run)",
    )


def evaluate_guards(
    cint_texts: Mapping[int, str],
    reac_texts: Mapping[int, str],
    *,
    crack_depth_mm: float = 2.0,
    crack_half_length_mm: float = 4.0,
    front_geometry: Mapping | None = None,
    extra_tokens: Iterable[str] | None = None,
    cracked: bool = True,
) -> dict[str, GuardResult]:
    """Evaluate guards (a)-(f) from the solved output of two mesh densities.

    ``cracked=False`` (uncracked global model, limit-load run): there is no
    crack front, so (c) and (d) are ``not_applicable``; (a), (b), (e) and (f)
    are evaluated on the reaction output.
    """
    geometry = front_geometry or {
        "type": "ellipse",
        "a": crack_depth_mm,
        "c": crack_half_length_mm,
    }
    reactions = {lvl: parse_reaction_file(t) for lvl, t in reac_texts.items()}
    tokens = None if extra_tokens is None else tuple(extra_tokens)
    if not cracked:
        return {
            "a_equilibrium": guard_equilibrium(reactions),
            "b_mesh_load": guard_mesh_load(reactions),
            "c_contour": _not_applicable(),
            "d_complete": _not_applicable(),
            "e_sanitised": guard_sanitised(list(reac_texts.values()), tokens),
            "f_units": guard_units({}, reactions, {}),
            "g_j_mesh": _not_applicable(),
            LEGACY_GUARD: _not_applicable(),
            END_NODE_RECORD: _not_applicable(),
        }
    tables = {lvl: parse_cint_table(t) for lvl, t in cint_texts.items()}
    fronts = {
        lvl: front_records(t, front_geometry=geometry) for lvl, t in tables.items()
    }
    g_mesh, g_record = guard_j_mesh_for(fronts, geometry)
    return {
        "a_equilibrium": guard_equilibrium(reactions),
        "b_mesh_load": guard_mesh_load(reactions),
        "c_contour": guard_contour(fronts),
        "d_complete": guard_complete(tables),
        "e_sanitised": guard_sanitised(
            list(cint_texts.values()) + list(reac_texts.values()), tokens
        ),
        "f_units": guard_units(tables, reactions, fronts),
        "g_j_mesh": g_mesh,
        LEGACY_GUARD: guard_contour_legacy(fronts),
        END_NODE_RECORD: g_record,
    }


# --------------------------------------------------------------------------- #
# Host-free records, CSV and receipt checks
# --------------------------------------------------------------------------- #
def reaction_record_dict(reac: ReactionRecord) -> dict:
    """Host-free dictionary of a parsed reaction file (receipt ``reactions``)."""
    return {
        "stress_mpa": reac.stress_mpa,
        "loaded_area_mm2": reac.loaded_area_mm2,
        "reaction_sum_n": reac.reaction_sum,
        "fz_ligament_n": reac.fz_ligament,
        "fx_symmetry_n": reac.fx_symmetry,
        "fy_point_n": reac.fy_point,
        "n_ligament_nodes": reac.n_ligament,
        "extras": dict(sorted(reac.extras.items())),
    }


def build_mesh_record(
    *,
    level: int,
    cint_text: str | None,
    reac_text: str,
    crack_depth_mm: float | None = None,
    crack_half_length_mm: float | None = None,
    front_geometry: Mapping | None = None,
) -> dict:
    """Host-free record of one solved mesh density (K in MPa*sqrt(m)).

    ``cint_text=None`` (uncracked model): no front, zero declared nodes.
    """
    reac = parse_reaction_file(reac_text)
    geometry = front_geometry or {
        "type": "ellipse",
        "a": crack_depth_mm,
        "c": crack_half_length_mm,
    }
    if cint_text is None:
        return {
            "level": level,
            "declared_front_nodes": 0,
            "declared_contours": 0,
            "reactions": reaction_record_dict(reac),
            "front": [],
        }
    table = parse_cint_table(cint_text)
    return {
        "level": level,
        "declared_front_nodes": table.declared_front_nodes,
        "declared_contours": table.declared_contours,
        "reactions": reaction_record_dict(reac),
        "front": front_records(table, front_geometry=geometry),
    }


def front_csv(record: dict) -> str:
    """Flat CSV of one mesh record: one row per front node and contour."""
    buf = io.StringIO()
    w = csv.writer(buf, lineterminator="\n")
    w.writerow(["node", "phi_deg", "contour", "K1_MPa_sqrt_m", "K2_MPa_sqrt_m",
                "K3_MPa_sqrt_m", "J_N_per_mm"])
    for node in record["front"]:
        for c in node["contours"]:
            w.writerow([node["node"], node["phi_deg"], c["contour"], c["K1"], c["K2"],
                        c["K3"], c["J"]])
    return buf.getvalue()


_REQUIRED_TOP = {
    "schema": str,
    "state": str,
    "issue": int,
    "kind": str,
    "spec": dict,
    "units": dict,
    "meshing": dict,
    "run": dict,
    "meshes": list,
    "primary_level": int,
    "guards": dict,
}
_REQUIRED_RUN = {
    "producing_commit": str,
    "generator_tree_clean": bool,
    "generator_files": dict,
    "mapdl_version": str,
    "cores": int,
    "platform": str,
}
_REQUIRED_MESH = {
    "level": int,
    "deck_sha256": str,
    "run": dict,
    "artifacts": dict,
    "declared_front_nodes": int,
    "declared_contours": int,
    "reactions": dict,
    "front": list,
}
_REQUIRED_MESH_RUN = {"argv": list, "mapdl_version": str, "solve_seconds": (int, float)}
# kind-specific top-level sections
_REQUIRED_BY_KIND = {
    "verification": {"comparator": dict, "front_geometry": dict},
    "weldolet_uncracked": {"sigma_ref": dict, "plausibility": dict},
    "weldolet_crack": {"crack": dict, "sigma_ref": dict, "front_geometry": dict},
    "limit_load": {"limit_load": dict},
}
CRACKED_KINDS = ("verification", "weldolet_crack")


def _check_fields(obj: dict, spec: dict, where: str, problems: list[str]) -> None:
    for key, typ in spec.items():
        if key not in obj:
            problems.append(f"{where}: missing '{key}'")
            continue
        val = obj[key]
        types = typ if isinstance(typ, tuple) else (typ,)
        bad_bool = isinstance(val, bool) and bool not in types
        if not isinstance(val, types) or bad_bool:
            names = "/".join(t.__name__ for t in types)
            problems.append(f"{where}: '{key}' is not {names}")


def validate_receipt_schema(receipt: dict) -> list[str]:
    """Structural validation of a v2 receipt (empty list = valid)."""
    problems: list[str] = []
    if not isinstance(receipt, dict):
        return ["receipt is not an object"]
    _check_fields(receipt, _REQUIRED_TOP, "receipt", problems)
    if problems:
        return problems
    if receipt["schema"] != RECEIPT_SCHEMA_ID:
        problems.append(f"receipt: schema is not {RECEIPT_SCHEMA_ID}")
    kind = receipt["kind"]
    if kind not in RECEIPT_KINDS:
        problems.append(f"receipt: unknown kind {kind!r}")
        return problems
    _check_fields(receipt, _REQUIRED_BY_KIND[kind], "receipt", problems)
    _check_fields(receipt["run"], _REQUIRED_RUN, "run", problems)
    if not re.fullmatch(r"[0-9a-f]{40}", str(receipt["run"].get("producing_commit", ""))):
        problems.append("run: producing_commit is not a 40-hex SHA")
    for path, blob in receipt["run"].get("generator_files", {}).items():
        if not re.fullmatch(r"[0-9a-f]{40}", str(blob)):
            problems.append(f"run.generator_files[{path}]: not a 40-hex blob id")
    if not receipt["run"].get("generator_files"):
        problems.append("run: generator_files is empty")
    units = receipt["units"]
    if units.get("length") != "mm" or units.get("force") != "N":
        problems.append("units: expected length=mm and force=N")
    if kind in CRACKED_KINDS and units.get("k") != K_UNIT:
        problems.append("units: expected k=MPa*sqrt(m)")
    levels = []
    for i, mesh in enumerate(receipt["meshes"]):
        where = f"meshes[{i}]"
        if not isinstance(mesh, dict):
            problems.append(f"{where}: not an object")
            continue
        _check_fields(mesh, _REQUIRED_MESH, where, problems)
        if isinstance(mesh.get("run"), dict):
            _check_fields(mesh["run"], _REQUIRED_MESH_RUN, f"{where}.run", problems)
        if not re.fullmatch(r"[0-9a-f]{64}", str(mesh.get("deck_sha256", ""))):
            problems.append(f"{where}: deck_sha256 is not a 64-hex digest")
        arts = mesh.get("artifacts", {}) if isinstance(mesh.get("artifacts"), dict) else {}
        needed = ("reac", "cint") if kind in CRACKED_KINDS else ("reac",)
        for name in needed:
            if name not in arts:
                problems.append(f"{where}.artifacts: missing '{name}'")
        for name, art in arts.items():
            if not isinstance(art, dict) or not re.fullmatch(
                r"[0-9a-f]{64}", str(art.get("sha256", ""))
            ) or not isinstance(art.get("path"), str):
                problems.append(f"{where}.artifacts.{name}: needs path and sha256")
        levels.append(mesh.get("level"))
        for j, node in enumerate(mesh.get("front", [])):
            for key in ("node", "phi_deg", "contours", "K1_reported"):
                if key not in node:
                    problems.append(f"{where}.front[{j}]: missing '{key}'")
        if kind in CRACKED_KINDS and not mesh.get("front"):
            problems.append(f"{where}: cracked model with an empty front")
    if kind == "limit_load":
        if len(levels) not in (1, 2):
            problems.append("meshes: one or two mesh densities required")
    elif len(levels) != 2:
        problems.append("meshes: exactly two mesh densities required")
    if receipt["primary_level"] not in levels:
        problems.append("primary_level is not one of the mesh levels")
    missing_guards = set(GUARD_NAMES) - set(receipt["guards"])
    if missing_guards:
        problems.append(f"guards: missing {sorted(missing_guards)}")
    for name, g in receipt["guards"].items():
        if not isinstance(g, dict) or g.get("status") not in GUARD_STATUSES:
            problems.append(f"guards.{name}: status must be one of {GUARD_STATUSES}")
    if kind == "verification":
        for key in ("deepest", "surface"):
            if key not in receipt["comparator"]:
                problems.append(f"comparator: missing '{key}'")
    return problems


def evaluate_receipt_guards(
    receipt: dict, extra_tokens: Iterable[str] | None = None
) -> dict[str, GuardResult]:
    """Re-evaluate the guards from the solved data recorded in a receipt.

    (a), (b) and (c) are recomputed from the recorded reactions and per-contour
    values; (d) from the declared counts; (e) on the receipt text; (f) from the
    recorded unit tags. Models without a crack front report (c) and (d) as
    ``not_applicable``.
    """
    cracked = receipt.get("kind", "verification") in CRACKED_KINDS
    reactions = {}
    fronts = {}
    missing = 0
    for mesh in receipt["meshes"]:
        r = mesh["reactions"]
        reactions[mesh["level"]] = ReactionRecord(
            units=dict(EXPECTED_BASE_UNITS),
            stress_mpa=r.get("stress_mpa"),
            loaded_area_mm2=r.get("loaded_area_mm2"),
            fz_ligament=r.get("fz_ligament_n"),
            reaction_sum=r.get("reaction_sum_n", r.get("fz_ligament_n")),
        )
        fronts[mesh["level"]] = mesh["front"]
        if len(mesh["front"]) != mesh["declared_front_nodes"]:
            missing += 1
        for node in mesh["front"]:
            if len(node["contours"]) != mesh["declared_contours"]:
                missing += 1
            for c in node["contours"]:
                if any(c.get(k) is None for k in ("K1", "K2", "K3", "J")):
                    missing += 1
    units = receipt["units"]
    unit_ok = all(units.get(k) == v for k, v in EXPECTED_BASE_UNITS.items())
    if cracked:
        unit_ok = unit_ok and (
            units.get("k") == K_UNIT
            and units.get("k_raw") == RAW_K_UNIT
            and units.get("j") == J_UNIT
            and math.isclose(
                float(units.get("k_conversion_factor", 0.0)), K_RAW_TO_SI, rel_tol=1e-12
            )
        )
    single = receipt.get("kind") == "limit_load" and len(reactions) == 1
    if cracked:
        g_mesh, g_record = guard_j_mesh_for(fronts, receipt.get("front_geometry"))
    else:
        g_mesh, g_record = _not_applicable(), _not_applicable()
    return {
        "a_equilibrium": guard_equilibrium(reactions),
        "b_mesh_load": single_mesh_not_applicable() if single else guard_mesh_load(reactions),
        "c_contour": guard_contour(fronts) if cracked else _not_applicable(),
        "d_complete": (
            GuardResult(_pass(missing == 0), float(missing), 0.0, "recorded")
            if cracked
            else _not_applicable()
        ),
        "e_sanitised": guard_sanitised(
            [json.dumps(receipt, sort_keys=True)], extra_tokens
        ),
        "f_units": GuardResult(_pass(unit_ok), None, None, "recorded unit tags"),
        "g_j_mesh": g_mesh,
        LEGACY_GUARD: guard_contour_legacy(fronts) if cracked else _not_applicable(),
        END_NODE_RECORD: g_record,
    }
